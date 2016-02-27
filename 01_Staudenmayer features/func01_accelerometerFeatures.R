#####################################################
### Authour: Matin                                ###
###                                               ###
### Converts time series signal to Frequency      ###
###      cs -> time series data                   ###
###      sample.rate -> sampling rate             ###
### Returns                                       ###
###      df$cycle -> frequency step               ###
###      df$strength -> modulus at each frequency ###
###      df$delay -> delay for each frequency (deg)##
###                                               ###
#####################################################

# Note: half of the frequency domain should be used because of Nyquist effect.

library(stats)
convert.fft <- function(cs, sample.rate = 1) {
     cs <- cs / length(cs) # normalize
     
     distance.center <- function(c) signif(Mod(c), 4)
     angle           <- function(c) signif(180 * Arg(c) / pi, 3)
     
     df <- data.frame(cycle    = 0:(length(cs)-1),
                      freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                      strength = sapply(cs, distance.center),
                      delay    = sapply(cs, angle))
     df
}

p625 <- function(VM, window.length = 1500) {
     temp <- matrix(ncol = 1, nrow = 0)
     for(i in seq(1, (length(VM) - window.length), by = window.length)) {
          VM_freq <- convert.fft(fft(VM[i:(min(i+window.length, length(VM)))]), sample.rate = 100)
          VM_freq <- VM_freq[-(ceiling(nrow(VM_freq)/2):nrow(VM_freq)), ]
          
          idx0_6 <- min(which(signif(VM_freq$freq, digits = 1) == 0.6))
          idx2_5 <- min(which(signif(VM_freq$freq, digits = 2) == 2.5))
          temp <- rbind(temp, sum(VM_freq$strength[idx0_6:idx2_5]) / sum(VM_freq$strength[2:(nrow(VM_freq)-1)]))
     }
     mean(temp, na.rm = T)
}

df <- function(VM, window.length = 1500) {
     temp <- matrix(ncol = 1, nrow = 0)
     for(i in seq(1, (length(VM) - window.length), by = window.length)) {
          VM_freq <- convert.fft(fft(VM[i:(min(i+window.length, length(VM)))]), sample.rate = 100)
          VM_freq <- VM_freq[-(ceiling(nrow(VM_freq)/2):nrow(VM_freq)), ]
          temp_idx <- min(which(VM_freq$strength == max(VM_freq$strength[2:length(VM_freq$strength)])))
          if(length(temp_idx) > 0) {
               idx_max <- temp_idx
          }
          temp <- rbind(temp, VM_freq$freq[idx_max])
     }
     mean(temp, na.rm = T)
}

fpdf <- function(VM, window.length = 1500) {
     temp <- matrix(ncol = 1, nrow = 0)
     for(i in seq(1, (length(VM) - window.length), by = window.length)) {
          VM_freq <- convert.fft(fft(VM[i:(min(i+window.length, length(VM)))]), sample.rate = 100)
          VM_freq <- VM_freq[-(ceiling(nrow(VM_freq)/2):nrow(VM_freq)), ]
          
          idx_max <- which(VM_freq$strength == max(VM_freq$strength[2:length(VM_freq$strength)]))
          indices <- which(round(VM_freq$freq, digits = 1) == round(VM_freq$freq[idx_max], digits = 1))
          temp <- rbind(temp, sum(VM_freq$strength[indices]) / sum(VM_freq$strength[2:(nrow(VM_freq))]))
     }
     mean(temp, na.rm = T)
}

mangle <- function(x, VM, window.length = 1500) {
     temp <- matrix(ncol = 1, nrow = 0)
     for(i in seq(1, length(VM), by = window.length)) {
          angle <- (90 * asin(x[i:(i+window.length)] / VM[i:(i+window.length)])) / (pi/2)
          temp <- rbind(temp, mean(angle))
     }
     mean(temp, na.rm = T)
}

sdangle <- function(x, VM, window.length = 1500) {
     temp <- matrix(ncol = 1, nrow = 0)
     for(i in seq(1, length(VM), by = window.length)) {
          angle <- (90 * asin(x[i:(i+window.length)] / VM[i:(i+window.length)])) / (pi/2)
          temp <- rbind(temp, sd(angle))
     }
     mean(temp, na.rm = T)
}

mvm <- function(VM, window.length = 1500) {
     temp <- matrix(ncol = 1, nrow = 0)
     for(i in seq(1, length(VM), by = window.length)) {
          temp <- rbind(temp, mean(VM[i:(i+window.length)]))
     }
     mean(temp, na.rm = T)
}

sdvm <- function(VM, window.length = 1500) {
     temp <- matrix(ncol = 1, nrow = 0)
     for(i in seq(1, length(VM), by = window.length)) {
          temp <- rbind(temp, sd(VM[i:(i+window.length)]))
     }
     mean(temp, na.rm = T)
}

constructFeatures <- function(x, y, z) {
     VM <- sqrt((x * x) + (y * y) + (z * z))
     result <- data.frame(mvm = mvm(VM), sdvm = sdvm(VM), p625 = 0, df = 0, fpdf = 0, mangle = 0, sdangle = 0)
     result$p625 <- p625(VM)
     result$df <- df(VM)
     result$fpdf <- fpdf(VM)
     result$mangle <- mangle(x, VM)
     result$sdangle <- sdangle(x, VM)
     result
}


CV <- function(x){
     a <- (sd(x) / mean(x)) * 100
     if(is.na(a)) {
          a <- Inf
     }
     a
}

findTheBestWindow <- function(cosmedData) {
     WINDOW_DURATION <- 120 #2-min window
     MIN_WINDOW_THRESHOLD_FOR_TWO_MIN <- 5 * 60 # At least there shold 5 minute to consider a 2 minute window
     
     # Sometimes the task ends so quickly, that 2:30 window is not feasible
     if(as.numeric(cosmedData$artificialTime[nrow(cosmedData)] - cosmedData$artificialTime[1]) < MIN_WINDOW_THRESHOLD_FOR_TWO_MIN) {
          WINDOW_DURATION <- 60
     }
     
     offset <- floor(length(cosmedData$VO2.Kg) / 20)
     cosmedData <- cosmedData[offset:(nrow(cosmedData)-offset), ]
     
     best.start <- 1
     best.end <- best.start + 1
     currIdx <- 1
     endIdx <- currIdx + 1
     best.CV <- Inf
     while(endIdx <= length(cosmedData$VO2.Kg)) {
          endTime <- unlist(as.POSIXlt(cosmedData$artificialTime[currIdx]) + WINDOW_DURATION)[[1]]
          temp <- which(cosmedData$artificialTime > endTime)
          if(length(temp) > 0) {
               endIdx <- min(temp)
               currCV <- CV(cosmedData$VO2.Kg[currIdx:endIdx])
               if(best.CV > currCV) {
                    best.start <- currIdx
                    best.end <- endIdx
                    best.CV <- currCV
               }
          } else {
               break
          }
          currIdx <- currIdx + 1
          endIdx <- currIdx + 1
     }
     data.frame(best.start, best.end)
}