#####Discrepencies between task times and files#####
convertTo_HH_MM_SS <- function(timeStr) {
     timeStr <- unlist(strsplit(timeStr, ":"))
     h <- 0; m <- 0; s <- as.numeric(timeStr[2])
     h <- floor(as.numeric(timeStr[1]) / 60)
     m <- as.numeric(timeStr[1]) - (h * 60)
     h <- as.character(h); while(nchar(h) < 2) {h <- paste("0", h, sep = "")}
     m <- as.character(m); while(nchar(m) < 2) {m <- paste("0", m, sep = "")}
     s <- as.character(s); while(nchar(s) < 2) {s <- paste("0", s, sep = "")}
     paste(h, ":", m, ":", s, sep = "")
}

convertTo_militaryTime <- function(timeStr) {
     str <- unlist(strsplit(timeStr, ":"))
     h <- str[1]
     if(as.numeric(str[1]) < 7) {
          h <- as.character(as.numeric(str[1]) + 12)
     }
     if(nchar(h) < 2) {
          h <- paste("0", h, sep = "")
     }
     paste(h, str[2], str[3], sep = ":")
}

#####Main Function#####
main <- function(participantCode, taskTimeInfo, wristAccelData, cosmedFileData, visualize = TRUE) {
     result <- tryCatch({
          participant_task_features(participantCode, taskTimeInfo, wristAccelData, cosmedFileData, visualize)
     },
     error=function(cond) {
          message(paste("An error occured for:", taskTimeInfo$Task, "(", participantCode, ")"))
          message("Here's the original error message:")
          message(paste(cond, "\n"))
          # Choose a return value in case of error
          return(NA)
     },
     #warning=function(cond) {
     #     message(paste("Warning:", taskTimeInfo$Task, "(", participantCode, ")"))
     #     message("Here's the original warning message:")
     #     message(cond)
     #     print()
          # Choose a return value in case of warning
          #return(NULL)
     #},
     finally={
          # NOTE:
          # Here goes everything that should be executed at the end,
          # regardless of success or error.
          # If you want more than one expression to be executed, then you 
          # need to wrap them in curly brackets ({...}); otherwise you could
          # just have written 'finally=<expression>' 
          
          #message(paste("Processed URL:", url))
          #message("Some other message at the end")
     }
     )    
     return(result)
}

participant_task_features <- function(participant.metaInfo, taskTimeInfo, wristAccelData, cosmedFileData, visualize = TRUE) {
     temp <- str_trim(taskTimeInfo$Phone.Start.Time, side = "both")
     temp <- unlist(strsplit(temp, split = " "))[1]
     startTime <- paste(as.character(temp), ".000", sep = "")
     temp <- str_trim(taskTimeInfo$Phone.End.Time, side = "both")
     temp <- unlist(strsplit(temp, split = " "))[1]
     endTime <- paste(as.character(temp), ".000", sep = "")
     
     #####Accelerometer Analysis####
     wristData <- wristAccelData
     wristData$Timestamp <- as.character(wristData$Timestamp)
     timeStamps <- lapply(wristData$Timestamp, function(str) {
          result <- unlist(strsplit(str, split = " "))
          result[2]
     })
     
     wristData$Timestamp <- unlist(timeStamps)
     rm(timeStamps)
     
     startIdx <- which(wristData$Timestamp == convertTo_militaryTime(startTime))
     endIdx <- which(wristData$Timestamp == convertTo_militaryTime(endTime))
     wristData <- wristData[startIdx:endIdx, ]
     participant_row <- constructFeatures(wristData$Accelerometer.X, wristData$Accelerometer.Y, wristData$Accelerometer.Z)
     
     #####Cosmed Analysis####
     participant_row$MET <- NA
     participant_row$sedentary <- FALSE
     participant_row$locomotion <- FALSE
     participant_row$participant <- participant.metaInfo$participantCode
     participant_row$age <- participant.metaInfo$age
     participant_row$BMI <- participant.metaInfo$BMI
     participant_row$gender <- participant.metaInfo$gender
     participant_row$task <- taskTimeInfo$Task
     
     cosmedStartTime <- convertTo_HH_MM_SS(as.character(taskTimeInfo$Cosmed.Start.Time))
     cosmedEndTime <- convertTo_HH_MM_SS(as.character(taskTimeInfo$Cosmed.End.Time))
     
     cosmedData <- cosmedFileData
     cosmedData$artificialTime <- strptime(as.character(cosmedData$t), "%T")
     cosmedStartIdx <- min(which(cosmedData$artificialTime > strptime(cosmedStartTime, "%T")))
     cosmedEndIdx <- min(which(cosmedData$artificialTime > strptime(cosmedEndTime, "%T")))
     cosmedData <- cosmedData[cosmedStartIdx:cosmedEndIdx, ]
     cosmedData$VO2.Kg <- as.numeric(as.character(cosmedData$VO2.Kg))
     cosmedData$t <- as.character(cosmedData$t)
     
     res <- findTheBestWindow(cosmedData)
     participant_row$MET <- (mean(cosmedData$VO2.Kg[res$best.start:res$best.end]) / 3.5)
     cosmedData$code <- 1
     cosmedData$code[res$best.start:res$best.end] <- 2
     cosmedData$code <- as.factor(cosmedData$code)
     
     if(visualize) {
          #####Visulaization#####
          library(gridExtra)
          accelData <- data.frame(x = wristData$Accelerometer.X, y = wristData$Accelerometer.Y, z = wristData$Accelerometer.Z)
          accelData$vm <- sqrt((accelData$x * accelData$x) + (accelData$y * accelData$y) + (accelData$z * accelData$z))
          
          idx_plot <- seq(1, length(accelData$vm), by = 100)
          
          g_accel <- ggplot(data = accelData[idx_plot, ]) + theme_grey()
          g_accel <- g_accel + geom_line(aes(x = seq_along(x), y = x, group = "A", color = "X")) +
               geom_line(aes(x = seq_along(y), y = y, group = "A", color = "Y")) +
               geom_line(aes(x = seq_along(x), y = z, group = "A", color = "Z")) +
               geom_line(aes(x = seq_along(x), y = vm, group = "A", color = "VM"))
          g_accel <- g_accel + scale_color_manual(values=c("blue", "green", "red", "purple")) + labs(x = "Time (sec)", y = "Acceleration (G, m/s2)", title = paste(taskTimeInfo$Task, " (", participantCode, ")"))
          
          freqData <- convert.fft(fft(accelData$vm), sample.rate = 100)
          freqData <- freqData[-(ceiling(nrow(freqData)/2):nrow(freqData)), ]
          
          g_freq <- ggplot(data = freqData[seq(2, nrow(freqData), by = 50), ]) + theme_grey()
          g_freq <- g_freq + geom_line(aes(x = freq, y = strength, group = "A", color = "F"))
          g_freq <- g_freq + scale_color_manual(values = c("blue")) + coord_cartesian(xlim = c(0, 20)) + labs(x = "Frequency (Hz)", y = "Strength", title = "Frequency Domain")
          
          g_cosmed <- ggplot(data = cosmedData) + theme_grey()
          g_cosmed <- g_cosmed + geom_line(aes(size = code, x = seq_along(VO2.Kg), y = round(VO2.Kg, digits = 2), color = code, group = "A")) + scale_x_continuous(breaks = seq(1, length(cosmedData$VO2.Kg), 10), labels = cosmedData$t[seq(1, length(cosmedData$VO2.Kg), 10)])
          g_cosmed <- g_cosmed + scale_color_manual(values=c("red", "blue"), labels = c("VO2", "Plateau")) + labs(x = "Time", y = "VO2.Kg", title = "Cosmed")
          
          grid.arrange(g_accel, g_freq, g_cosmed, ncol = 1)
     }
     participant_row
}