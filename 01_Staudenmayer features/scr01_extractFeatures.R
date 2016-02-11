# One participant
# One task

setwd("~/Dropbox/Work-Research/Current Directory/Chores/R Scripts/")

clr <- function(){cat(rep("\n", 50))}

######Tasks Specifics####
taskName <- "LEISURE WALK" # "VACUUMING", "HEAVY LIFTING", "COMPUTER WORK"
outputFileName <- "../Datasets/01_outputs_012516/task_wrist_dataset_012516.csv" #paste("../Datasets/01_outputs_012516/", taskName, "_012516.csv", sep = "")

#####Task Times####
# Choose task times
clr()
print("Select Task Times Now")
fileName <- file.choose()
participant <- unlist(strsplit(fileName, split = "/"))
participant <- unlist(strsplit(participant[length(participant)], split = "_"))
participant <- strtrim(participant[2], width = 7)
info.table <- read.table(file = fileName, sep = ",", header = T)
rm(fileName)

info <- info.table[info.table$Task == taskName, ]
startTime <- paste(as.character(info$Phone.start.time), ".000", sep = "")
endTime <- paste(as.character(info$Phone.End.Time), ".000", sep = "")

# Selecting Wrist accelerometer file
clr()
print("Select wrist accelerometer file")
fileName <- file.choose()

#####Accleremoter Analysis####
wristData <- read.csv(file = fileName, skip = 10)
rm(fileName)

wristData$Timestamp <- as.character(wristData$Timestamp)
timeStamps <- lapply(wristData$Timestamp, function(str) {
  result <- unlist(strsplit(str, split = " "))
  result[2]
})
wristData$Timestamp <- unlist(timeStamps)
rm(timeStamps)

startIdx <- which(wristData$Timestamp == startTime)
endIdx <- which(wristData$Timestamp == endTime)
wristData <- wristData[startIdx:endIdx, ]

source("01_Staudenmayer features/func01_accelerometerFeatures.R")
participant_row <- constructFeatures(wristData$Accelerometer.X, wristData$Accelerometer.Y, wristData$Accelerometer.Z)


#####Cosmed Analysis####
participant_row$MET <- NA

# Select COSMED file
convertTo_HH_MM_SS <- function(timeStr) {
     timeStr <- unlist(strsplit(timeStr, ":"))
     h <- 0; m <- 0; s <- as.numeric(timeStr[2])
     h <- floor(as.numeric(timeStr[1]) / 60)
     m <- as.numeric(timeStr[1]) - (h * 60)
     paste(h, ":", m, ":", s, sep = "")
}
clr()
print("Select cosmed file")
fileName <- file.choose()
cosmedStartTime <- convertTo_HH_MM_SS(as.character(info$Cosmed.Start.Time))
cosmedEndTime <- convertTo_HH_MM_SS(as.character(info$Cosmed.End.Time))
cosmedData <- read.csv(fileName, colClasses = c(rep("NULL", 9), NA, rep("NULL", 9), NA, rep("NULL", 106)))[-c(1:2), ]
rm(fileName)
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

#####Wrapping up####
# Writing into a file
write.csv(participant_row, file = outputFileName, append = T, col.names = T, row.names = F)

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
g_accel <- g_accel + scale_color_manual(values=c("blue", "green", "red", "purple")) + labs(x = "Time (sec)", y = "", title = "Acceleration")

freqData <- convert.fft(fft(accelData$vm), sample.rate = 100)
freqData <- freqData[-(ceiling(nrow(freqData)/2):nrow(freqData)), ]

g_freq <- ggplot(data = freqData[seq(2, nrow(freqData), by = 50), ]) + theme_grey()
g_freq <- g_freq + geom_line(aes(x = freq, y = strength, group = "A", color = "F"))
g_freq <- g_freq + scale_color_manual(values = c("blue")) + coord_cartesian(xlim = c(0, 20)) + labs(x = "Frequency (Hz)", y = "Strength", title = "Frequency Domain")

g_cosmed <- ggplot(data = cosmedData) + theme_grey()
g_cosmed <- g_cosmed + geom_line(aes(size = code, x = seq_along(VO2.Kg), y = round(VO2.Kg, digits = 2), color = code, group = "A")) + scale_x_continuous(breaks = seq(1, length(cosmedData$VO2.Kg), 10), labels = cosmedData$t[seq(1, length(cosmedData$VO2.Kg), 10)])
g_cosmed <- g_cosmed + scale_color_manual(values=c("red", "blue"), labels = c("VO2", "Plateau")) + labs(x = "Time", y = "VO2.Kg", title = "Cosmed")

grid.arrange(g_accel, g_freq, g_cosmed, ncol = 1)

#####Fin#####
rm(list = ls())
