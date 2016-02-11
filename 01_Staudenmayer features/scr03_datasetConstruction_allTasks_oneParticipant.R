# This script loops over all the tasks defined in 
setwd("~/Dropbox/Work-Research/Current Directory/Chores/R Scripts/")
library(utils)
clr <- function(){cat(rep("\n", 50))}

outputFileName <- "../Datasets/01_outputs_012516/task_wrist_dataset_012516.csv"
temp_row = data.frame(mvm = NA, sdvm = NA, p625 = NA, df = NA, fpdf = NA, mangle = NA, sdangle = NA, MET = NA, sedentary = NA, locomotion = NA, participant = NA, task = NA)
write.table(temp_row, file = outputFileName, append = F, col.names = T, row.names = F, sep = ",")
rm(temp_row)

#####Task Times####
# Choose task times
clr()
print("Select Task Times Now")
fileName <- file.choose()
participantCode <- unlist(strsplit(fileName, split = "/"))
participantCode <- unlist(strsplit(participantCode[length(participantCode)], split = "_"))
participantCode <- strtrim(participantCode[2], width = 7)
info.table <- read.table(file = fileName, sep = ",", header = T)
rm(fileName)

# Selecting Wrist accelerometer file for V1
clr()
print("Select wrist accelerometer file for V1 (V-ONE)")
fileName <- file.choose()
wristData.v1 <- read.csv(file = fileName, skip = 10)

# Selecting Wrist accelerometer file for V2
clr()
print("Select wrist accelerometer file for V2 (V-TWO)")
fileName <- file.choose()
wristData.v2 <- read.csv(file = fileName, skip = 10)

# Selecting Wrist accelerometer file for V3
clr()
print("Select wrist accelerometer file for V3 (V-THREE)")
fileName <- file.choose()
wristData.v3 <- read.csv(file = fileName, skip = 10)

# Selecting Wrist accelerometer file for V4
clr()
print("Select wrist accelerometer file for V4 (V-FOUR)")
fileName <- file.choose()
wristData.v4 <- read.csv(file = fileName, skip = 10)

# Select COSMED file for v1
clr()
print("Select cosmed file for V1 (V-ONE)")
fileName <- file.choose()
cosmedData.v1 <- read.csv(fileName, colClasses = c(rep("NULL", 9), NA, rep("NULL", 9), NA, rep("NULL", 106)))[-c(1:2), ]

# Select COSMED file for v2
clr()
print("Select cosmed file for V2 (V-TWO)")
fileName <- file.choose()
cosmedData.v2 <- read.csv(fileName, colClasses = c(rep("NULL", 9), NA, rep("NULL", 9), NA, rep("NULL", 106)))[-c(1:2), ]

# Select COSMED file for v3
clr()
print("Select cosmed file for V3 (V-THREE)")
fileName <- file.choose()
cosmedData.v3 <- read.csv(fileName, colClasses = c(rep("NULL", 9), NA, rep("NULL", 9), NA, rep("NULL", 106)))[-c(1:2), ]

# Select COSMED file for v4
clr()
print("Select cosmed file for V4 (V-FOUR)")
fileName <- file.choose()
cosmedData.v4 <- read.csv(fileName, colClasses = c(rep("NULL", 9), NA, rep("NULL", 9), NA, rep("NULL", 106)))[-c(1:2), ]


#####Function Sourcings#####
source("01_Staudenmayer features/func01_accelerometerFeatures.R")
source("01_Staudenmayer features/func02_allFeatures_oneTask.R")

for(t in 1:nrow(info.table)) {
     if(!is.na(info.table$Cosmed.Start.Time[t]) &&
        !is.na(info.table$Cosmed.End.Time[t]) &&
        !is.na(info.table$Phone.start.time[t]) &&
        !is.na(info.table$Phone.End.Time[t]) &&
        !is.na(info.table$Visit.Date[t])) {
          taskTimeInfo <- info.table[t, ]
          visit <- unlist(strsplit(as.character(taskTimeInfo$Visit.Date), ":"))[1]
          if(visit == "V1") {
               features_row <- main(participantCode, taskTimeInfo, wristData.v1, cosmedData.v1, visualize = TRUE)
               # Writing into a file
               if(!is.na(features_row)) {
                    write.table(features_row, file = outputFileName, append = T, col.names = F, row.names = F, sep = ",")
               }
          } else if(visit == "V2") {
               features_row <- main(participantCode, taskTimeInfo, wristData.v2, cosmedData.v2, visualize = TRUE)
               # Writing into a file
               if(!is.na(features_row)) {
                    write.table(features_row, file = outputFileName, append = T, col.names = F, row.names = F, sep = ",")
               }
          } else if(visit == "V3") {
               features_row <- main(participantCode, taskTimeInfo, wristData.v3, cosmedData.v3, visualize = TRUE)
               # Writing into a file
               if(!is.na(features_row)) {
                    write.table(features_row, file = outputFileName, append = T, col.names = F, row.names = F, sep = ",")
               }
          } else if(visit == "V4") {
               features_row <- main(participantCode, taskTimeInfo, wristData.v4, cosmedData.v4, visualize = TRUE)
               # Writing into a file
               if(!is.na(features_row)) {
                    write.table(features_row, file = outputFileName, append = T, col.names = F, row.names = F, sep = ",")
               }
          } else {
               print(paste("Unknown visit for ", taskTimeInfo$Task, ": ", taskTimeInfo$Visit.Date, sep = ""))
          }
     }
}


rm(list = ls())
