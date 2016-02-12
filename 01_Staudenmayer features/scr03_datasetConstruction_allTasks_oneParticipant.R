# This script loops over all the tasks defined in 

# Parameter initializations and global data -------------------------------
setwd("~/Dropbox/Work-Research/Current Directory/Chores/wrist-accelerometry-analysis/")
library(utils)
library(stringr)
clr <- function(){cat(rep("\n", 50))}

outputFileName <- "../Datasets/Outputs/task_wrist_dataset_021116.csv"

# Only for the first time!
clr()
print("Are you sure you want to initialize your output file? This will overwrite the existing file.")
temp_row = data.frame(mvm = NA, sdvm = NA, p625 = NA, df = NA, fpdf = NA, mangle = NA, sdangle = NA, MET = NA, sedentary = NA, locomotion = NA, participant = NA, task = NA, age = NA, BMI = NA, gender = NA)
write.table(temp_row, file = outputFileName, append = F, col.names = T, row.names = F, sep = ",")
rm(temp_row)

# Reading meta information
meta.df <- read.csv("../Datasets/Participants Meta Info/under45_metaInfo.csv")


# Temporary - For easier file selection
setwd("~/../../Volumes/FILES/AGING/SHARE/ARRC/Active_Studies/CHORES-XL_087-2013/Participant Data/Matin01_Under 45/")


# Selecting Raw Data Files ----------------------------------------------
# Task Times 
# Choose task times
clr()
print("Select Task Times Now")
fileName <- file.choose()
info.table <- read.table(file = fileName, sep = ",", header = F)

participantCode <- unlist(strsplit(fileName, split = "/"))
participantCode <- unlist(strsplit(participantCode[length(participantCode)], split = "_"))
participantCode <- strtrim(participantCode[2], width = 7)
clr()
print("Check [info.table]. If necessary remove the first row.")

colnames(info.table) <- c("Task", "Cosmed.Start.Time", "Cosmed.End.Time", "Phone.start.time", "Phone.End.Time", "Visit.Date")
# info.table <- info.table[-1, ]

rm(fileName)

participant.metaInfo <- data.frame(participantCode = participantCode, age = meta.df$Age[meta.df$ID == participantCode], BMI = meta.df$BMI[meta.df$ID == participantCode], gender = meta.df$Gender[meta.df$ID == participantCode])

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


# Feature Construction: For all the tasks -------------------------------
setwd("~/Dropbox/Work-Research/Current Directory/Chores/wrist-accelerometry-analysis/")
source("01_Staudenmayer features/func01_accelerometerFeatures.R")
source("01_Staudenmayer features/func02_allFeatures_oneTask.R")

for(t in 1:nrow(info.table)) {
     if(!is.na(info.table$Cosmed.Start.Time[t]) &&
        !is.na(info.table$Cosmed.End.Time[t]) &&
        !is.na(info.table$Phone.start.time[t]) &&
        !is.na(info.table$Phone.End.Time[t]) &&
        !is.na(info.table$Visit.Date[t])) {
          taskTimeInfo <- info.table[t, ]
          visit <- toupper(str_trim(unlist(strsplit(as.character(taskTimeInfo$Visit.Date), ":"))[1], side = "both"))
          if(visit == "V1") {
               features_row <- main(participant.metaInfo, taskTimeInfo, wristData.v1, cosmedData.v1, visualize = TRUE)
               # Writing into a file
               if(length(which(is.na(features_row))) < 1) {
                    write.table(features_row, file = outputFileName, append = T, col.names = F, row.names = F, sep = ",")
               }
          } else if(visit == "V2") {
               features_row <- main(participant.metaInfo, taskTimeInfo, wristData.v2, cosmedData.v2, visualize = TRUE)
               # Writing into a file
               if(length(which(is.na(features_row))) < 1) {
                    write.table(features_row, file = outputFileName, append = T, col.names = F, row.names = F, sep = ",")
               }
          } else if(visit == "V3") {
               features_row <- main(participant.metaInfo, taskTimeInfo, wristData.v3, cosmedData.v3, visualize = TRUE)
               # Writing into a file
               if(length(which(is.na(features_row))) < 1) {
                    write.table(features_row, file = outputFileName, append = T, col.names = F, row.names = F, sep = ",")
               }
          } else if(visit == "V4") {
               features_row <- main(participant.metaInfo, taskTimeInfo, wristData.v4, cosmedData.v4, visualize = TRUE)
               # Writing into a file
               if(length(which(is.na(features_row))) < 1) {
                    write.table(features_row, file = outputFileName, append = T, col.names = F, row.names = F, sep = ",")
               }
          } else {
               print(paste("Unknown visit for ", taskTimeInfo$Task, ": ", taskTimeInfo$Visit.Date, sep = ""))
          }
     }
}


rm(list = ls())
