# This script loops over all the tasks defined in 

# Parameter initializations and global data -------------------------------
setwd("~/Workspaces/R workspace/Wrist Accelerometry Analysis/wrist-accelerometry-analysis/")
library(utils)
library(stringr)
clr <- function(){cat(rep("\n", 50))}

outputFileName <- "~/Dropbox/Work-Research/Current Directory/Chores/Datasets/Outputs/task_wrist_dataset_022616.csv"

# Only for the first time!
clr()
print("Are you sure you want to initialize your output file? This will overwrite the existing file.")
features_rows <- data.frame(matrix(nrow = 0, ncol = 15))
colnames(features_rows) <- c("mvm", "sdvm", "p625", "df", "fpdf", "mangle", "sdangle", "MET", "sedentary", "locomotion", "participant", "age", "BMI", "gender", "task")
write.table(features_rows, file = outputFileName, append = F, col.names = T, row.names = F, sep = ",")

# Reading meta information
# >>>> Participants Meta Info / Chores_Participants.csv
meta.df <- read.csv(file.choose())
ordered.idx <- with(meta.df, order(meta.df$Age, decreasing = F))
meta.df <- meta.df[ordered.idx, ]
rm(ordered.idx)



#
#
# START FROM HERE FOR EVERY PARTICIPANT
#
#
# Temporary - For easier file selection
setwd("~/../../Volumes/aging/SHARE/ARRC/Active_Studies/CHORES-XL_087-2013/Participant Data/ERPU08907282015/")


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

colnames(info.table) <- c("Task", "Cosmed.Start.Time", "Cosmed.End.Time", "Phone.Start.Time", "Phone.End.Time", "Visit.Date")
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
setwd("~/Workspaces/R workspace/Wrist Accelerometry Analysis/wrist-accelerometry-analysis/")
source("01_Staudenmayer features/func01_accelerometerFeatures.R")
source("01_Staudenmayer features/func02_allFeatures_oneTask.R")

features_rows <- data.frame(matrix(nrow = 0, ncol = 15))
colnames(features_rows) <- c("mvm", "sdvm", "p625", "df", "fpdf", "mangle", "sdangle", "MET", "sedentary", "locomotion", "participant", "age", "BMI", "gender", "task")
for(t in 30:nrow(info.table)) {
     if(!is.na(info.table$Cosmed.Start.Time[t]) &&
        !is.na(info.table$Cosmed.End.Time[t]) &&
        !is.na(info.table$Phone.Start.Time[t]) &&
        !is.na(info.table$Phone.End.Time[t]) &&
        !is.na(info.table$Visit.Date[t])) {
          taskTimeInfo <- info.table[t, ]
          visit <- toupper(str_trim(unlist(strsplit(as.character(taskTimeInfo$Visit.Date), ":"))[1], side = "both"))
          if(visit == "V1") {
               features_row <- main(participant.metaInfo, taskTimeInfo, wristData.v1, cosmedData.v1, visualize = TRUE)
               # Writing into a file
               if(length(which(is.na(features_row))) < 1) {
                    features_rows <- rbind(features_rows, features_row)
               }
          } else if(visit == "V2") {
               features_row <- main(participant.metaInfo, taskTimeInfo, wristData.v2, cosmedData.v2, visualize = TRUE)
               # Writing into a file
               if(length(which(is.na(features_row))) < 1) {
                    features_rows <- rbind(features_rows, features_row)
               }
          } else if(visit == "V3") {
               features_row <- main(participant.metaInfo, taskTimeInfo, wristData.v3, cosmedData.v3, visualize = TRUE)
               # Writing into a file
               if(length(which(is.na(features_row))) < 1) {
                    features_rows <- rbind(features_rows, features_row)
               }
          } else if(visit == "V4") {
               features_row <- main(participant.metaInfo, taskTimeInfo, wristData.v4, cosmedData.v4, visualize = TRUE)
               # Writing into a file
               if(length(which(is.na(features_row))) < 1) {
                    features_rows <- rbind(features_rows, features_row)
               }
          } else {
               print(paste("Unknown visit for ", taskTimeInfo$Task, ": ", taskTimeInfo$Visit.Date, sep = ""))
          }
     }
}
write.table(features_rows, file = outputFileName, append = T, col.names = F, row.names = F, sep = ",")

rm(list = ls())
