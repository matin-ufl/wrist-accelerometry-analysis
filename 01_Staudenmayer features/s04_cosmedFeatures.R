# This script is for constructing Cosmed Features (like MET) separately.
# Because they are normally calculated much faster than accelerometry features, and should be debugged separately.
# Feb 29, 2016 --- matin@cise.ufl.edu

setwd("~/Workspaces/R workspace/Wrist Accelerometry Analysis/wrist-accelerometry-analysis/01_Staudenmayer features/")

library(utils)
library(stringr)
clr <- function(){cat(rep("\n", 50))}

outputFileName <- "~/Dropbox/Work-Research/Current Directory/Chores/Datasets/Outputs/cosmed_features_022916_missing.csv"

# Only for the first time!
clr()
print("Are you sure you want to initialize your output file? This will overwrite the existing file.")
cosmed_rows <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(cosmed_rows) <- c("MET", "participant", "task")
write.table(cosmed_rows, file = outputFileName, append = F, col.names = T, row.names = F, sep = ",")

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
setwd("~/../../Volumes/aging/SHARE/ARRC/Active_Studies/CHORES-XL_087-2013/Participant Data/TAPA04404132015/")

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

cosmed_rows <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(cosmed_rows) <- c("MET", "participant", "task")
for(t in 1:nrow(info.table)) {
     if(!is.na(info.table$Cosmed.Start.Time[t]) &&
        !is.na(info.table$Cosmed.End.Time[t]) &&
        !is.na(info.table$Visit.Date[t])) {
          taskCosmedData <- data.frame()
          curr_cosmed <- data.frame(MET = NA, participant = participantCode, task = info.table$Task[t])
          taskTimeInfo <- info.table[t, ]
          visit <- toupper(str_trim(unlist(strsplit(as.character(taskTimeInfo$Visit.Date), ":"))[1], side = "both"))
          if(visit == "V1") {
               taskCosmedData <- giveRightCosmedData(cosmedData.v1, taskTimeInfo$Cosmed.Start.Time, taskTimeInfo$Cosmed.End.Time)
          } else if(visit == "V2") {
               taskCosmedData <- giveRightCosmedData(cosmedData.v2, taskTimeInfo$Cosmed.Start.Time, taskTimeInfo$Cosmed.End.Time)
          } else if(visit == "V3") {
               taskCosmedData <- giveRightCosmedData(cosmedData.v3, taskTimeInfo$Cosmed.Start.Time, taskTimeInfo$Cosmed.End.Time)
          } else if(visit == "V4") {
               taskCosmedData <- giveRightCosmedData(cosmedData.v4, taskTimeInfo$Cosmed.Start.Time, taskTimeInfo$Cosmed.End.Time)
          } else {
               print(paste("Unknown visit for ", taskTimeInfo$Task, ": ", taskTimeInfo$Visit.Date, sep = ""))
          }
          
          if(length(taskCosmedData) > 0) {
               cosmedFeatures <- giveCosmedFeatures(taskCosmedData, visualize = T, plot.title = curr_cosmed$task)
               if(length(cosmedFeatures) > 0) {
                    curr_cosmed$MET <- cosmedFeatures$MET
                    cosmed_rows <- rbind(cosmed_rows, curr_cosmed)
               }
          }
     }
     taskCosmedData <- data.frame()
     rm(cosmedFeatures, curr_cosmed)
}

write.table(cosmed_rows, file = outputFileName, append = T, col.names = F, row.names = F, sep = ",")

rm(list = ls())





