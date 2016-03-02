
# This script is to merge data constructed for ChoresXL participants.
# Just a place holder for the steps to make sure the dataset constructed, contains no serious bug.
# This script should not be used automatically, and every step should be checked manually.
setwd("~/Workspaces/R workspace/Wrist Accelerometry Analysis/wrist-accelerometry-analysis/03_Merging and Cleaning Variable Dataset/")

load("~/Dropbox/Work-Research/Current Directory/Chores/Datasets/V1 Under 50/d02_under50.RData")


metValues <- read.csv("~/Dropbox/Work-Research/Current Directory/Chores/Datasets/Outputs/cosmed_features_022916.csv")
taskNames <- c("COMPUTER WORK",
               "DIGGING",
               "DRESSING",
               "HEAVY LIFTING",
               "IRONING",
               "LAUNDRY WASHING",
               "LEISURE WALK",
               "LIGHT GARDENING",
               "LIGHT HOME MAINTENANCE",
               "MOPPING",
               "PERSONAL CARE",
               "PREPARE SERVE MEAL",
               "RAPID WALK",
               "REPLACING SHEETS ON A BED",
               "SHOPPING",
               "STAIR ASCENT",
               "STAIR DESCENT",
               "STANDING STILL",
               "STRAIGHTENING UP DUSTING",
               "STRENGTH EXERCISE CHEST PRESS",
               "STRENGTH EXERCISE LEG CURL",
               "STRENGTH EXERCISE LEG EXTENSION",
               "STRETCHING YOGA",
               "SWEEPING",
               "TRASH REMOVAL",
               "TREADMILL TEST",
               "TV WATCHING",
               "UNLOADING STORING DISHES",
               "VACUUMING",
               "WALKING AT RPE 1",
               "WALKING AT RPE 5",
               "WASHING DISHES",
               "WASHING WINDOWS",
               "YARD WORK")

# To change task names
metValues <- rbind(metValues, extra.cosmed.df)
tasks <- metValues$task
levels(tasks)
# Follow this part for all levels in tasks
currLevel <- 1
correctLevel <- 1
levels(tasks)[currLevel]
taskNames[correctLevel]
idx <- which(metValues$task == levels(tasks)[currLevel]) # Computer Work >>> COMPUTER WORK
tasks[idx] <- taskNames[correctLevel]

tasks <- as.character(tasks)
metValues$task <- tasks
write.csv(metValues, file = "~/Dropbox/Work-Research/Current Directory/Chores/Datasets/Outputs/cosmed_features_022916.csv", row.names = F)
rm(correctLevel, currLevel, idx, taskNames, tasks)

# Correct MET values
MET <- data.frame(MET = rep(NA, nrow(chores.u50.df)))
for (i in 1:nrow(chores.u50.df)) {
     participant <- as.character(chores.u50.df$participant[i])
     task <- as.character(chores.u50.df$task[i])
     print(paste(participant, task, sep = " - "))
     participant.met.tasks <- metValues[metValues$participant == participant, ]
     MET$MET[i] <- participant.met.tasks$MET[participant.met.tasks$task == task]
}

chores.u50.df$MET <- MET$MET
save(chores.u50.df, file = "~/Dropbox/Work-Research/Current Directory/Chores/Datasets/V1 Under 50/d02_under50.RData")

# Removing duplicate participants
participants <- chores.u50.df$participant # Checked: Just DIWI034 should be removed.
dupIdx <- which(participants == levels(participants)[3])[1:21]
chores.u50.df <- chores.u50.df[-dupIdx, ]
chores.u50.df$participant <- factor(as.character(chores.u50.df$participant))
levels(chores.u50.df$participant)
save(chores.u50.df, file = "~/Dropbox/Work-Research/Current Directory/Chores/Datasets/V1 Under 50/d02_under50.RData")


# Checking to make sure values for features actually make sense
for (t in 1:length(levels(chores.u50.df$task))) {
     task <- levels(chores.u50.df$task)[t]
     for (f in 1:8) {
          values <- chores.u50.df[chores.u50.df$task == task, f]
          print(paste(as.character(task), "<<<", colnames(chores.u50.df)[f], ">>> MIN: ", round(min(values), digits = 2), " - Max: ", round(max(values), digits = 2), " ### AVG (SD): ", round(mean(values), digits = 2), " (", round(sd(values), digits = 2), ")", sep = ""))
     }
}

for (t in 1:length(levels(chores.u50.df$task))) {
     task <- levels(chores.u50.df$task)[t]
     f <- 8
          plot.df <- data.frame(variable = chores.u50.df[chores.u50.df$task == task, f])
          g <- ggplot(data = plot.df) + geom_boxplot(aes(x = "Variable", y = variable), colour = "red", fill = "blue", alpha = 0.7)
          print(g + labs(x = colnames(chores.u50.df)[f], title = as.character(task)))
     
}





