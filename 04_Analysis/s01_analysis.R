# Should follow the results for manuscript
setwd("~/Workspaces/R workspace/Wrist Accelerometry Analysis/wrist-accelerometry-analysis/")

load("~/Dropbox/Work-Research/Current Directory/Chores/Datasets/V1 Under 50/d02_under50.RData")

# Table 1: Participants characteristics ---------------------------------------------------------
characteristic.df <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(characteristic.df) <- c("Participant", "Age", "BMI", "Gender")
for (participant in levels(chores.u50.df$participant)) {
     participant.records <- chores.u50.df[chores.u50.df$participant == participant, ]
     characteristic.df <- rbind(characteristic.df,
                                data.frame(Participant = participant,
                                           Age = participant.records$age[1],
                                           BMI = participant.records$BMI[1],
                                           Gender = participant.records$gender[1]))
     
}

rm(participant, participant.records)
characteristic.df$BMI <- as.numeric(as.character(characteristic.df$BMI))


# A summary of participants - in text -----------------------------------------------------------
rm(characteristic.df)

setwd("~/../../Volumes/aging/SHARE/ARRC/Active_Studies/CHORES-XL_087-2013/Participant Data/")
meta.df <- read.csv(file.choose())
ordered.idx <- with(meta.df, order(Age, decreasing = F))
meta.df <- meta.df[ordered.idx, ]
rm(ordered.idx)

currParticipant <- 1
participant <- as.character(meta.df$ID[currParticipant])

task.times <- read.table("BAVA07006102015/tasktimes_BAVA070.txt", header = F, sep = ",")
colnames(task.times) <- c("Task", "Cosmed.Start", "Cosmed.End", "Phone.Start", "Phone.End", "Visit.Date")

# remove first row, if necessary
for(i in 1:nrow(task.times)) {
     start <- strptime(as.character(task.times$Cosmed.Start[i]), "%M:%S")
     end <- strptime(as.character(task.times$Cosmed.End[i]), "%M:%S")
     print(paste(task.times$Task[i], (end - start), sep = " >>> "))
}


rm(start, end, i, participant, currParticipant, meta.df, task.times)


# Putting everything in one dataset -----------------------------------------------------------------------------
setwd("~/Workspaces/R workspace/Wrist Accelerometry Analysis/wrist-accelerometry-analysis/")

# Outputs/chores_tasks_target_labels.csv
task.labels <- read.csv(file = file.choose())
task.labels$SEDENTARY <- as.logical(task.labels$SEDENTARY)
task.labels$LOCOMOTION <- as.logical(task.labels$LOCOMOTION)
task.labels$ACTIVITY_LEVEL <- as.character(task.labels$ACTIVITY_LEVEL)

# Sedentary
sedentary <- rep(F, nrow(chores.u50.df))
for(i in 1:length(levels(chores.u50.df$task))) {
     currTask <- levels(chores.u50.df$task)[i]
     label <- task.labels$SEDENTARY[task.labels$TASK == currTask]
     cat(paste("The task is ", as.character(currTask), " ...  Is it sedentary?  ", label, "\n", sep = ""))
     sedentary[chores.u50.df$task == currTask] <- label
}
rm(currTask, label, i)

# Locomotion
locomotion <- rep(F, nrow(chores.u50.df))
for(i in 1:length(levels(chores.u50.df$task))) {
     currTask <- levels(chores.u50.df$task)[i]
     label <- task.labels$LOCOMOTION[task.labels$TASK == currTask]
     cat(paste("The task is ", as.character(currTask), " ...  Is it locomotion?  ", label, "\n", sep = ""))
     locomotion[chores.u50.df$task == currTask] <- label
}
rm(currTask, label, i)

chores.u50.df$sedentary <- sedentary
chores.u50.df$locomotion <- locomotion

# Activity Level
activityLevel <- rep("L", nrow(chores.u50.df))
for(i in 1:length(levels(chores.u50.df$task))) {
     currTask <- levels(chores.u50.df$task)[i]
     label <- task.labels$ACTIVITY_LEVEL[task.labels$TASK == currTask]
     cat(paste("The task is ", as.character(currTask), " ...  Activity Level?  ", label, "\n", sep = ""))
     activityLevel[chores.u50.df$task == currTask] <- label
}
rm(currTask, label, i)

# Creating a dataset which contains all the variables and classes
chores.u50 <- data.frame(matrix(nrow = nrow(chores.u50.df), ncol = 16))
colnames(chores.u50) <- c("Participant", "Age", "BMI", "Gender", "Task",
                          "class.sedentary", "class.locomotion", "class.activityLevel", "MET",
                          "mvm", "sdvm", "p625", "df", "fpdf", "mangle", "sdangle")
chores.u50$Participant <- chores.u50.df$participant
chores.u50$Age <- chores.u50.df$age
chores.u50$BMI <- chores.u50.df$BMI
chores.u50$Gender <- chores.u50.df$gender
chores.u50$Task <- chores.u50.df$task
chores.u50$class.sedentary <- chores.u50.df$sedentary
chores.u50$class.locomotion <- chores.u50.df$locomotion
chores.u50$class.activityLevel <- activityLevel
chores.u50$MET <- chores.u50.df$MET
chores.u50$mvm <- chores.u50.df$mvm
chores.u50$sdvm <- chores.u50.df$sdvm
chores.u50$p625 <- chores.u50.df$p625
chores.u50$df <- chores.u50.df$df
chores.u50$fpdf <- chores.u50.df$fpdf
chores.u50$mangle <- chores.u50.df$mangle
chores.u50$sdangle <- chores.u50.df$sdangle
save(chores.u50, file = "~/Dropbox/Work-Research/Current Directory/Chores/Datasets/V1 Under 50/d03_dataset_u50.RData")


rm(task.labels, activityLevel, chores.u50.df)

# Creating a summary table of the accelerometry features
tab3.result <- data.frame(matrix(nrow = 0, ncol = 19))
colnames(tab3.result) <- c("Task", "Locomotion", "Sedentary", "METs.avg", "METs.sd",
                           "mvm.avg", "mvm.sd", "sdvm.avg", "sdvm.sd",
                           "mangle.avg", "mangle.sd", "sdangle.avg", "sdangle.sd",
                           "p625.avg", "p625.sd", "df.avg", "df.sd", "fpdf.avg", "fpdf.sd")

for(task in levels(chores.u50.df$task)) {
     if(as.character(task) != "TREADMILL TEST") {
          idx <- which(chores.u50.df$task == task)
          new.row <- data.frame(Task = task, Locomotion = task.labels$LOCOMOTION[task.labels$TASK == task],
                                Sedentary = task.labels$SEDENTARY[task.labels$TASK == task],
                                METs.avg = mean(chores.u50.df$MET[idx]), METs.sd = sd(chores.u50.df$MET[idx]),
                                mvm.avg = mean(chores.u50.df$mvm[idx]), mvm.sd = sd(chores.u50.df$mvm[idx]),
                                sdvm.avg = mean(chores.u50.df$sdvm[idx]), sdvm.sd = sd(chores.u50.df$sdvm[idx]),
                                mangle.avg = mean(chores.u50.df$mangle[idx]), mangle.sd = sd(chores.u50.df$mangle[idx]),
                                sdangle.avg = mean(chores.u50.df$sdangle[idx]), sdangle.sd = sd(chores.u50.df$sdangle[idx]),
                                p625.avg = mean(chores.u50.df$p625[idx]), p625.sd = sd(chores.u50.df$p625[idx]),
                                df.avg = mean(chores.u50.df$df[idx]), df.sd = sd(chores.u50.df$df[idx]),
                                fpdf.avg = mean(chores.u50.df$fpdf[idx]), fpdf.sd = sd(chores.u50.df$fpdf[idx]))
          tab3.result <- rbind(tab3.result, new.row)
     }
}

colnames(tab3.result) <- c("Task", "Locomotion", "Sedentary", "METs.avg", "METs.sd",
                           "mvm.avg", "mvm.sd", "sdvm.avg", "sdvm.sd",
                           "mangle.avg", "mangle.sd", "sdangle.avg", "sdangle.sd",
                           "p625.avg", "p625.sd", "df.avg", "df.sd", "fpdf.avg", "fpdf.sd")
rm(task, idx, new.row)
write.csv(tab3.result, file = "~/Dropbox/Work-Research/Current Directory/Chores/Documents/022916/table3-results.csv", row.names = F)
rm(tab3.result)

rm(list = ls())




# Analysis -------------------------------------------------------------------
load("~/Dropbox/Work-Research/Current Directory/Chores/Datasets/V1 Under 50/d03_dataset_u50.RData")


# Correlation Heatmap between variables ========================================================
r <- cor(chores.u50[, 10:16])
corr.df <- data.frame(feature = row.names(r), r); rm(r)
corr.plot <- melt(corr.df)
corr.plot$abs_cor <- corr.plot$value
corr.plot$feature <- factor(corr.plot$feature, levels = corr.plot$feature)
corr.plot$variable <- factor(corr.plot$variable, levels = levels(corr.plot$feature))
g <- ggplot(data = corr.plot) + geom_tile(aes(x = feature, y = variable, fill = abs_cor), colour = "white")
g + scale_fill_continuous(low = "yellow", high = "red") + labs(x = "Variable", y = "Variable", title = "Correlation Heatmap between Variables")
rm(g, r, corr.plot, corr.df)

# Parallel Coordinate Plot - class.sedentary ===================================================
plot.df <- chores.u50[, c(6, 10:16)]
c <- colnames(plot.df)
plot.df <- data.frame(plot.df$class.sedentary, scale(plot.df[, 2:ncol(plot.df)]))
colnames(plot.df) <- c
rm(c)
plot.df <- melt(plot.df)
plot.df$id <- as.factor(rep((1:nrow(chores.u50)), 7))

g <- ggplot(data = plot.df) + geom_line(aes(x = variable, y = value, group = id, color = class.sedentary))
g + theme_bw() + scale_color_manual(values = c("red", "blue"))


# Feature table for different classes - Sedentary ============================================================
sedentary.idx <- which(chores.u50$class.sedentary)
feature.table <- data.frame(matrix(nrow = 0, ncol = 6))
colnames(feature.table) <- c("Feature", "Sedentary.Avg", "Sednetary.SD", "Nonsedentary.Avg", "Nonsedentary.SD", "P.Value")
for(i in 10:16) {
     curr.row <- data.frame(Feature = colnames(chores.u50)[i],
                            Sedentary.Avg = NA, Sedentary.SD = NA, Nonsedentary.Avg = NA, Nonsedentary.SD = NA, P.Value = NA)
     test <- t.test(chores.u50[sedentary.idx, i], chores.u50[-sedentary.idx, i], var.equal = T)
     curr.row$Sedentary.Avg <- round(mean(chores.u50[sedentary.idx, i]), digits = 2)
     curr.row$Sedentary.SD <- round(sd(chores.u50[sedentary.idx, i]), digits = 2)
     curr.row$Nonsedentary.Avg <- round(mean(chores.u50[-sedentary.idx, i]), digits = 2)
     curr.row$Nonsedentary.SD <- round(sd(chores.u50[-sedentary.idx, i]), digits = 2)
     curr.row$P.Value <- round(test$p.value, digits = 3)
     feature.table <- rbind(feature.table, curr.row)
}
rm(i, curr.row, test, sedentary.idx)
write.csv("~/Dropbox/Work-Research/Current Directory/Chores/Documents/022916/table3_sedentary_features.csv", row.names = F)
rm(feature.table)

# Two-Dim plot for different classes - Sedentary ================================================================
sedentary.idx <- which(chores.u50$class.sedentary)
plot.df <- chores.u50[, c(6, 10:16)]
plot.df$size <- 1
plot.df$size[sedentary.idx] <- 2
plot.df$size <- as.factor(plot.df$size)
g <- ggplot(data = plot.df, aes(x = sdvm, y = sdangle)) + geom_point(aes(colour = class.sedentary, size = size))
g + theme_bw() + scale_size_manual(values = c(2, 3)) + scale_colour_manual(values = c("red", "blue"))

rm(plot.df, g, sedentary.idx)

# Classification - Sedentary =====================================================================================
set.seed(5855)
random.idx <- sample(x = nrow(chores.u50), size = nrow(chores.u50))
classification.df <- chores.u50[random.idx, c(6, 10:16)]
rm(random.idx)
cv.points <- seq(1, nrow(chores.u50), by = floor(nrow(chores.u50)/5))

classification.results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(classification.results) <- c("Method", "Accuracy", "Recall.Sedentary", "Recall.Nonsedentary")

# LDA - Sedentary ################################
library(MASS)
lda.results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(lda.results) <- c("Accuracy", "Recall.Sedentary", "Recal.Nonsedentary")
for(i in 1:5) {
     test.idx <- cv.points[i]:cv.points[i+1]
     lda.fit <- lda(class.sedentary ~ ., data = classification.df[-test.idx, ])
     
     # Prediction on test set
     predicted <- as.character(predict(lda.fit, classification.df[test.idx, -1])$class)
     tbl <- table(classification.df$class.sedentary[test.idx], predicted)
     temp.row <- data.frame(Accuracy = round((tbl[1, 1] + tbl[2, 2]) * 100 / sum(tbl), digits = 2),
                            Recall.Sedentary = round((tbl[2, 2] * 100) / sum(tbl[2, ]), digits = 2),
                            Recall.Nonsedentary = round((tbl[1, 1] * 100) / sum(tbl[1, ]), digits = 2))
     lda.results <- rbind(lda.results, temp.row)
}
rm(i, test.idx, tbl, predicted, temp.row, lda.fit)

temp.row <- data.frame(Method = "LDA",
                       Accuracy = round(mean(lda.results$Accuracy), digits = 2),
                       Recall.Sedentary = round(mean(lda.results$Recall.Sedentary), digits = 2),
                       Recall.Nonsedentary = round(mean(lda.results$Recall.Nonsedentary), digits = 2))
classification.results <- rbind(classification.results, temp.row)
rm(temp.row, lda.results)

# QDA - Sedentary ################################
library(MASS)
qda.results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(qda.results) <- c("Accuracy", "Recall.Sedentary", "Recal.Nonsedentary")
for(i in 1:5) {
     test.idx <- cv.points[i]:cv.points[i+1]
     qda.fit <- lda(class.sedentary ~ ., data = classification.df[-test.idx, ])
     
     # Prediction on test set
     predicted <- as.character(predict(qda.fit, classification.df[test.idx, -1])$class)
     tbl <- table(classification.df$class.sedentary[test.idx], predicted)
     temp.row <- data.frame(Accuracy = round((tbl[1, 1] + tbl[2, 2]) * 100 / sum(tbl), digits = 2),
                            Recall.Sedentary = round((tbl[2, 2] * 100) / sum(tbl[2, ]), digits = 2),
                            Recall.Nonsedentary = round((tbl[1, 1] * 100) / sum(tbl[1, ]), digits = 2))
     qda.results <- rbind(qda.results, temp.row)
}
rm(i, test.idx, tbl, predicted, temp.row, qda.fit)

temp.row <- data.frame(Method = "QDA",
                       Accuracy = round(mean(qda.results$Accuracy), digits = 2),
                       Recall.Sedentary = round(mean(qda.results$Recall.Sedentary), digits = 2),
                       Recall.Nonsedentary = round(mean(qda.results$Recall.Nonsedentary), digits = 2))
classification.results <- rbind(classification.results, temp.row)
rm(temp.row, qda.results)

# kNN - Sedentary ####################################
library(class)
knn.df <- data.frame(class.sedentary = classification.df$class.sedentary, scale(classification.df[, -1]))
knn.results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(knn.results) <- c("Accuracy", "Recall.Sedentary", "Recal.Nonsedentary")
neighbors <- 10
for(i in 1:5) {
     test.idx <- cv.points[i]:cv.points[i+1]
     
     # Prediction on test set
     predicted <- as.character(knn(train = knn.df[-test.idx, -1], test = knn.df[test.idx, -1], cl = knn.df[-test.idx, 1], k = neighbors))
     tbl <- table(classification.df$class.sedentary[test.idx], predicted)
     temp.row <- data.frame(Accuracy = round((tbl[1, 1] + tbl[2, 2]) * 100 / sum(tbl), digits = 2),
                            Recall.Sedentary = round((tbl[2, 2] * 100) / sum(tbl[2, ]), digits = 2),
                            Recall.Nonsedentary = round((tbl[1, 1] * 100) / sum(tbl[1, ]), digits = 2))
     knn.results <- rbind(knn.results, temp.row)
}
rm(i, test.idx, tbl, predicted, temp.row, neighbors)

temp.row <- data.frame(Method = "kNN-10",
                       Accuracy = round(mean(knn.results$Accuracy), digits = 2),
                       Recall.Sedentary = round(mean(knn.results$Recall.Sedentary), digits = 2),
                       Recall.Nonsedentary = round(mean(knn.results$Recall.Nonsedentary), digits = 2))
classification.results <- rbind(classification.results, temp.row)
rm(temp.row, knn.results, classification.df, knn.df)


# Decision Tree - Sedentary ################################
classification.df$class.sedentary <- as.factor(classification.df$class.sedentary)
library(rpart)
dt.results <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(dt.results) <- c("Accuracy", "Recall.Sedentary", "Recal.Nonsedentary")
tree.control <- rpart.control(minsplit = 10, minbucket = 3, xval = 0)

for(i in 1:5) {
     test.idx <- cv.points[i]:cv.points[i+1]
     dt.fit <- rpart(formula = class.sedentary~., data = classification.df[-test.idx, ], control = tree.control)
     
     # Prediction on test set
     predicted <- as.character(predict(dt.fit, classification.df[test.idx, -1], type = "class"))
     tbl <- table(classification.df$class.sedentary[test.idx], predicted)
     temp.row <- data.frame(Accuracy = round((tbl[1, 1] + tbl[2, 2]) * 100 / sum(tbl), digits = 2),
                            Recall.Sedentary = round((tbl[2, 2] * 100) / sum(tbl[2, ]), digits = 2),
                            Recall.Nonsedentary = round((tbl[1, 1] * 100) / sum(tbl[1, ]), digits = 2))
     dt.results <- rbind(dt.results, temp.row)
}
rm(i, test.idx, tbl, predicted, temp.row, tree.control)

temp.row <- data.frame(Method = "DecisionTree-10",
                       Accuracy = round(mean(dt.results$Accuracy), digits = 2),
                       Recall.Sedentary = round(mean(dt.results$Recall.Sedentary), digits = 2),
                       Recall.Nonsedentary = round(mean(dt.results$Recall.Nonsedentary), digits = 2))
classification.results <- rbind(classification.results, temp.row)
rm(temp.row, dt.results)

# Plotting decision tree
tree.control <- rpart.control(minsplit = 5, minbucket = 3, xval = 0)
dt.fit <- rpart(formula = class.sedentary~., classification.df, control = tree.control)
plot(dt.fit, compress = T, margin = 0.2, uniform = T, nspace = 11)
text(dt.fit, use.n = T)
rm(tree.control, dt.fit)


# Random Forest - Decision Tree ################################
library(randomForest)
library(gridExtra)
set.seed(5855)
rf.fit <- randomForest(class.sedentary~., data = classification.df, ntree = 100000, mtry = 2, importance = T)
print(rf.fit)
temp.row <- data.frame(Method = "Random Forest-2",
                       Accuracy = round((rf.fit$confusion[1, 1] + rf.fit$confusion[2, 2]) * 100 / (sum(rf.fit$confusion[, 1]) + sum(rf.fit$confusion[, 2])), digits = 2),
                       Recall.Sedentary = round((100 - (rf.fit$confusion[2, 3] * 100)), digits = 2),
                       Recall.Nonsedentary = round((100 - (rf.fit$confusion[1, 3] * 100)), digits = 2))
classification.results <- rbind(classification.results, temp.row)
varImpPlot(rf.fit)
rm(temp.row, rf.fit)

set.seed(5855)
rf.fit <- randomForest(class.sedentary~., data = classification.df, ntree = 100000, mtry = 3, importance = T)
print(rf.fit)
temp.row <- data.frame(Method = "Random Forest-3",
                       Accuracy = round((rf.fit$confusion[1, 1] + rf.fit$confusion[2, 2]) * 100 / (sum(rf.fit$confusion[, 1]) + sum(rf.fit$confusion[, 2])), digits = 2),
                       Recall.Sedentary = round((100 - (rf.fit$confusion[2, 3] * 100)), digits = 2),
                       Recall.Nonsedentary = round((100 - (rf.fit$confusion[1, 3] * 100)), digits = 2))
classification.results <- rbind(classification.results, temp.row)
rm(temp.row, rf.fit)


write.csv(classification.results, file = "~/Dropbox/Work-Research/Current Directory/Chores/Documents/022916/table3-classification-sedentary.csv", row.names = F)


g_acc <- ggplot(data = classification.results, aes(x = Method, y = Accuracy)) + geom_bar(aes(fill = Method), colour = "red", stat = "identity", alpha = 0.5) + theme_bw()
g_acc <- g_acc + scale_fill_manual(values = c(rep("green", 8), rep("blue", 2)), guide = F) + labs(title = "Total Accuracy")
g_acc

g_sed <- ggplot(data = classification.results, aes(x = Method, y = Recall.Sedentary)) + geom_bar(aes(fill = Method), colour = "red", stat = "identity", alpha = 0.5) + theme_bw()
g_sed <- g_sed + scale_fill_manual(values = c(rep("blue", 2), rep("green", 8)), guide = F) + labs(title = "Accuracy for Sedentary")
g_sed

g_nonsed <- ggplot(data = classification.results, aes(x = Method, y = Recall.Nonsedentary)) + geom_bar(aes(fill = Method), colour = "red", stat = "identity", alpha = 0.5) + theme_bw()
g_nonsed <- g_nonsed + scale_fill_manual(values = c(rep("green", 8), "blue", "green"), guide = F) + labs(title = "Accuracy for Non-Sedentary")
g_nonsed

grid.arrange(g_acc, g_sed, g_nonsed, nrow = 1)
rm(g_acc, g_sed, g_nonsed)


# Classification - Locomotion ==========================================================
Do this just like sedentary (do not worry. it will pay off some day)
