# A simple script to read the meta information for the specified subset of participants.
# Author: Matin Kheirkhahan - matin@cise.ufl.edu
# Chores XL study. Institue on Aging, University of Florida.

setwd("~/Dropbox/Work-Research/Current Directory/Chores/wrist-accelerometry-analysis/")

df <- read.csv(file = "../Datasets/Participants Meta Info/Chores_Participants.csv")
selected.age.df <- df[df$Age < 45, ]
selected.age.df <- selected.age.df[with(data = selected.age.df, order(Age, decreasing = F)), ]
write.csv(selected.age.df, "../Datasets/Participants Meta Info/under45_metaInfo.csv", row.names = F)

rm(list = ls())
