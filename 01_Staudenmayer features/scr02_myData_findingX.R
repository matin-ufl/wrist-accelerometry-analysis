setwd("~/Dropbox/Work-Research/Current Directory/Chores/R Scripts/")
library(ggplot2)
matin.acc.data <- read.csv(file = "../Datasets/My Accelerometer File/csv/MOS2A04140630 (2016-01-22)RAW.csv", skip = 10)
g <- ggplot(data = matin.acc.data) + theme_gray()
g + geom_line(aes(x = seq_along(Accelerometer.X), y = Accelerometer.X, color = "X")) +
geom_line(aes(x = seq_along(Accelerometer.Y), y = Accelerometer.Y, color = "Y")) +
geom_line(aes(x = seq_along(Accelerometer.Z), y = Accelerometer.Z, color = "Z")) +
scale_color_manual(values=c("blue", "red", "green")) + labs(x = "Time (ms)", y = "")
