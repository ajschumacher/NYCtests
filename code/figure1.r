
data <- read.csv("../data/all.csv", as.is=TRUE)

data <- subset(data, !is.na(score))
stopifnot(all(data$score %% 1 == 0))
# all "average scores" are reported as integers

data <- subset(data, grade != "All Grades")
stopifnot(all(as.numeric(data$grade) %in% 3:8))
# only data for grade 3 to 8 (class averages)

stopifnot(all(data$year %in% 2006:2013))
# all data is for years 2006 to 2013

data$grade <- paste("grade", data$grade)


library(ggplot2)

png(width=1280, height=720, filename="../figure/1a.png")
ggplot(subset(data, subject=="Math")) + aes(x=score) +
  geom_histogram(binwidth=1, fill="steelblue") +
  facet_grid(year ~ grade) +
  xlab("average score as reported by NYCDOE") +
  ylab("number of schools reporting such an average for the grade") +
  ggtitle("Figure 1a. All reported average math scores for NYC public schools (charter and non-charter) grades 3-8, 2006-2013")
dev.off()

png(width=1280, height=720, file="../figure/1b.png")
ggplot(subset(data, subject=="ELA")) + aes(x=score) +
  geom_histogram(binwidth=1, fill="steelblue") +
  facet_grid(year ~ grade) +
  xlab("average score as reported by NYCDOE") +
  ylab("number of schools reporting such an average for the grade") +
  ggtitle("Figure 1b. All reported average ELA scores for NYC public schools (charter and non-charter) grades 3-8, 2006-2013")
dev.off()
