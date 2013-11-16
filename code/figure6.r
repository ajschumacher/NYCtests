
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


data$d75 <- substr(data$dbn, 1, 2) == "75"

library(ggplot2)

png(width=1280, height=720, filename="../figure/6a.png")
ggplot(subset(data, subject=="Math")) + aes(x=score, fill=d75) +
  guides(fill=FALSE) + scale_fill_manual(values=c("steelblue", "red")) +
  geom_histogram(binwidth=1) +
  facet_grid(year ~ grade) +
  xlab("average score as reported by NYCDOE") +
  ylab("number of schools reporting such an average for the grade") +
  ggtitle("Figure 6a. All reported average Math scores for NYC public schools (charter and non-charter) grades 3-8, 2006-2013\n(District 75 school results are in red; all other schools' results are in blue.)\n")
dev.off()

png(width=1280, height=720, filename="../figure/6b.png")
ggplot(subset(data, subject=="ELA")) + aes(x=score, fill=d75) +
  guides(fill=FALSE) + scale_fill_manual(values=c("steelblue", "red")) +
  geom_histogram(binwidth=1) +
  facet_grid(year ~ grade) +
  xlab("average score as reported by NYCDOE") +
  ylab("number of schools reporting such an average for the grade") +
  ggtitle("Figure 6b. All reported average ELA scores for NYC public schools (charter and non-charter) grades 3-8, 2006-2013\n(District 75 school results are in red; all other schools' results are in blue.)\n")
dev.off()

# check that there aren't any 2013 D75 results at all
stopifnot(sum(data$year==2013 & data$d75==TRUE) == 0)
