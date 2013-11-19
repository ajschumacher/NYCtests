
data <- read.csv("../data/all.csv", as.is=TRUE)

stopifnot(all(!is.na(data$n)))
# everything has a student count
stopifnot(all(!duplicated(data[, 1:4])))
# exactly one row per DBN-grade-year-subject

# best not to use the pre-computed sums; one error as seen earlier
data <- subset(data, grade != "All Grades")
data$grade <- as.numeric(data$grade)

# remove D75; have seen issues with these schools
data <- subset(data, substr(dbn,1,2) != "75")

stopifnot(sum(is.na(data$score))==sum(is.na(data)))
stopifnot(sum(is.na(data$score))==94) # no surprises
stopifnot(all(data$n[is.na(data$score)] <= 5))
# these are suppressed values and there's nothing to be done with them
data <- data[complete.cases(data),]

# nicer labeling
data$grade <- paste("grade", data$grade)

library(plyr)

# work out (an attempt at) an average (median) student score...
data <- merge(data, ddply(data, c("grade", "year", "subject"),
                          summarize, student_median=median(rep(score, times=n))))

# try to even out the spreads with the median absolute deviation (with funky constant)
data <- merge(data, ddply(data, c("grade", "year", "subject"),
                          summarize, mad=mad(score)))

# check out how the scores look vs. number of students tested
png(width=1280, height=720, file="../figure/10a.png")
ggplot(subset(data, subject=="Math")) + aes(x=n, y=(score-student_median)/mad) +
  geom_point(size=1.2) + theme_bw() +
  facet_grid(year ~ grade) + geom_abline(intercept=0, slope=0, color="red") +
  xlab("number of tested students") +
  ylab("normalized school average score (subtract estimate of student median, divide by median absolute deviation)") +
  ggtitle("Figure 10a. Normalized average Math scores vs. number of tested students for non-D75 NYC public schools (charter and non-charter) grades 3-8, 2006-2013")
dev.off()
png(width=1280, height=720, file="../figure/10b.png")
ggplot(subset(data, subject=="ELA")) + aes(x=n, y=(score-student_median)/mad) +
  geom_point(size=1.2) + theme_bw() +
  facet_grid(year ~ grade) + geom_abline(intercept=0, slope=0, color="red") +
  xlab("number of tested students") +
  ylab("normalized school average score (subtract estimate of student median, divide by median absolute deviation)") +
  ggtitle("Figure 10b. Normalized average ELA scores vs. number of tested students for non-D75 NYC public schools (charter and non-charter) grades 3-8, 2006-2013")
dev.off()
