
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
stopifnot(sum(is.na(data$score))==94)
stopifnot(all(data$n[is.na(data$score)] <= 5))
# these are suppressed values and there's nothing to be done with them
data <- data[complete.cases(data),]


library(ggplot2)

# initial plots of distributions
ggplot(subset(data, subject=="ELA")) + aes(x=score) +
  geom_histogram(binwidth=1) +
  facet_grid(year ~ grade) +
  xlab("average score reported by NYCDOE") +
  ylab("number of schools reporting such an average") +
  ggtitle("average ELA scores by grade for all NYC public schools (charter and non-charter)")
ggplot(subset(data, subject=="Math")) + aes(x=score) +
  geom_histogram(binwidth=1) +
  facet_grid(year ~ grade) +
  xlab("average score reported by NYCDOE") +
  ylab("number of schools reporting such an average") +
  ggtitle("average Math scores by grade for all NYC public schools (charter and non-charter)")


library(plyr)

# work out the average student score...
data <- merge(data, ddply(data, c("grade", "year", "subject"), summarize, mean=weighted.mean(score, n)))
# also look at some others:
data <- merge(data, ddply(data, c("grade", "year", "subject"), summarize, straightmean=mean(score)))
data <- merge(data, ddply(data, c("grade", "year", "subject"), summarize, straightmedian=median(score)))
# and plot de-meaned stuff
ggplot(subset(data, subject=="ELA")) + aes(x=score-mean) +
  geom_histogram(binwidth=1) +
  facet_grid(year ~ grade) +
  xlab("de-meaned average score") +
  ylab("number of schools reporting such an average") +
  ggtitle("de-meaned average ELA scores by grade for all NYC public schools (charter and non-charter)")
ggplot(subset(data, subject=="Math")) + aes(x=score-mean) +
  geom_histogram(binwidth=1) +
  facet_grid(year ~ grade) +
  xlab("de-meaned average score") +
  ylab("number of schools reporting such an average") +
  ggtitle("de-meaned average Math scores by grade for all NYC public schools (charter and non-charter)")

# plot densities to see distribution without number of schools
ggplot(subset(data, subject=="ELA")) + aes(x=score-mean) +
  geom_density(binwidth=1) +
  facet_grid(year ~ grade) +
  xlab("de-meaned average score") +
  ylab("number of schools reporting such an average") +
  ggtitle("de-meaned average ELA scores by grade for all NYC public schools (charter and non-charter)")
ggplot(subset(data, subject=="Math")) + aes(x=score-mean) +
  geom_density(binwidth=1) +
  facet_grid(year ~ grade) +
  xlab("de-meaned average score") +
  ylab("number of schools reporting such an average") +
  ggtitle("de-meaned average Math scores by grade for all NYC public schools (charter and non-charter)")


# try to even out the spreads with the median absolute deviation (with funky constant)
data <- merge(data, ddply(data, c("grade", "year", "subject"), summarize, mad=mad(score)))
# also try other measures of spread
data <- merge(data, ddply(data, c("grade", "year", "subject"), summarize, sd=sd(score)))
# and plot normalized stuff
ggplot(subset(data, subject=="ELA")) + aes(x=(score-straightmedian)/mad) +
  geom_density(binwidth=0.1) +
  facet_grid(year ~ grade) +
  xlab("normalized average score") +
  ylab("number of schools reporting such an average") +
  ggtitle("de-meaned average ELA scores by grade for all NYC public schools (charter and non-charter)")
ggplot(subset(data, subject=="Math")) + aes(x=(score-mean)/mad) +
  geom_density(binwidth=0.1) +
  facet_grid(year ~ grade) +
  xlab("normalized average score") +
  ylab("number of schools reporting such an average") +
  ggtitle("de-meaned average Math scores by grade for all NYC public schools (charter and non-charter)")
# looked at sd vs. mad - mad gives a better evening-out by far. mad constant doesn't matter; just a factor
# hmm there are D75 schools in here?
ggplot(subset(data, subject=="ELA")) + aes(x=(score-mean)/mad) +
  aes(fill=substr(dbn,1,2)=="75") +
  geom_histogram(binwidth=0.1) +
  facet_grid(year ~ grade) +
  xlab("normalized average score") +
  ylab("number of schools reporting such an average") +
  ggtitle("de-meaned average ELA scores by grade for all NYC public schools (charter and non-charter)")
ggplot(subset(data, subject=="Math")) + aes(x=(score-mean)/mad) +
  geom_density(binwidth=0.1) +
  facet_grid(year ~ grade) +
  xlab("normalized average score") +
  ylab("number of schools reporting such an average") +
  ggtitle("de-meaned average Math scores by grade for all NYC public schools (charter and non-charter)")
# okay; should really take out the D75 schools...
# whoa out of order~ time loop~ whoa~


ggplot(subset(data, subject=="ELA")) + aes(x=n, y=(score-mean)/mad) +
  geom_point() +
  facet_grid(year ~ grade) + xlab("") + ylab("")