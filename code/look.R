
data <- read.csv("../data/all.csv", as.is=TRUE)

data <- subset(data, grade != "All Grades")
data$grade <- paste("grade", data$grade)
data <- data[complete.cases(data),]
# note: 2013 has no missings for grade 7 and 8?


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
data <- merge(data, ddply(data, c("grade", "year", "subject"), summarize, mad1=mad(score, constant=1)))
data <- merge(data, ddply(data, c("grade", "year", "subject"), summarize, sd=sd(score)))
# and plot normalized stuff
ggplot(subset(data, subject=="ELA")) + aes(x=(score-mean)/mad) +
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


ggplot(subset(data, subject=="ELA")) + aes(x=n, y=score) +
  geom_point() +
  facet_grid(year ~ grade) + xlab("") + ylab("")