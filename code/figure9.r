
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

# nicer labeling
data$grade <- paste("grade", data$grade)
# and then I need this
data$yeargrade <- paste(data$year, data$grade)

library(ggplot2)

# initial plots of distributions
png(width=1280, height=720, file="../figure/9-1a.png")
ggplot(subset(data, subject=="Math")) + aes(x=score) +
  geom_density() + theme_bw() +
  facet_grid(year ~ grade) +
  xlab("average score reported by NYCDOE") +
  ylab("proportion of schools reporting such an average") +
  ggtitle("Figure 9-1a. Average reported Math scores for non-D75 NYC public schools (charter and non-charter) grades 3-8, 2006-2013")
dev.off()
png(width=1280, height=720, file="../figure/9-1b.png")
ggplot(subset(data, subject=="ELA")) + aes(x=score) +
  geom_density() + theme_bw() +
  facet_grid(year ~ grade) +
  xlab("average score reported by NYCDOE") +
  ylab("proportion of schools reporting such an average") +
  ggtitle("Figure 9-1b. Average reported ELA scores for non-D75 NYC public schools (charter and non-charter) grades 3-8, 2006-2013")
dev.off()

library(plyr)

# work out the average (mean) student score...
data <- merge(data, ddply(data, c("grade", "year", "subject"),
                          summarize, student_mean=weighted.mean(score, n)))
# work out (an attempt at) an average (median) student score...
data <- merge(data, ddply(data, c("grade", "year", "subject"),
                          summarize, student_median=median(rep(score, times=n))))
# work out the average (mean) of the school-year-grade-subject mean scores:
data <- merge(data, ddply(data, c("grade", "year", "subject"),
                          summarize, mean=mean(score)))
# work out the average (median) of the school-year-grade-subject mean scores:
data <- merge(data, ddply(data, c("grade", "year", "subject"),
                          summarize, median=median(score)))

# try to even out the spreads with the median absolute deviation (with funky constant)
data <- merge(data, ddply(data, c("grade", "year", "subject"),
                          summarize, mad=mad(score)))
# also try other measures of spread
data <- merge(data, ddply(data, c("grade", "year", "subject"),
                          summarize, sd=sd(score)))
# could also try interquartile range, others...


# decided process plots first
png(width=1280, height=720, file="../figure/9-2a.png")
ggplot(subset(data, subject=="Math")) + aes(x=score-median) +
  geom_density() + theme_bw() +
  facet_grid(year ~ grade) +
  xlab("average score reported by NYCDOE minus median score") +
  ylab("proportion of schools reporting thus") +
  ggtitle("Figure 9-2a. Centered average Math scores for non-D75 NYC public schools (charter and non-charter) grades 3-8, 2006-2013")
dev.off()
png(width=1280, height=720, file="../figure/9-2b.png")
ggplot(subset(data, subject=="ELA")) + aes(x=score-median) +
  geom_density() + theme_bw() +
  facet_grid(year ~ grade) +
  xlab("average score reported by NYCDOE minus median score") +
  ylab("proportion of schools reporting thus") +
  ggtitle("Figure 9-2b. Centered average ELA scores for non-D75 NYC public schools (charter and non-charter) grades 3-8, 2006-2013")
dev.off()

png(width=1280, height=720, file="../figure/9-3a.png")
ggplot(subset(data, subject=="Math")) + aes(x=(score-median)/mad) +
  geom_density() + theme_bw() +
  facet_grid(year ~ grade) +
  xlab("average score reported by NYCDOE minus median score and divided by median absolute deviation") +
  ylab("proportion of schools reporting thus") +
  ggtitle("Figure 9-3a. Normalized average Math scores for non-D75 NYC public schools (charter and non-charter) grades 3-8, 2006-2013")
dev.off()
png(width=1280, height=720, file="../figure/9-3b.png")
ggplot(subset(data, subject=="ELA")) + aes(x=(score-median)/mad) +
  geom_density() + theme_bw() +
  facet_grid(year ~ grade) +
  xlab("average score reported by NYCDOE minus median score and divided by median absolute deviation") +
  ylab("proportion of schools reporting thus") +
  ggtitle("Figure 9-3b. Normalized average ELA scores for non-D75 NYC public schools (charter and non-charter) grades 3-8, 2006-2013")
dev.off()

# and here's why it's median and mad
library(gridExtra)

# many separate plots for Math...
a <- ggplot(subset(data, subject=="Math")) + aes(x=(score-mean)/sd) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3,4))
b <- ggplot(subset(data, subject=="Math")) + aes(x=(score-median)/sd) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3,4))
c <- ggplot(subset(data, subject=="Math")) + aes(x=(score-student_mean)/sd) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3,4))
d <- ggplot(subset(data, subject=="Math")) + aes(x=(score-student_median)/sd) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3,4))
e <- ggplot(subset(data, subject=="Math")) + aes(x=(score-mean)/mad) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3,4))
f <- ggplot(subset(data, subject=="Math")) + aes(x=(score-median)/mad) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3,4))
g <- ggplot(subset(data, subject=="Math")) + aes(x=(score-student_mean)/mad) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3,4))
h <- ggplot(subset(data, subject=="Math")) + aes(x=(score-student_median)/mad) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3,4))

png(width=1280, height=720, file="../figure/9-4a.png")
grid.arrange(a,b,c,d,e,f,g,h,nrow=2, main="\nFigure 9-4a. Average Math scores for non-D75 NYC public schools (charter and non-charter) grades 3-8, 2006-2013, normalized several ways, overplotted")
dev.off()

# many separate plots for ELA...
a <- ggplot(subset(data, subject=="ELA")) + aes(x=(score-mean)/sd) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3.5,5))
b <- ggplot(subset(data, subject=="ELA")) + aes(x=(score-median)/sd) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3.5,5))
c <- ggplot(subset(data, subject=="ELA")) + aes(x=(score-student_mean)/sd) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3.5,5))
d <- ggplot(subset(data, subject=="ELA")) + aes(x=(score-student_median)/sd) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3.5,5))
e <- ggplot(subset(data, subject=="ELA")) + aes(x=(score-mean)/mad) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3.5,5))
f <- ggplot(subset(data, subject=="ELA")) + aes(x=(score-median)/mad) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3.5,5))
g <- ggplot(subset(data, subject=="ELA")) + aes(x=(score-student_mean)/mad) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3.5,5))
h <- ggplot(subset(data, subject=="ELA")) + aes(x=(score-student_median)/mad) +
  geom_density() + aes(group=yeargrade) + theme_bw() + ylim(c(0, 0.62)) + xlim(c(-3.5,5))

png(width=1280, height=720, file="../figure/9-4b.png")
grid.arrange(a,b,c,d,e,f,g,h,nrow=2, main="\nFigure 9-4b. Average ELA scores for non-D75 NYC public schools (charter and non-charter) grades 3-8, 2006-2013, normalized several ways, overplotted")
dev.off()
