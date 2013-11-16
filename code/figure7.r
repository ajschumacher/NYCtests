
data <- read.csv("../data/all.csv", as.is=TRUE)

stopifnot(all(!is.na(data$n)))
# everything has a student count
stopifnot(all(!duplicated(data[, 1:4])))
# exactly one row per DBN-grade-year-subject

# best not to use the pre-computed sums; one error as seen earlier
data <- subset(data, grade != "All Grades")
data$grade <- as.numeric(data$grade)

# look at ELA and math
data <- merge(subset(data, subject=="Math"), subset(data, subject=="ELA"),
              by=c("dbn", "grade", "year"), all=TRUE)
data <- data[, c(1:3, 5, 8)]
names(data)[4:5] <- c("Math", "ELA")

# missing means zero
data$Math[is.na(data$Math)] <- 0
data$ELA[is.na(data$ELA)] <- 0

data$n <- pmax(data$Math, data$ELA)
data$m <- data$Math + data$ELA

library(reshape)

summary <- ddply(data, .(year, grade), summarize, n = sum(n), m=sum(m))
# introduce "cohort" - the years a student would be tested for grades 3-8,
# assuming the student moves through the system one grade per year (as normal)
summary$cohort <- paste(summary$year-summary$grade+3, summary$year-summary$grade+8, sep="-")
# and put this in for labeling niceness
summary$long_grade <- paste("grade", summary$grade)

library(ggplot2)
library(scales)
library(gridExtra)

png(width=800, height=480, filename="../figure/6a.png")
ggplot(summary) + aes(x=year, y=m) + geom_point() + facet_wrap(~long_grade) +
  theme_bw() + scale_y_continuous(labels=comma) +
  ylab("total number of tests (Math and ELA)") + xlab("") +
  ggtitle("Figure 6a. Total number of tests reported for NYC public schools (charter and non-charter) by grade for 2006-2013\n")
dev.off()

png(width=800, height=480, filename="../figure/6b.png")
ggplot(summary) + aes(x=grade, y=m) + geom_point() + facet_wrap(~cohort) +
  theme_bw() + scale_y_continuous(labels=comma) +
  ylab("total number of tests (Math and ELA)") +
  ggtitle("Figure 6b. Total number of tests reported for NYC public schools (charter and non-charter) by cohort for 2006-2013\n")
dev.off()


a <- ggplot(summary) + aes(x=year, y=m) + geom_point() + facet_grid(~long_grade) +
  theme_bw() + scale_y_continuous(labels=comma, limits=c(130000, 155000), breaks=(13:15)*1e4) +
  ylab("total number of tests") + xlab("")
b <- ggplot(subset(summary, as.numeric(substr(cohort,1,4)) %in% 2005:2011)) +
  aes(x=grade, y=m) + geom_point() + facet_grid(~cohort) +
  theme_bw() + scale_y_continuous(labels=comma, limits=c(130000, 155000), breaks=(13:15)*1e4) +
  ylab("total number of tests")
grid.arrange(a, b, main="\nFigure 6c")

ggplot(summary) + aes(x=year, y=n) + geom_point() + facet_wrap(~grade) + theme_bw() + ylim(c(67500, 77500))
ggplot(subset(summary, as.numeric(substr(cohort,1,4)) %in% 2006:2011)) + aes(x=grade, y=n) + geom_point() + facet_wrap(~cohort) + theme_bw() + ylim(c(67500, 77500))

a <- ggplot(summary) + aes(x=paste("grade", grade), y=n) + geom_point() +
  theme_bw() + scale_y_continuous(labels=comma) + xlab("") +
  ylab("lower bound on number of tested students")
b <- ggplot(summary) + aes(x=grade, y=n) + geom_line() + geom_point() +
  facet_grid(~year) + theme_bw() + scale_y_continuous(labels=comma) +
  ylab("lower bound on number of tested students")

png(width=800, height=640, filename="../figure/5a.png")
grid.arrange(a, b, main="\nFigure 5a. Lower bound on the number of tested students in NYC public schools (charter and non-charter) for grades 3-8 in 2006-2013")
dev.off()

a <- ggplot(summary) + aes(x=paste("grade", grade), y=m) + geom_point() +
  theme_bw() + scale_y_continuous(labels=comma) + xlab("") +
  ylab("total number of tests")
b <- ggplot(summary) + aes(x=grade, y=m) + geom_line() + geom_point() +
  facet_grid(~year) + theme_bw() + scale_y_continuous(labels=comma) +
  ylab("total number of tests")

png(width=800, height=640, filename="../figure/5b.png")
grid.arrange(a, b, main="\nFigure 5b. Total number of tests reported for NYC public schools (charter and non-charter) for grades 3-8 in 2006-2013")
dev.off()
