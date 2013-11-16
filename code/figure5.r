
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

library(ggplot2)
library(scales)

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
