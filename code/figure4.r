
data <- read.csv("../data/all.csv", as.is=TRUE)

stopifnot(all(!is.na(data$n)))
# everything has a student count
stopifnot(all(!duplicated(data[, 1:4])))
# exactly one row per DBN-grade-year-subject

# best not to use the pre-computed sums; one error as seen earlier
data <- subset(data, grade != "All Grades")

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

summary <- ddply(data, .(year), summarize, n = sum(n), m=sum(m))

library(ggplot2)
library(scales)
library(gridExtra)

a <- ggplot(summary) + aes(x=year, y=n) + geom_point() + geom_line() +
  scale_x_continuous(breaks=2006:2013) + scale_y_continuous(labels=comma) +
  theme_classic() + xlab("") + ylab("students (zoomed in)")
b <- ggplot(summary) + aes(x=year, y=n) + geom_point() + geom_line() +
  scale_x_continuous(breaks=2006:2013) + scale_y_continuous(labels=comma, limits=c(0,440000)) +
  theme_classic() + xlab("") + ylab("students (absolute scale)")

png(width=800, height=400, filename="../figure/4a.png")
grid.arrange(b, a, main="\nFigure 4a. Lower bound on the number of grade 3-8 tested students in NYC public schools (charter and non-charter) in 2006-2013")
dev.off()

a <- ggplot(summary) + aes(x=year, y=m) + geom_point() + geom_line() +
  scale_x_continuous(breaks=2006:2013) + scale_y_continuous(labels=comma) +
  theme_classic() + xlab("") + ylab("tests (zoomed in)")
b <- ggplot(summary) + aes(x=year, y=m) + geom_point() + geom_line() +
  scale_x_continuous(breaks=2006:2013) + scale_y_continuous(labels=comma, limits=c(0,860000)) +
  theme_classic() + xlab("") + ylab("tests (absolute scale)")

png(width=800, height=400, filename="../figure/4b.png")
grid.arrange(b, a, main="\nFigure 4b. Total number of grade 3-8 Math and ELA tests reported for NYC public schools (charter and non-charter) in 2006-2013")
dev.off()
