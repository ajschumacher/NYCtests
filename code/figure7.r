
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
summary$cohort <- paste(summary$year-summary$grade+3, "-",
                        summary$year-summary$grade+8, " cohort", sep="")
# and put this in for labeling niceness
summary$long_grade <- paste("grade", summary$grade)

library(ggplot2)
library(scales)
library(gridExtra)

png(width=800, height=480, filename="../figure/7a.png")
ggplot(summary) + aes(x=year, y=m) + geom_point() + facet_wrap(~long_grade) +
  theme_bw() + scale_y_continuous(labels=comma, breaks=(13:15)*1e4) +
  ylab("total number of tests (Math and ELA)") + xlab("") +
  ggtitle("Figure 7a. Total number of tests reported for NYC public schools (charter and non-charter) by grade for 2006-2013\n")
dev.off()

png(width=800, height=480, filename="../figure/7b.png")
ggplot(summary) + aes(x=grade, y=m) + geom_point() + facet_wrap(~cohort) +
  theme_bw() + scale_y_continuous(labels=comma, breaks=(13:15)*1e4) +
  ylab("total number of tests (Math and ELA)") +
  ggtitle("Figure 7b. Total number of tests reported for NYC public schools (charter and non-charter) by cohort for 2006-2013\n")
dev.off()


a <- ggplot(summary) + aes(x=year, y=m) + geom_point() + facet_grid(~long_grade) +
  theme_bw() + scale_y_continuous(labels=comma, limits=c(130000, 155000), breaks=(13:15)*1e4) +
  ylab("total number of tests")
b <- ggplot(subset(summary, as.numeric(substr(cohort,1,4)) %in% 2006:2011)) +
  aes(x=grade, y=m) + geom_point() + facet_grid(~cohort) +
  theme_bw() + scale_y_continuous(labels=comma, limits=c(130000, 155000), breaks=(13:15)*1e4) +
  ylab("total number of tests")
png(width=800, height=480, filename="../figure/7c.png")
grid.arrange(a, b, main="\nFigure 7c. A direct comparison of NYC grade 3-8 Math and ELA testing numbers by grade (top) versus by cohort (bottom)")
dev.off()
