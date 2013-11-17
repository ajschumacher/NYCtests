
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

# cohort as previously defined
data$cohort <- paste(data$year-data$grade+3, "-",
                     data$year-data$grade+8, " cohort", sep="")

set.seed(42);data$random <- sample(1:nrow(data), nrow(data))
data$prior_random <- data$random - 1
data$prior_year <- data$year - 1
data$prior_grade <- data$grade - 1

stable_random <- merge(data[, c("random", "subject", "n")],
                       data[, c("prior_random", "year", "n")],
                       by.x="random", by.y="prior_random")

stable_grade <- merge(data[, c("dbn", "grade", "subject", "year", "n")],
                      data[, c("dbn", "grade", "subject", "prior_year", "n")],
                      by.x=c("dbn", "grade", "subject", "year"),
                      by.y=c("dbn", "grade", "subject", "prior_year"))

stable_cohort <- merge(data[, c("dbn", "cohort", "subject", "grade", "year", "n")],
                       data[, c("dbn", "cohort", "subject", "prior_grade", "n")],
                       by.x=c("dbn", "cohort", "subject", "grade"),
                       by.y=c("dbn", "cohort", "subject", "prior_grade"))

stable_random$delta <- stable_random$n.y - stable_random$n.x
stable_random$into_year <- stable_random$year + 1
stable_grade$delta <- stable_grade$n.y - stable_grade$n.x
stable_grade$into_year <- stable_grade$year + 1
stable_cohort$delta <- stable_cohort$n.y - stable_cohort$n.x
stable_cohort$into_year <- stable_cohort$year + 1

library(ggplot2)
library(gridExtra)

a <- ggplot(subset(stable_random, subject=="Math")) +
  aes(x=(n.x+n.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_year) + ylim(c(-800, 800)) + xlim(c(0, 850)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average number of students tested (random pairs)") +
  ylab("change from prior to current") +
  theme_bw()

b <- ggplot(subset(stable_grade, subject=="Math")) +
  aes(x=(n.x+n.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_year) + ylim(c(-800, 800)) + xlim(c(0, 850)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average number of students tested (same school and grade, consecutive years)") +
  ylab("change from prior to current") +
  theme_bw()

c <- ggplot(subset(stable_cohort, subject=="Math")) +
  aes(x=(n.x+n.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_year) + ylim(c(-800, 800)) + xlim(c(0, 850)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average number of students tested (same school and cohort, consecutive grades") +
  ylab("change from prior to current") +
  theme_bw()

grid.arrange(a, b, c)

a <- ggplot(subset(stable_random, subject=="ELA")) +
  aes(x=(n.x+n.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_year) + ylim(c(-800, 800)) + xlim(c(0, 850)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average number of students tested (random pairs)") +
  ylab("change from prior to current") +
  theme_bw()

b <- ggplot(subset(stable_grade, subject=="ELA")) +
  aes(x=(n.x+n.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_year) + ylim(c(-800, 800)) + xlim(c(0, 850)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average number of students tested (same school and grade, consecutive years)") +
  ylab("change from prior to current") +
  theme_bw()

c <- ggplot(subset(stable_cohort, subject=="ELA")) +
  aes(x=(n.x+n.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_year) + ylim(c(-800, 800)) + xlim(c(0, 850)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average number of students tested (same school and cohort, consecutive grades") +
  ylab("change from prior to current") +
  theme_bw()

grid.arrange(a, b, c)

ggplot(subset(stable_cohort, subject=="Math")) +
  aes(x=(n.x+n.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~grade) + ylim(c(-800, 800)) + xlim(c(0, 850)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average number of students tested (same school and cohort, consecutive grades") +
  ylab("change from prior to current") +
  theme_bw()

