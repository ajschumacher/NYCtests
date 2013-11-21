
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

library(plyr)
# work out (an attempt at) an average (median) student score...
data <- merge(data, ddply(data, c("grade", "year", "subject"),
                          summarize, student_median=median(rep(score, times=n))))
# try to even out the spreads with the median absolute deviation (with funky constant)
data <- merge(data, ddply(data, c("grade", "year", "subject"),
                          summarize, mad=mad(score)))
data$nscore <- (data$score - data$student_median) / data$mad

# cohort as previously defined
data$cohort <- paste(data$year-data$grade+3, "-",
                     data$year-data$grade+8, " cohort", sep="")

data$prior_year <- data$year - 1
data$prior_grade <- data$grade - 1

stable_grade <- merge(data[, c("dbn", "grade", "subject", "year", "nscore")],
                      data[, c("dbn", "grade", "subject", "prior_year", "nscore")],
                      by.x=c("dbn", "grade", "subject", "year"),
                      by.y=c("dbn", "grade", "subject", "prior_year"))

stable_cohort <- merge(data[, c("dbn", "cohort", "subject", "grade", "year", "nscore")],
                       data[, c("dbn", "cohort", "subject", "prior_grade", "nscore")],
                       by.x=c("dbn", "cohort", "subject", "grade"),
                       by.y=c("dbn", "cohort", "subject", "prior_grade"))

stable_grade$delta <- stable_grade$nscore.y - stable_grade$nscore.x
stable_grade$into_year <- stable_grade$year + 1
stable_grade$grade <- paste("grade", stable_grade$grade)
stable_cohort$delta <- stable_cohort$nscore.y - stable_cohort$nscore.x
stable_cohort$into_year <- stable_cohort$year + 1
stable_cohort$into_grade <- paste("grade", stable_cohort$grade + 1)


library(ggplot2)
library(gridExtra)


a <- ggplot(subset(stable_grade, subject=="Math")) +
  aes(x=(nscore.x+nscore.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_year) + ylim(c(-2, 2)) + xlim(c(-4, 4)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("mean average Math score (same school/grade, consecutive years)") +
  ylab("Math change from year to year") +
  theme_bw()
b <- ggplot(subset(stable_cohort, subject=="Math" & !is.na(into_year))) +
  aes(x=(nscore.x+nscore.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_year) + ylim(c(-2, 2)) + xlim(c(-4, 4)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("mean average Math score (same school/cohort, consecutive grades)") +
  ylab("Math change from grade to grade") +
  theme_bw()
c <- ggplot(subset(stable_grade, subject=="ELA")) +
  aes(x=(nscore.x+nscore.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_year) + ylim(c(-2, 2)) + xlim(c(-4, 4)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("mean average ELA score (same school/grade, consecutive years)") +
  ylab("ELA change from year to year") +
  theme_bw()
d <- ggplot(subset(stable_cohort, subject=="ELA" & !is.na(into_year))) +
  aes(x=(nscore.x+nscore.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_year) + ylim(c(-2, 2)) + xlim(c(-4, 4)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("mean average ELA score (same school/cohort, consecutive grades)") +
  ylab("ELA change from grade to grade") +
  theme_bw()

png(width=800, height=480, filename="../figure/12a.png")
grid.arrange(a, c, b, d, main="\nFigure 12a. Changes in average Math and ELA test scores for same grade year to year and same cohort grade to grade, 2006-2013", ncol=2)
dev.off()


a <- ggplot(subset(stable_grade, subject=="Math")) +
  aes(x=delta) + geom_density() +
  facet_grid(~into_year) + ylim(c(0, 1.2)) + xlim(c(-3, 3)) +
  xlab("Math change from year to year for grades") +
  theme_bw()
b <- ggplot(subset(stable_cohort, subject=="Math" & !is.na(into_year))) +
  aes(x=delta) + geom_density() +
  facet_grid(~into_year) + ylim(c(0, 1.2)) + xlim(c(-3, 3)) +
  xlab("Math change from grade to grade for cohorts") +
  theme_bw()
c <- ggplot(subset(stable_grade, subject=="ELA")) +
  aes(x=delta) + geom_density() +
  facet_grid(~into_year) + ylim(c(0, 1.2)) + xlim(c(-3, 3)) +
  xlab("ELA change from year to year for grades") +
  theme_bw()
d <- ggplot(subset(stable_cohort, subject=="ELA" & !is.na(into_year))) +
  aes(x=delta) + geom_density() +
  facet_grid(~into_year) + ylim(c(0, 1.2)) + xlim(c(-3, 3)) +
  xlab("ELA change from grade to grade for cohorts") +
  theme_bw()

png(width=800, height=480, filename="../figure/12b.png")
grid.arrange(a, c, b, d, main="\nFigure 12b. Density of changes in average Math and ELA test scores for same grade year to year and same cohort grade to grade, 2006-2013", ncol=2)
dev.off()
