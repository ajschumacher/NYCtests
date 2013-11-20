
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

set.seed(42);data$random <- sample(1:nrow(data), nrow(data))
data$prior_random <- data$random - 1
data$prior_year <- data$year - 1
data$prior_grade <- data$grade - 1

stable_random <- merge(data[, c("random", "subject", "nscore")],
                       data[, c("prior_random", "year", "grade", "nscore")],
                       by.x="random", by.y="prior_random")

stable_grade <- merge(data[, c("dbn", "grade", "subject", "year", "nscore")],
                      data[, c("dbn", "grade", "subject", "prior_year", "nscore")],
                      by.x=c("dbn", "grade", "subject", "year"),
                      by.y=c("dbn", "grade", "subject", "prior_year"))

stable_cohort <- merge(data[, c("dbn", "cohort", "subject", "grade", "year", "nscore")],
                       data[, c("dbn", "cohort", "subject", "prior_grade", "nscore")],
                       by.x=c("dbn", "cohort", "subject", "grade"),
                       by.y=c("dbn", "cohort", "subject", "prior_grade"))

stable_random$delta <- stable_random$nscore.y - stable_random$nscore.x
stable_random$grade <- paste("grade", stable_random$grade)
stable_grade$delta <- stable_grade$nscore.y - stable_grade$nscore.x
stable_grade$into_year <- stable_grade$year + 1
stable_grade$grade <- paste("grade", stable_grade$grade)
stable_cohort$delta <- stable_cohort$nscore.y - stable_cohort$nscore.x
stable_cohort$into_year <- stable_cohort$year + 1
stable_cohort$into_grade <- paste("grade", stable_cohort$grade + 1)
# force an empty panel for stable cohort graph
stable_cohort <- rbind(stable_cohort, data.frame(dbn=c(NA,NA),
                                                 cohort=c(NA,NA),
                                                 subject=c("ELA", "Math"),
                                                 grade=c(NA,NA),
                                                 year=c(NA,NA),
                                                 nscore.x=c(NA,NA),
                                                 nscore.y=c(NA,NA),
                                                 delta=c(NA,NA),
                                                 into_year=c(NA,NA),
                                                 into_grade=c("grade 3", "grade 3")))

library(ggplot2)
library(gridExtra)

a <- ggplot(subset(stable_random, subject=="Math")) +
  aes(x=(nscore.x+nscore.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~grade) + ylim(c(-5, 5)) + xlim(c(-4, 4)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average average score (random pairs)") +
  ylab("change from one to other") +
  theme_bw()

b <- ggplot(subset(stable_grade, subject=="Math")) +
  aes(x=(nscore.x+nscore.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~grade) + ylim(c(-5, 5)) + xlim(c(-4, 4)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average average score (same school and grade, consecutive years)") +
  ylab("change from year to year") +
  theme_bw()

c <- ggplot(subset(stable_cohort, subject=="Math")) +
  aes(x=(nscore.x+nscore.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_grade) + ylim(c(-5, 5)) + xlim(c(-4, 4)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average average score (same school and cohort, consecutive grades)") +
  ylab("change from grade to grade") +
  theme_bw()

png(width=800, height=640, filename="../figure/11-1a.png")
grid.arrange(a, b, c, main="\nFigure 11-1a. Changes in average average Math test scores for random records, same grade year to year, and same cohort grade to grade")
dev.off()

a <- ggplot(subset(stable_random, subject=="ELA")) +
  aes(x=(nscore.x+nscore.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~grade) + ylim(c(-5, 5)) + xlim(c(-4, 4)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average average score (random pairs)") +
  ylab("change from one to other") +
  theme_bw()

b <- ggplot(subset(stable_grade, subject=="ELA")) +
  aes(x=(nscore.x+nscore.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~grade) + ylim(c(-5, 5)) + xlim(c(-4, 4)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average average score (same school and grade, consecutive years)") +
  ylab("change from year to year") +
  theme_bw()

c <- ggplot(subset(stable_cohort, subject=="ELA")) +
  aes(x=(nscore.x+nscore.y)/2, y=delta) + geom_point(size=0.6) +
  facet_grid(~into_grade) + ylim(c(-5, 5)) + xlim(c(-4, 4)) +
  geom_abline(intercept=0, slope=0, color="red") +
  xlab("average average score (same school and cohort, consecutive grades)") +
  ylab("change from grade to grade") +
  theme_bw()

png(width=800, height=640, filename="../figure/11-1b.png")
grid.arrange(a, b, c, main="\nFigure 11-1b. Changes in average average ELA test scores for random records, same grade year to year, and same cohort grade to grade")
dev.off()

# try one more thing...
a <- ggplot(subset(stable_grade, subject=="Math")) +
  aes(x=delta) + geom_density() +
  facet_grid(~grade) + xlim(c(-2.5, 2.5)) + ylim(c(0, 1.25)) +
  xlab("Math change from year to year (same grade)") +
  theme_bw()
b <- ggplot(subset(stable_cohort, subject=="Math")) +
  aes(x=delta) + geom_density() +
  facet_grid(~into_grade) + xlim(c(-2.5, 2.5)) + ylim(c(0, 1.25)) +
  xlab("Math change from grade to grade (same cohort)") +
  theme_bw()
c <- ggplot(subset(stable_grade, subject=="ELA")) +
  aes(x=delta) + geom_density() +
  facet_grid(~grade) + xlim(c(-2.5, 2.5)) + ylim(c(0, 1.25)) +
  xlab("ELA change from year to year (same grade)") +
  theme_bw()
d <- ggplot(subset(stable_cohort, subject=="ELA")) +
  aes(x=delta) + geom_density() +
  facet_grid(~into_grade) + xlim(c(-2.5, 2.5)) + ylim(c(0, 1.25)) +
  xlab("ELA change from grade to grade (same cohort)") +
  theme_bw()

png(width=800, height=480, filename="../figure/11-2.png")
grid.arrange(a, c, b, d, main="\nFigure 11-2. Density of changes in average Math and ELA test scores for same grade year to year and same cohort grade to grade", ncol=2)
dev.off()
