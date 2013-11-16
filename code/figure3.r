
data <- read.csv("../data/all.csv", as.is=TRUE)

stopifnot(all(!is.na(data$n)))
# everything has a student count
stopifnot(all(!duplicated(data[, 1:4])))
# exactly one row per DBN-grade-year-subject

# add up grades to get a better all-grade total
library(reshape)
data <- merge(subset(data, grade == "All Grades"),
              ddply(subset(data, grade != "All Grades"),
                    .(dbn, year, subject), summarize, m=sum(n)),
              all=TRUE)
stopifnot(which(data$n != data$m) == 11314) # the case we know about

# and use the sums we got
data <- data[, c(1:3, 7)]

# but do we test the same numbers in Math and ELA?
data <- merge(subset(data, subject=="Math"), subset(data, subject=="ELA"),
              by=c("dbn", "year"), all=TRUE)

data <- data[, c(1, 2, 4, 6)]
names(data)[3:4] <- c("Math", "ELA")

# missing data taken as no students tested,
# not an unknown number of students tested
data$ELA[is.na(data$ELA)] <- 0
data$Math[is.na(data$Math)] <- 0

# do the same number of students take the Math and ELA exams?
library(ggplot2)
library(scales)
png(width=960, height=768, filename="../figure/3a.png")
ggplot(data) + aes(x=(Math + ELA)/2, y=(Math - ELA)) + geom_point() + theme_classic() +
  xlab("Average of number of students tested in Math and ELA") + ylab("Number of students tested in Math minus number of students tested in ELA") +
  geom_abline(intercept=0, slope=0, color="red") +
  ggtitle("Figure 3a. Number of students tested in Math minus number of students tested in ELA vs. average of number of students tested in\nMath and ELA for all NYC public schools (charter and non-charter) in 2006-2013. (The red lines show where the numbers are equal.)") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) + facet_wrap(~year)
dev.off()

png(width=960, height=768, filename="../figure/3b.png")
ggplot(data) + aes(x=(Math + ELA), y=Math/((Math + ELA))) + geom_point() + theme_classic() +
  xlab("Number of students tested in Math plus number of students tested in ELA") + ylab("Percent of all tests that were in Math") +
  geom_abline(intercept=0.5, slope=0, color="red") +
  ggtitle("Figure 3b. Percent of tests that were in Math vs. total number of Math and ELA tests for all NYC public\nschools (charter and non-charter) in 2006-2013. (The red lines show where the numbers are equal.)") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = percent) + facet_wrap(~year)
dev.off()
