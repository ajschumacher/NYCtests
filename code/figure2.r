
data <- read.csv("../data/all.csv", as.is=TRUE)

stopifnot(all(!is.na(data$n)))
# everything has a student count
stopifnot(all(!duplicated(data[, 1:4])))
# exactly one row per DBN-grade-year-subject

# this is the one case where "All Grades" isn't the sum of all grades
subset(data, year==2007 & subject=="ELA" & dbn=="22K245",
       select=c("grade", "n"))

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

# do the same number of students take the Math and ELA exams?
library(ggplot2)
library(scales)
png(width=608, height=640, filename="../figure/2.png")
ggplot(data) + aes(x=ELA, y=Math) + geom_point(size=0.8) + coord_fixed() + theme_classic() +
  xlab("Number of students tested in ELA") + ylab("Number of students tested in Math") +
  geom_abline(intercept=0, slope=1, color="red") +
  ggtitle("Figure 2. Number of students tested in Math and ELA\nfor all NYC public schools (charter and non-charter) grades 3-8, 2006-2013.\n(The red line is at points where the numbers are equal.)") +
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma)
dev.off()

# ggplot gave us a warning to check out
data[!complete.cases(data), ]
# very rarely a school tests in only one or the other subjects
