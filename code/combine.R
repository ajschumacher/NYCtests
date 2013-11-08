# read in 8 CSV files
# which themselves come from 6 Excel files
# and produce one CSV with all the average score information to look at

readDOE <- function(filename) {
  read.csv(filename,
           skip=7,
           header=FALSE,
           as.is=TRUE,
           na.strings=c("", "s"))
}

ela <- do.call(rbind, lapply(c("../data/nonCharterELA2006-2012.csv",
                               "../data/charterELA2006-2012.csv",
                               "../data/nonCharterELA2013.csv",
                               "../data/charterELA2013.csv"),
                             readDOE))[,1:6]
ela$subject <- "ELA"
math <- do.call(rbind, lapply(c("../data/nonCharterMath2006-2012.csv",
                                "../data/charterMath2006-2012.csv",
                                "../data/nonCharterMath2013.csv",
                                "../data/charterMath2013.csv"),
                              readDOE))[,1:6]
math$subject <- "Math"
all <- rbind(ela, math)
names(all) <- c("dbn", "grade", "year", "category", "n", "score", "subject")
stopifnot(all(all$category=="All Students"))
all$category <- NULL
all <- all[,c("dbn", "grade", "year", "subject", "n", "score")]
stopifnot(all(!duplicated(all[,1:4])))

write.csv(all, "../data/all.csv", row.names=FALSE)
