# postponing this to the future~

"""
In 2013 the tests aligned to the Common Core. As we've seen, average scores were way down from 2012, but we've attempted to normalize that away. If the new tests truly are testing different things, then I'd expect to see more variability (for both grades and cohorts) in the changes into the year 2013 tests. We shall see!

Okay! So we're aligning tests to the Common Core! In 2013 the tests will be so different! Are teachers ready? Are students ready? WHO KNOWS?!?! It's the Common Core, and it's shaking things up!

Nope.
"""

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

png(width=800, height=480, filename="../figure/11-3.png")
grid.arrange(a, c, b, d, main="\nFigure 11-3. Changes in average Math and ELA test scores for same grade year to year and same cohort grade to grade, 2006-2013", ncol=2)
dev.off()


a <- ggplot(subset(stable_grade, subject=="Math")) +
  aes(x=delta) + geom_density() +
  facet_grid(~into_year) +# ylim(c(-2, 2)) + xlim(c(-4, 4)) +
  xlab("Math change from year to year") +
  theme_bw()
b <- ggplot(subset(stable_cohort, subject=="Math" & !is.na(into_year))) +
  aes(x=delta) + geom_density() +
  facet_grid(~into_year) +# ylim(c(-2, 2)) + xlim(c(-4, 4)) +
  xlab("Math change from grade to grade") +
  theme_bw()
c <- ggplot(subset(stable_grade, subject=="ELA")) +
  aes(x=delta) + geom_density() +
  facet_grid(~into_year) +# ylim(c(-2, 2)) + xlim(c(-4, 4)) +
  xlab("ELA change from year to year") +
  theme_bw()
d <- ggplot(subset(stable_cohort, subject=="ELA" & !is.na(into_year))) +
  aes(x=delta) + geom_density() +
  facet_grid(~into_year) +# ylim(c(-2, 2)) + xlim(c(-4, 4)) +
  xlab("ELA change from grade to grade") +
  theme_bw()

png(width=800, height=480, filename="../figure/11-4.png")
grid.arrange(a, c, b, d, main="\nFigure 11-4. Density of changes in average Math and ELA test scores for same grade year to year and same cohort grade to grade, 2006-2013", ncol=2)
dev.off()
