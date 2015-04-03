# Making averages with dplyr
# install.packages(c('dplyr', 'magrittr'))
library(dplyr)
library(magrittr)
library(lme4)

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

# magrittr is a package that improves the visibility of your code
# it does this by providing you with a "pipe" operator %>%
# A "pipe" takes the result of your last operation and passes it forward to the next
# so instead of 
paste("Mean log mpg is ", round(mean(log(mtcars$mpg+10)), 2), ".", sep="")
# you can say
mtcars$mpg %>%
  add(10) %>%
  log %>%
  mean %>%
  round(2) %>%
  paste("Mean log mpg is ", ., ".", sep="")
# The %>% passes the result forward to the first argument of the next function
# If it needs to go in a different position, use a . to mark the spot

# the name comes from a Magritte joke, "Ceci n'est pas un pipe."

# Anyway you may find piping useful in making your code more legible.
# Remember that legibility is always nice -- even if nobody else is looking at it,
#   you will be returning to your code in a few months!

# The dplyr package is another powerful Hadley Wickham package for 
  # filtering & summarizing data.
# Suppose we want just one average RT and Accuracy for each trial type w/in each subject.
names(dat)
# dplyr gives filtering functions that are often tidier than the base indexing stuff
dat = 
  dat %>%
  select(-Race, -Fix)

dat2 =
  dat %>%
  filter(Block == 2)
# We use dplyr's group_by() to identify what we want to separate by
dat2.acc = 
  dat2  %>% 
  group_by(ID, race, WordValence, fixation) %>%
  summarise(meanACC = 1 - mean(Error, na.rm=T),
            count = n())

m1 = lmer(meanACC ~ race * WordValence * fixation + (1|ID), data=dat2.acc)
summary(m1)
Anova(m1, type=3)
summary(m1)$coefficients

dat2.rt = 
  dat2  %>% 
  group_by(ID, race, WordValence, fixation) %>%
  summarise(meanRT = mean(RT, na.rm=T),
            count = n())

m2 = lmer(meanRT ~ race * WordValence * fixation + (1|ID), data=dat2.rt)
Anova(m2, type=3)
summary(m2)
