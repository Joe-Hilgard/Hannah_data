# Data cleaning and inspection for bad subjects
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

# break off indiv. blocks to make things easier
dat2 = dat[dat$Block == 2,]
dat3 = dat[dat$Block == 3,]

# Look for people with insanely fast RTs -- they're just jamming a button
meanRT = tapply(dat2$RT, INDEX=dat2$ID, FUN=mean)
meanRT
barplot(meanRT)
hist(meanRT, breaks=10)

meanRT = tapply(dat3$RT, INDEX=dat3$ID, FUN=mean, na.rm=T)
meanRT
barplot(meanRT)
hist(meanRT, breaks=10)

countNA = function(x) {
  temp = is.na(x)
  count = sum(temp)
  return(count)
}
numNA = tapply(dat2$Error, INDEX=dat2$ID, FUN=countNA)
numNA
barplot(numNA)
hist(numNA)

numNA = tapply(dat3$Error, INDEX=dat3$ID, FUN=countNA)
numNA
barplot(numNA)
hist(numNA)
# who's doing shitty?
meanError = tapply(dat2$Error, INDEX=dat2$ID, FUN=mean, na.rm=T)
meanError
barplot(meanError)
hist(meanError, breaks=10)
# Looks like we have some shitty subjects

meanError = tapply(dat3$Error, INDEX=dat3$ID, FUN=mean, na.rm=T)
meanError
barplot(meanError)
hist(meanError, breaks=10)

# I find that significance testing sometimes works well for
  # subject inclusion/exclusion.
# We want participants whose behavior would be weird (p<.01)
  # if they were totally inaccurate.

# What's the critical accuracy for p<.01?
  # How many trials do we have / subject?
dim(dat2[dat2$ID==2,]) # 192 rows / subject
table(dat2$ID)
table(dat2$fixation[dat2$ID==2]) # 64+64+64 = 192
# Probability of getting trial correct by chance is 50%
# so what's the critical number of correct trials to be 
  # not an asshole, p<.01?
alpha = .01
qbinom(p = 1-alpha, 
       size = 192,
       prob=.5)
# grab subjects who got <112 correct
tab = 192 - tapply(dat2$Error, dat2$ID, FUN=sum)
badTab = tab[tab < 112]
# make a log of it
badSubs2 = data.frame("ID" = names(badTab),
                     "Block" = 2,
                     "correctCount" = badTab,
                     "reason" = "Bad accuracy")
# Repeat for Block 3
table(dat3$ID) # 96 trials/subject
# critical number of correct?
alpha = .01
qbinom(p = 1-alpha, 
       size = 96,
       prob=.5) # 59
# grab subjects who got <59 correct
tab = 96 - tapply(dat3$Error, dat3$ID, FUN=sum)
badTab = tab[tab < 59]
# make a log of it
badSubs3 = data.frame("ID" = names(badTab),
                     "Block" = 3,
                     "correctCount" = badTab,
                     "reason" = "Bad accuracy")
# combine the data tables
badSubs = rbind(badSubs2, badSubs3)

write.table(badSubs, file="badSubs.txt", sep="\t", row.names=F)
