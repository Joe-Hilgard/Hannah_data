# Data cleaning and inspection for bad subjects
dat = read.delim("cleaned_data.txt", stringsAsFactors=F)

# break off indiv. blocks to make things easier
dat2 = dat[dat$Block == 2,]
dat3 = dat[dat$Block == 3,]

# Look for people with insanely fast RTs -- they're just jamming a button
meanRT = tapply(dat2$RT, INDEX=dat2$ID, FUN=mean, na.rm=T)
meanRT
barplot(meanRT)
hist(meanRT, breaks=10)

meanRT = tapply(dat3$RT, INDEX=dat3$ID, FUN=mean, na.rm=T)
meanRT
barplot(meanRT)
hist(meanRT, breaks=10)

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

