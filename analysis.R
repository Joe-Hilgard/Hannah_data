# mixed-model tests
require(lme4)
require(car)

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
dat$ID = as.factor(dat$ID)

dat2  = dat[dat$Block == 2,]
dat3  = dat[dat$Block == 3,]

# read in badSub and discard accordingly
badSubs = read.delim("badSubs.txt")
badSubs2 = badSubs[badSubs$Block == 2,]
badSubs3 = badSubs[badSubs$Block == 3,]
dat2 = dat2[!(dat2$ID %in% badSubs2$ID),]
dat3 = dat3[!(dat3$ID %in% badSubs3$ID),]

# correct trials only for RT analysis
dat2c = dat2[dat2$Error == 0,]
dat3c = dat3[dat3$Error == 0,]

# heirarchical linear models
model1 = lmer(RT ~ race*fixation*WordValence + (1|ID), data=dat2c)
summary(model1)
Anova(model1, type=3)

model2 = lmer(RT ~ race*fixation + (1|ID), data=dat3c)
summary(model2)
Anova(model2, type=3)

model3 = glmer(Error ~ race*fixation*WordValence + (1|ID), data=dat2, family="binomial")
summary(model3)
Anova(model3, type=3)

model4 = glmer(Error ~ race*fixation + (1|ID), data=dat3, family="binomial")
summary(model4)
Anova(model4, type=3)
