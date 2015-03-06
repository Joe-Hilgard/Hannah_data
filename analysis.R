# mixed-model tests
require(lme4)
require(car)

dat = read.delim("cleaned_data.txt", stringsAsFactors=F)
dat$ID = as.factor(dat$ID)

dat2  = dat[dat$Block == 2,]
dat2c = dat[dat$Block == 2 & dat$Error == 0,]
dat3  = dat[dat$Block == 3,]
dat3c = dat[dat$Block == 3 & dat$Error == 0,]

model1 = lmer(RT ~ Race*fixation*WordValence + (1|ID), data=dat2c)
summary(model1)
Anova(model1, type=3)

model2 = lmer(RT ~ Race*fixation + (1|ID), data=dat3c)
summary(model2)
Anova(model2, type=3)

model3 = glmer(Error ~ Race*fixation*WordValence + (1|ID), data=dat2, family="binomial")
summary(model3)
Anova(model3, type=3)

model4 = glmer(Error ~ Race*fixation + (1|ID), data=dat3, family="binomial")
summary(model4)
Anova(model4, type=3)
