# Testing models of various factors that influence how Members of Congress vote on healthcare bills

setwd("~/Documents/Learning R!/Congressional Voting Records/")
library(nnet)
library(lattice)

# House of Representatives
house <- read.csv("fullrec_house.csv")
house$vote.bi <- as.factor(ifelse(house$vote2==1, house$vote.bi<-1, 0))

# Health care subsets
hc.h <- c(6, 58, 256, 308)
sub.hc.h <- subset(house, house$votenum %in% hc.h)
hcplus.h <- c(6, 58, 256, 308, 337)
sub.hcplus.h <- subset(house, house$votenum %in% hcplus.h)

# sub.hc.h6 <- subset(house, house$votenum==6)
# sub.hc.h58 <- subset(house, house$votenum==58)
# sub.hc.h256 <- subset(house, house$votenum==256)
# sub.hc.h308 <- subset(house, house$votenum==308)

# Relevel to make M the reference level for genders
sub.hc.h <- within(sub.hc.h, gender <- relevel(gender, ref = "M"))
sub.hcplus.h <- within(sub.hcplus.h, gender <- relevel(gender, ref = "M"))

# Subset of Republicans
subR.hc.h <- subset(sub.hc.h, sub.hc.h$party == "Republican")

# Health care models
# Binomial/binary logistic: coefs give "the change in the log odds of the outcome for a one unit increase in the predictor variable"
# https://stats.idre.ucla.edu/r/dae/logit-regression/
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
model.hc.h <- glm(vote.bi ~ party + medicaid + gender + white + black + latinx + asian + lgbtq, family=binomial(link="logit"), data=sub.hc.h)
summary(model.hc.h)
model2.hc.h <- glm(vote.bi ~ party + medicaid + gender + white + black + party*black + latinx + party*latinx + asian + party*asian + lgbtq, family=binomial(link="logit"), data=sub.hc.h)
summary(model2.hc.h)
model3.hc.h <- glm(vote.bi ~ white + black + latinx + asian, family=binomial(link="logit"), data=sub.hc.h)
summary(model3.hc.h)
model4.hc.h <- glm(vote.bi ~ party + medicaid + gender + white + black + latinx + asian, family=binomial(link="logit"), data=sub.hc.h)
summary(model4.hc.h)
# odds are [number] to one that if congressperson is [whatever], they voted yes
exp(coef(model4.hc.h))
model5.hc.h <- glm(vote.bi ~ party + medicaid + gender + poc, family=binomial(link="logit"), data=sub.hc.h)
summary(model5.hc.h)
coef5.hc.h <- data.frame(summary(model5.hc.h)$coef[summary(model5.hc.h)$coef[,4] <= .1, 1])
colnames(coef5.hc.h) <- "coef"
coef5.hc.h$sig <- summary(model5.hc.h)$coef[summary(model5.hc.h)$coef[,4] <= .1, 4]
coef5.hc.h$odds <- exp(coef5.hc.h$coef)
coef5.hc.h$prob <- coef5.hc.h$odds/(1+coef5.hc.h$odds)
coef5.hc.h
# coef5.hc.h <- data.frame(exp(coef(model5.hc.h)))
# colnames(coef5.hc.h) <- "odds"
model6.hc.h <- glm(vote.bi ~ party + medicaid + gender + poc + party*gender + party*poc, family=binomial(link="logit"), data=sub.hc.h)
summary(model6.hc.h)
coef6.hc.h <- data.frame(summary(model6.hc.h)$coef[summary(model6.hc.h)$coef[,4] <= .1, 1])
colnames(coef6.hc.h) <- "coef"
coef6.hc.h$sig <- summary(model6.hc.h)$coef[summary(model6.hc.h)$coef[,4] <= .1, 4]
coef6.hc.h$odds <- exp(coef6.hc.h$coef)
coef6.hc.h$prob <- coef6.hc.h$odds/(1+coef6.hc.h$odds)
coef6.hc.h
# coef6.hc.h <- data.frame(exp(coef(model6.hc.h)))
# colnames(coef6.hc.h) <- "odds"

# Models with Republican subset
model5R.hc.h <- glm(vote.bi ~ medicaid + gender + poc, family=binomial(link="logit"), data=subR.hc.h)
summary(model5R.hc.h)
exp(coef(model5R.hc.h))

# Models by individual vote
# model5.hc.h6 <- glm(vote.bi ~ party + medicaid + gender + poc, family=binomial(link="logit"), data=sub.hc.h6)
# summary(model5.hc.h6)
# exp(coef(model5.hc.h6))

# medicaid dropped when in model with state --> collinearity
# Run model with just poc instead of separate categories

# Senate
senate <- read.csv("fullrec_senate.csv")
senate$vote.bi <- as.factor(ifelse(senate$vote2==1, senate$vote.bi<-1, 0))

# Health care subsets
hc.s <- c(26, 167, 168, 169, 179)
sub.hc.s <- subset(senate, senate$votenum %in% hc.s)
sub.hc.s$party <- as.factor(sub("Independent", "Democrat", sub.hc.s$party))

# Relevel to make M the reference level for gender
sub.hc.s <- within(sub.hc.s, gender <- relevel(gender, ref = "M"))

# Health care models
# Binomial/binary logistic: coefs give "the change in the log odds of the outcome for a one unit increase in the predictor variable"
# https://stats.idre.ucla.edu/r/dae/logit-regression/
model.hc.s <- glm(vote.bi ~ party + up_2018 + medicaid + gender + white + black + latinx + asian + lgbtq, family=binomial(link="logit"), data=sub.hc.s)
summary(model.hc.s)
model2.hc.s <- glm(vote.bi ~ white + black + latinx + asian, family=binomial(link="logit"), data=sub.hc.s)
summary(model2.hc.s)
model3.hc.s <- glm(vote.bi ~ party + up_2018 + medicaid + gender + poc, family=binomial(link="logit"), data=sub.hc.s)
summary(model3.hc.s)
coef3.hc.s <- data.frame(summary(model3.hc.s)$coef[summary(model3.hc.s)$coef[,4] <= .1, 1])
colnames(coef3.hc.s) <- "coef"
coef3.hc.s$sig <- summary(model3.hc.s)$coef[summary(model3.hc.s)$coef[,4] <= .1, 4]
coef3.hc.s$odds <- exp(coef3.hc.s$coef)
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
coef3.hc.s$prob <- coef3.hc.s$odds/(1+coef3.hc.s$odds)
coef3.hc.s

# Try party-race interaction varbs

# See https://www.r-bloggers.com/learn-logistic-regression-and-beyond/
# Understanding "glm.fit: fitted probabilities numerically 0 or 1 occurred" 
densityplot(predict(model.hc.s,type='link'),groups=sub.hc.s$vote.bi,auto.key=T)

# No, not continuous: http://thestatsgeek.com/2015/01/17/why-shouldnt-i-use-linear-regression-if-my-outcome-is-binary/
# lm.hc.s <- glm(vote.bi ~ party + up_2018 + medicaid + gender + party*gender + white + black + latinx + asian + lgbtq, data=sub.hc.s)
# summary(lm.hc.s)
