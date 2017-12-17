# Testing models of factors influencing how Members of Congress vote on healthcare bills

setwd("~/Documents/Learning R!/Congressional Voting Records/")
library(nnet)
library(lattice)

# House of Representatives data
house <- read.csv("fullrec_house.csv")
house$vote.bi <- as.factor(ifelse(house$vote2==1, house$vote.bi<-1, 0))

# Data subsets of House of Reps votes on healthcare
hc.h <- c(6, 58, 256, 308)
sub.hc.h <- subset(house, house$votenum %in% hc.h)
hcplus.h <- c(6, 58, 256, 308, 337)
sub.hcplus.h <- subset(house, house$votenum %in% hcplus.h)

# Relevel to make M the reference level for genders
sub.hc.h <- within(sub.hc.h, gender <- relevel(gender, ref = "M"))
sub.hcplus.h <- within(sub.hcplus.h, gender <- relevel(gender, ref = "M"))

# Models of healthcare votes
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
exp(coef(model4.hc.h))
# medicaid dropped when in model with state --> collinearity
# Run model with just poc instead of separate categories

# model5 and model6: closer 
# odds are [number] to one that if congressperson is [variable], they voted yes
model5.hc.h <- glm(vote.bi ~ party + medicaid + gender + poc, family=binomial(link="logit"), data=sub.hc.h)
summary(model5.hc.h)
coef5.hc.h <- data.frame(summary(model5.hc.h)$coef[summary(model5.hc.h)$coef[,4] <= .1, 1])
colnames(coef5.hc.h) <- "coef"
# pull variables that are significant at the 0.1 level
coef5.hc.h$sig <- summary(model5.hc.h)$coef[summary(model5.hc.h)$coef[,4] <= .1, 4]
coef5.hc.h$odds <- exp(coef5.hc.h$coef)
coef5.hc.h$prob <- coef5.hc.h$odds/(1+coef5.hc.h$odds)
# shows that gender was not significant
# shows there was a 99.6% likelihood that Republican reps voted for healthcare bills;
# 37.4% likelihood that reps in Medicaid expansion states voted yes
# 31.1% likelihood that reps of color voted yes
coef5.hc.h

model6.hc.h <- glm(vote.bi ~ party + medicaid + gender + poc + party*gender + party*poc, family=binomial(link="logit"), data=sub.hc.h)
summary(model6.hc.h)
coef6.hc.h <- data.frame(summary(model6.hc.h)$coef[summary(model6.hc.h)$coef[,4] <= .1, 1])
colnames(coef6.hc.h) <- "coef"
# pull variables that are significant at the 0.1 level
coef6.hc.h$sig <- summary(model6.hc.h)$coef[summary(model6.hc.h)$coef[,4] <= .1, 4]
coef6.hc.h$odds <- exp(coef6.hc.h$coef)
coef6.hc.h$prob <- coef6.hc.h$odds/(1+coef6.hc.h$odds)
# shows that gender was not significant
# shows there was a 99.6% likelihood that Republican reps voted for healthcare bills;
# 37.5% likelihood that reps in Medicaid expansion states voted yes
# 35.5% likelihood that reps of color voted yes
coef6.hc.h

# Senate data
senate <- read.csv("fullrec_senate.csv")
senate$vote.bi <- as.factor(ifelse(senate$vote2==1, senate$vote.bi<-1, 0))

# Data subsets of Senate votes on healthcare
hc.s <- c(26, 167, 168, 169, 179)
sub.hc.s <- subset(senate, senate$votenum %in% hc.s)

# Group Senate Independents and Democrats together
sub.hc.s$party <- as.factor(sub("Independent", "Democrat", sub.hc.s$party))

# Relevel to make Male the reference level for gender
sub.hc.s <- within(sub.hc.s, gender <- relevel(gender, ref = "M"))

# Health care models
# Binomial/binary logistic: coefs give "the change in the log odds of the outcome for a one unit increase in the predictor variable"
# https://stats.idre.ucla.edu/r/dae/logit-regression/
model.hc.s <- glm(vote.bi ~ party + up_2018 + medicaid + gender + white + black + latinx + asian + lgbtq, family=binomial(link="logit"), data=sub.hc.s)
summary(model.hc.s)
model2.hc.s <- glm(vote.bi ~ white + black + latinx + asian, family=binomial(link="logit"), data=sub.hc.s)
summary(model2.hc.s)

# model3: closer
model3.hc.s <- glm(vote.bi ~ party + up_2018 + medicaid + gender + poc, family=binomial(link="logit"), data=sub.hc.s)
summary(model3.hc.s)
coef3.hc.s <- data.frame(summary(model3.hc.s)$coef[summary(model3.hc.s)$coef[,4] <= .1, 1])
colnames(coef3.hc.s) <- "coef"
# pull variables that are significant at the 0.1 level
coef3.hc.s$sig <- summary(model3.hc.s)$coef[summary(model3.hc.s)$coef[,4] <= .1, 4]
coef3.hc.s$odds <- exp(coef3.hc.s$coef)
# https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
coef3.hc.s$prob <- coef3.hc.s$odds/(1+coef3.hc.s$odds)
# shows there was a 11.0% likelihood that women Senators voted for healthcare bills; somewhat surprising
coef3.hc.s