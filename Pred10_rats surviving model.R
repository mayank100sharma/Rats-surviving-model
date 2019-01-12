#Reading the file and loading the data
install.packages("survival")
library(survival)

#Using the GLM functionand giving it a vector fitB1
fitB1 = glm(cbind(status, 1-status) ~ rx, family=binomial, data = rats)
fitB1
summary(fitB1)$coef

#Finding the standard error by sqrt of diagonal
sqrt(diag(summary(fitB1)$cov.scaled))
c(fitB1$deviance, -2*logLik(fitB1))

#Null deviance is -2 times the logLik
c(fitB1$null.dev, -2*logLik(update(fitB1, formula = .~ 1)) )

#refit the model as previous one
fitB2 = update(fitB1, family=binomial(link="probit"))
rbind(logit=fitB1$coef, probit=fitB2$coef, rescal.probit=fitB2$coef/0.5513)

#putting expression log(time) with I() for new predictor
fitB3=update(fitB1, formula= .~. + I(log(time)))

#Using time as a predictor
summary(rats$time[rats$status==1])
summary(rats$time[rats$status==0])
cbind(rats[1:10,], model.matrix(fitB3)[1:10,])

#Design matrix column by GLM
summary(fitB3)$coef

#compare deviances with fitB1
c(2*(logLik(fitB3)-logLik(fitB1)), fitB1$dev-fitB3$dev)

#LRT stat to compare with chisq 1
1-pchisq(24.373,1)

#Some additional terms
fitB4=update(fitB3, .~. +I(rx*log(time)) +I(log(time)^2))
summary(fitB4)$coef
fitB3$dev-fitB4$dev

#result to be compared with chisq 2df, and using analysis of deviance table
anova(fitB4)
Devs=c(fitB1$null.dev, fitB1$dev, fitB3$dev, update(fitB3, .~.+I(rx*log(time)))$dev, fitB4$dev)
Devs

#successive difference of LLK
round(-diff(Devs), 3)