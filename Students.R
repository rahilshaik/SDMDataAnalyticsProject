##Reading .csv file into a variable
setwd("E:/USF/ISM6137-SDM/Project/student/Final");
schoolData=read.csv("SchoolDataFinal.csv")
View(schoolData)

boxplot(schoolData$first.period.grade)
hist(schoolData$first.period.grade)
hist(schoolData$second.period.grade)
hist(schoolData$Age)


schoolFirstPeriodModel=lm(first.period.grade ~ Studytime
                          + as.factor(quality.of.family.relationships)
                          + as.factor(extra.curricular.activities)
                          + as.factor(Internet.access.at.home)
                          + as.factor(workday.alcohol.consumption) 
                          + as.factor(weekend.alcohol.consumption)
                          + number.of.school.absences
                          + as.factor(current.health.status) 
                          + as.factor(wants.to.take.higher.education)
                          + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                          + as.factor(Language)
                          + as.factor(School)
                          + as.factor(School.educational.support_Lag1)
                          + as.factor(School.educational.support_Lag2)
                          + as.factor(Family.educational.support_Lag1)
                          + as.factor(Family.educational.support_Lag2)
                          + as.factor(extra.paid.classes_Lag1)
                          + as.factor(extra.paid.classes_Lag2)
                          + Failures,data = schoolData)
summary(schoolFirstPeriodModel)
plot(schoolFirstPeriodModel$residuals ~schoolFirstPeriodModel$fitted.values )


#Multi variate normality Assumptions
hist(schoolFirstPeriodModel$residuals)
qqnorm(schoolFirstPeriodModel$residuals)
qqline(schoolFirstPeriodModel$residuals,col="red")
shapiro.test(schoolFirstPeriodModel$res)
#Normally Distributed
#Homoskedasticity
plot(schoolFirstPeriodModel)

bartlett.test(list(schoolFirstPeriodModel$res, schoolFirstPeriodModel$fit))
#heteroskedastic

plot(schoolFirstPeriodModel$res~(schoolData$first.period.grade))

#Nonlinear

#Multicollinearity
library("car")
vif(schoolFirstPeriodModel)
#VIF analysis shows no predictor with VIF>10. Hence multicollinearity is not a problem in this model.

wl = 1 / apply(schoolData, 2, function(x){ var(x, na.rm=T) })
warnings()
w = with(schoolData, ifelse(ind=="A", wl[1], ifelse(ind=="B", wl[2], wl[3])))

schoolFirstPeriodModelWeights=lm(first.period.grade ~ Studytime + as.factor(extra.curricular.activities)+as.factor(Internet.access.at.home) + 
                            as.factor(workday.alcohol.consumption)+ as.factor(weekend.alcohol.consumption)+ number.of.school.absences+
                            as.factor(current.health.status) + as.factor(wants.to.take.higher.education)+
                            as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)+
                            as.factor(Language)+as.factor(School)
                          +Failures,data = schoolData,weights = exp(-Studytime))
summary(schoolFirstPeriodModelWeights)
plot(schoolFirstPeriodModelWeights)
length(schoolData$Failures)
plot(schoolFirstPeriodModelWeights$residuals ~schoolFirstPeriodModelWeights$fitted.values )

length(schoolData)
bartlett.test(list(schoolFirstPeriodModelWeights$res, schoolFirstPeriodModelWeights$fit))

#GLS

library(nlme)
schoolFirstPeriodModelgls=gls(first.period.grade ~ Studytime + as.factor(extra.curricular.activities)+as.factor(Internet.access.at.home) + 
                            as.factor(workday.alcohol.consumption)+ as.factor(weekend.alcohol.consumption)+ number.of.school.absences+
                            as.factor(current.health.status) + as.factor(wants.to.take.higher.education)+
                            as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)+
                            as.factor(Language)+as.factor(School)
                          +Failures,data = schoolData)
summary(schoolFirstPeriodModelgls)
plot(glsModel)

bartlett.test(list(glsModel$res, glsModel$fitted))


schoolSecondPeriodModel=lm(second.period.grade ~ Studytime + as.factor(extra.curricular.activities)+as.factor(Internet.access.at.home) + 
                            as.factor(workday.alcohol.consumption)+ as.factor(weekend.alcohol.consumption)+ number.of.school.absences+
                            as.factor(current.health.status) + as.factor(wants.to.take.higher.education)+
                            as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)+
                            as.factor(Language)+as.factor(School)
                          +Failures,data = schoolData )
summary(schoolSecondPeriodModel)




plot(schoolFirstPeriodModel$residuals ~schoolData$first.period.grade )