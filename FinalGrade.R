##Reading .csv file into a variable
setwd("E:/USF/ISM6137-SDM/Project/student/Final/Final1");
schoolData=read.csv("SchoolDataFinal.csv")
rm(list = ls())
schoolDataPortu=schoolData[schoolData$Language == 'Portugese',]
nrow(schoolDataPortu)
hist(log(schoolDataPortu$number.of.school.absences))
boxplot(schoolDataPortu$first.period.grade)
hist(log(schoolDataPortu$Failures))
influencePlot(schoolFirstPeriodModel_Math,id.method=identify)

#Maths Model
schoolPeriodModel_Portu=lm(Total.Grade ~ Studytime
                                + as.factor(extra.curricular.activities)
                                + as.factor(Internet.access.at.home)
                                + log(number.of.school.absences+1)
                                + as.factor(School.educational.support_Lag1)
                           #+ as.factor(School.educational.support_Lag1)
                           #    + as.factor(School.educational.support_Lag2)
                           #    + as.factor(Family.educational.support_Lag1)
                           #    + as.factor(Family.educational.support_Lag2)
                           #    + as.factor(extra.paid.classes_Lag1)
                           #    + as.factor(extra.paid.classes_Lag2)
                               + workday.alcohol.consumption
                               + weekend.alcohol.consumption
                               + current.health.status
                               + quality.of.family.relationships
                               + as.factor(wants.to.take.higher.education)
                           #+ as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                                + as.factor(School)
                                + Failures,data = schoolDataPortu)
summary(schoolPeriodModel_Portu)
AIC(schoolPeriodModel_Portu)
BIC(schoolPeriodModel_Portu)

shapiro.test(schoolFirstPeriodModel_Portu$res)
#Normally Distributed
#Homoskedasticity
plot(schoolFirstPeriodModel_Portu)

bartlett.test(list(schoolFirstPeriodModel_Portu$res, schoolFirstPeriodModel_Portu$fit))

#Second Period Model
schoolSecondPeriodModel_Portu=lm(second.period.grade ~ Studytime
                                 + as.factor(extra.curricular.activities)
                                 + as.factor(Internet.access.at.home)
                                 + log(number.of.school.absences+1)
                                 + first.period.grade
                                 + as.factor(School.educational.support_Lag1)
                                 + as.factor(School.educational.support_Lag2)
                                 + as.factor(Family.educational.support_Lag1)
                                 + as.factor(Family.educational.support_Lag2)
                                 + as.factor(extra.paid.classes_Lag1)
                                 + as.factor(extra.paid.classes_Lag2)
                                 + workday.alcohol.consumption
                                 + weekend.alcohol.consumption
                                 + current.health.status
                                 + quality.of.family.relationships
                                 + as.factor(wants.to.take.higher.education)
                                 + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                                 + as.factor(School)
                                 + Failures,data = schoolDataPortu)
summary(schoolSecondPeriodModel_Portu)
AIC(schoolSecondPeriodModel_Portu)
BIC(schoolSecondPeriodModel_Portu)

shapiro.test(schoolSecondPeriodModel_Portu$res)
#Normally Distributed
#Homoskedasticity
plot(schoolSecondPeriodModel_Portu)

bartlett.test(list(schoolSecondPeriodModel_Portu$res, schoolSecondPeriodModel_Portu$fit))


#Second Grade
schoolThirdPeriodModel_Portu=lm(final.grade ~ Studytime
                                + as.factor(extra.curricular.activities)
                                + as.factor(Internet.access.at.home)
                                + log(number.of.school.absences+1)
                                + first.period.grade
                                + second.period.grade
                                + as.factor(School.educational.support_Lag1)
                                + as.factor(School.educational.support_Lag2)
                                + as.factor(Family.educational.support_Lag1)
                                + as.factor(Family.educational.support_Lag2)
                                + as.factor(extra.paid.classes_Lag1)
                                + as.factor(extra.paid.classes_Lag2)
                                + workday.alcohol.consumption
                                + weekend.alcohol.consumption
                                + current.health.status
                                + quality.of.family.relationships
                                + as.factor(wants.to.take.higher.education)
                                + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                                + as.factor(School)
                                + Failures,data = schoolDataPortu)
summary(schoolThirdPeriodModel_Portu)
AIC(schoolThirdPeriodModel_Portu)
BIC(schoolThirdPeriodModel_Portu)


#Multi variate normality Assumptions
hist(schoolThirdPeriodModel_Portu$residuals)
qqnorm(schoolThirdPeriodModel_Portu$residuals)
qqline(schoolThirdPeriodModel_Portu$residuals,col="red")
shapiro.test(schoolThirdPeriodModel_Portu$res)
#Normally Distributed
#Homoskedasticity
plot(schoolThirdPeriodModel_Portu)

bartlett.test(list(schoolThirdPeriodModel_Portu$res, schoolThirdPeriodModel_Portu$fit))
#heteroskedastic


#GLS
schoolFirstPeriodModel_Portu_GLS=gls(Total.Grade ~ Studytime
                                     + as.factor(extra.curricular.activities)
                                     + as.factor(Internet.access.at.home)
                                     + log(number.of.school.absences+1)
                                     + as.factor(School.educational.support_Lag1)
                                     + as.factor(School.educational.support_Lag2)
                                     + as.factor(Family.educational.support_Lag1)
                                     + as.factor(Family.educational.support_Lag2)
                                     + as.factor(extra.paid.classes_Lag1)
                                     + as.factor(extra.paid.classes_Lag2)
                                     + workday.alcohol.consumption
                                     + weekend.alcohol.consumption
                                     + current.health.status
                                     #+ first.period.grade
                                     #+ second.period.grade
                                     + quality.of.family.relationships
                                     + as.factor(wants.to.take.higher.education)
                                     + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                                     + as.factor(School)
                                     + Failures,data = schoolDataPortu,na.action=na.exclude)
summary(schoolFirstPeriodModel_Portu_GLS)
AIC(schoolFirstPeriodModel_Portu_GLS)
BIC(schoolFirstPeriodModel_Portu_GLS)
t.test(schoolDataPortu$first.period.grade~schoolDataPortu$School)

library(car)
scatterplot(schoolDataPortu$Studytime~schoolDataPortu$Total.Grade, boxplots=FALSE, smooth=TRUE, reg.line=FALSE)

schoolSecondPeriodModel_Portu_GLS=gls(second.period.grade ~ Studytime
                                      + as.factor(extra.curricular.activities)
                                      + as.factor(Internet.access.at.home)
                                      + log(number.of.school.absences+1)
                                      + as.factor(School.educational.support_Lag1)
                                      + as.factor(School.educational.support_Lag2)
                                      + as.factor(Family.educational.support_Lag1)
                                      + as.factor(Family.educational.support_Lag2)
                                      + as.factor(extra.paid.classes_Lag1)
                                      + as.factor(extra.paid.classes_Lag2)
                                      + workday.alcohol.consumption
                                      + weekend.alcohol.consumption
                                      + current.health.status
                                      + first.period.grade
                                      #+ second.period.grade
                                      + quality.of.family.relationships
                                      + as.factor(wants.to.take.higher.education)
                                      + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                                      + as.factor(School)
                                      + Failures,data = schoolDataPortu,na.action=na.exclude)
summary(schoolSecondPeriodModel_Portu_GLS)
AIC(schoolSecondPeriodModel_Portu_GLS)
BIC(schoolSecondPeriodModel_Portu_GLS)

schoolThirdPeriodModel_Portu_GLS=gls(final.grade ~ Studytime
                                     + as.factor(extra.curricular.activities)
                                     + as.factor(Internet.access.at.home)
                                     + log(number.of.school.absences+1)
                                     + as.factor(School.educational.support_Lag1)
                                     + as.factor(School.educational.support_Lag2)
                                     + as.factor(Family.educational.support_Lag1)
                                     + as.factor(Family.educational.support_Lag2)
                                     + as.factor(extra.paid.classes_Lag1)
                                     + as.factor(extra.paid.classes_Lag2)
                                     + workday.alcohol.consumption
                                     + weekend.alcohol.consumption
                                     + current.health.status
                                     + first.period.grade
                                     + second.period.grade
                                     + quality.of.family.relationships
                                     + as.factor(wants.to.take.higher.education)
                                     + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                                     + as.factor(School)
                                     + Failures,data = schoolDataPortu,na.action=na.exclude)
summary(schoolThirdPeriodModel_Portu_GLS)
AIC(schoolThirdPeriodModel_Portu_GLS)
BIC(schoolThirdPeriodModel_Portu_GLS)

First_gradeModel_Portu = lmer(first.period.grade ~ Studytime
                              + as.factor(extra.curricular.activities)
                              + as.factor(Internet.access.at.home)
                              + log(number.of.school.absences+1)
                              + as.factor(School.educational.support_Lag1)
                              + as.factor(School.educational.support_Lag2)
                              + as.factor(Family.educational.support_Lag1)
                              + as.factor(Family.educational.support_Lag2)
                              + as.factor(extra.paid.classes_Lag1)
                              + as.factor(extra.paid.classes_Lag2)
                              + workday.alcohol.consumption
                              + weekend.alcohol.consumption
                              + current.health.status
                              # + first.period.grade
                              # + second.period.grade
                              + quality.of.family.relationships
                              + as.factor(wants.to.take.higher.education)
                              + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                              + as.factor(School)
                              + Failures
                              + (1|School), data=schoolDataPortu )
summary(First_gradeModel_Portu)
AIC(First_gradeModel_Portu)
BIC(First_gradeModel_Portu)
ranef(First_gradeModel_Portu)

Second_gradeModel_Portgu = lmer(second.period.grade ~ Studytime
                                + as.factor(extra.curricular.activities)
                                + as.factor(Internet.access.at.home)
                                + log(number.of.school.absences+1)
                                + as.factor(School.educational.support_Lag1)
                                + as.factor(School.educational.support_Lag2)
                                + as.factor(Family.educational.support_Lag1)
                                + as.factor(Family.educational.support_Lag2)
                                + as.factor(extra.paid.classes_Lag1)
                                + as.factor(extra.paid.classes_Lag2)
                                + workday.alcohol.consumption
                                + weekend.alcohol.consumption
                                + current.health.status
                                + first.period.grade
                                # + second.period.grade
                                + quality.of.family.relationships
                                + as.factor(wants.to.take.higher.education)
                                + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                                + as.factor(School)
                                + Failures
                                + (1|School), data=schoolDataPortu )
summary(Second_gradeModel_Portgu)
AIC(Second_gradeModel_Portgu)
BIC(Second_gradeModel_Portgu)
ranef(Second_gradeModel_Portgu)


Final_gradeModel_Portu = lmer(final.grade ~ Studytime
                              + as.factor(extra.curricular.activities)
                              + as.factor(Internet.access.at.home)
                              + log(number.of.school.absences+1)
                              + as.factor(School.educational.support_Lag1)
                              + as.factor(School.educational.support_Lag2)
                              + as.factor(Family.educational.support_Lag1)
                              + as.factor(Family.educational.support_Lag2)
                              + as.factor(extra.paid.classes_Lag1)
                              + as.factor(extra.paid.classes_Lag2)
                              + workday.alcohol.consumption
                              + weekend.alcohol.consumption
                              + current.health.status
                              + first.period.grade
                              + second.period.grade
                              + quality.of.family.relationships
                              + as.factor(wants.to.take.higher.education)
                              + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                              + as.factor(School)
                              + Failures
                              + (1|School), data=schoolDataPortu )
summary(Final_gradeModel_Portu)
AIC(Final_gradeModel_Portu)
BIC(Final_gradeModel_Portu)
ranef(Final_gradeModel_Portu)

