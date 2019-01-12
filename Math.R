##Reading .csv file into a variable
setwd("E:/USF/ISM6137-SDM/Project/student/Final/Final1");
schoolData=read.csv("SchoolDataFinal.csv")

schoolDataMath=schoolData[schoolData$Language == 'Math',]
nrow(schoolDataMath)
hist(schoolDataMath$Failures)
boxplot(schoolDa000taMath$first.period.grade)
hist(log(schoolDataMath$Failures))
influencePlot(schoolFirstPeriodModel_Math,id.method=identify)

#Maths Model
schoolFirstPeriodModel_Math=lm(first.period.grade ~ Studytime
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
                               + quality.of.family.relationships
                               + as.factor(wants.to.take.higher.education)
                               + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                               + as.factor(School)
                               + Failures,data = schoolDataMath)
summary(schoolFirstPeriodModel_Math)
AIC(schoolFirstPeriodModel_Math)
BIC(schoolFirstPeriodModel_Math)
library("car")
vif(schoolFirstPeriodModel_Math)

shapiro.test(schoolFirstPeriodModel_Math$res)
#Normally Distributed
#Homoskedasticity
plot(schoolFirstPeriodModel_Math)

bartlett.test(list(schoolFirstPeriodModel_Math$res, schoolFirstPeriodModel_Math$fit))

#Second Period Model
schoolSecondPeriodModel_Math=lm(second.period.grade ~ Studytime
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
                               + Failures,data = schoolDataMath)
summary(schoolSecondPeriodModel_Math)
summary(schoolSecondPeriodModel_Math)
AIC(schoolSecondPeriodModel_Math)
BIC(schoolSecondPeriodModel_Math)

shapiro.test(schoolSecondPeriodModel_Math$res)
#Normally Distributed
#Homoskedasticity
plot(schoolSecondPeriodModel_Math)

bartlett.test(list(schoolSecondPeriodModel_Math$res, schoolSecondPeriodModel_Math$fit))


#Second Grade
schoolThirdPeriodModel_Math=lm(final.grade ~ Studytime
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
                               + Failures,data = schoolDataMath)
summary(schoolThirdPeriodModel_Math)
AIC(schoolThirdPeriodModel_Math)
BIC(schoolThirdPeriodModel_Math)


#Multi variate normality Assumptions
hist(schoolThirdPeriodModel_Math$residuals)
qqnorm(schoolThirdPeriodModel_Math$residuals)
qqline(schoolThirdPeriodModel_Math$residuals,col="red")
shapiro.test(schoolThirdPeriodModel_Math$res)
#Normally Distributed
#Homoskedasticity
plot(schoolThirdPeriodModel_Math)

bartlett.test(list(schoolThirdPeriodModel_Math$res, schoolThirdPeriodModel_Math$fit))
#heteroskedastic


#GLS
schoolFirstPeriodModel_Math_GLS=gls(first.period.grade ~ Studytime
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
                               + Failures,data = schoolDataMath,na.action=na.exclude)
summary(schoolFirstPeriodModel_Math_GLS)
AIC(schoolFirstPeriodModel_Math_GLS)
BIC(schoolFirstPeriodModel_Math_GLS)

schoolSecondPeriodModel_Math_GLS=gls(second.period.grade ~ Studytime
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
                                    + Failures,data = schoolDataMath,na.action=na.exclude)
summary(schoolSecondPeriodModel_Math_GLS)
AIC(schoolSecondPeriodModel_Math_GLS)
BIC(schoolSecondPeriodModel_Math_GLS)

schoolThirdPeriodModel_Math_GLS=gls(final.grade ~ Studytime
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
                                     + Failures,data = schoolDataMath,na.action=na.exclude)
summary(schoolThirdPeriodModel_Math_GLS)
AIC(schoolThirdPeriodModel_Math_GLS)
BIC(schoolThirdPeriodModel_Math_GLS)

First_gradeModel_Math = lmer(first.period.grade ~ Studytime
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
                        + (1|School), data=schoolDataMath )
summary(First_gradeModel_Math)
AIC(First_gradeModel_Math)
BIC(First_gradeModel_Math)
ranef(Thirdmodel_grade)

Second_gradeModel_Math = lmer(second.period.grade ~ Studytime
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
                             + (1|School), data=schoolDataMath )
summary(Second_gradeModel_Math)
AIC(Second_gradeModel_Math)
BIC(Second_gradeModel_Math)
ranef(Second_gradeModel_Math)


Final_gradeModel_Math = lmer(final.grade ~ Studytime
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
                              + (1|School), data=schoolDataMath )
summary(Final_gradeModel_Math)
AIC(Final_gradeModel_Math)
BIC(Final_gradeModel_Math)
ranef(Final_gradeModel_Math)

