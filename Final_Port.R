##Reading .csv file into a variable
setwd("E:/USF/ISM6137-SDM/Project/student/Final/Final1");
schoolData=read.csv("SchoolDataFinal.csv")

schoolDataMath=schoolData[schoolData$Language == 'Portugese',]
nrow(schoolDataMath)
hist(schoolDataMath$Failures)
boxplot(schoolDa000taMath$first.period.grade)
hist(log(schoolDataMath$Failures))
influencePlot(schoolFirstPeriodModel_Math,id.method=identify)

#Maths Model
schoolFinalPeriodModel_Math=lm(final.grade ~ Studytime
                               + as.factor(extra.curricular.activities)
                               + as.factor(Internet.access.at.home)
                               + log(number.of.school.absences+1)
                               + workday.alcohol.consumption
                               +first.period.grade
                               +second.period.grade
                               + weekend.alcohol.consumption
                               + current.health.status
                               + quality.of.family.relationships
                               + as.factor(wants.to.take.higher.education)
                               + as.factor(School)
                               + Failures,data = schoolDataMath)
summary(schoolFinalPeriodModel_Math)
AIC(schoolFinalPeriodModel_Math)
BIC(schoolFinalPeriodModel_Math)
library("car")
vif(schoolFinalPeriodModel_Math)

shapiro.test(schoolFinalPeriodModel_Math$res)
#Normally Distributed
#Homoskedasticity
plot(schoolFirstPeriodModel_Math)

bartlett.test(list(schoolFinalPeriodModel_Math$res, schoolFinalPeriodModel_Math$fit))

schoolFinalPeriodModel_Math_Inter=lm(final.grade ~ Studytime
                                     + as.factor(extra.curricular.activities)
                                     + as.factor(Internet.access.at.home)
                                     + log(number.of.school.absences+1)
                                     + workday.alcohol.consumption
                                     + weekend.alcohol.consumption
                                     + current.health.status
                                     +first.period.grade
                                     +second.period.grade
                                     + quality.of.family.relationships
                                     + as.factor(wants.to.take.higher.education)
                                     + as.factor(School)
                                     + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                                     + Failures,data = schoolDataMath)
summary(schoolFinalPeriodModel_Math_Inter)
AIC(schoolFinalPeriodModel_Math_Inter)
BIC(schoolFinalPeriodModel_Math_Inter)
library("car")
vif(schoolFinalPeriodModel_Math_Inter)

shapiro.test(schoolFinalPeriodModel_Math_Inter$res)
#Normally Distributed
#Homoskedasticity
plot(schoolFirstPeriodModel_Math)

bartlett.test(list(schoolFinalPeriodModel_Math_Inter$res, schoolFinalPeriodModel_Math_Inter$fit))


schoolFinalPeriodModel_Math_InterLag=lm(final.grade ~ Studytime
                                        + as.factor(extra.curricular.activities)
                                        + as.factor(Internet.access.at.home)
                                        + log(number.of.school.absences+1)
                                        + workday.alcohol.consumption
                                        + weekend.alcohol.consumption
                                        + current.health.status
                                        +first.period.grade
                                        +second.period.grade
                                        + quality.of.family.relationships
                                        + as.factor(wants.to.take.higher.education)
                                        + as.factor(School)
                                        + as.factor(School.educational.support_Lag1)
                                        + as.factor(School.educational.support_Lag2)
                                        + as.factor(Family.educational.support_Lag1)
                                        + as.factor(Family.educational.support_Lag2)
                                        + as.factor(extra.paid.classes_Lag1)
                                        + as.factor(extra.paid.classes_Lag2)
                                        + as.factor(School.educational.support)*as.factor(Family.educational.support)*as.factor(extra.paid.classes)
                                        + Failures,data = schoolDataMath)
summary(schoolFinalPeriodModel_Math_InterLag)
AIC(schoolFinalPeriodModel_Math_InterLag)
BIC(schoolFinalPeriodModel_Math_InterLag)
library("car")
vif(schoolFinalPeriodModel_Math_InterLag)

shapiro.test(schoolFinalPeriodModel_Math_InterLag$res)
#Normally Distributed
#Homoskedasticity
plot(schoolFirstPeriodModel_Math)

bartlett.test(list(schoolFinalPeriodModel_Math_InterLag$res, schoolFinalPeriodModel_Math_InterLag$fit))


schoolFinalPeriodModel_Math_InterLag=gls(final.grade ~ Studytime
                                        + as.factor(extra.curricular.activities)
                                        + as.factor(Internet.access.at.home)
                                        + log(number.of.school.absences+1)
                                        + workday.alcohol.consumption
                                        + weekend.alcohol.consumption
                                        + current.health.status
                                        +first.period.grade
                                        +second.period.grade
                                        + quality.of.family.relationships
                                        + as.factor(wants.to.take.higher.education)
                                        + as.factor(School)
                                       + Failures,data = schoolDataMath)
summary(schoolFinalPeriodModel_Math_InterLag)
AIC(schoolFinalPeriodModel_Math_InterLag)
BIC(schoolFinalPeriodModel_Math_InterLag)

