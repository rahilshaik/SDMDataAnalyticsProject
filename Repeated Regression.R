##Reading .csv file into a variable
setwd("E:/USF/ISM6137-SDM/Project/student/Final");
schoolData=read.csv("SchoolDataFinal - RepeatedRegression.csv")
schoolData$gr
# Mixed regression with Grades data
library(lme4)
model_grade <- lmer(Grade ~ Studytime
                    + as.factor(quality.of.family.relationships) 
                    + as.factor(extra.curricular.activities )
                    + as.factor(Internet.access.at.home) 
                    + as.factor(weekend.alcohol.consumption)
                    + as.factor(workday.alcohol.consumption )
                    + number.of.school.absences 
                    + Failures
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
                    + (1|StudentId)
                    + (1|School), data=schoolData )
summary(model_grade)
ranef(model_grade)
fixef(model_grade)
