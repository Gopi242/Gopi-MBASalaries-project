# Analysis of MBA SALARIES
# NAME: Sai Gopi Krishna Govindarajula
# EMAIL: saigopikrishna.g@gmail.com
# COLLEGE / COMPANY: BITS PILANI/Capgemini 

#To read the data
MBA.df <- read.csv(paste("E:/Studies/Sai Gopi Krishna Govindarajula/Udemy/Data sets/MBA Starting Salaries Data.csv", sep=""))
View(MBA.df)

#Summary
summary(MBA.df)

#Boxplot
#Boxplot to see Average age of candidates
boxplot(MBA.df$age,horizontal=TRUE,xlab="Age of candidates",main ="Average age of candidates")
# Boxplot to see total GMAT score of candidates
boxplot(MBA.df$gmat_tot,horizontal=TRUE,xlab="total GMAT score of candidates",main ="total GMAT score of candidates")
#Boxplot to see quantitative GMAT percentile of candidates
boxplot(MBA.df$gmat_qpc,horizontal=TRUE,xlab="quantitative GMAT percentile of candidates",main ="quantitative GMAT percentile of candidates")
#Boxplot to see verbal GMAT percentile of candidates
boxplot(MBA.df$gmat_vpc,horizontal=TRUE,xlab="verbal GMAT percentile of candidates",main ="verbal GMAT percentile of candidates")
# Boxplot to see overall GMAT percentile
boxplot(MBA.df$gmat_tpc,horizontal=TRUE,xlab="overall GMAT percentile of candidates",main ="overall GMAT percentile of candidates")
# Boxplot to see spring MBA average pf candidates
boxplot(MBA.df$s_avg,horizontal=TRUE,xlab="spring MBA average of candidates",main ="spring MBA average of candidates")
#Boxplot to see fall MBA average of candidates
boxplot(MBA.df$f_avg,horizontal=TRUE,xlab="fall MBA average of candidates",main ="fall MBA average of candidates")
#Boxplot to see years of work experience
boxplot(MBA.df$work_yrs,horizontal=TRUE,xlab="years of work experience of candidates",main ="Average year of work experience of candidates")
# Boxplot to see first language (1=English; 2=other) 
boxplot(MBA.df$frstlang,horizontal=TRUE,xlab="first language (1=English; 2=other) of candidates",main ="first language of candidates")
#Boxplot to see Average salary
boxplot(MBA.df$salary,horizontal=TRUE,xlab="Salary of candidates",main ="Average salary of candidates")
# Boxplot to see degree of satisfaction with MBA program (1= low, 7 = high satisfaction)
boxplot(MBA.df$satis,horizontal=TRUE,xlab="degree of satisfaction with MBA program (1= low, 7 = high satisfaction) of candidates",main ="Average degree of satisfaction of candidates")

#Scatterplot matrix
library(car)
#scatterplot matrix for salary and sex
scatterplotMatrix(formula=~salary+sex,cex=0.6,data=MBA.df,diagonal="histogram")
#Scatterplot matrix for salary and age
scatterplotMatrix(formula=~salary+age,cex=0.6,data=MBA.df,diagonal="histogram")
#Scatterplot matrix for salary and gmat total
scatterplotMatrix(formula=~salary+gmat_tot,cex=0.6,data=MBA.df,diagonal="histogram")
#Scatterplot matrix for salary and gmat overall
scatterplotMatrix(formula=~salary+gmat_tpc,cex=0.6,data=MBA.df,diagonal="histogram")
#Scatterplot matrix for salary and spring avg
scatterplotMatrix(formula=~salary+s_avg,cex=0.6,data=MBA.df,diagonal="histogram")
#Scatterplot matrix for salary and fall avg
scatterplotMatrix(formula=~salary+f_avg,cex=0.6,data=MBA.df,diagonal="histogram")
#Scatterplot matrix for salary and work experience
scatterplotMatrix(formula=~salary+work_yrs,cex=0.6,data=MBA.df,diagonal="histogram")
#Scatterplot matrix for salary and first language
scatterplotMatrix(formula=~salary+frstlang,cex=0.6,data=MBA.df,diagonal="histogram")

#corrgram
library(corrgram)
corrgram(MBA.df, lower.panel=panel.shade, upper.panel=panel.pie, diag.panel=panel.minmax, text.panel=panel.txt)

#Subset of candidates who got a job fith factors that affect it
job <- subset(MBA.df, salary>1000, select = sex:salary)
#Subset of candidates who are not employed
unemployed.sub <- subset(MBA.df, salary<1, select = sex:salary)

#Regression model
#Regression model through origin to estimate the salary (Y) from predictors sex(X1), overall GMAT(X2),spring avg(X3), Fall avg(X4), work exp(X5),First lan(X6)
#Y=B1*X1+B2*X2+B3*X3+B4*X4+B5*X5+B6*X6+B7*X7
#Not taking intercept and running the regression through origin as without experience and degree, no one can get a job of high salary
reg1<- lm(salary~sex+gmat_tpc+s_avg+f_avg+work_yrs+frstlang+quarter-1, data = job)
summary(reg1)
#removing the insignificant variables
reg2<- lm(salary~s_avg+work_yrs+frstlang+sex-1, data = job)
summary(reg2)
#Model 2 gives beeter r square so considering it
# Positive t value for s_avg, first language English and work experience(Higher the positive value, lesser the likelihood of value being 0 by chance)
# The model's, p-value:2.2e-16 is also lower than the statistical significance level of 0.05, this indicates that we can safely reject the null hypothesis that the value for the coefficient is zero (or in other words, the predictor variable has no explanatory relationship with the response variable)
# The model has a F Statistic of 920.3 on 4 and 99 DF which is moderately high
#t.test
t.test(MBA.df$salary~MBA.df$sex)
t.test(MBA.df$salary~MBA.df$frstlang)
#Chi-square test
mytable <- xtabs(~salary+gmat_tpc, data = job)
chisq.test(mytable)
mytable <- xtabs(~salary+s_avg, data = job)
chisq.test(mytable)
mytable <- xtabs(~salary+f_avg, data = job)
chisq.test(mytable)
mytable <- xtabs(~salary+quarter, data = job)
chisq.test(mytable)
mytable <- xtabs(~salary+work_yrs, data = job)
chisq.test(mytable)

#Logistic Regression
library(Amelia)
missmap(MBA.df, main = "Missing values vs observed")
#No missing value so will consider whole data

