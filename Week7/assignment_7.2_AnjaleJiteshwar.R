# Assignment: ASSIGNMENT 5
# Name: Anjale, Jiteshwar
# Date: 2021-04-28
# Student Survey

## As a data science intern with newly learned knowledge in skills in statistical
## correlation and R programming, you will analyze the results of a survey recently
## given to college students. You learn that the research question being investigated
## is: “Is there a significant relationship between the amount of time spent reading
## and the time spent watching television?” You are also interested if there are other
## significant relationships that can be discovered? The survey data is located
## in this StudentSurvey.csv file.

# import required packages
library(ggplot2)
library(ggm)

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/anjal/OneDrive/Desktop/MS/DSC520/dsc520")

## Load the `data/student-survey.csv` to
student_df <- read.csv("data/student-survey.csv")

head(student_df)

## i.Use R to calculate the covariance of the Survey variables and provide an explanation
## of why you would use this calculation and what the results indicate.

cov(student_df)


# Conclusions from the  covariance 
# 1.Time of reading is negatively related to Time of watching TV,
# 2.Time of reading is negatively related to Happiness.
# 3.Time of watching TV is positively related to Happiness.
# 4.As gender is represented as integer, we can ignore the covariance associated with gender.


# ii. Examine the Survey data variables. What measurement is being used for the variables? 
# Explain what effect changing the measurement being used for the variables would have 
# on the covariance calculation. Would this be a problem? Explain and provide a better 
# alternative if needed.


str(student_df)

summary(student_df)


# TimeReading - By looking at the values, I assumed that the TimeReading is measure in minutes. 
#               It looks like TimeReading varies from 1 minutes to 6 minutes.
# TimeTV - By looking at the values, I assumed that the TimeTV is measure in minutes. 
#         It looks like TimeTV varies from 50 minutes to 95 minutes.
# Happiness - By looking at the values, I assumed that the Happiness is measure in percentages.
#             It looks like Happiness index varies from 45.67% to 89.52%
# Gender - By looking at the values, I assumed that the Gender is measure in boolean. 
#          It is not specified that 0 or 1 mean male/female. Need so more info on the variable.
#
# covariance calculated for the variables have different units. I feel that we need to use 
# correlation coefficient to determine the relationship between these variable.

cor(student_df)

# iii.Choose the type of correlation test to perform, explain why you chose this test, 
# and make a prediction if the test yields a positive or negative correlation?

#checking normality of data


#Probability Plot of the TimeReading variable.
ggplot(student_df, aes(sample=TimeReading)) + stat_qq() + stat_qq_line()

#Probability Plot of the TimeTV variable.
ggplot(student_df, aes(sample=TimeTV)) + stat_qq() + stat_qq_line()

#Probability Plot of the Happiness variable.
ggplot(student_df, aes(sample=Happiness)) + stat_qq() + stat_qq_line()


#By looking at plots, I can confirm that data is normally distributed. 
#We can used Perason’s correlation coefficient to check the correlation between variables.

cor.test(student_df$TimeReading,student_df$TimeTV)

cor.test(student_df$TimeReading,student_df$Happiness)

cor.test(student_df$Happiness,student_df$TimeTV)


# iv. Perform a correlation analysis of:
# 1.All variables
cor(student_df)

# 2.A single correlation between two a pair of the variables
cor(student_df$TimeReading, student_df$TimeTV)


# 3.Repeat your correlation test in step 2 but set the confidence interval at 99%
cor.test(student_df$TimeReading, student_df$TimeTV, conf.level = 0.99)


# Describe what the calculations in the correlation matrix suggest about the relationship between the variables. Be specific with your explanation.
cor(student_df)

#Correlation coefficient between a variable and itself is 1 i.e. completely positively correlated.
#Correlation coefficient is < 0 that would signify negative correlation.
# TimeReading and TimeTV have negative correlation.
# TimeReading and Happiness have negative correlation.
# TimeTV and Happiness have positive correlation.

# V. Calculate the correlation coefficient and the coefficient of determination, 
# describe what you conclude about the results.

# calculating correlation coefficient (r) between variables
cor(student_df)

# calculating coefficient of determination (rˆ2) between two variables
cor(student_df)^2

# The coefficient of determination is a measurement used to explain how 
# much variability of one factor can be caused by its relationship to another
# related factor. This correlation, known as the "goodness of fit," is 
# represented as a value between 0.0 and 1.0.

# Looking at coeffiecient of determination between TimeTV and Happiness shared variability is
# about 40% which would imply that TV time variability effects Happiness upto 40% only, while
# remaining 60% variability in Happiness must be caused by some other variable.



# vi. Based on your analysis can you say that watching more TV caused students to read less? Explain.

cor(student_df$TimeReading, student_df$TimeTV)^2

# Looking at coefficient of determination (rˆ2) we can say that variability in TimeReading can
# cause upto 77% variability in TimeTV  
# There could be other variables that may cause 23% variability in TimeTV.

# vii. Pick three variables and perform a partial correlation, documenting which variable you are 
# “controlling”. Explain how this changes your interpretation and explanation of the results.

student_df2 <- student_df[,1:3]

# Run partial correlation between TimeTV and Happiness while controlling TimeReading

pcor(c("TimeTV","Happiness","TimeReading"), var(student_df2))

pcor(c("TimeTV","Happiness","TimeReading"), var(student_df2))^2


#If we keep TimeReading controlling , the correlation coefficient between TV time and happiness decrease to 0.59
# and coefficient of determination has decreased to 35%. This decrease
# suggests that variation in Happiness was also effected positively by TimeReading by about 5%.
