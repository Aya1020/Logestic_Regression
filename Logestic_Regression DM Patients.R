library(tidyverse)
#Importing the data
dt = read_csv("Logestic_Regression/Data/diapetic patients.csv")
view(dt)
#To inspect the dimensions of my data 
dim(dt) # So i have 403 rows, in this case patients, and 24 columns

#tell R which variables are categorical, as it will assume they’re all continuous by default:
chol = dt["chol"] # cholesterol is continuous, so it’s easy
gender = as.factor(dt$gender) # but gender isn’t.
dm = as.factor(dt$dm)

#To see how many males and females & getting the total, using the “table” command.
t = table(gender) # store the tabulation for further manipulation
addmargins(t)  # this will sum up the gender totals to give an overall total and print the results


#Getting percentages of the categories
round(prop.table(t),digits=3) # get proportions rounded to 3dp
round(100*prop.table(t),digits=1) # get %s rounded to 1dp.

is.na(dt$dm) %>% sum() #to know the count of the missing data
dm2 = factor(dm, exclude=NULL) # make new factor from the old one
table(dm2) # display the counts including the missings (NAs)

summary(chol)
hist(dt$chol,
     main = "Histogram of Cholesterol",
     col = "Dark RED",
     breaks = 10)

height = dt$height
weight = dt$weight
summary(height)
summary(weight)

height.si = height*0.0254
weight.si = weight*0.453592
bmi = weight.si/height.si^2

summary(bmi)
hist(bmi,
     main = "Histogram of BMI",
     col = "Dark BLUE",
     breaks = 6)
#If your BMI is less than 18.5, it falls within the underweight range.
#If your BMI is 18.5 to 24.9, it falls within the normal or Healthy Weight range.
#If your BMI is 25.0 to 29.9, it falls within the overweight range.
#If your BMI is 30.0 or higher, it falls within the obese range.

bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# check that the bmi_categorised variable has worked  
table(bmi_categorised, exclude = NULL) 

# frequencies of diabetes by BMI category 
dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL) 

# check 
dm_by_bmi_category 

round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) 

##### Here is the R code to do the cross-tabulations and the resulting output 

# creating "age" variable
age = dt$age
summary(age)
hist(dt$age,
     main = "Age Distribution",
     col = "light blue")

# creating a categorical variable "age_grouped" 

age_categorised = ifelse(age < 45, "< 45", 
                          ifelse(age >= 45 & age <= 64, "45-64", 
                                 ifelse(age > 64 & age <= 74, "65-74", 
                                        ifelse(age > 74, "> 74", NA)))) 
# displaying new variable in a table 

 table(age_categorised)
 
 # cross tabulating with gender 
 
gender_age_categorised = table(age_categorised,gender, exclude = NULL)
gender_age_categorised
# display the cross tabulation as proportion of whole sample, converting to percentage and rounding to 1 decimal place 

round(100*prop.table(gender_age_categorised),digits=1)


