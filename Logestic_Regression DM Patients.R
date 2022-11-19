library(tidyverse)
#Importing the data
#he “header=TRUE” option tells R that the first row in the file contains the column names.
dt = diapetic_patients <- read_csv("Logestic_Regression/Data/diapetic patients.csv")
view(dt)
#To inspect the dimentions of my data 
dim(dt) # So i have 403 rows, in this case patients, and 24 columns

#tell R which variables are categorical, as it will assume they’re all continuous by default:
chol = dt["chol"] # cholesterol is continuous, so it’s easy
gender = as.factor(dt[,"gender"]) # but gender isn’t.
dm = as.factor(dt[,"dm"]) # neither is dm

#To see how many males and females & getting the total, useing the “table” command.
t <- table(gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results


#Getting percentages of the categories
round(prop.table(t),digits=3) # get proportions rounded to 3dp
round(100*prop.table(t),digits=1) # get %s rounded to 1dp.