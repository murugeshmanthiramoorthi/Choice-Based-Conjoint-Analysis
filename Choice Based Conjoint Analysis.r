#Let us load the data 
data=read.csv("choicebasedconjoint.csv")

# Now we will look at the variables present in tha dataset
names(data)

#Serial number is not necessary. So we will remove it from the dataset
data=data[,-1]


# Now we will look at the structure of the data
str(data)

# We will include suurvival library for performing conditional logistic regression
library(survival)

# We can find that the Monthly Income is represented as int
# We will convert it into factor
data$MonthlyIncome=factor(data$MonthlyIncome)

# Now we will perform conditional logistic regression on our data
clogout1=clogit(choice~(Distance+Reputation+Delivery+Payment.Method+Price)
                +strata(V1),data=data)

clogout1

# Now we will calculate AIC value for the model
AIC(clogout1)

# Now we will add gender and Health Insurance as an interaction method to the model
clogout2=clogit(choice~(Distance+Reputation+Delivery+Payment.Method+Price)*
                  (Gender+HealthInsurance)
                +strata(V1),data=data)

clogout2

# Now let us calculate the AIC value for the new model
AIC(clogout2)
