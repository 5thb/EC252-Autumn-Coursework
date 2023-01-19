install.packages("readxl")
install.packages("ggplot2",dependencies = TRUE)
install.packages("gmodels",dependencies = TRUE)
install.packages("Hmisc",dependencies = TRUE)
install.packages("stargazer",dependencies = TRUE)
install.packages("effects",dependencies = TRUE)
install.packages("gridExtra",dependencies = TRUE)
install.packages("lmtest")
install.packages("strucchange")
install.packages("BreakPoints")
install.packages("bfast")
install.packages("optimx")
install.packages("lm_robust")

library(BreakPoints)
library(ggplot2)
library(gmodels)
library(Hmisc)
library(stargazer)
library(effects)
library(gridExtra)
library(readxl)
library(readr)
library(lmtest)
library(strucchange)
library(bfast)
library(optimx)

dataset <- read.table(file.choose(), header = T, sep = ",")

str(dataset)
attach(dataset)
##1c Amount of Observations in the data set ##
datadim = dim(dataset)
datadim.row <- nrow(dataset)
datadim.cols <- ncol(dataset)
Numbobservation <- datadim.row * datadim.cols
## Amount of Observations in the data set is 3500

##1a average, minimum, maximum, standard deviation of birthweight
##unmarried and education variables
summary(dataset $bwght)
sd(dataset $ bwght)
bweight <- as.data.frame(apply(dataset ["bwght"],2,summary))
sdbweight <- sd(as.numeric(unlist(dataset["bwght"])))
bwght.mean <- mean(bwght)
bwght.max <- max(bwght)
bwght.min <- min(bwght)
sdbweight
##
##The average bwght is 3372.974
##The maximum value of bwght is 5755
##The minimum value of bwght is 710
##The sdbwght is 568.3905

summary(dataset $educ)
sd(dataset $ educ)
educ <- as.data.frame(apply(dataset ["educ"],2,summary))
sdeduc <- sd(as.numeric(unlist(dataset["educ"])))
educ.mean <- mean(educ)
educ.max <- max(educ)
educ.min <- min(educ)
sdeduc
##
##The maximum value for educ is 17
##The average value for edu is 12.86
##The minimum value for edu is 6
##The sd for edu is 2.134154
summary(dataset $unmarried)
sd(dataset $ unmarried)
unmarried <- as.data.frame(apply(dataset ["unmarried"],2,summary))
sdunmarried <- sd(as.numeric(unlist(dataset["unmarried"])))
unmarried.mean <- mean(unmarried)
unmarried.max <- max(unmarried)
unmarried.min <- min(unmarried)
sdunmarried
##
##The mean unmarried is 0.24
##The max unmarried value is 1
##The sd deviation value is 0.4275109
##The minimum unmarried value is 0

##1b Histogram of child birthweight
hist(bwght)
##There is a high density in the frequency in the histogram between 3000
##and the 4000 bwght values - showing that there is a large percentage of the
##population that fit within this region, like meaning the median will fall in
##between 3500. Furthermore - the histogram is relatively symmetrical on both
##sides meaning that you could possibly assume the birthweight is normally 
##distributed assuming that the dataset is considered large enough.

##2a

marriedmothers = subset(dataset, unmarried == 0)
unmarriedmothers = subset(dataset, unmarried == 1)
nprevistun <- as.data.frame(apply(unmarriedmothers,2, mean))
nprevistmar <- as.data.frame(apply(marriedmothers,2, mean))
##There are 120 unmarried mothers which would constitute of 24% of amount of the 
##percentage of women that were unmarried at the time of their pregnancy.
##The average amount of prenatal visits by unmarried mothers at the time of 
##their pregnancy is 9.2583333


age_under_20 = 0
age_20 = 0
age_30 = 0
age_40 = 0

agegroup = rep(NA, length(dataset$age))

#2B
for(i in 1:length(dataset$age))
{
  if(dataset$age[i] < 20)
  {
    age_under_20 = age_under_20 + 1
    agegroup[i] = 1
  }
  
  else if(dataset$age[i] >= 20 & dataset$age[i] <= 29)
  {
    age_20 = age_20 + 1
    agegroup[i] = 2
  }
  
  else if(dataset$age[i] >= 30 & dataset$age[i] <= 39)
  {
    age_30 = age_30 + 1
    agegroup[i] = 3
  }
  
  else if(dataset$age[i] >= 40)
  {
    age_40 = age_40 + 1
    agegroup[i] = 4
  }
  
}

agegroup
num_30_older = age_30 + age_40

#There are 164 mothers aged 30 and older


#3A
dataset$agegroup = agegroup
counts = table(dataset$unmarried, dataset$agegroup)

barplot(counts, main="Relationship between maritial status and age group",
        xlab="Age group", col=c("black","red"),
        legend = c("Married", "Unmarried"))
## Age group one has 72.5490196% of women unmarried and 27.45090804% married
## Age group two has 25% of women unmarried and 75% married
## Age group three has 7.45341615% of women unmarried and 92.5465838% married
## Age group four has 0% of women unmarried and 100% married
#3B
plot(dataset$age, dataset$unmarried)
corr_age_marital_status = cor(dataset$age, dataset$unmarried)
#The PMCC between age and marital status is -0.4603992 meaning that there's
#a somewhat inversely strong correlation between the two variables.

########################################################

##4a
x <- dataset$bwght
y <- dataset$educ
cor(x,y)
regression <- lm(y~x)
model = lm(dataset$bwght ~ dataset$age + dataset$educ, data=dataset)
plot(model)
summary(model)
summary(model)$coefficient
##4b
##The estimated coefficient from the summary is 7.324 meaning that for an increase
##in the year there is a predicted 7.324 unit (gram) increase in the birth weight
##If there was a 4 year increase there should be a 29.296 gram increase in the
##birth weight
##4c
##The estimated coefficient from the summary is 7.218 meaning that for an increase
##in the year there is a predicted 7.218 unit (gram) increase in the birth weight
##If there was a 5 year increase there should be a 36.09 gram increase in the
##birth weight    

##5a
lbwght <- log(dataset$ bwght)
x <- dataset$educ
y <- lbwght
cor(x,y)
regressiontwo<- lm(y~x)
##5b
log_model = lm(lbwght ~ dataset$age + dataset$educ, data=dataset)
summary(log_model)
##5c
####The estimated coefficient from the summary is 0.001805 meaning that for an increase
##in the year there is a predicted 0.001805 unit (gram) increase in the birth weight
##If there was a 4 year increase there should be a 0.00722 gram increase in the
##birth weight since the model takes the natural log of the birth weight 
##the observed changes behave differently.

##5d
stargazer(log_model, type= "text", model.numbers = FALSE, title= "Hypothesis Test")
##The observed age of the updated model that takes the natural log is
##significant at the 5% level


##6a
agesq <- (dataset$age^2)
dataset$agesq
agesq_model = lm(dataset$bwght ~ dataset$age + dataset$educ + agesq, data=dataset)
summary(agesq_model)

lr6<- lm_robust(lb)

#6B
stargazer(agesq_model, model, type = "text", title = "Regression Results",
          dep.var.labels = "Child's Birth Weight", column.labels = c("Age sq Model", "Age Model"))

stargazer(agesq_model, type = "text", title = "Regression Results",
          dep.var.labels = "Child's Birth Weight", column.labels = c("Age sq Model", "Age Model"))

coef_agesq = coef(agesq_model)

lr6 <- lm_robust(lb)
summary(agesq_model)
####
agesq <- (dataset$age^2)
dataset$agesq
modelx <- lm_robust(dataset$bwght ~ dataset$age + dataset$educ + agesq, data=dataset)

#6C To figure out the agesquared the birth weight is maximised at.
## You get the maximum birth weight from the model then you solve the 
## equation with age squared as the unknown then you solve.

##7a

bw_model_nprevist = lm(dataset$bwght ~ dataset$age + dataset$educ + dataset$nprevist, data=dataset)
summary(bw_model_nprevist)



#7B
stargazer(bw_model_nprevist, type = "text", title = "Regression Results",
          dep.var.labels = "Child's Birth Weight", column.labels = c("Age sq Model", "Age Model"))

#7C

##When added the number of previous visits to the model the Observed R^2 values
##becomes: Multiple R-squared:  0.03627; Adjusted R-squared:  0.03044 
##In comparision the model in 4a with Observed R^2 values of 
##Multiple R -Squared 0.007535; Adjusted R-squared:  0.003541
##An increase of 0.028735 and 0.026899 respectively. Higher R^2 values
##Is indicative of your model being a better fit for your data.
##Consequently the addition of the number of previous visits data being added
##increases the fit to the model by around 3.81 multiplier  and 7.59 multiplier


#7D
residuals = residuals(bw_model_nprevist)

plot(residuals(bw_model_nprevist), fitted(bw_model_nprevist), abline(h = 0))


# Calculate the sample mean of the residuals
mean_residuals <- mean(residuals)

# Calculate the sample standard deviation of the residuals
sd_residuals <- sd(residuals)

plot(bw_model_nprevist, which = 1)




##8a
bwight_model <- lm(bwght ~ age + educ + nprevist + smoker, data = dataset)
summary(bwight_model)
lm(formula = bwght ~ age + educ + nprevist + smoker, data = dataset)

##8b
dataset_subset <- subset(dataset, age >= 30)
newmodel_underequalthirthy<- lm(bwght ~ age + educ + nprevist + smoker, data = dataset_subset)
summary(bwight_modeltwo)

##8c
dataset_filter <- dataset[dataset$age < 30,]
newmodel_overthirthy <- lm(bwght ~ age + educ + nprevist + smoker, data = dataset_filter)
summary(bwight_mo)

##8d
RSS = sum(bwight_model$residuals^2)
RSS2 = sum(newmodel_underequalthirthy$residuals^2)
RSS3 = sum(newmodel_overthirthy$residuals^2)

N= nobs(bwight_model)
k = 4

Fnum = (RSS - (RSS2 + RSS3)/k)
Fden = (RSS2 + RSS3)/(N- 2*(k-1))

Fhat = round(Fnum/Fden, digits=3)

df_num = k
df_den = N - 2* (k+1)
critical_value <- qf(.90, df1 = df_num, df2 = df_den)
Fhat > critical_value



#Based on the F-value and p-value obtained from the Chow test, we fail to reject the null hypothesis that the relationship between bwght on age, educ, nprevist, and smoker is the same for mothers aged 30 years and older as for mothers aged less than 30 years.
#The p-value of 0.2124 is greater than the commonly used significance level of 0.05, which means that there is not enough evidence to suggest that the relationship between bwght and the other independent variables is different for the two age groups. Therefore, we can conclude that the relationship between bwght and the other independent variables is similar for mothers aged 30 years and older and mothers aged less than 30 years.

## 9
x <- dataset$bwght
y <- dataset$educ
regression <- lm(y~x)
model = lm(dataset$bwght ~ dataset$age + dataset$educ, data=dataset)
agesq = dataset$age * dataset$age
agesq_model = lm(dataset$bwght ~ dataset$age + dataset$educ + agesq,
                 data=dataset)
bw_model_nprevist = lm(dataset$bwght ~ dataset$age + dataset$educ +
                         dataset$nprevist, data = dataset)
bwight_model <- lm(bwght ~ age + educ + nprevist + smoker, data = dataset)
dataset_subset <- subset(dataset, age >= 30)
bwight_model1 <- lm(bwght ~ age + educ + nprevist + smoker, data = dataset_subset)
dataset_filter <- dataset[dataset$age < 30,]
bwight_model2 <- lm(bwght ~ age + educ + nprevist + smoker, data = dataset_filter)
# Create a list of the models
m
models <- list(regression, model, agesq_model, bw_model_nprevist, bwight_model,
               bwight_model1, bwight_model2)
# Create the table
stargazer(models, type = "text",
          title = "Regression Results",
          column.labels = c("4a", "6a", "7a", "8a", "8b", "8c"),
          digits = 3)


