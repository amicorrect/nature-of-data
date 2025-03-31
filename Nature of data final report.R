# Assignment Tasks 2024 Spring v2
## a) Test whether there is an association between shopping frequency and payment method.
#attach data 
retail=read.csv("Retail.csv")
attach(retail)
#check data structor
str(retail)
head(retail)
dim(retail)

#table them
freq_table = table(retail$ShoppingFrequency, retail$PaymentMethod)
freq_table
freq=row.names(freq_table)
method=colnames(freq_table)
#plot
barplot(freq_table, col = 1:4, beside = TRUE, legend = TRUE, 
        main = "Shopping Frequency vs Payment Method",
        xlab = "Payment Method", ylab = "Frequency")
chisq.test
#Hypothesis:
#• H0: there is no  association between shopping frequency and payment method.
#• H1: there is an association between shopping frequency and payment method.

n = sum(freq_table)
freq.counts = rowSums(freq_table) #row totals
method.counts = colSums(freq_table) #column totals
#under null hypothesis we calculate expected values
e = outer(freq.counts, method.counts )/n 
e
h=chisq.test(freq_table)
h$statistic
#calculate chisq
freq.chisq = sum((freq_table - e)^2/e)
freq.chisq

## simulate the data, assuming that the null hypotheses is true
set.seed(3)
s = replicate(10000, {
  ## sample the set of species
  fr = sample(freq, size = n, replace = TRUE, prob = freq.counts/n)
  ## sample the set of locations
  me = sample(method, size = n, replace = TRUE, prob = method.counts/n)
  ## tabulate the results
  x = table(fr, me)
  # compute the expected value from the tabulated sample
  sc = rowSums(x)
  p = colSums(x)
  ex = outer(sc, p)/sum(x)
  # compute the distance of the sample fromt the expected count
  sum((x - ex)^2/ex)
})
hist(s)
abline(v=freq.chisq)
#it is a two sided test compute p value 
mean(abs(s)>abs(freq.chisq))


#conclusion p value is 0.0655 , less than 0.05 we can not reject ho, 
#therefore we can not say there is an association between shopping frequency and payment method.
#it is not suitable if we want to increase shopping ferequency specificly targeting purely on shopping method.



#q2
# Subset the data for Baby and Personal Care purchases
baby_group = retail[retail$BabyPurchase == 1, "Age"]
personal_group = retail[retail$PersonalCarePurchase == 1, "Age"]
length(baby_group)
str(retail)
hist(baby_group)
hist(personal_group)
length(personal_group)

library(car)
qqPlot(baby_group)
qqPlot(personal_group)
qqnorm(personal_group)
qqline(personal_group, col = "red")
#sample size is large 
boxplot(baby_group, personal_group, names = c("BabyPurchase", "PersonalCarePurchase"), 
        ylab = "Age", col = c("lightblue", "lightgreen"))
#from the boxplot we can tell that variance is  different
head(baby_group)
hist(baby_group)
#Hypothesis:
#• H0: ubaby=upersonal
#• H1: ubaby<upersonal

# Perform a one-tailed t-test assuming unequal variance
t_test = t.test(baby_group, personal_group, alternative = "less", var.equal = FALSE)
t_test

#compute pvalue 
t_test$p.value
# Display the test result
#pvalue is small we reject the h0 accept h1 the mean age of customers who purchased baby products is significantly smaller than the mean age of customers who purchased personal care items
#Marketing should target younger consumers for baby products and older shoppers for personal care items.



#q3
str(retail)

## dot chart
dotchart(retail$NumberOfItems, group = as.factor(retail$PaymentMethod), xlab = "NumberOfItems", pch = 16)
## barplot check the distribution 
barplot(table(retail$PaymentMethod),
        main = "Distribution of Payment Methods",
        xlab = "Payment Method",
        ylab = "Count",
        col = "lightblue")

## box plot
boxplot(NumberOfItems ~ PaymentMethod, data = retail)

#check sample size 
ns=table(retail$PaymentMethod)
ns
#step1 :state the research question.
#whether there is a significant difference in the mean number of items across different payment methods.

# step2 state hypotheses testing 
#Test for equality of Means
# H0: all means are equal for all categories
# HA: at least one pair of means are not equal.

# step 3 Perform the test using the Normal approximation.since each sample size is larger than 30,we do not need to do permutaion
test=oneway.test(NumberOfItems ~ PaymentMethod, data = retail, var.equal = TRUE)

# step 4   compute the p value
test$p.value

# step 4 conclusion 
#p-value is less than 0.05, it is very small. Reject the null hypothesis. 
#There is evidence of a difference. At least one pair of means are not equal
#The results show that the mean number of items purchased differs across payment methods

# Post-hoc pairwise comparisons
sd_values <- tapply(retail$NumberOfItems, retail$PaymentMethod, sd)
sd_values
# since each sample size is larger than 30  we can use F-distribution
fitted = aov(NumberOfItems ~ PaymentMethod, data = retail)
fitted

# The Studentized Range Distribution to approximate the distribution of maximum t values from all pairs of categories are
result=TukeyHSD(fitted)

# only print the significant results
significant_results = result$PaymentMethod[result$PaymentMethod[, "p adj"] < 0.05, ]
print(significant_results)

#Debit Card vs. Credit Card,Online Payment vs. Credit Card,Mobile Payment vs. Debit Card show significant difference
#Credit Card and Mobile Payment users tend to purchase more items ,therefore promote Credit Card and Mobile Payment options,
#possibly offering incentives to encourage the use of payment methods associated with higher purchase volumes.

freq_table
#q4
# Scatter Plot
plot(PurchaseAmount ~ Age, data=retail, pch=16)

#step 1 state research question Test if age and  purchase amount have linear relationship 
# step 2 state hypotheses 
# • H0 : b = 0
# • HA : b ̸= 0

# step 3 
## Create a bootstrap distribution of b where the null hypothesis is true (b=0)
x= replicate(1000, {
  age.perm = sample(retail$Age) # shuffle one variable to force population b = 0
  fit = lm(retail$PurchaseAmount ~ age.perm, data=retail) # fit the straight line model
  coef(fit)[2] # return the fitted b
})
## examine the distribution of b, when the population b = 0
hist(x, col="lightblue", main="", xlab="")

## compute the slope of the original data
fit = lm(PurchaseAmount ~Age, data=retail) # fit the data
slope = coef(fit)[2] # extract the slope (b) from the fitted model
slope

# step 4 compute pvalue 
pValue = mean(abs(x) > abs(slope))
pValue


#

# Confidence Interval for the slope (b)
n = nrow(retail)
bootb = replicate(1000, {
  s = sample(1:n, size = n, replace = TRUE) # sample the row ids
  boot = retail[s,] # create a new table with only the selected row ids
  m = lm(PurchaseAmount ~Age, boot) # compute the slop from the bootstrap data
  coef(m)[2] # extract the slope from the fit
})
## examine the bootstrap distribution of b,add the 95% interval lines
hist(bootb, col = "lightblue",  
     main = "Bootstrap Distribution of Slope Coefficient for Purchase Amount vs. Age", 
     xlab = "Slope Coefficient")
## 
abline(v = quantile(bootb,c(0.025, 0.975)), col = "blue", lwd = 2)

## Print out the interval boundaries (95% interval)
ci95 = quantile(bootb, c(0.025, 0.975)) # 95% interval
ci95


#95% confidence interval for the slope of the relationship between age and purchase amount, 
#ranging from approximately 1.30 to 2.18. This suggests that for each additional year of age, the purchase amount increases by about 1.30 to 2.18 units on average.
#the conclusion that older customers tend to spend more.

predict(fit, newdata=data.frame(Age=31),interval = "conf") 
age_31_min = min(subset(retail, Age == 31)$PurchaseAmount)
age_31_max = max(subset(retail, Age == 31)$PurchaseAmount)
age_31_mean=mean(subset(retail, Age == 31)$PurchaseAmount)
#The model predicts that a customer who is 31 years old will have a purchase amount of 95%  The confidence interval of (102.85, 116.69)  
#while the actual average purchase amount for this dataset customers aged 31 in the dataset is 124.7 units. This actual value falls outside the predicted confidence interval, suggesting that the model may not fully account for the purchasing behavior of this age group.
subset(retail, Age == 31)
# Residuals
head(retail)
unique_values <- lapply(retail, unique)
retail
#residuals(fit) # print out the model residuals
plot(residuals(fit) ~Age, data = retail, pch = 16) # plot the residuals
abline(h = 0) # add a zero line for reference
# Residuals are not  spread equally around the horizontal line,  there is an obvious patterns.
str(retail)
## Examine the distribution of the residuals.
hist(residuals(fit))
barplot(table(retail$StoreLocation), 
        main = "Distribution of Store Locations", 
        xlab = "Store Location", 
        ylab = "Frequency", 
        col = "lightblue", 
        las = 2) # las=2 makes the x-axis labels perpendicular to the axis
hist(StoreLocation)
# Residuals look normal.
summary(fit)
# conculusion 
# The histogram of the residuals appears to be normally distributed.This supports the assumption of normality of residuals, which is essential for hypothesis.
# residuals display a pattern (e.g., they fan out or cluster), it suggests that the variability of the response variable is not constant across all levels of the predictor variable. This can lead to inefficient estimates and inflated Type I error rates.