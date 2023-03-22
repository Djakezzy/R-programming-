
# Group 10

# Import dataset
kiva.df <- read.csv('Kivadata.csv')

### Data preparation
# Check for missing values
sapply(kiva.df, function(x) sum(is.na(x)))

# Drop missing values
kiva.df <- na.omit(kiva.df) #Drop 4 observations

# Check the distribution of two variables: LOAN_AMOUNT and NUM_LENDERS_TOTAL
hist(kiva.df$LOAN_AMOUNT)
hist(kiva.df$NUM_LENDERS_TOTAL)
library(moments)
skewness(kiva.df$LOAN_AMOUNT)
skewness(kiva.df$NUM_LENDERS_TOTAL)

# Transform
kiva.df$lgLoan <- log(kiva.df$LOAN_AMOUNT + 1)
kiva.df$lgLenders <- log(kiva.df$NUM_LENDERS_TOTAL + 1)


### Data Exploration
# How many loans in each sector
table(kiva.df$SECTOR_NAME)

library(ggplot2)

ggplot(data = kiva.df)+
  geom_bar(
    mapping = aes(y = SECTOR_NAME)
  )

# How many loans in each country
table(kiva.df$COUNTRY_NAME)

# central tendency values for LOAN_AMOUNT
mean(kiva.df$LOAN_AMOUNT)
summary(kiva.df$LOAN_AMOUNT)

# What is the relationship between LOAN_AMOUNT and LENDER_TERM?
cor(kiva.df$LOAN_AMOUNT, kiva.df$LENDER_TERM)

ggplot(data = kiva.df) +
  geom_point(aes(x = LENDER_TERM, y = LOAN_AMOUNT))
# There is no significant correlation between Lender Term and Loan Amount.
names(kiva.df)
### Predictive Models and Analysis
# Drop variables
kiva1.df <- kiva.df[ , -c(1,2,4,5,7,12,16)]
names(kiva1.df)
# Dependent variable is Status.
# Run a model to predict whether loans get funded.
kiva1.train <- glm(STATUS ~ ., data = kiva1.df,
                  family = 'binomial')
options(scipen = 999)
summary(kiva1.train)
# The more the borrower asks, the less likely to be funded.
# The longer the term, the less likely working with field partners
# increases the chance.

#	Generate the confusion matrix.
# What is the accuracy rate? What is the value of sensitivity?
# What is the value of specificity?
   
set.seed(0)
trainindex <-sample(c(1:160482), 160482 * 0.6)
train.df <- kiva1.df[trainindex, ]
valid.df <- kiva1.df[-trainindex, ]

kiva.logit <- glm(STATUS ~ .,
                 data = train.df, 
                 family = 'binomial')

kiva.prob <- predict(kiva.logit, valid.df, type = 'response')
kiva.class <- ifelse(kiva.prob > 0.5, '1', '0')

options(scipen = 999)
kiva.detect <- data.frame('Probability' = kiva.prob,
                           'logit' = kiva.logit,
                           'kivaClass' = kiva.class,
                           'Actual' = valid.df$STATUS)

ctab <- table(Actual = valid.df$STATUS, Predicted = kiva.class)
summary(ctab)
install.packages('caret')
library(caret)
confusionMatrix(ctab, positive = '1')
# Sensitivity = 0.9222
# Specificity = 0.9104

# Draw the ROC curve and calculate the auc.
install.packages('pROC')
library(pROC)
rb <- roc(valid.df$STATUS, kiva.prob) # Sensitivity curve graph
par(pty = 's') #it improves the curve (not necessary)
plot.roc(rb, legacy.axes = TRUE,
         xlab = '1-Specificity',
         ylab = 'Sensitivity')
# Calculate the auc
auc(rb) # 0.9715

# New Model #
names(kiva.df)
kiva2.df <- kiva.df[ , -c(1,2,4,7,16,18,19)]
set.seed(0)
trainindex1 <-sample(c(1:160482), 160482 * 0.6)
train1.df <- kiva2.df[trainindex1, ]
valid1.df <- kiva2.df[-trainindex1, ]
install.packages('usethis')
library(usethis)
usethis::edit_r_environ()
memory.limit()
kiva1.logit <- glm(STATUS ~ .,
                  data = train1.df, 
                  family = 'binomial')

kiva1.prob <- predict(kiva1.logit, valid1.df, type = 'response')
kiva1.class <- ifelse(kiva1.prob > 0.5, '1', '0')

options(scipen = 999)

ctab1 <- table(Actual = valid1.df$STATUS, Predicted = kiva1.class)
summary(ctab1)
install.packages('caret')
library(caret)
confusionMatrix(ctab1, positive = '1')
# Sensitivity = 0.9161
# Specificity = 0.9114

# Draw the ROC curve and calculate the auc.
install.packages('pROC')
library(pROC)
rb1 <- roc(valid1.df$STATUS, kiva1.prob) # Sensitivity curve graph
par(pty = 's') #it improves the curve (not necessary)
plot.roc(rb1, legacy.axes = TRUE,
         xlab = '1-Specificity',
         ylab = 'Sensitivity')
# Calculate the auc
auc(rb1) # 0.9687

