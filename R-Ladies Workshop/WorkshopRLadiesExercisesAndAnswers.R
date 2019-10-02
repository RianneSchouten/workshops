### Workshop RLadies ###
### Handling missing data in R ###
### Author: Rianne Schouten ###
### All files available at github.com/rianneschouten/workshops ###

# Install and load the following packages

install.packages("mice")
install.packages("ggplot2")

library(mice)
library(ggplot2)

# 1. Missing Values Analysis

## We will use a sample dataset called 'Red Wine Quality'
## Load the complete dataset and select the three variables we will work with

com_data <- read.csv("red_wine_quality.txt", sep = '\t')
com_data <- com_data[, c('quality', 'alcohol', 'density')]

## Inspect the complete dataset
## How do you know there are no missing values in this dataset?

nrow(com_data)
summary(com_data)
cor(com_data)
md.pattern(com_data)

## Suppose you want to predict the quality of wines from the amount of alcohol
## they contain. Make a plot to visualize this relationship.

ggplot(com_data, aes(alcohol, quality)) +
  geom_point()

## In real life, you never have the complete dataset. We will therefore 
## create missing values in com_data and pretend we do not know about com_data. 
## In 'mice' we created function 'ampute()' to do the amputation for us. 
## We will not discuss 'ampute()' today but information is 
## available at rianneschouten.github.io

inc_data <- ampute(com_data, patterns = c(1, 0, 1))$amp

## Inspect the dataset again. How do you know that the dataset is now 
## incomplete? Can you find some information about which variable(s) is/are 
## incomplete and by how many? 

md.pattern(inc_data)

## Try to find out whether the dataset is MCAR, MAR or MNAR missing?
## Do this by: creating a vector that indicates whether a row has a missing 
## value.

R1 <- is.na(inc_data$alcohol)
red_data <- inc_data[R1 == 1, ]
blue_data <- inc_data[R1 == 0, ]

## Then, create a histogram to compare the distribution of 'quality' for 
## rows with missing values and rows without missing values

ggplot(data = inc_data, aes(quality)) + 
  geom_density(data = red_data, fill = "red", alpha = 0.2) + 
  geom_density(data = blue_data, fill = "blue", alpha = 0.2)

## Is the missingness likely to be MCAR or MAR/MNAR?

### Answer: the data is likely to be MAR/MNAR because the distributions do not 
### overlap. This means that on average, the values from the observed data are 
### different from the values from the unobserved data.

## If you have extra time, try to use ampute() to generate MCAR missingness in 
## com_data. Follow the same steps as above. How do you expect the histogram
## plot to look?

mcar_data <- ampute(com_data, 
                    patterns = c(1, 0, 1),
                    mech = 'MCAR')$amp

R2 <- is.na(mcar_data$alcohol)
red_data_mcar <- mcar_data[R2 == 1, ]
blue_data_mcar <- mcar_data[R2 == 0, ]
ggplot(data = mcar_data, aes(quality)) + 
  geom_density(data = red_data_mcar, fill = "red", alpha = 0.2) + 
  geom_density(data = blue_data_mcar, fill = "blue", alpha = 0.2)

# 2. Implementing missing data methods

## Earlier, you visualized the relationship between 'quality' and 'alcohol' in 
## the complete dataset. Now, use com_data and inc_data to visualize the 
## missing values. On average, where are the missing values? 
## How will this influence the relationship between the two variables? 

com_data$R1 <- R1
ggplot(data = com_data, aes(alcohol, quality, color = R1)) +
  geom_point() + 
  scale_color_manual(values = c('blue', 'red'), 
                     labels = c('observed', 'missing'))

### The missing values are more to the right hand side of the 'alcohol' values.
### Because of the positive correlation between 'alcohol' and 'quality', on 
### average, larger values of 'quality' are missing.

## Apply the technique of mean imputation
## To do this, first calculate the mean of the incomplete variable

mean_alcohol <- mean(inc_data$alcohol, na.rm = TRUE)

## Second, impute the missing values with this mean

mean_data <- inc_data
mean_data[R1 == 1, 'alcohol'] <- mean_alcohol

## Now, use the imputed dataset to visualize the relationship between 'quality'
## and 'alcohol'. What do you see? How will this missing data method influence 
## the relationship between the two variables? 

ggplot(data = mean_data, aes(alcohol, quality, color = R1)) +
  geom_point() + 
  scale_color_manual(values = c('blue', 'green'), 
                     labels = c('observed', 'mean imputed'))

## Intuitively, it would make sense to use the observed values of 'quality' and
## 'density' to find better imputations for 'alcohol'. 
## We will now set up our own regression imputation model to do this. 
## First, make a regression model using the observed data 

imp_model <- lm(alcohol ~ density * quality, inc_data, na.action = na.exclude)

## Second, use the model to find imputations for the missing 'alcohol' values
## Impute them in the incomplete dataset

imputations <- predict(imp_model, ic(inc_data))
reg_data <- inc_data
reg_data[R1 == 1, 'alcohol'] <- imputations

## Inspect the imputed dataset with a plot like above. What do you think of this
## imputation method? 

ggplot(data = reg_data, aes(alcohol, quality, color = R1)) +
  geom_point() + 
  scale_color_manual(values = c('blue', 'orange'), 
                     labels = c('observed', 'regression imputed'))

## The histogram method will also work to compare the observed with the imputed
## values. Compare the distribution of the observed values of alcohol with the 
## mean imputed values. 

ggplot(data = mean_data, aes(alcohol)) + 
  geom_density(data = mean_data[R1 == 1, ], fill = "green", alpha = 0.2) + 
  geom_density(data = mean_data[R1 == 0, ], fill = "blue", alpha = 0.2)

## Do the same for the regression imputed values. What are the differences? 

ggplot(data = reg_data, aes(alcohol)) + 
  geom_density(data = reg_data[R1 == 1, ], fill = "orange", alpha = 0.2) + 
  geom_density(data = reg_data[R1 == 0, ], fill = "blue", alpha = 0.2)

### With the regression imputation method, the distribution of the imputed 
### values looks much more like the disribution of the observed data. The
### distribution of the imputed values seems a bit more to the right, and a bit
### smaller.

## Package 'mice' is developed to perform multiple imputation. But it can also
## be used to perform single imputation. For mean imputation, the sintax is:

mids <- mice(inc_data, method = "mean", m = 1, maxit = 1)
summary(mids) # gives you the specifications
mice_mean_data <- complete(mids) # extracts the (first) imputed dataset

## Use 'mice' to perform the imputation method "sample"
## Then, make a histogram plot to compare the imputed values with the observed
## values. What do you see?

mids <- mice(inc_data, method = "sample", m = 1, maxit = 1)
mice_sample_data <- complete(mids)

ggplot(data = mice_sample_data, aes(alcohol)) + 
  geom_density(data = mice_sample_data[R1 == 1, ], fill = "orange", alpha = 0.2) + 
  geom_density(data = mice_sample_data[R1 == 0, ], fill = "blue", alpha = 0.2)

### 'Sample' samples from the observed 'alcohol' to find imputations. Therefore,
### the imputed values have the same distribution as the observed values. 
### Although this sounds like a perfect method, remember that in this simulation
### we know that on average, larger values of 'alcohol' are missing from the
### incomplete dataset. With 'sample', we are still missing those values.

# 3. Evaluating Missing Data Methods

## It depends on the specifics of the missingness and on the analysis you do 
## which missing data method is sufficient. So far, we have compared the imputed 
## values with the observed values. Hopefully, this has helped to understand
## what imputation is. In the end, the goal of imputation is not to recreate the
## data. The goal of imputation, in a research setting, is to find unbiased and
## efficient estimates. 

## Make a regression model predicting 'quality' from 'alcohol'. Do this for the 
## complete dataset (the truth) and the different imputed datasets you have 
## created so far. Check out the parameter estimate of the B coefficient, but 
## also check the uncertainty (standard error) of that estimate. Which methods
## performs best, so far? 

com_model <- summary(lm(quality ~ alcohol, com_data))$coefficients
drop_model <- summary(lm(quality ~ alcohol, inc_data, 
                         na.action = na.exclude))$coefficients
mean_model <- summary(lm(quality ~ alcohol, mean_data))$coefficients
reg_model <- summary(lm(quality ~ alcohol, reg_data))$coefficients
sample_model <- summary(lm(quality ~ alcohol, mice_sample_data))$coefficients

com_model
drop_model
mean_model
reg_model
sample_model

## Use 'mice' with method = "norm" and m = 5 to perform multiple imputation
## Perform the same linear model and pool the estimates. You can do this quite
## easily by using mice(), with() and pool() in three separate lines. 
## Compare the pooled regression estimates with the truth. What do you find?

mids <- mice(inc_data, method = "norm", m = 5, maxit = 5)
fit <- with(mids, lm(quality ~ alcohol))
mi_model <- summary(pool(fit))

com_model
mi_model

### For many analyses, pooling rules are available. Also, for most analyses,
### it is known which missing data methods work well. If you have a missing data
### situation in your scientific research (i.e. you need to find unbiased and 
### efficient estimates), search the literature for the best method.

### In prediction models, the aim of the analysis is generally to find the best
### predictions. Having unbiased and efficient parameter estimates is less
### important, and other factors play a role, such as the possibility to fit the
### missing data method in the pipeline.

### We will now practice with implementing a missing data method in a prediction
### model. We have to do the whole missing values analysis and missing values 
### treatment as part of the cleaning/feature selection process: in the training
### dataset. The parameters of the imputation method will have to be stored, and 
### then applied to the testset. We will evaluate the model with the mse. 

## First, split inc_data into a training and test dataset
train_test <- rbinom(n = nrow(inc_data), size = 1, prob = 0.7)
train <- inc_data[train_test == 1, ]
test <- inc_data[train_test == 0, ]

## Then, apply median imputation to the training set
median_alcohol <- median(train$alcohol, na.rm = TRUE)
R3 <- is.na(train$alcohol)
compl_train <- train
compl_train[R3 == 1, 'alcohol'] <- median_alcohol

### We will make a regression prediction model: predicting 'quality' from 
### 'alcohol'. Although the analysis method is the same here, the aim of the 
### analysis is different. We are not interested in the B coefficient, but we 
### wish to obtain the lowest mse. 

## Make a lm in the imputed training set.

train_model <- lm(quality ~ alcohol, compl_train)
predictions_train <- predict(train_model, compl_train)
mse_train <- mean(sum((compl_train$quality - predictions_train)^2))

## Apply the imputation procedure and analysis method to the test dataset.
## Make sure you use the information from the training dataset.

R4 <- is.na(test$alcohol)
compl_test <- test
compl_test[R4 == 1, 'alcohol'] <- median_alcohol
predictions_test <- predict(train_model, compl_test)
mse_test <- mean(sum((compl_test$quality - predictions_test)^2))

## Now, create your own imputation method or choose one that we have used above.
## Are you able to lower the mse_test? 

### End of the workshop ###
### Do not hestitate to contact me by email ###
### My contact details are at https://rianneschouten.github.io ###
### Or through twitter: @missD_ta ###