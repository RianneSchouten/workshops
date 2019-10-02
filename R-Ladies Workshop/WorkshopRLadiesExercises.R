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

## Suppose you want to predict the quality of wines from the amount of alcohol
## they contain. Make a plot to visualize this relationship.

## In real life, you never have the complete dataset. We will therefore 
## create missing values in com_data and pretend we do not know about com_data. 
## In 'mice' we created function 'ampute()' to do the amputation for us. 
## We will not discuss 'ampute()' today but information is 
## available at rianneschouten.github.io

inc_data <- ampute(com_data, patterns = c(1, 0, 1))$amp

## Inspect the dataset again. How do you know that the dataset is now 
## incomplete? Can you find some information about which variable(s) is/are 
## incomplete and by how many? 

## Try to find out whether the dataset is MCAR, MAR or MNAR missing?
## Do this by: creating a vector that indicates whether a row has a missing 
## value.

## Then, create a histogram to compare the distribution of 'quality' for 
## rows with missing values and rows without missing values

## Is the missingness likely to be MCAR or MAR/MNAR?

## If you have extra time, try to use ampute() to generate MCAR missingness in 
## com_data. Follow the same steps as above. How do you expect the histogram
## plot to look?

# 2. Implementing missing data methods

## Earlier, you visualized the relationship between 'quality' and 'alcohol' in 
## the complete dataset. Now, use com_data and inc_data to visualize the 
## missing values. On average, where are the missing values? 
## How will this influence the relationship between the two variables? 

## Apply the technique of mean imputation
## To do this, first calculate the mean of the incomplete variable

## Second, impute the missing values with this mean

## Now, use the imputed dataset to visualize the relationship between 'quality'
## and 'alcohol'. What do you see? How will this missing data method influence 
## the relationship between the two variables? 

## Intuitively, it would make sense to use the observed values of 'quality' and
## 'density' to find better imputations for 'alcohol'. 
## We will now set up our own regression imputation model to do this. 
## First, make a regression model using the observed data 

## Second, use the model to find imputations for the missing 'alcohol' values
## Impute them in the incomplete dataset

## Inspect the imputed dataset with a plot like above. What do you think of this
## imputation method? 

## The histogram method will also work to compare the observed with the imputed
## values. Compare the distribution of the observed values of alcohol with the 
## mean imputed values. 

## Do the same for the regression imputed values. What are the differences? 

## Package 'mice' is developed to perform multiple imputation. But it can also
## be used to perform single imputation. For mean imputation, the sintax is:

mids <- mice(inc_data, method = "mean", m = 1, maxit = 1)
summary(mids) # gives you the specifications
mice_mean_data <- complete(mids) # extracts the (first) imputed dataset

## Use 'mice' to perform the imputation method "sample"
## Then, make a histogram plot to compare the imputed values with the observed
## values. What do you see?

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

## Use 'mice' with method = "norm" and m = 5 to perform multiple imputation
## Perform the same linear model and pool the estimates. You can do this quite
## easily by using mice(), with() and pool() in three separate lines. 
## Compare the pooled regression estimates with the truth. What do you find?

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

## Then, apply median imputation to the training set

## We will make a regression prediction model: predicting 'quality' from 
## 'alcohol'. Although the analysis method is the same here, the aim of the 
## analysis is different. We are not interested in the B coefficient, but we 
## wish to obtain the lowest mse. 

## Make a linear model in the imputed training set.

## Apply the imputation procedure and analysis method to the test dataset.
## Make sure you use the information from the training dataset.

## Now, create your own imputation method or choose one that we have used above.
## Are you able to lower the mse_test? 

### End of the workshop ###
### Do not hestitate to contact me: riannemargarethaschouten@gmail.com ###
### Follow my work at https://rianneschouten.github.io ###
### Or through twitter: @missD_ta
