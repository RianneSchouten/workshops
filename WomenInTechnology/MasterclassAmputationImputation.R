### Masterclass Dealing with Missing Data in R ###
### Amputation or Imputation? ###
### By: Rianne Schouten ###
### All files available through github ###

# Download instructions
# First, download R and RStudio
# Then, install the packages in your console

install.packages("mice")
install.packages("ggplot2")

library(mice)
library(ggplot2)

# Introduction
set.seed(111)

# Complete data
data <- read.csv("red_wine_quality.txt", sep = '\t')
data <- data[, c('alcohol', 'density', 'pH')]
nrow(data)
summary(data)
cor(data)

plotdata <- data
ggplot(plotdata, aes(density, alcohol)) +
  geom_point()

# Define truth
truemodel <- glm(alcohol ~ density, family = 'gaussian', data)
true_coefs <- summary(truemodel)$coefficients
true_coefs

ggplot(plotdata, aes(density, alcohol)) +
  geom_point() +
  geom_abline(slope = true_coefs[2, 1], 
              intercept = true_coefs[1, 1],
              color = 'black') +
  geom_text(aes(x = 1.003, y = 8.3), 
            label = 'complete', color = 'black')

# Amputation
?ampute
mads <- ampute(data)
mads$patterns
md.pattern(mads$amp)

### Question 1 ###
mypatterns <- 

mads <- ampute(data = data,
                 patterns = mypatterns)
inc_data <- mads$amp
md.pattern(inc_data)

### Question 2 ###

### Question 3 ###
mads <- ampute()

MCAR_coefs <- summary(lm(alcohol ~ density, mads$amp))$coefficients
plotdata["R"] <- !is.na(mads$amp$density)

ggplot(plotdata, aes(density, alcohol, color = R)) +
  geom_point() +
  geom_abline(slope = true_coefs[2, 1], 
              intercept = true_coefs[1, 1],
              color = 'black') +
  geom_text(aes(x = 1.003, y = 8.3), 
            label = 'complete', color = 'black') +
  geom_abline(slope = MCAR_coefs[2, 1], 
              intercept = MCAR_coefs[1, 1],
              color = 'darkblue') +
  geom_text(aes(x = 1.0045, y = 8.85), 
            label = 'MCAR', color = 'darkblue') +
  scale_color_manual(values = c('red', 'darkblue'), 
                     labels = c('missing', 'observed'))

### Question 4 ###
mads <- ampute()

inc_data <- mads$amp
md.pattern(inc_data)

MAR_coefs <- summary(lm(alcohol ~ density, inc_data))$coefficients
plotdata["R"] <- !is.na(inc_data$density)

ggplot(plotdata, aes(density, alcohol, color = R)) +
  geom_point() +
  geom_abline(slope = true_coefs[2, 1], 
              intercept = true_coefs[1, 1],
              color = 'black') +
  geom_text(aes(x = 1.003, y = 8.3), 
            label = 'complete', color = 'black') +
  geom_abline(slope = MCAR_coefs[2, 1], 
              intercept = MCAR_coefs[1, 1],
              color = 'darkblue') +
  geom_text(aes(x = 1.0045, y = 8.85), 
            label = 'MCAR', color = 'darkblue') +
  geom_abline(slope = MAR_coefs[2, 1], 
              intercept = MAR_coefs[1, 1],
              color = 'darkblue') +
  geom_text(aes(x = 1.003, y = 9.4), 
            label = 'MAR', color = 'darkblue') +
  scale_color_manual(values = c('red', 'darkblue'), 
                     labels = c('missing', 'observed'))

ggplot(plotdata, aes(pH, density, color = R)) +
  geom_point() + 
  scale_color_manual(values = c('red', 'darkblue'), 
                     labels = c('missing', 'observed'))

mads$type

true_coefs
MCAR_coefs
MAR_coefs

### Missing data methods

# Question 5.

mean <- ...
imputations <- rep(...)
imp_data <- inc_data
imp_data[...] <- imputations

# Question 6. 

imp_coefs_mean <- summary(lm(alcohol ~ density, imp_data))$coefficients
imp_data["R"] <- !is.na(inc_data$density)

ggplot(imp_data, aes(density, alcohol, color = R)) +
  geom_point() +
  geom_abline(slope = true_coefs[2, 1], 
              intercept = true_coefs[1, 1],
              color = 'black') +
  geom_text(aes(x = 1.003, y = 8.3), 
            label = 'complete', color = 'black') +
  geom_abline(slope = MAR_coefs[2, 1], 
              intercept = MAR_coefs[1, 1],
              color = 'darkblue') +
  geom_text(aes(x = 1.003, y = 8.9), 
            label = 'MAR', color = 'darkblue') +
  geom_abline(slope = imp_coefs_mean[2, 1], 
              intercept = imp_coefs_mean[1, 1],
              color = 'orange') +
  geom_text(aes(x = 1.003, y = 9.2), 
            label = 'MEAN', color = 'orange') +
  scale_color_manual(values = c('orange', 'darkblue'), 
                     labels = c('imputed', 'observed'))

true_coefs
imp_coefs_mean

# Question 7.

imp_model <- 
imp_coefs <- 
imputations <- inc_data[] * imp_coefs[2, 1] + imp_coefs[1, 1]
imp_data <- inc_data
imp_data[] <- imputations

plotdata <- imp_data
plotdata["R"] <- !is.na(inc_data$density)

ggplot(plotdata, aes(pH, density, color = R)) +
  geom_point() +
  scale_color_manual(values = c('orange', 'darkblue'), 
                     labels = c('imputed', 'observed'))

md.pattern(imp_data)

imp_coefs_reg <- summary(lm(alcohol ~ density, imp_data))$coefficients
imp_data["R"] <- !is.na(inc_data$density)

ggplot(imp_data, aes(density, alcohol, color = R)) +
  geom_point() +
  geom_abline(slope = true_coefs[2, 1], 
              intercept = true_coefs[1, 1],
              color = 'black') +
  geom_text(aes(x = 1.003, y = 8.3), 
            label = 'complete', color = 'black') +
  geom_abline(slope = MAR_coefs[2, 1], 
              intercept = MAR_coefs[1, 1],
              color = 'darkblue') +
  geom_text(aes(x = 1.003, y = 9.2), 
            label = 'MAR', color = 'darkblue') +
  geom_abline(slope = imp_coefs_reg[2, 1], 
              intercept = imp_coefs_reg[1, 1],
              color = 'orange') +
  geom_text(aes(x = 1.003, y = 8.9), 
            label = 'REG', color = 'orange') +
  scale_color_manual(values = c('orange', 'darkblue'), 
                     labels = c('imputed', 'observed'))

true_coefs
MAR_coefs
imp_coefs_reg

# Multiple imputation

imps <- mice(inc_data, m = 10, method = 'norm') 
fit <- with(imps, lm(alcohol ~ density))
out <- summary(pool(fit))

### End of the masterclass ###
### Do not hestitate to ask question by email ###
### My contact details are on rianneschouten.github.io ###