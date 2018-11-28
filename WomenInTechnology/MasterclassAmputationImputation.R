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
data <- read.csv("red_wine_quality.txt", 
                 sep = '\t')
data <- data[, c('alcohol', 'density', 'pH')]
nrow(data)
summary(data)
cor(data)

plotdata <- data
ggplot(plotdata, aes(density, alcohol)) +
  geom_point()

# Define truth
truemodel <- glm(alcohol ~ density, 
                 family = 'gaussian', data)
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
summary(mads)
mads$patterns
md.pattern(mads$amp)

### Question 1 ###
mypatterns <- 

mads <- ampute()
inc_data <- mads$amp
md.pattern(inc_data)

### Question 2 ###

### Question 3 ###
mads$mech
mads$weights
mads$patterns

### Question 4 ###
mypatterns <- c(1, 0, 1)
myweights <- c(0, 0, 1)
mads <- ampute(data,
               patterns = mypatterns,
               weights =  myweights)

inc_data <- mads$amp
md.pattern(inc_data)

plotdata["R"] <- !is.na(inc_data$density)
ggplot(plotdata, aes(pH, density, color = R)) +
  geom_point() + 
  scale_color_manual(
    values = c('red', 'darkblue'), 
    labels = c('missing', 'observed'))

MAR_coefs <- summary(lm(alcohol ~ density, 
                        inc_data))$coefficients

ggplot(plotdata, aes(density, alcohol, 
                     color = R)) +
  geom_point() +
  geom_text(aes(x = 1.003, y = 8.3), 
            label = 'complete', color = 'black') +
  geom_abline(slope = true_coefs[2, 1], 
              intercept = true_coefs[1, 1],
              color = 'black') +
  geom_abline(slope = MAR_coefs[2, 1], 
              intercept = MAR_coefs[1, 1],
              color = 'darkblue') +
  geom_text(aes(x = 1.003, y = 9.4), 
            label = 'MAR', color = 'darkblue') +
  scale_color_manual(values = c('red', 'darkblue'), 
                     labels = c('missing', 'observed'))

true_coefs
MAR_coefs

# Missing data methods

### Question 5. ###
mean_density <- ...
imputations <- rep(...)
imp_data <- inc_data
imp_data[...] <- imputations

md.pattern(imp_data)

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

### Question 6. ###

imp_model <- lm(density ~ pH, inc_data)
imp_coefs <- summary(imp_model)$coefficients
imputations <- 
  inc_data[is.na(inc_data$density), 'pH'] * imp_coefs[2, 1] + imp_coefs[1, 1]
imp_data <- inc_data
imp_data[is.na(inc_data$density), 'density'] <- 
  imputations

md.pattern(imp_data)

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

# Multiple imputation

imps <- mice(inc_data, m = 5, method = 'norm') 
fit <- with(imps, lm(alcohol ~ density))
imp_coefs_mi <- summary(pool(fit))

true_coefs
imp_coefs_reg
imp_coefs_mi

### End of the masterclass ###
### Do not hestitate to ask question by email ###
### My contact details are on rianneschouten.github.io ###