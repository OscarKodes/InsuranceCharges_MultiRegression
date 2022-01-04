
# Clear Everything
rm(list = ls())

# Set working directory
setwd("C:\Users\Oscar Ko\Desktop\Insurance_Charges")

# Get working directory
getwd()

# Get data
data <- read.csv("Insurance Charges.csv")

# Examine data
head(data)
tail(data)
str(data)
summary(data)
colnames(data)

# library
library(tidyverse)
library(hmisc)
library(Hmisc)

# Select 6 variables
data <- select(data,
               age,
               sex_N,
               BMI,
               N_of_Children,
               Smoker_N,
               charges)


# Get correlation matrix (r-statistic) with p-values===============================

# standard correlation matrix
corMatrix <- cor(data)
corMatrix

# correlation matrix with p vals
R_corr_matrix <- rcorr(as.matrix(data))
R_corr_matrix


# Write matrices into csv files
write.csv(corMatrix,"6-InsuranceCharges/correlationMatrix.csv", row.names = FALSE)
write.csv(R_corr_matrix$P,"6-InsuranceCharges/p-values.csv", row.names = FALSE)


# Visualize with Scatterplots and Linear Regression Lines =================================

# Create Function to create Scatterplots
make_scatterplot <- function(df, var1, var2) {
  
  # Get linear regression formula for label
  lm_eqn <- function(df){
    m <- lm(paste(var2, "~", var1, sep = " "), df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
  }
  
  # Create scatterplot
  test_plot <- ggplot(data = df,
                      aes(x = .data[[var1]],
                          y = .data[[var2]])) +
    geom_point() +
    geom_smooth(method = "lm", se = F, color = "red") + 
    geom_text(aes(x=Inf,y=Inf,hjust=1,
                  vjust=1,label=annotateText), 
              # place linear regression text in upper right corner
              label = lm_eqn(df), parse = TRUE, color = "red")
  
  # add labels
  test_plot <- test_plot +
    ggtitle(paste(var1, "vs", var2, sep=" ")) +
    theme_classic() +
    theme(axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          
          
          plot.title = element_text(size=20)
    )
  
  # display plot
  test_plot
  
  # save current plot as pdf
  ggsave(paste("6-InsuranceCharges/", var1, "_vs_", var2, ".png", sep=""))
}

#make_scatterplot(selectedData, "HS_Grads", "Poverty")

# Loop Through All Combinations while exporting pngs
varNames <- colnames(data)
varNames

for(i in 1:length(varNames)) {
  for (j in (1+i):length(varNames)) {
    make_scatterplot(data, varNames[i], varNames[j])
  }
}





# Simple Regression Models ================================

# single variable regression formulas 
age_model <- lm(charges ~ age, data = data)
sex_N_model <- lm(charges ~ sex_N, data = data)
BMI_model <- lm(charges ~ BMI, data = data)
N_of_Children_model <- lm(charges ~ N_of_Children, data = data)
Smoker_N_model <- lm(charges ~ Smoker_N, data = data)

# summaries for simple regressions
summary(age_model)
summary(sex_N_model)
summary(BMI_model)
summary(N_of_Children_model)
summary(Smoker_N_model)

# Multivariate Regression Models =========================


# smoker had the highest R squared of 0.6198-------------
# age had the second highest R squared of 0.08941

Smoker_N_age_model <- lm(charges ~ Smoker_N + age, data = data)
summary(Smoker_N_age_model)
# This model with smoker and age is good with 72% explained variability


# BMI had the third highest R square of 0.03934---------

Smoker_N_age_BMI_model <- lm(charges ~ Smoker_N + age + BMI, data = data)
summary(Smoker_N_age_BMI_model)
# This model with smoker, age, and BMI isn't great with 75% explained v.


# N_of_Children had R square 0.004624------------

Smoker_N_age_N_of_Children_model <- lm(charges ~ Smoker_N + age + N_of_Children, data = data)
summary(Smoker_N_age_N_of_Children_model)
# This model with smoker, age, and children isn't great with 72% explained v.


# sex_N had R square 0.003282----------

Smoker_N_age_sex_N_model <- lm(charges ~ Smoker_N + age + sex_N, data = data)
summary(Smoker_N_age_sex_N_model)
# This model with smoker, age, and children isn't great with 72% explained v.



# Everything -----------------------
everything_model <- lm(charges ~ Smoker_N + age + BMI + N_of_Children + sex_N, data = data)
summary(everything_model)



# Interaction effects ################################


########################################################################

#### Model 1  (Smoker_N, Age, BMI) ---- Without interaction 

# (R-squared:  0.7475)
Smoker_N_age_BMI_model <- lm(charges ~ Smoker_N + age + BMI, data = data)
summary(Smoker_N_age_BMI_model)


data$itrctn_bmi_smoker <- data$BMI * data$Smoker_N

#### Model 2  (Smoker_N, Age, BMI, itrctn_bmi_smoker) ---- Interaction Term added

# (R-squared:  0.8363)
Interaction_model_9 <- lm(charges ~ Smoker_N + age + BMI + itrctn_bmi_smoker, data = data)
summary(Interaction_model_9)


#### Model 3  (Smoker_N, Age, itrctn_bmi_smoker) ---- Interaction Term replaces BMI

# (R-squared:  0.8363) 
Interaction_model_10 <- lm(charges ~ Smoker_N + age + itrctn_bmi_smoker, data = data)
summary(Interaction_model_10)











