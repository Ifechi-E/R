library(tidyverse)
library(AER)

### Define handy functions #########################################
correlate <- function(x){
  return(cor(data$lwage,x))
}

# Function to calculate correlations between a target column and all other columns
correlations <- function(df, target_column) {
 
  # Calculate correlations between the target column and other columns
  correlations <- sapply(df, function(col) {
    if (is.numeric(col)) {
      cor(df[[target_column]], col, use = "complete.obs")
    } else {
      NA # Return NA if the column is not numeric
    }
  })
  
  # Return the correlation values, excluding the target column itself
  correlations <- correlations[names(correlations) != target_column]
  
  return(correlations)
}


### Load data ##########
setwd('/Users/ifechiekekwe/Documents/Uni work/4th Year/Econ/Econometrics/Topic 1 Labs/Assignment 1 (IV Topic)-20241002')
load('card.Rdata')
data <- card
view(data)

data$high_edu <- data$educ > 12
data$high_edu <- data$high_edu * 1

data <- na.omit(data)

rural <- data[data$smsa == 0, ]
metro <- data[data$smsa == 1, ]

### Setting up Variables & data Frames ########################################################
  ### Main ###############################################
Y <- data$lwage
X <- data$high_edu
Z <- data$nearc4

age <- data$age

fath <- data$fatheduc
moth <- data$motheduc
singm <- data$sinmom14

south66 <- data$south66
black <- data$black
IQ <- data$IQ

df <- data.frame(Y = Y, X = X, Z= Z, Age = age, Fath= fath, Moth = moth, Singm = singm, South66 = south66, Black = black, IQ = IQ)

### clear placholder variables
rm(Y,X,Z,age,fath,moth,singm,south66,black,IQ)

  ### Rural ###############################################
Y <- rural$lwage
X <- rural$high_edu
Z <- rural$nearc4

age <- rural$age

fath <- rural$fatheduc
moth <- rural$motheduc
singm <- rural$sinmom14

south66 <- rural$south66
black <- rural$black
IQ <- rural$IQ

rural_df <- data.frame(Y = Y, X = X, Z= Z, Age = age, Fath= fath, Moth = moth, Singm = singm, South66 = south66, Black = black, IQ = IQ)
### clear placholder variables
rm(Y,X,Z,age,fath,moth,singm,south66,black,IQ)

  ### Metro ###############################################
Y <- metro$lwage
X <- metro$high_edu
Z <- metro$nearc4

age <- metro$age

fath <- metro$fatheduc
moth <- metro$motheduc
singm <- metro$sinmom14

south66 <- metro$south66
black <- metro$black
IQ <- metro$IQ

metro_df <- data.frame(Y = Y, X = X, Z= Z, Age = age, Fath= fath, Moth = moth, Singm = singm, South66 = south66, Black = black, IQ = IQ)
metro_df <- na.omit(metro_df)
  ### clear placholder variables
rm(Y,X,Z,age,fath,moth,singm,south66,black,IQ)

  ### Descriptive Stats on variable shortlist#####################################
for (col in colnames(df)) {
  # Summary statistics for the column
  cat("Summary of", col, ":\n")
  print(summary(df[[col]]))
  cat("\n")
  
  # Plot histogram for the column
  hist(df[[col]], 
       main = paste("Histogram of", col), 
       xlab = col, 
       col = "grey", 
       border = "black")
}

    # correlation accross all data
correlation_matrix_all <- correlations(data, 'lwage')
hehe <- lapply(data,correlate)
### The Lab ###########################################################
  ### Main Model ##############################
C <- lm(Y ~ X + Age + Fath + Moth + Singm + South66 + Black + IQ, data = df)
summary(C)
residualPlot(C)
AIC(C)

A <- lm(Y ~ X + Age + Fath + Moth + Singm + Black, data = df)
summary(A)
AIC(A)

B <- lm(Y ~ X + Age + Moth + Black, data = df)
summary(B)
AIC(B)

################################################################################
The_IV <- ivreg(Y ~ X + Age + Moth + Black 
                   |Z + Age + Moth + Black, data = df)
summary(The_IV, diagnostics = TRUE)
### Residuals Checks ##############################
summary(The_IV$residuals)
plot(The_IV$residuals)

ggplot(data_frame(The_IV$residuals), aes(x = The_IV$residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Residuals",
       x = "Residuals",
       y = "Density") +
  theme_minimal()

# Optional: Q-Q plot to check normality of residuals
qqnorm(The_IV$residuals)
qqline(The_IV$residuals, col = "grey")


################################################################################
### REDUCED FORM ######################################
Reduced_Form <- lm(Y ~ Z + Age + Moth + Black, data = df)
summary(Reduced_Form)
residualPlot(Reduced_Form)
AIC(Reduced_Form)

### FIRST STAGE ######################################
First_Stage <- lm(X ~ Z + Age + Moth + Black, data = df)
summary(First_Stage, diagnostics = TRUE)
residualPlot(First_Stage)
AIC(First_Stage)

  ### Weak Instrument Check #######################
fstat <- linearHypothesis(First_Stage , c('Z = 0'))
summary(fstat$F)

  ### Correlation with 2nd Stage residuals #######################
cor(df$Z, The_IV$residuals)
cor2_test <- lm(The_IV$residuals ~ df$Z)
summary(cor2_test)

 
### SECOND STAGE #####################################
Second_Stage <- lm(Y ~ X + Age + Moth + Black, data = df)
summary(Second_Stage)
residualPlot(Second_Stage)
AIC(Second_Stage)

### Balance Testing ######################################
IQ_check <- lm(IQ ~ Z + Age + Moth + Black, data = df)
summary(IQ_check)

Race_check <- lm(Black ~ Z + Age + Moth + Black, data = df) 
summary(Race_check)

Reigion_check <- lm(South66 ~ Z + Age + Moth + Black, data = df)
summary(Reigion_check)



################################################################################
# Metropolitan vs Rural
################################################################################
### Rural ##############################
Rural_IV <- ivreg(Y ~ X + Age + Moth + Black| 
                  Z + Age + Moth + Black, data = rural_df)
summary(Rural_IV, diagnostics = TRUE)

### Descriptive Stats ##################
for (col in colnames(rural_df)) {
  # Summary statistics for the column
  cat("Summary of", col, ":\n")
  print(summary(rural_df[[col]]))
  cat("\n")
}

  ### REDUCED FORM ######################################
Rural_Reduced_Form <- lm(Y ~ Z + Age + Moth + Black, data = rural_df)
summary(Rural_Reduced_Form)
residualPlot(Rural_Reduced_Form)
AIC(Rural_Reduced_Form)

  ### FIRST STAGE ######################################
Rural_First_Stage <- lm(X ~ Z + Age + Moth + Black, data = rural_df)
summary(Rural_First_Stage, diagnostics = TRUE)
residualPlot(Rural_First_Stage)
AIC(Rural_First_Stage)

      ### Weak Instrument Check #######################
fstat <- linearHypothesis(Rural_First_Stage , c('Z = 0'))
summary(fstat$F)

  ### SECOND STAGE #####################################
Rural_Second_Stage <- lm(Y ~ X + Age + Moth + Black, data = rural_df)
summary(Rural_Second_Stage)
residualPlot(Rural_Second_Stage)
AIC(Rural_Second_Stage)

      ### Correlation with 2nd Stage residuals #######################
cor(rural_df$Z, Rural_IV$residuals)
rural_cor2_test <- lm(Rural_IV$residuals ~ rural_df$Z)
summary(rural_cor2_test)

  ### Balance Testing ######################################
Rural_IQ_check <- lm(IQ ~ Z + Age + Moth, data = rural_df)
summary(Rural_IQ_check)

Rural_Race_check <- lm(Black ~ Z + Age + Moth, data = rural_df) 
summary(Rural_Race_check)

Rural_Reigion_check <- lm(South66 ~ Z + Age + Moth, data = rural_df)
summary(Rural_Reigion_check)

### Metropolitan ##############################
Metro_IV <- ivreg(Y ~ X + Age + Moth + Black| 
                    Z + Age + Moth + Black, data = metro_df)
summary(Metro_IV, diagnostics = TRUE)

### Descriptive Stats ##################
for (col in colnames(metro_df)) {
  # Summary statistics for the column
  cat("Summary of", col, ":\n")
  print(summary(metro_df[[col]]))
  cat("\n")
}
  ### REDUCED FORM ######################################
Metro_Reduced_Form <- lm(Y ~ Z + Age + Moth + Black, data = metro_df)
summary(Metro_Reduced_Form)
residualPlot(Metro_Reduced_Form)
AIC(Metro_Reduced_Form)

  ### FIRST STAGE ######################################
Metro_First_Stage <- lm(X ~ Z + Age + Moth + Black, data = metro_df)
summary(Metro_First_Stage, diagnostics = TRUE)
residualPlot(Metro_First_Stage)
AIC(Metro_First_Stage)

      ### Weak Instrument Check #######################
fstat <- linearHypothesis(Metro_First_Stage , c('Z = 0'))
summary(fstat$F)

  ### SECOND STAGE #####################################
Metro_Second_Stage <- lm(Y ~ X + Age + Moth + Black, data = metro_df)
summary(Metro_Second_Stage)
residualPlot(Metro_Second_Stage)
AIC(Metro_Second_Stage)

      ### Correlation with 2nd Stage residuals #######################
cor(metro_df$Z, Metro_IV$residuals)
metro_cor2_test <- lm(Metro_IV$residuals ~ metro_df$Z)
summary(metro_cor2_test)

  ### Balance Testing ######################################
Metro_IQ_check <- lm(IQ ~ Z + Age + Moth, data = metro_df)
summary(Metro_IQ_check)

Metro_Race_check <- lm(Black ~ Z + Age + Moth, data = metro_df) 
summary(Metro_Race_check)

Metro_Reigion_check <- lm(South66 ~ Z + Age + Moth, data = metro_df)
summary(Metro_Reigion_check)

