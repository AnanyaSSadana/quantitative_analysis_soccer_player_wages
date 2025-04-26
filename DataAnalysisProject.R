### Data Analysis R project 
#.    HYPOTHESIS 1
# Null Hypothesis (H0): The average wage of players with contracts valid until 2026 or later (long contracts)
# is equal to the average wage of players with contracts ending before 2026 (short contracts).
# Alternate Hypothesis (H1): The average wage of players with long contracts is not equal to the average wage of
# players with short contracts.


install.packages("caret")
install.packages("datatable")
install.packages("mltools")
install.packages("Hmisc")
install.packages("ggplot2")
install.packages("BSDA")
library(caret)
library(data.table)
library(mltools)
library(Hmisc)
library(ggplot2)
library(BSDA)


# Read the CSV file
data <- read.csv("/Users/adityarana/Documents/DataAnalysisProject/FifaFinalEditied10.csv")


# View the first few rows of the dataframe
head(data)


# Extract the 'Wage' column
wage <- data$Wage
summary(wage)

# Remove the '€' and 'K' from the 'Wage' column and convert to numeric
# The log function in R computes the natural logarithm, which is the logarithm to the base ‘e’. This is often done in data analysis to handle skewed data.
wage <- as.numeric(gsub("K", "", gsub("€", "", wage)))
values <- as.numeric(gsub("M", "", gsub("€", "", data$Value)))
print(values)



# This line of code generates a histogram of the ‘wage’ variable with “Histogram for Wage” as the title and “Wage” as the x-axis label.

hist(wage, main = "Histogram for Wage", xlab = "Wage")

qqnorm(wage)
qqline(wage)
shapiro.test(wage)


# Extract the 'Contract Valid Until' column
contract_until <- data$Contract.Valid.Until
head(contract_until)

# Extract the wages of players with longer contracts (let's assume contracts valid until 2026 or later are considered long)
long_contract_wages <- wage[data$`Contract.Valid.Until` >= 2026]
head(long_contract_wages)
hist(long_contract_wages, main = "Histogram for Wage", xlab = "Wage")
shapiro.test(long_contract_wages)
qqnorm(long_contract_wages)
qqline(long_contract_wages)

# Extract the wages of players with Shorter contracts (let's assume contracts valid less than 2026)
short_contract_wages <- wage[data$`Contract.Valid.Until` < 2026]
head(short_contract_wages)
hist(short_contract_wages, main = "Histogram for Wage", xlab = "Wage")
shapiro.test(short_contract_wages)
qqnorm(short_contract_wages)
qqline(short_contract_wages)

# statistics for the long and short_contract_wages data
print(sd(long_contract_wages))
print(mean(long_contract_wages))
print(sd(short_contract_wages))
print(mean(short_contract_wages))


var.test(long_contract_wages,short_contract_wages)
t_test_result <- t.test(long_contract_wages, short_contract_wages)
print(t_test_result)

mann_whitney_test_results <- wilcox.test(long_contract_wages, short_contract_wages, alternative = "two.sided")

# Print the results
print(mann_whitney_test_results)



# with log on Wage 

log_wage <- log(wage)
print(log_wage)

# This line of code generates a histogram of the ‘wage’ variable with “Histogram for Wage” as the title and “Wage” as the x-axis label.

hist(log_wage, main = "Histogram for Wage", xlab = "Wage")

qqnorm(log_wage)
qqline(log_wage)
shapiro.test(log_wage)


# Extract the 'Contract Valid Until' column
contract_until <- data$Contract.Valid.Until
head(contract_until)

# Extract the wages of players with longer contracts (let's assume contracts valid until 2026 or later are considered long)
log_long_contract_wages <- log_wage[data$`Contract.Valid.Until` >= 2026]
head(log_long_contract_wages)
# Histogram plot for Long contract wages 
hist(log_long_contract_wages, main = "Histogram for Wage", xlab = "Wage")
# Shapiro-Wilk test for normality on log-transformed wages of players with longer contracts
shapiro.test(log_long_contract_wages)
qqnorm(log_long_contract_wages)
qqline(log_long_contract_wages)

# Extract the wages of players with Shorter contracts (let's assume contracts valid less than 2026)
log_short_contract_wages <- log_wage[data$`Contract.Valid.Until` < 2026]
head(log_short_contract_wages)
hist(log_short_contract_wages, main = "Histogram for Wage", xlab = "Wage")
# Shapiro-Wilk test for normality on log-transformed wages of players with shorter contracts
shapiro.test(log_short_contract_wages)
qqnorm(log_short_contract_wages)
qqline(log_short_contract_wages)

# statistics for the long and short_contract_wages data
print(sd(log_long_contract_wages))
print(mean(log_long_contract_wages))
print(sd(log_short_contract_wages))
print(mean(log_short_contract_wages))

# variance test for long and short contract wages
var.test(log_long_contract_wages,log_short_contract_wages)
# Independent two-sample t-test on log-transformed wages of players with longer and shorter contracts
t_test_result <- t.test(log_long_contract_wages, log_short_contract_wages)
print(t_test_result)

#.    HYPOTHESIS 2 
# Null Hypothesis (H0): The player’s age, overall rating, and contract length have no effect on their wage.
# Alternate Hypothesis (H1): At least one of the player’s age, overall rating, and contract length has a significant
# effect on their wage.
international_reputation <- data$International.Reputation
# The GLM is used here is to model the relationship between predictors (Age, International Reputation, Contract Valid Until, Potential, and Value)
# and the response variable (log-transformed wages).
model <- glm(log_wage ~  data$Age + international_reputation + contract_until + data$Potential + values, family = gaussian())
summary(model)

M_Wage = log_wage
# Set 20% of the data as NA
set.seed(143)  # for reproducibility
# This line is generating a random sample of indices representing 20% of the length of the M_Wage vector.
na_indices = sample(1:length(M_Wage), size = length(M_Wage) * 0.20 )
M_Wage[na_indices] = NA
print(M_Wage)

# "NA" values replaced with mean:
Wage.Missing = M_Wage
# This line is replacing the missing values in Wage.Missing with the mean of the non-missing values. The unname
# function is used to remove the names attribute of the result.
Wage.Missing = unname(impute(Wage.Missing, mean))
# This line is fitting a Generalized Linear Model (GLM) with a Gaussian family to the data. 
WageModel <- glm(Wage.Missing~ data$Age + international_reputation + contract_until + data$Potential + values, family = gaussian())
summary(WageModel)

# "NA" values replaced with median:
Wage.Missing = M_Wage
# This line is replacing the missing values in Wage.Missing with the median of the non-missing values. The unname
# function is used to remove the names attribute of the result.
Wage.Missing = unname(impute(Wage.Missing,median))
# This line is fitting a Generalized Linear Model (GLM) with a Gaussian family to the data. 
WageModel <- glm(Wage.Missing~ data$Age + international_reputation + contract_until + data$Potential + values, family = gaussian())
summary(WageModel)


# MNAR

sample_wage = log_wage 
# Create a binary variable that equals 1 if Age is 30 or older, and 0 otherwise
is_old_player = ifelse(data$Age >= 30, 1, 0)

# Set Wage1 to NA for a random 20% of the old player observations
set.seed(123)  # for reproducibility
# This line is creating a vector of indices for the ‘old’ players in the data.
old_player_indices = which(is_old_player == 1)
# This is for generating a random sample of indices representing 20% of the ‘old’ player observations.
na_indices = sample(old_player_indices, size = length(old_player_indices) * 0.2)
#We are setting the sample_wage values for the selected ‘old’ player observations to NA
sample_wage[na_indices] = NA

# "NA" values replaced with mean:
Wage.Missing = sample_wage
Wage.Missing = impute(Wage.Missing, mean)
# Here we are fitting a Generalized Linear Model (GLM) with a Gaussian family to the data, using Wage.Missing as the response variable and the player’s age,
# international reputation, contract length, potential and values as predictor variables.
WageModel <- glm(Wage.Missing~ data$Age+ international_reputation+ contract_until+ data$Potential + + values, family = gaussian())
summary(WageModel)

# "NA" values replaced with median:
Wage.Missing = sample_wage
Wage.Missing = impute(Wage.Missing,median)
# Here we are fitting a Generalized Linear Model (GLM) with a Gaussian family to the data, using Wage.Missing as the response variable and the player’s age,
# international reputation, contract length, potential and values as predictor variables.
WageModel <- glm(Wage.Missing~data$Age+international_reputation+contract_until+data$Potential+ values, family = gaussian())
summary(WageModel)

###############################################################
#  SECTION 3 – TRUE A/B EXPERIMENT (offline simulation)      #
###############################################################

library(dplyr)        # pipes & mutate
library(broom)        # tidy() for model output
library(pwr)          # quick power calc
library(ggplot2)

## 3·1  Fit / reuse the baseline GLM --------------------------
# If you already have a GLM called `model`, skip this chunk
glm_base <- glm(
  log_wage ~ Age + International.Reputation + contract_until +
             Potential + values,
  data   = data,
  family = gaussian()
)

## 3·2  One-shot power sketch ---------------------------------
# Detect a 5 % lift in mean wages on the original scale
sd_log  <- sd(log_wage, na.rm = TRUE)
delta   <- log10(1.05)                 # 5 % expressed in log-10 space
n_arm   <- ceiling(
  pwr.t.test(d = delta / sd_log,
             sig.level = 0.05,
             power     = 0.80,
             type      = "two.sample")$n
)                                      # ~4 000 per arm

## 3·3  Helper that runs ONE experiment -----------------------
simulate_once <- function(df, n_total) {
  boot <- df %>% 
    slice_sample(n = n_total, replace = TRUE) %>%            # bootstrap rows
    mutate(arm = sample(c("C", "T"), n_total, replace = TRUE)) %>% 
    mutate(Contract.Valid.Until = if_else(arm == "T", 2027, Contract.Valid.Until),
           contract_group       = if_else(Contract.Valid.Until >= 2026,
                                          "long", "short"))
  
  # predict synthetic outcomes with the frozen GLM
  boot$pred_log_wage <- predict(glm_base, newdata = boot, type = "response")
  
  # Welch two-sample t-test
  t_out  <- t.test(pred_log_wage ~ arm, data = boot)
  
  # lift on the original (euro) scale
  lift   <- boot %>% 
              group_by(arm) %>% 
              summarise(mu = mean(10^pred_log_wage)) %>% 
              summarise(lift = mu[arm=="T"]/mu[arm=="C"] - 1) %>% 
              pull(lift)
  
  tibble(p_value = t_out$p.value, lift = lift)
}

## 3·4  Run 1 000 Monte-Carlo replications --------------------
set.seed(42)
n_total <- 2 * n_arm                       # ~8 000 rows
mc      <- replicate(1000,
                     simulate_once(data, n_total),
                     simplify = FALSE) %>% 
           bind_rows()

## 3·5  Quick diagnostics -------------------------------------
mc_summary <- mc %>% 
  summarise(power_est    = mean(p_value < 0.05),
            mean_lift_pc = mean(lift) * 100)

print(mc_summary)
#> power_est     mean_lift_pc
#>     0.22            1.1   (example output)

ggplot(mc, aes(lift*100)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribution of estimated wage lift",
       x = "Lift (%)", y = "Count")
