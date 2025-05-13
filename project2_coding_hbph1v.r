rm(list=ls())
library(readxl)
data <- read_excel("C:/Users/bazsi/OneDrive/Asztali gép/School/4. Semester/Project 2/Thesis/Data_dirty (2).xlsx", sheet = "Data")

library(tidyverse)
library(dplyr)

data_2019 <- data %>% 
  select('Country Name', 'Series Name', '2019 [YR2019]')

library(tidyr)

data_final <- data_2019 %>%
  pivot_wider(names_from = `Series Name`, values_from = `2019 [YR2019]`)

install.packages("skimr")
library(skimr)


str(data_final)

data_final_numeric <- data_final %>%
  mutate(across(
    where(is.character) & !any_of("Country Name"), 
    ~ suppressWarnings(as.numeric(.))
  ))


missing_summary <- sapply(data_final_numeric, function(x) mean(is.na(x)))
print(missing_summary)


library(ggplot2)




install.packages("janitor")
library(janitor)
data_final_numeric <- clean_names(data_final_numeric)

str(data_final_numeric$mortality_from_cvd_cancer_diabetes_or_crd_between_exact_ages_30_and_70_percent)


cols_to_convert <- setdiff(names(data_final_numeric), "country_name")


data_final_numeric[cols_to_convert] <- lapply(data_final_numeric[cols_to_convert], function(col) {
  cleaned <- gsub("[% ,]", "", col)  
  as.numeric(cleaned)                
})


warnings()

str(data_final_numeric)



ggplot(data_final_numeric, aes(x=gdp_per_capita_ppp_current_international, y=mortality_from_cvd_cancer_diabetes_or_crd_between_exact_ages_30_and_70_percent)) + geom_line()

install.packages("corrplot")  
library(corrplot)


data_numbers <- data_final_numeric[sapply(data_final_numeric, is.numeric)]

cor_matrix <- cor(data_numbers, use = "pairwise.complete.obs")



n <- ncol(data_final_numeric)


cols_to_drop <- c(n,     
                  n-2,   
                  n-3,   
                  n-4,   
                  n-5)   

names(data_final_numeric)[sort(unique(cols_to_drop))]

data_final_numeric <- data_final_numeric[, -sort(unique(cols_to_drop))]



colnames(data_final_numeric)

names(data_final_numeric) <- c(
  "country",
  "ncd_mortality_pct",
  "alcohol_per_capita",
  "gni_usd",
  "suicide_rate",
  "pm2_5",
  "air_pollution_mort",
  "water_sanitation_mort",
  "safe_water_pct",
  "ppp_water_invest",
  "basic_water_pct",
  "health_exp_gdp_pct",
  "health_exp_usd",
  "health_exp_ppp",
  "gov_health_exp_gdp_pct",
  "gov_health_exp_usd",
  "gov_health_exp_ppp",
  "hospital_beds",
  "oop_exp_pct",
  "oop_exp_ppp",
  "oop_exp_usd",
  "gdp_ppp"
)



# Drop rows where ncd_mortality_pct is NA
data_final_numeric <- data_final_numeric[!is.na(data_final_numeric$ncd_mortality_pct), ]

data_final_numeric <- data_final_numeric[-c(184:231), ]

library(ggplot2)
ggplot(data_final_numeric, aes(x=gdp_ppp, y=ncd_mortality_pct)) + geom_line()


# Create log-transformed variables
data_final_numeric$log_health_exp_usd <- log(data_final_numeric$health_exp_usd)
data_final_numeric$log_gdp_ppp <- log(data_final_numeric$gdp_ppp)

# Drop any rows with remaining NA values in the model variables
regression_data <- data_final_numeric %>%
  select(ncd_mortality_pct, health_exp_usd, gdp_ppp, pm2_5, oop_exp_pct, log_health_exp_usd, log_gdp_ppp) %>%
  drop_na()

# MODEL 1 – simple linear model with health expenditure only
model1 <- lm(ncd_mortality_pct ~ health_exp_usd, data = regression_data)
summary(model1)

# MODEL 2 – add GDP
model2 <- lm(ncd_mortality_pct ~ health_exp_usd + gdp_ppp, data = regression_data)
summary(model2)

# MODEL 3 – full model with environmental and financial variables
model3 <- lm(ncd_mortality_pct ~ health_exp_usd + gdp_ppp + pm2_5 + oop_exp_pct, data = regression_data)
summary(model3)

# MODEL 4 – log-transformed predictors
model4 <- lm(ncd_mortality_pct ~ log_health_exp_usd + log_gdp_ppp + pm2_5 + oop_exp_pct, data = regression_data)
summary(model4)









