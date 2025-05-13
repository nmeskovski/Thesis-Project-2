rm(list=ls())
library(readxl)
data <- read_excel('/Users/gazmarkolos/Desktop/Project 2/Data_dirty (1).xlsx', sheet = "Data")

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



# Install the package if you haven't already
install.packages("corrplot")  # Run only once

# Load the library
library(corrplot)

# Ensure your data is numeric (excluding non-numeric columns like "country")
numeric_data <- data_final_numeric[sapply(data_final_numeric, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Plot the correlogram
corrplot(cor_matrix,
         method = "color",          # Use colored squares
         type = "upper",            # Show upper triangle only
         order = "hclust",          # Cluster variables by correlation
         tl.col = "black",          # Text label color
         tl.cex = 0.8,              # Text label size
         col = colorRampPalette(c("red", "white", "blue"))(200))  # Custom color gradient

write.csv(data_final_numeric, file = "/Users/gazmarkolos/Desktop/Project 2/data_final_numeric.csv", row.names = FALSE)









