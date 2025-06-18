library(tidyverse)
library(readxl)
library(stringr)

# HELPER FUNCTIONS --------------------------------------------------------

convert_to_num <- function(x){
  return(as.numeric(gsub(",", "", x)))
}




# VIOLENT CRIMES ----------------------------------------------------------
violent_crimes_df <- read.csv("./data/estimated_crimes_1979_2023.csv")
violent_crimes_df <- violent_crimes_df %>% filter(year == 2022)

summary(violent_crimes_df)
str(violent_crimes_df)


violent_crimes_df <- violent_crimes_df %>% 
  mutate(across(c(population, violent_crime, homicide, rape_revised, robbery, aggravated_assault,
                property_crime, burglary, larceny, motor_vehicle_theft),
         convert_to_num))

violent_crimes_df <- violent_crimes_df %>% mutate(across(c(state_abbr, state_name), stringr::str_trim))

# legacy definition not applicable, caveats empty
violent_crimes_df <- violent_crimes_df %>% select(-c(rape_legacy, caveats))
violent_crimes_df$state_name[1] <- "Total"

str(violent_crimes_df)

# DRUG OFFENCES -----------------------------------------------------------
drug_crimes_df <- read_excel("./data/Crimes_Against_Society_Offenses_Offense_Category_by_State_2022.xlsx",
                             sheet = "Data-edited")
str(drug_crimes_df)

drug_crimes_df$State <- stringr::str_trim(drug_crimes_df$State)

str(drug_crimes_df)


# UNAUTHORIZED IMMIGRANTS -------------------------------------------------
unauth_immigrant_df <- read_excel("./data/SR_24.07.22_unauthorized-immigrants_table-3.xlsx", 
                                  sheet = "Data-edited")
# calculate <5000s on percentages
# replace - with NA
