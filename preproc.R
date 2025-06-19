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

# check structure
str(violent_crimes_df)

# check NA
colSums(is.na(violent_crimes_df))

# convert to num
violent_crimes_df <- violent_crimes_df %>% 
  mutate(across(c(population, violent_crime, homicide, rape_revised, robbery, aggravated_assault,
                property_crime, burglary, larceny, motor_vehicle_theft),
         convert_to_num))

# trim elading and ending whitespace
violent_crimes_df <- violent_crimes_df %>% mutate(across(c(state_abbr, state_name), stringr::str_trim))

# legacy definition not applicable, caveats empty
violent_crimes_df <- violent_crimes_df %>% select(-c(rape_legacy, caveats))
violent_crimes_df$state_name[1] <- "Total"

# rename state, make it first col
violent_crimes_df <- violent_crimes_df %>% rename(State = state_name)
violent_crimes_df <- violent_crimes_df %>% relocate(State)
str(violent_crimes_df)

# DRUG OFFENCES -----------------------------------------------------------
drug_crimes_df <- read_excel("./data/Crimes_Against_Society_Offenses_Offense_Category_by_State_2022.xlsx",
                             sheet = "Data-edited")
str(drug_crimes_df)

drug_crimes_df$State <- stringr::str_trim(drug_crimes_df$State)

str(drug_crimes_df)
colSums(is.na(drug_crimes_df))


# UNAUTHORIZED IMMIGRANTS -------------------------------------------------
unauth_immigrant_df <- read_excel("./data/SR_24.07.22_unauthorized-immigrants_table-3.xlsx", 
                                  sheet = "Data-edited")
# calculate <5000s on percentages
# handle missing percentages of Mexican immigrants for small values

str(unauth_immigrant_df)

unauth_immigrant_df$State <-stringr::str_trim(unauth_immigrant_df$State) 

# replace line breaks and returns in column headers
colnames(unauth_immigrant_df) <- stringr::str_replace_all(colnames(unauth_immigrant_df), "[\r\n]", "")

# set NAs instead of "-"
unauth_immigrant_df[unauth_immigrant_df == "-"] <- NA



# FOREIGN BORN POPULATION -------------------------------------------------
foreign_pop_df <- read_excel("./data/statelevel_foreignborn_22.xlsx", sheet = "Data-edited")
foreign_pop_df <- foreign_pop_df %>% 
                    column_to_rownames("...1") %>% 
                    t() %>% 
                    as.data.frame()

str(foreign_pop_df)


foreign_pop_df <- foreign_pop_df %>% mutate(across(everything(), convert_to_num))
colSums(is.na(foreign_pop_df))
str(foreign_pop_df)

# make col State
foreign_pop_df$State <- rownames(foreign_pop_df)
rownames(foreign_pop_df) <- NULL

# make it first column
foreign_pop_df <- foreign_pop_df %>% relocate(State)

# OTHER CHARACTERISTICS ---------------------------------------------------
selected_profiles_df <- read_excel("./data/selected-profiles.xlsx", sheet = "Data-edited")
selected_profiles_df <- selected_profiles_df %>%  column_to_rownames("...1") %>% 
                                                      t() %>% 
                                                      as.data.frame()

colnames(selected_profiles_df) <- stringr::str_trim(colnames(selected_profiles_df))
str(selected_profiles_df)

# make col State
selected_profiles_df$State <- rownames(selected_profiles_df)
rownames(selected_profiles_df) <- NULL

# make it first column
selected_profiles_df <- selected_profiles_df %>% relocate(State)

# convert percentage to absolute and then per 100k
# select cols needed
