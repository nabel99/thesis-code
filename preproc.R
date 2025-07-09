library(tidyverse)
library(readxl)
library(stringr)

# HELPER FUNCTIONS --------------------------------------------------------

convert_to_num <- function(x){
  return(as.numeric(gsub(",", "", x)))
}

# convert percentage to num
convert_from_perc <- function(x){
  return(as.numeric(gsub("%", "", x))/100)
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

# trim leading and ending whitespace
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

str(unauth_immigrant_df)

unauth_immigrant_df$State <-stringr::str_trim(unauth_immigrant_df$State) 

# replace line breaks and returns in column headers
colnames(unauth_immigrant_df) <- stringr::str_replace_all(colnames(unauth_immigrant_df), "[\r\n]", "")

# set NAs instead of "-"
unauth_immigrant_df[unauth_immigrant_df == "-"] <- NA

# NAs handled later
colSums(is.na(unauth_immigrant_df))

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

colSums(is.na(foreign_pop_df))

# OTHER CHARACTERISTICS ---------------------------------------------------
selected_profiles_df_full <- read_excel("./data/selected-profiles.xlsx", sheet = "Data-edited")
selected_profiles_df_full <- selected_profiles_df_full %>%  column_to_rownames("...1") %>% 
                                                      t() %>% 
                                                      as.data.frame()

colnames(selected_profiles_df_full) <- stringr::str_trim(colnames(selected_profiles_df_full))
str(selected_profiles_df_full)

# make col State
selected_profiles_df_full$State <- rownames(selected_profiles_df_full)
rownames(selected_profiles_df_full) <- NULL

# make it first column
selected_profiles_df_full <- selected_profiles_df_full %>% relocate(State)

# convert percentage to absolute and then per 100k
# select cols needed
cols_needed <- c("State", "Total population", "Male", "Female", "18 to 24 years", "Average household size", 
                 "Average family size", "High school graduate or higher", 
                 "Unemployment Rate", "Median household income (dollars)", "Per capita income (dollars)",
                 "No health insurance coverage", "All people", "Median gross rent (dollars)")

# All people -> poverty rate


selected_profiles_df <- selected_profiles_df_full %>% select(all_of(cols_needed))

selected_profiles_df <- selected_profiles_df %>% rename(`Poverty rate` = `All people`)
str(selected_profiles_df)

selected_profiles_df <- selected_profiles_df %>% mutate(across(c("Total population", "Average household size",
                                                                 "Average family size",
                                                                 "Median household income (dollars)",
                                                                 "Per capita income (dollars)",
                                                                 "Median gross rent (dollars)"), 
                                                               convert_to_num))

selected_profiles_df <- selected_profiles_df %>% mutate(across(c("Male", "Female", "18 to 24 years", 
                                                                 "High school graduate or higher",
                                                                 "Unemployment Rate", "No health insurance coverage",
                                                                 "Poverty rate"), 
                                                               convert_from_perc))  

str(selected_profiles_df)
colSums(is.na(selected_profiles_df))


# JOINING -----------------------------------------------------------------


# join selected_profiles with unauth_immigrant
# need to fix values which are like <5000

unauth_immigrant_df_join <- unauth_immigrant_df %>% select(c("State", "Unauthorizedimmigrantpopulation",
                                                        "Unauthorizedimmigrant% ofpopulation", "Mexican % ofunauthorizedimmigrants"))

unauth_immigrant_df_join <- unauth_immigrant_df_join %>% rename(`Total unauthorized` = `Unauthorizedimmigrantpopulation`,
                                                      `Percentage of population unauthorized` = `Unauthorizedimmigrant% ofpopulation`,
                                                      `Mexican percentage of unauthorized` = `Mexican % ofunauthorizedimmigrants`)
str(unauth_immigrant_df_join)

unauth_immigrant_df_join <- unauth_immigrant_df_join %>% 
  mutate(`Total unauthorized` = ifelse(`Total unauthorized` == "<5,000", NA, `Total unauthorized`),
                `Total unauthorized` = convert_to_num(`Total unauthorized`))

# check joining key - one way check sufficient because of left join
setdiff(selected_profiles_df$State, unauth_immigrant_df_join$State)



df_joined <- left_join(selected_profiles_df, unauth_immigrant_df_join, by = "State")

# if total unauth NA, compute percentage based on total pop
df_joined$`Total unauthorized` <- ifelse(is.na(df_joined$`Total unauthorized`), 
                                         round(df_joined$`Total population` * df_joined$`Percentage of population unauthorized`),
                                         df_joined$`Total unauthorized`)

# still missings in mexican unauthorized
# make it per100k
df_joined$unauthorized_total_100k <- df_joined$`Total unauthorized` / df_joined$`Total population` * 100000
df_joined <- df_joined %>% select(-c("Total unauthorized"))

# rename columns
str(df_joined)
df_joined <- df_joined %>%
  rename(
    State = State,
    total_pop = `Total population`,
    male = Male,
    female = Female,
    age_18_24 = `18 to 24 years`,
    avg_household_size = `Average household size`,
    avg_family_size = `Average family size`,
    hs_grad_plus = `High school graduate or higher`,
    unemployment_rate = `Unemployment Rate`,
    median_household_income = `Median household income (dollars)`,
    per_capita_income = `Per capita income (dollars)`,
    uninsured = `No health insurance coverage`,
    poverty_rate = `Poverty rate`,
    median_gross_rent = `Median gross rent (dollars)`,
    unauthorized_mexican_pct = `Mexican percentage of unauthorized`,
    unauthorized_total_pct = `Percentage of population unauthorized`
  )

# joining foreign born
foreign_pop_df_join <- foreign_pop_df %>% select("State", "Total:", "Americas:", "Mexico")

# rename and make it per100k
foreign_pop_df_join <- foreign_pop_df_join %>% rename(foreignborn_total = `Total:`,
                                                      foreignborn_Americas_100k = `Americas:`,
                                                      foreignborn_Mexico_100k = Mexico)

str(foreign_pop_df_join)
setdiff(selected_profiles_df$State, foreign_pop_df_join$State)

df_joined <- left_join(df_joined, foreign_pop_df_join, by = "State")

df_joined$foreignborn_total_100k <- df_joined$foreignborn_total / df_joined$total_pop * 100000
df_joined$foreignborn_Americas_100k <- df_joined$foreignborn_Americas_100k / df_joined$total_pop * 100000
df_joined$foreignborn_Mexico_100k <- df_joined$foreignborn_Mexico_100k / df_joined$total_pop * 100000

df_joined <- df_joined %>% select(-c("foreignborn_total"))
# joining violent_crimes
str(violent_crimes_df)
violent_crimes_df_join <- violent_crimes_df %>% select(c("State", "violent_crime", "homicide", "rape_revised"))

setdiff(selected_profiles_df$State, violent_crimes_df_join$State)

df_joined <- left_join(df_joined, violent_crimes_df_join, by = "State")
df_joined$violent_crime_100k <- df_joined$violent_crime / df_joined$total_pop * 100000
df_joined$homicide_100k <- df_joined$homicide / df_joined$total_pop * 100000
df_joined$rape_100k <- df_joined$rape_revised / df_joined$total_pop * 100000

df_joined <- df_joined %>% select(-c("violent_crime", "homicide", "rape_revised"))

# joining drug_crimes
drug_crimes_df_join <- drug_crimes_df %>% select(c("State", "Drug/Narcotic Offenses"))
drug_crimes_df_join <- drug_crimes_df_join %>% rename(drug_offenses = `Drug/Narcotic Offenses`)

setdiff(selected_profiles_df$State, drug_crimes_df_join$State)
df_joined <- left_join(df_joined, drug_crimes_df_join, by = "State")

# convert to per 100k
df_joined$drug_offenses_100k <- df_joined$drug_offenses / df_joined$total_pop * 100000
df_joined <- df_joined %>% select(-c("drug_offenses"))


# CHECK JOINED
str(df_joined)
colSums(is.na(df_joined))

# changed scope due to missings in Mexican unauthorized population, see reasoning in Chapter 2.4
df_joined <- df_joined %>% select(-c("unauthorized_mexican_pct", "foreignborn_Americas_100k", "foreignborn_Mexico_100k")) 

# pooling of outcome variables, see Chapter 2.4
df_joined <- df_joined %>% select(-c("homicide_100k", "rape_100k"))


# write csv into new file
#write.csv(df_joined, "data/df_joined.csv", row.names = FALSE)
