#we can see the statistics of the main data after it has been cleaned
describe(EFA_data_filtered)


#then looking at the country-specific data

#we want to look at a couple of variables and see if there is a big difference between the countries
exclude_cols <- c("name", "essround", "edition", "proddate", "idno", 
                  "dweight", "pspwght", "pweight", "anweight")

cleaned_data <- ess_r6_clean %>%
  select(-all_of(exclude_cols)) %>%
  filter_data(., exclude_values)  #exclude non substantive answers

#we pick some variables to compare between the countries
key_vars <- c("accdng", "deaimpp", "flapppl", "rehlppl", "sedirlf")

#compute summary statistics (mean and sd) for key variables by country
descriptive_stats <- cleaned_data %>%
  select(c(cntry, all_of(key_vars))) %>%
  group_by(cntry) %>%
  summarise(across(everything(), 
                   list(mean = ~mean(., na.rm = TRUE), 
                        sd = ~sd(., na.rm = TRUE)), 
                   .names = "{col}_{fn}"))

#we look at the first few rows of the summary statistics
head(descriptive_stats)
#we can see these differ between countries



