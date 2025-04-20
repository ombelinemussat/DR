#finally we look at how many factors we need for each ocuntry (and run the EFA)

#we define a function to run the EFA process and return results for each country, to know how many factors to include
run_efa_for_country <- function(country_data) {
  EFA.cor <- cor(country_data, use = "pairwise.complete.obs")
  scree(EFA.cor, factors = FALSE)  
  vss_results <- vss(EFA.cor, rotate = "none")  
  return(vss_results)
}

#we again want to run the analysis separately for each country, to see their scree plot and VSS structure
country_names <- names(cleaned_country_data_list)
print(country_names)

# Albania
results_AL <- run_efa_for_country(cleaned_country_data_list$AL)
print(results_AL)
#optimal is 8


# Belgium
results_BE <- run_efa_for_country(cleaned_country_data_list$BE)
print(results_BE)
#optimal is 5

# Bulgaria
results_BG <- run_efa_for_country(cleaned_country_data_list$BG)
print(results_BG)
#optimal is 8

# Switzerland
results_CH <- run_efa_for_country(cleaned_country_data_list$CH)
print(results_CH)
#optimal is 6

# Cyprus
results_CY <- run_efa_for_country(cleaned_country_data_list$CY)
print(results_CY)
#optimal is 8

# Czech Republic
results_CZ <- run_efa_for_country(cleaned_country_data_list$CZ)
print(results_CZ)
#optimal is 8

# Germany
results_DE <- run_efa_for_country(cleaned_country_data_list$DE)
print(results_DE)
#optimal is 5

# Denmark
results_DK <- run_efa_for_country(cleaned_country_data_list$DK)
print(results_DK)
# optimal is 6

# Estonia
results_EE <- run_efa_for_country(cleaned_country_data_list$EE)
print(results_EE)
# optimal is 6

# Spain
results_ES <- run_efa_for_country(cleaned_country_data_list$ES)
print(results_ES)
#optimal is 7

# Finland
results_FI <- run_efa_for_country(cleaned_country_data_list$FI)
print(results_FI)
# optimal is 6

# France
results_FR <- run_efa_for_country(cleaned_country_data_list$FR)
print(results_FR)
# optimal is 5

# United Kingdom
results_GB <- run_efa_for_country(cleaned_country_data_list$GB)
print(results_GB)
# optimal is 7

# Hungary
results_HU <- run_efa_for_country(cleaned_country_data_list$HU)
print(results_HU)
# optimal is 7

# Ireland
results_IE <- run_efa_for_country(cleaned_country_data_list$IE)
print(results_IE)
# optimal is 8


# Israel
results_IL <- run_efa_for_country(cleaned_country_data_list$IL)
print(results_IL)
# optimal is 7

# Iceland
results_IS <- run_efa_for_country(cleaned_country_data_list$IS)
print(results_IS)
# optimal is 8

# Italy
results_IT <- run_efa_for_country(cleaned_country_data_list$IT)
print(results_IT)
# optimal is 8

# Lithuania
results_LT <- run_efa_for_country(cleaned_country_data_list$LT)
print(results_LT)
# optimal is 8

# Netherlands
results_NL <- run_efa_for_country(cleaned_country_data_list$NL)
print(results_NL)
# optimal is 6

# Norway
results_NO <- run_efa_for_country(cleaned_country_data_list$NO)
print(results_NO)
# optimal is 6

# Poland
results_PL <- run_efa_for_country(cleaned_country_data_list$PL)
print(results_PL)
# optimal is 8

# Portugal
results_PT <- run_efa_for_country(cleaned_country_data_list$PT)
print(results_PT)
# optimal is 7

# Russia
results_RU <- run_efa_for_country(cleaned_country_data_list$RU)
print(results_RU)
# optimal is 7

# Sweden
results_SE <- run_efa_for_country(cleaned_country_data_list$SE)
print(results_SE)
# optimal is 6

# Slovenia
results_SI <- run_efa_for_country(cleaned_country_data_list$SI)
print(results_SI)
# optimal is 8

# Slovakia
results_SK <- run_efa_for_country(cleaned_country_data_list$SK)
print(results_SK)
# optimal is 8

# Ukraine
results_UA <- run_efa_for_country(cleaned_country_data_list$UA)
print(results_UA)
# optimal is 7

# Kosovo
results_XK <- run_efa_for_country(cleaned_country_data_list$XK)
print(results_XK)
# optimal is 8


#we can run the EFA to see the loading of the variables for countries that have different number of factors
#for example we can run for Germany (optimal is 5) and Ireland (optimal is 8)

#Germany - 5 factors
EFA_DE_5 <- fa(cleaned_country_data_list$DE, nfactors = 5)
fa.diagram(EFA_DE_5)

#we look at the uniqueness, the communality, the loadings and the BIC value
EFA_DE_5$uniquenesses
apply(EFA_DE_5$loadings^2, 1, sum)
EFA_DE_5$loadings
EFA_DE_5$BIC

#we can evaluate the model too
#chronbach's alpha
cronbach_alpha <- psych::alpha(cleaned_country_data_list$DE, check.keys=TRUE)
print(cronbach_alpha)
#split half reliability
splitHalf(cleaned_country_data_list$DE)
#Hoffman’s index of complexity
hoffman_complexity <- EFA_DE_5$complexity
print(hoffman_complexity)

#we run the EFA for Ireland too (8 factors)
EFA_IE_8 <- fa(cleaned_country_data_list$IE, nfactors = 8)
fa.diagram(EFA_IE_8)

#we look at the uniqueness, the communality, the loadings and the BIC value
EFA_IE_8$uniquenesses
EFA_IE_8$loadings
apply(EFA_IE_8$loadings^2, 1, sum)
EFA_IE_8$BIC

#we can evaluate that model as well
#chronbach's alpha
cronbach_alpha_IE <- psych::alpha(cleaned_country_data_list$IE, check.keys=TRUE)
print(cronbach_alpha_IE)

# Split-half reliability
splitHalf(cleaned_country_data_list$IE)

#Hoffman’s index of complexity
hoffman_complexity <- EFA_IE_8$complexity
print(hoffman_complexity)
