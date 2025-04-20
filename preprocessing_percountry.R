#We want to see if we have a different structure in the factors for the different countries:

#we want to separate the data by country to run separate EFA for each 
country_data_list <- split(ess_r6_clean, ess_r6_clean$cntry)

#we define the columns we want to take out of the analysis (unrelated to well-being)
exclude_cols <- c("name", "essround", "edition", "proddate", "idno", 
                  "dweight", "pspwght", "pweight", "anweight", "cntry")

#we create a list to store the cleaned datasets per country
cleaned_country_data_list <- lapply(country_data_list, function(df) {
  df_clean <- df %>%
    select(-all_of(exclude_cols)) 
    df_filtered <- filter_data(df_clean, exclude_values)
  return(df_filtered)
})

#also want to make sure it is only numeric
str(cleaned_country_data_list)


#we can check the dimensions of the country dataset
country_dimensions <- sapply(cleaned_country_data_list, dim)
country_dimensions

#some countries don't have a lot of observations so we might want to be a bit more careful 
#example of Italy, Albania...

#but overall countries have enough observations and still have 32 columns for each 


#now we can also check the correlations between the variables for each country
get_country_correlations <- function(country_data) {
  corr_matrix <- cor(country_data, use = "pairwise.complete.obs")  
  corr_test <- corr.test(country_data, use = "pairwise.complete.obs")  
  corr_pvals <- corr_test$p  
  ci_matrix <- corr_test$ci  
  include_zero <- apply(ci_matrix, 1, function(ci) ci[1] <= 0 & ci[2] >= 0)
  ci_zero_names <- rownames(ci_matrix)[include_zero]
  return(list(
    corr_matrix = corr_matrix,
    pvals = corr_pvals,
    ci_zero_pairs = ci_zero_names
  ))
}


#we apply the correlation function to each country's data
country_correlations <- lapply(cleaned_country_data_list, get_country_correlations)

#we print out the names of the countries to run through them the function
country_names <- names(cleaned_country_data_list)
print(country_names)

#we can run the function for each country
al_corr <- country_correlations[["AL"]]
al_corr
#assumptions don't hold very well for Albania 
#with a lot of p value above the threshold and a high number of pairs of variables with a confidence interval with 0

be_corr <- country_correlations[["BE"]]
be_corr

bg_corr <- country_correlations[["BG"]]
bg_corr

ch_corr <- country_correlations[["CH"]]
ch_corr

cy_corr <- country_correlations[["CY"]]
cy_corr
#assumptions don't hold very well for Cyprus 
#with a lot of p value above the threshold and a high number of pairs of variables with a confidence interval with 0


cz_corr <- country_correlations[["CZ"]]
cz_corr

de_corr <- country_correlations[["DE"]]
de_corr

dk_corr <- country_correlations[["DK"]]
dk_corr

ee_corr <- country_correlations[["EE"]]
ee_corr

es_corr <- country_correlations[["ES"]]
es_corr

fi_corr <- country_correlations[["FI"]]
fi_corr

fr_corr <- country_correlations[["FR"]]
fr_corr

gb_corr <- country_correlations[["GB"]]
gb_corr

hu_corr <- country_correlations[["HU"]]
hu_corr

ie_corr <- country_correlations[["IE"]]
ie_corr

il_corr <- country_correlations[["IL"]]
il_corr

is_corr <- country_correlations[["IS"]]
is_corr
#assumptions don't hold very well for Israel either 
#with a lot of p value above the threshold and a high number of pairs of variables with a confidence interval with 0


it_corr <- country_correlations[["IT"]]
it_corr
#assumptions don't hold very well for Italy 
#with a lot of p value above the threshold and a high number of pairs of variables with a confidence interval with 0

lt_corr <- country_correlations[["LT"]]
lt_corr

nl_corr <- country_correlations[["NL"]]
nl_corr

no_corr <- country_correlations[["NO"]]
no_corr

pl_corr <- country_correlations[["PL"]]
pl_corr

pt_corr <- country_correlations[["PT"]]
pt_corr
#assumptions don't hold very well for Portugal 
#with a lot of p value above the threshold and a high number of pairs of variables with a confidence interval with 0

ru_corr <- country_correlations[["RU"]]
ru_corr
##assumptions don't hold very well for Russia 
#with a lot of p value above the threshold and a high number of pairs of variables with a confidence interval with 0


se_corr <- country_correlations[["SE"]]
se_corr

si_corr <- country_correlations[["SI"]]
si_corr
#assumptions don't hold very well for Slovenia 
#with a lot of p value above the threshold and a high number of pairs of variables with a confidence interval with 0

sk_corr <- country_correlations[["SK"]]
sk_corr

ua_corr <- country_correlations[["UA"]]
ua_corr
#assumptions don't hold very well for Ukraine 
#with a lot of p value above the threshold and a high number of pairs of variables with a confidence interval with 0

xk_corr <- country_correlations[["XK"]]
xk_corr
##assumptions don't hold very well for Kosovo 
#with a lot of p value above the threshold and a high number of pairs of variables with a confidence interval with 0

#overall we have a couple of countries where the assumptions don't hold very well
