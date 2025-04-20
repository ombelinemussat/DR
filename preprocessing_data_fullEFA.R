#we first load the libraries we will need
library(tidyverse)
library(psych)
library(GPArotation)
library(dplyr)
library(gridExtra)


#We download the data and have a first look at it
ess <- read.csv("/Users/ombelinemussat/Documents/TCD/Dimensionality Reduction/Assignment /data/ESS Theme - Personal and social wellbeing (1)/ESSdata.csv") 
dim(ess)
describe(ess)

#we then can already subset our round of data we want (round 6)
ess_r6 <- subset(ess, essround == 6)
head(ess_r6)
dim(ess_r6)

#we can see that some columns just have missing values (100% NA)
all_missing_cols <- colnames(ess_r6)[colSums(is.na(ess_r6)) == nrow(ess_r6)]

#some other columns are full of values (no missing values), they were the variables only available in round 3
full_cols_r6 <- colnames(ess_r6)[colSums(is.na(ess_r6)) == 0]

#we can see that they are completely separate so we can take the ones with the missing values out
intersect(all_missing_cols, full_cols_r6)
#it returns `character(0)` which means they are completely separate

#we can look at the length of our full columns
length(full_cols_r6)
#we only have 42 columns without missing values

#we clean the dataset to only have the variables from round 6
ess_r6_clean <- ess_r6[, full_cols_r6]
head(ess_r6_clean)

#then we need to exclude the columns which do not describe any well being item 
exclude_cols <- c("name", "essround", "edition", "proddate", "idno", 
                  "dweight", "pspwght", "pweight", "anweight", "cntry")

#we subset the dataset to exclude the specified columns
EFA_data <- ess_r6_clean[, !(colnames(ess_r6_clean) %in% exclude_cols)]

head(EFA_data)

dim(EFA_data)
#we still have 32 variables

#we want to see what are the unique values for each variable:
colnames(EFA_data)
lapply(EFA_data, unique)



#we create a list of the values we want to exclude (such as "Don't know", "No answer"...) so they don't bias the results 
exclude_values <- list(
  accdng = c(7, 8, 9),
  dclvlf = c(7, 8, 9),
  deaimpp = c(77, 88, 99),
  dngval = c(7, 8, 9),
  enrglot = c(7, 8, 9),
  flapppl = c(77, 88, 99),
  flclpla = c(7, 8, 9),
  flrms = c(7, 8, 9),
  fltanx = c(7, 8, 9),
  fltpcfl = c(7, 8, 9),
  lchshcp = c(7, 8, 9),
  lfwrs = c(7, 8, 9),
  lotsgot = c(7, 8, 9),
  lrnntlf = c(7, 8, 9),
  nhpftr = c(7, 8, 9),
  optftr = c(7, 8, 9),
  physact = c(77, 88, 99),
  plinsoc = c(77, 88, 99),
  pplahlp = c(7, 8, 9),
  prhlppl = c(7, 8, 9),
  pstvms = c(7, 8, 9),
  rehlppl = c(7, 8, 9),
  sedirlf = c(77, 88, 99),
  stfjb = c(66,77, 88, 99),
  tmabdng = c(77, 88, 99),
  tmdotwa = c(77, 88, 99),
  tmendng = c(77, 88, 99),
  tmimdng = c(77, 88, 99),
  tnapsur = c(77, 88, 99),
  trtrsp = c(7, 8, 9),
  wkvlorg = c(7, 8, 9),
  wrbknrm = c(7, 8, 9)
)

#create a function to filter them out
filter_data <- function(df, exclude_values) {
  for (var in names(exclude_values)) {
    df <- df %>% filter(!get(var) %in% exclude_values[[var]])
  }
  return(df)
}

#and apply the filter to EFA_data
EFA_data_filtered <- filter_data(EFA_data, exclude_values)

#we can see that our data is much smaller (less than half of what it was but still sufficient to run the analysis), this could be a limitation 
dim(EFA_data_filtered)
head(EFA_data_filtered)


#we need to make sure our data is numeric
str(EFA_data_filtered)
#we only have numeric variables



#the following lines of code are pre-EFA diagnostics to make sure your variables are suitable for factor analysis:
#we want to see if items correlate well enough to justify factor extraction
#so that there's shared variance
#scale isn't just noise
lowerCor(EFA_data_filtered)
#we can see that there is shared variance, items are not independent 

##### Testing Correlations' significance: p-values
corr.test(EFA_data_filtered, use = "pairwise.complete.obs")$p
#most p vaues show statistical significance
#but we have physact - deaimpp, physact - fltanx, flclpla- nhpftr, lrnntlf - physact, optftr - physact, stfjb - physact
#those are not statistically significant 'p value below 0.05)

#the following lines of code are to create a plot to add to the final report
#with the correlation matrix and the significance levels 
cor_results <- corr.test(EFA_data_filtered)
cor_matrix <- round(cor_results$r, 2)
p_matrix <- cor_results$p
stars <- ifelse(p_matrix < 0.001, "***",
                ifelse(p_matrix < 0.01, "**",
                       ifelse(p_matrix < 0.05, "*", "")))
cor_with_stars <- paste0(format(cor_matrix, nsmall = 2), stars)
cor_with_stars_matrix <- matrix(cor_with_stars, 
                                nrow = nrow(cor_matrix), 
                                dimnames = dimnames(cor_matrix))
png("correlation_with_significance.png", width = 2000, height = 2000)
grid.table(cor_with_stars_matrix)
dev.off()



##### Confidence Intervals
#we don't want a confidence interval below 0
#we want to see the pairs that have a ci that includes 0 
cor_test_result <- corr.test(EFA_data_filtered, use = "pairwise.complete.obs")
ci_matrix <- cor_test_result$ci
str(cor_test_result$ci)
include_zero <- apply(ci_matrix, 1, function(ci) ci[1] <= 0 & ci[2] >= 0)
which(include_zero)

#fltnx-physc, lfwrs-ltsgt,lrnnt-physc, physc-stfjb, physc-wkvlr include 0: not statistically significant

#physact seems to be an outlier --> we might want to take it out of the analysis 

