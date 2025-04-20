#then we can run the EFA on our full dataset (round 6 ESS)

#first we need to define the number of factors to include using the scree plot and the VSS

EFA.cor <- cor(EFA_data_filtered, use = "pairwise.complete.obs")
#create the Scree plot
scree(EFA.cor, factors = FALSE)
#or use Very Simple Structure (VSS) 
vss(EFA.cor, rotate = "none")

#based on parsimony and theoretical relevance, we might want 2 factors, it aligns with Hedonic vs. Eudaimonic well-being
#best overall fit: 6 factors
#Velicer MAP: 4 factors 

#we can try each of these and see which one is better 

#run the EFA with 2 factors
EFA.mod.2 <- fa(EFA_data_filtered, nfactors = 2)
EFA.mod.2
fa.diagram(EFA.mod.2)


#run the EFA with 4 factors
EFA.mod.4 <- fa(EFA_data_filtered, nfactors = 4)
EFA.mod.4
fa.diagram(EFA.mod.4)

#run the EFA with 7 factors
EFA.mod.6 <- fa(EFA_data_filtered, nfactors = 6)
EFA.mod.6
fa.diagram(EFA.mod.6)

#the we can look at the results:
###uniqueness
EFA.mod.2$uniquenesses
EFA.mod.4$uniquenesses
EFA.mod.6$uniquenesses


#####Communality
apply(EFA.mod.2$loadings^2, 1, sum)
apply(EFA.mod.4$loadings^2, 1, sum)
apply(EFA.mod.6$loadings^2, 1, sum)


####loadings
EFA.mod.2$loadings
EFA.mod.4$loadings
EFA.mod.6$loadings

###BIC values
EFA.mod.2$BIC
EFA.mod.4$BIC
EFA.mod.6$BIC

#model 6 is the best based on everything 


#to check the validity and reliability of items

#Validity relates to the extent to which an item is measuring what it (theoretically) is supposed to be measuring
#reliability: external and internal

#we use Chronbach's alpha to check internal reliability 
cronbach_alpha <- psych::alpha(EFA_data_filtered, check.keys=TRUE)
print(cronbach_alpha)

#then the split-half reliability
splitHalf(EFA_data_filtered)

#finally Hoffman’s index of complexity
hoffman_complexity <- EFA.mod.6$complexity
print(hoffman_complexity)

