require(mvProbit)
library(dplyr)

medem <- read.csv('medem_simpler.csv')

invs <- ifelse(medem[,'Snails'] + medem[,'Arthropods']==2,1,medem[,'Snails'] + medem[,'Arthropods'])

# Essentially, w. of the Cordillera Occidental, e. of the Cordillera Occidental, the aporis river, and others 

Region <- ifelse(medem[,'Location']=='Acandi Bajo Atrato Baudo','West of West Andes',
        ifelse(medem[,'Location']=='Cienaga Grande, Cano Betanci, Tolu','West of East Andes',
        ifelse(medem[,'Location']=='Medio apaporis','Apaporis River',' cis-Andes'))) # Add a space in front of "Other" to ensure it is sorted to become the default factor

# Convert location names, merge chiapasius and fuscus (west of cordilleras, or WoC), and finally remove apaporiensis
PurportedSubspecies <- ifelse(Region %in% c('West of West Andes','West of East Andes'),'C. c. fuscus',ifelse(Region=='Apaporis River','C. c. apaporiensis','C. c.  crocodilus')) # Similarly, add a space in front of "C. c.  crocodilus" to ensure it is sorted to become the default factor level
Lineage <- ifelse(PurportedSubspecies=='C. c. apaporiensis','C. c.  crocodilus',PurportedSubspecies)

medem <- cbind.data.frame(medem,'Invertebrates'=invs,'Region'=as.factor(Region),'PurportedSubspecies' = as.factor(PurportedSubspecies), 'Lineage' = as.factor(Lineage))
medem <- medem[,-which(colnames(medem) %in% c('Lat','Long','Arthropods','Snails','Subspecies','Location'))]

# Reorganize the columns a bit
medem <- medem[,c('id', 'Length', 'Invertebrates','Fish','Tetrapods', 'Region', 'PurportedSubspecies', 'Lineage')]
# Reorder rows by Region so that 'cis-Andes' is first, followed by 'apaporis', then w. of the western Andes, and finally west of the Eastern Andes
target <- c(' cis-Andes','Apaporis River','West of West Andes','West of East Andes')
medem <- medem %>% arrange(factor(Region,levels=target))

# Store the csv file for later use
write.csv(medem, 'medem_revision.csv',row.names=F)

# General case, without geographic coordinates
estResult_Lineage <- mvProbit(cbind(Invertebrates,Fish,Tetrapods) ~ Length + Lineage + Length:Lineage,data = medem, reltol=1e-16)
estResult_PurportedSubspecies <- mvProbit(cbind(Invertebrates,Fish,Tetrapods) ~ Length + PurportedSubspecies + Length:PurportedSubspecies,data = medem, reltol=1e-16)
estResult_Region <- mvProbit(cbind(Invertebrates,Fish,Tetrapods) ~ Length + Region + Length:Region,data = medem, reltol=1e-16)
# The additive model
estResult_noInt_Lineage <- mvProbit(cbind(Invertebrates,Fish,Tetrapods) ~ Length + Lineage,data = medem, reltol=1e-16)
estResult_noInt_PurportedSubspecies <- mvProbit(cbind(Invertebrates,Fish,Tetrapods) ~ Length + PurportedSubspecies,data = medem, reltol=1e-16)
estResult_noInt_Region <- mvProbit(cbind(Invertebrates,Fish,Tetrapods) ~ Length + Region,data = medem, reltol=1e-16)

# To confirm the order of coefficients, use the code for generating xMat from the mvProbit source code: https://github.com/cran/mvProbit/blob/383e309af6971232b656db033b48a4398db26491/R/mvProbit.R#L38C1-L49C1
form <- cbind(medem[,'Invertebrates'],medem[,'Fish'],medem[,'Tetrapods']) ~ medem[,'Length'] + medem[,'PurportedSubspecies'] + medem[,'Length']:medem[,'PurportedSubspecies']
# preparing model matrix
mc <- match.call( expand.dots = FALSE )
m <- match( "data", names( mc ), 0 )
mf <- mc[ c( 1, m ) ]
mf$formula <- form
attributes( mf$formula ) <- NULL
mf$na.action <- na.pass
mf[[ 1 ]] <- as.name( "model.frame" )
mf <- eval( mf, parent.frame() )
mt <- attr( mf, "terms" )
xMat <- model.matrix( mt, mf )
colnames(xMat)


# Summary statistics
aicRes_Region <- AIC(estResult_Region) + 2*nrow(summary(estResult_Region)$estimate)*(nrow(summary(estResult_Region)$estimate) + 1) / (nrow(medem) - nrow(summary(estResult_Region)$estimate) - 1)

aicRes_PurportedSubspecies <- AIC(estResult_PurportedSubspecies) + 2*nrow(summary(estResult_PurportedSubspecies)$estimate)*(nrow(summary(estResult_PurportedSubspecies)$estimate) + 1) / (nrow(medem) - nrow(summary(estResult_PurportedSubspecies)$estimate) - 1)

aicRes_Lineage <- AIC(estResult_Lineage) + 2*nrow(summary(estResult_Lineage)$estimate)*(nrow(summary(estResult_Lineage)$estimate) + 1) / (nrow(medem) - nrow(summary(estResult_Lineage)$estimate) - 1)

aicRes_noInt_Region <- AIC(estResult_noInt_Region) + 2*nrow(summary(estResult_noInt_Region)$estimate)*(nrow(summary(estResult_noInt_Region)$estimate) + 1) / (nrow(medem) - nrow(summary(estResult_noInt_Region)$estimate) - 1)

aicRes_noInt_PurportedSubspecies <- AIC(estResult_noInt_PurportedSubspecies) + 2*nrow(summary(estResult_noInt_PurportedSubspecies)$estimate)*(nrow(summary(estResult_noInt_PurportedSubspecies)$estimate) + 1) / (nrow(medem) - nrow(summary(estResult_noInt_PurportedSubspecies)$estimate) - 1)

aicRes_noInt_Lineage <- AIC(estResult_noInt_Lineage) + 2*nrow(summary(estResult_noInt_Lineage)$estimate)*(nrow(summary(estResult_noInt_Lineage)$estimate) + 1) / (nrow(medem) - nrow(summary(estResult_noInt_Lineage)$estimate) - 1)

# McFadden's pseudo R2
nullMod  <- mvProbit(cbind(Invertebrates,Fish, Tetrapods)~1, data = medem, reltol=1e-16)

mcfadR2_Region <- 1-summary(estResult_Region)$loglik/summary(nullMod)$loglik
mcfadR2_PurportedSubspecies <- 1-summary(estResult_PurportedSubspecies)$loglik/summary(nullMod)$loglik
mcfadR2_Lineage <- 1-summary(estResult_Lineage)$loglik/summary(nullMod)$loglik

mcfadR2_noInt_Region <- 1-summary(estResult_noInt_Region)$loglik/summary(nullMod)$loglik
mcfadR2_noInt_PurportedSubspecies <- 1-summary(estResult_noInt_PurportedSubspecies)$loglik/summary(nullMod)$loglik
mcfadR2_noInt_Lineage <- 1-summary(estResult_noInt_Lineage)$loglik/summary(nullMod)$loglik

# Calculate the expected results (y^hat):
expResult_Lineage <- mvProbitExp( ~ Length + Lineage + Length:Lineage,data = medem, coef=summary(estResult_Lineage)$estimate[,1])
expResult_noInt_Lineage <- mvProbitExp( ~ Length + Lineage,data = medem, coef=summary(estResult_noInt_Lineage)$estimate[,1])
expResult_PurportedSubspecies <- mvProbitExp( ~ Length + PurportedSubspecies + Length:PurportedSubspecies,data = medem, coef=summary(estResult_PurportedSubspecies)$estimate[,1])
expResult_noInt_PurportedSubspecies <- mvProbitExp( ~ Length + PurportedSubspecies,data = medem, coef=summary(estResult_noInt_PurportedSubspecies)$estimate[,1])
expResult_Region <- mvProbitExp( ~ Length + Region + Length:Region,data = medem, coef=summary(estResult_Region)$estimate[,1])
expResult_noInt_Region <- mvProbitExp( ~ Length + Region,data = medem, coef=summary(estResult_noInt_Region)$estimate[,1])

# per the documentation: https://search.r-project.org/CRAN/refmans/mvProbit/html/mvProbit.html
# b i,j​  is the coefficient of the jth explanatory variable in the ith equation. 
# R i,j  is the correlation coefficient corresponding to the ith and jth equation.

write.csv(format(round(summary(estResult_Lineage)$estimate,digits=3), scientific=F), file='interactive_mvProbit_Lineage.csv', row.names=c("Invertebrates, Intercept","Invertebrates, Length","Invertebrates, C. c. fuscus","Invertebrates, Length x C. c. fuscus",
 "Fish, Intercept","Fish, Length","Fish, C. c. fuscus","Fish, Length x C. c. fuscus",
"Tetrapods, Intercept","Tetrapods, Length","Tetrapods, C. c. fuscus","Tetrapods, Length x C. c. fuscus",
 "Invertebrates, Fish","Invertebrates, Tetrapods","Fish, Tetrapods"))

 write.csv(format(round(summary(estResult_PurportedSubspecies)$estimate,digits=3), scientific=F), file='interactive_mvProbit_PurportedSubspecies.csv', row.names=c("Invertebrates, Intercept","Invertebrates, Length","Invertebrates, C. c. apaporiensis","Invertebrates, C. c. fuscus","Invertebrates, Length x C. c. apaporiensis","Invertebrates, Length x  C. c. fuscus",
 "Fish, Intercept","Fish, Length","Fish, C. c. apaporiensis","Fish, C. c. fuscus","Fish, Length x C. c. apaporiensis","Fish, Length x C. c. fuscus",
 "Tetrapods, Intercept","Tetrapods, Length","Tetrapods, C. c. apaporiensis","Tetrapods, C. c. fuscus","Tetrapods, Length x C. c. apaporiensis","Tetrapods, Length x C. c. fuscus",
 "Invertebrates, Fish","Invertebrates, Tetrapods","Fish, Tetrapods"))

 write.csv(format(round(summary(estResult_Region)$estimate,digits=3), scientific=F), file='interactive_mvProbit_Region.csv', row.names=c("Invertebrates, Intercept","Invertebrates, Length","Invertebrates, Apaporis River","Invertebrates, West of West Andes","Invertebrates, West of East Andes","Invertebrates, Length x Apaporis River","Invertebrates, Length x West of West Andes","Invertebrates, Length x West of East Andes",
"Fish, Intercept","Fish, Length","Fish, Apaporis River","Fish, West of West Andes","Fish, West of East Andes","Fish, Length x Apaporis River","Fish, Length x West of West Andes","Fish, Length x West of East Andes",
  "Tetrapods, Intercept","Tetrapods, Length","Tetrapods, Apaporis River","Tetrapods, West of West Andes","Tetrapods, West of East Andes","Tetrapods, Length x Apaporis River","Tetrapods, Length x West of West Andes","Tetrapods, Length x West of East Andes",
  "Invertebrates, Fish","Invertebrates, Tetrapods","Fish, Tetrapods"))

write.csv(format(round(summary(estResult_noInt_Lineage)$estimate,digits=3), scientific=F), file='additive_mvProbit_Lineage.csv', row.names=c("Invertebrates, Intercept","Invertebrates, Length","Invertebrates, C. c. fuscus",
 "Fish, Intercept","Fish, Length","Fish, C. c. fuscus",
"Tetrapods, Intercept","Tetrapods, Length","Tetrapods, C. c. fuscus",
 "Invertebrates, Fish","Invertebrates, Tetrapods","Fish, Tetrapods"))

write.csv(format(round(summary(estResult_noInt_PurportedSubspecies)$estimate,digits=3), scientific=F), file='additive_mvProbit_PurportedSubspecies.csv', row.names=c("Invertebrates, Intercept","Invertebrates, Length","Invertebrates, C. c. apaporiensis","Invertebrates, C. c. fuscus",
 "Fish, Intercept","Fish, Length","Fish, C. c. apaporiensis","Fish, C. c. fuscus",
 "Tetrapods, Intercept","Tetrapods, Length","Tetrapods, C. c. apaporiensis","Tetrapods, C. c. fuscus",
 "Invertebrates, Fish","Invertebrates, Tetrapods","Fish, Tetrapods"))

write.csv(format(round(summary(estResult_noInt_Region)$estimate,digits=3), scientific=F), file='additive_mvProbit_Region.csv', row.names=c("Invertebrates, Intercept","Invertebrates, Length","Invertebrates, Apaporis River","Invertebrates, West of West Andes","Invertebrates, West of East Andes",
 "Fish, Intercept","Fish, Length","Fish, Apaporis River","Fish, West of West Andes","Fish, West of East Andes",
  "Tetrapods, Intercept","Tetrapods, Length","Tetrapods, Apaporis River","Tetrapods, West of West Andes","Tetrapods, West of East Andes",
  "Invertebrates, Fish","Invertebrates, Tetrapods","Fish, Tetrapods"))

write.csv(expResult_Lineage, file='fitted_interactive_mvProbit_Lineage.csv', row.names=FALSE)
write.csv(expResult_PurportedSubspecies, file='fitted_interactive_mvProbit_PurportedSubspecies.csv', row.names=FALSE)
write.csv(expResult_Region, file='fitted_interactive_mvProbit_Region.csv', row.names=FALSE)

write.csv(expResult_noInt_Lineage, file='fitted_additive_mvProbit_Lineage.csv', row.names=FALSE)
write.csv(expResult_noInt_PurportedSubspecies, file='fitted_additive_mvProbit_PurportedSubspecies.csv', row.names=FALSE)
write.csv(expResult_noInt_Region, file='fitted_additive_mvProbit_Region.csv', row.names=FALSE)
