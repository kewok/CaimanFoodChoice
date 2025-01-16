# @author: Kenichi W. Okamoto

require(mvProbit)

medem <- read.csv('records.csv')

# Interaction model
estResult <- mvProbit(cbind(Arthropods,Snails,Fish,Tetrapods) ~ Length + Subspecies + Length:Subspecies,data = medem, reltol=1e-16)

# Additive model
estResult_noInt <- mvProbit(cbind(Arthropods, Snails,Fish, Tetrapods)~Length + Subspecies, data = medem, reltol=1e-16)

# Summary statistics
aicRes <- AIC(estResult) + 2*nrow(summary(estResult)$estimate)*(nrow(summary(estResult)$estimate) + 1) / (nrow(medem) - nrow(summary(estResult)$estimate) - 1)
aic_noInt <- AIC(estResult_noInt) + 2*nrow(summary(estResult_noInt)$estimate)*(nrow(summary(estResult_noInt)$estimate) + 1) / (nrow(medem) - nrow(summary(estResult_noInt)$estimate) - 1)

# McFadden's pseudo R2
nullMod  <- mvProbit(cbind(Arthropods, Snails,Fish, Tetrapods)~1, data = medem, reltol=1e-16)

mcfadR2 <- 1-summary(estResult)$loglik/summary(nullMod)$loglik
mcfadR2_noInt <- 1-summary(estResult_noInt)$loglik/summary(nullMod)$loglik

# Calculate the expected results (y^hat):
expResult <- mvProbitExp( ~ Length + Subspecies + Length:Subspecies,data = medem, coef=summary(estResult)$estimate[,1])
expResult_noInt <- mvProbitExp( ~ Length + Subspecies,data = medem, coef=summary(estResult_noInt)$estimate[,1])
