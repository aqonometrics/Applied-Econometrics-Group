# If have not install readxl
install.packages("readxl")

# Include the libraries needed for the project
library(readxl) # to include data from excel
library(stargazer) # to present finding

# Main Model # 
mdl2<-lm(lngca~lngp+lntr,data=dt2)
summary(mdl2)

mdl2_robust<-lm_robust(lngca~lngp+lntr,data=dt2,se_type="HC3")
summary(mdl2_robust)
linearHypothesis(mdl2_robust,c("lngp=0","lntr=0"),test="F")
# Our model shows that both regressors tax-exclusive price and tax rates imposed to gasoline
# are statistically significant at all levels.
# The coefficients of the βs implies elasticity. From our model's regression,
# we found that both have negative relationship with gasoline consumption per adult.
# However, consumers are relatively inelastic to the change in tax-exclusive price (due to
# change in global oil price). On the other hand, consumers are relatively elastic/responsive to a change
# in gasoline price due to tax levied upon gasoline. This significant difference in terms of
# elasticity is due to the salience of taxation compared to the implicit change in non-tax
# price change.

aux_mdl2<-lm(resid(mdl2)^2~lngp+lntr,data=dt2)
summary(aux_mdl2)

mdl2_rsq<-summary(aux_mdl2)$r.squared
LM_mdl2<-mdl2_rsq*nrow(dt2)
qchisq(0.95, df=2)

qchisq(0.95, df=2) > LM_mdl2  # crit. = 6.114, chisq = 5.991
                              # Reject the null hypothesis. Model is heteroskedastic.

plot(resid(mdl2),x=predict(mdl2))
plot(resid(mdl2_robust),x=predict(mdl2))

# --- Performing Weighted Least Square --- #
residual_ols<-resid(mdl2)
weights<-1/(residual_ols^2)

weighted_mdl2<-lm(lngca~lngp+lntr,data=dt2,weights = weights)
summary(weighted_mdl2)

aux_wmdl2<-lm(resid(weighted_mdl2)^2~lngp+lntr,data=dt2,weights=weights)
summary(aux_wmdl2)

wmdl2_rsq<-summary(aux_wmdl2)$r.squared
LM_wmdl2<-wmdl2_rsq*nrow(dt2)
qchisq(0.95,df=2)
qchisq(0.95,df=2) > LM_wmdl2  # crit. = 1.49, chisq = 5.991 
LM_wmdl2                      # Do not reject the null hypothesis. Model is homoscedastic.

stargazer(mdl2,weighted_mdl2,type="text")

# Augmented Model #
mdl3<-lm(lngca~lngp+lntr+fsize+lnincpop+railpop*urbanization,data=dt2)
summary(mdl3)

bptest(mdl3)
aux_mdl3<-lm(resid(mdl3)^2~lngp+lntr+fsize+lnincpop+railpop*urbanization,data=dt2)
summary(aux_mdl3)
