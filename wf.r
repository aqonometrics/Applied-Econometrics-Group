# Libraries
library(readxl)
library(stargazer)
library(psych)
library(lmtest)
library(AER)
library(estimatr)
library(corrplot)

# Include data from excel
dt<-data.frame(read.csv('/Users/afiqizzuddin/Library/Mobile Documents/com~apple~CloudDocs/UBD_Masters/Modules/Semester 1/BE-5103 Applied Econometrics/group_project/Gasoline Taxes.csv'))
attach(dt)

# DESCRIPTIVE STATISTICS #
describe(dt)

# CORRELATION MATRIX #
corrplot(cor(dt), method="number")

# ----- MODEL (1) ----- #
mdl<-lm(lngca~lngp+lntr, data=dt)
summary(mdl)

linearHypothesis(mdl,c("lngp=0","lntr=0"), test="F")
linearHypothesis(mdl,c("lntr=0"), test="F")
linearHypothesis(mdl,c("lngp=0"), test="F")

ax_mdl<-lm(resid(mdl)^2~lngp+lntr, data=dt)
summary(ax_mdl)
  # There is significant relationship between u^2 and lntr. Thus, heteroskedasticity
plot(resid(mdl),x=predict(mdl),
     main="Residual vs Predicted Value Plot",
     ylab="Residual Squared of the Model",
     xlab="Predicted Value of the Model")

# BP-LM Test #
mdl_rsq<-summary(ax_mdl)$r.squared
LM_mdl<-mdl_rsq*nrow(dt) # 6.114
crit_mdl<-qchisq(0.95, 2) # 5.9914

  # H0: Homoscedastic
  # H1: Heteroskedastic

LM_mdl > crit_mdl # True, we reject the null hypothesis.

# ROBUST STANDARD ERROR PROCESS #
t(sapply(c("const","HC0","HC1","HC2","HC3","HC4"), function(x) sqrt(diag(
  vcovHC(mdl,type=x)))))

ttest_b1=function(x){
  ttest<-summary(mdl)$coefficients[2]/x
  tresult<-ttest>qt(p=0.05,df=2, lower.tail=TRUE)
  
  if(tresult==FALSE){result="We reject the null. There is a Significant Relationship at 5% level"}
  else {result="We do not reject the null. There is no significant relationship at 5% level"}
  return(result)
}

ttest_b1(0.008988828)

ttest_b2=function(x){
  ttest<-summary(mdl)$coefficients[3]/x
  tresult<-ttest>qt(p=0.05,df=2, lower.tail=TRUE)
  
  if(tresult==FALSE){result="We reject the null. There is a Significant Relationship at 5% level"}
  else {result="We do not reject the null. There is no significant relationship at 5% level"}
  return(result)
}

ttest_b2(0.04300663)

mdl_robust<-coeftest(mdl,vcov=vcovHC(mdl,type="HC1"))
mdl_robust
# mdl_robust<-lm_robust(lngca~lngp+lntr, data=dt,
#                      se_type = "HC1")
# summary(mdl_robust)

stargazer(mdl_robust, type="text")

# MODEL 1 INTERPRETATION #

  # 1% increase in the gasoline price not due to taxation reduces the gasoline consumption per adult
  # by 0.137%.
  # 1% increase in the gasoline price due to taxation reduces the gasoline consumption per adult
  # by 0.361%
  
  # This result shows that consumers responds negatively towards the change in gasoline prices.
  # However, contrasting the two variables, we see that consumers are relatively elastic to a
  # change in gasoline price due to taxation compared to the change in gasoline price not due to
  # taxation (such as change in global oil price as a result of supply and demand).
  
  # This happen because of the salience of changes due to taxation compared to non-taxes. In which,
  # consumers can see directly what changes the price of gasoline. When the price changes due to
  # demand and supply, consumers barely feel it just accept the price (relatively inelastic).


# ----- MODEL (2) ----- #
  # In this model, we want to examine the determinants of gasoline consumption per adult.
  # We augmented model (1) with new regressors including family size, income per capita,
  # fraction of population living in metro areas with rail transportations, and fraction of
  # population living in metro areas.

mdl2<-lm(lngca~lngp+lntr+fsize+lnincpop+railpop*urbanization, data=dt)
summary(mdl2)

# REGRESSOR RELEVANCE TEST (F-TEST) #
linearHypothesis(mdl2,c("fsize=0","lnincpop=0","railpop=0","urbanization=0","railpop:urbanization=0"),
                 test="F")
  # At least one of the regressors' beta coefficients are not equal to 0

linearHypothesis(mdl2,c("fsize=0"),
                 test="F")
  # We found that fsize is irrelevant to be included in the model
  # Therefore, we can exclude family size in this case.
  # But before we do so, let's check with Akaike Information Criterion to support this claim.

step(mdl2)
  # The new best model is as below
mdl2_aic<-lm(formula = lngca ~ lngp + lntr + lnincpop + railpop + urbanization + 
               railpop:urbanization, data = dt)
  # Which supports our claim to exclude family size.
summary(mdl2_aic)

# HETEROSKEDASTICITY TEST #
ax_mdl2aic<-lm(resid(mdl2_aic)^2~ lngp+lntr+lnincpop+railpop+urbanization+railpop*urbanization,
               data=dt)
summary(ax_mdl2aic)
  # There is significant relationship at the 5% level between residual squared and lngp, lntr, lnincpop, and
  # urbanization. Indicating heteroskedasticity
plot(resid(mdl2_aic),x=predict(mdl2_aic),
     main="Residual vs Predicted Value Plot",
     ylab="Residual Squared of the Model",
     xlab="Predicted Value of the Model")
  # There is clustering of points.

# BP-LM Test #
mdl2_rsq<-summary(ax_mdl2aic)$r.squared
LM_mdl2<-mdl2_rsq*nrow(dt) # 245.113
crit_mdl2<-qchisq(0.95, 6) # 12.59159

  # H0: Homoscedastic
  # H1: Heteroskedastic

LM_mdl > crit_mdl # True, we reject the null hypothesis.

# ROBUST STANDARD ERROR PROCESS #
t(sapply(c("const","HC0","HC1","HC2","HC3","HC4"), function(x) sqrt(diag(
  vcovHC(mdl2_aic,type=x)))))

mdl2_robust<-coeftest(mdl2_aic, vcov=vcovHC(mdl2_aic, type="HC1"))
mdl2_robust
  # In this robust second model, railpop is no longer significant at the 5% level.
  # Which is what we want and make sense now when we interpret it.

