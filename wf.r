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

# PLOT #
par(mfrow=c(1,2))
plot(dt$lngca, x=dt$lngp)
plot(dt$lngca, x=dt$lntr)
  # The relationship using scatter plots are rather undeterministic

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

# PLOT #
par(mfrow=c(2,3))
plot(dt$lngca, x=dt$lngp)
plot(dt$lngca, x=dt$lntr)
plot(dt$lngca, x=dt$lnincpop)
plot(dt$lngca, x=dt$railpop)
plot(dt$lngca, x=dt$urbanization)
plot(dt$lngca, x=(dt$railpop)*(dt$urbanization))

  # The relationship using scatter plots are rather undeterministic

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

# ----- MODEL (3) ----- #
# In this model, we aim to examine the degree of taxation which consumers start to react significantly

mdl3<-lm(lngca~lngpinc+I(lngpinc^2)+I(lngpinc^3), data=dt)
summary(mdl3) 

plot(resid(mdl3), predict(mdl3))
boxplot(resid(mdl3))

# TEST OF RELEVANCE (F-TEST) #
linearHypothesis(mdl3, c("I(lngpinc^2)=0","I(lngpinc^3)=0"), test="F")
linearHypothesis(mdl3, c("I(lngpinc^2)=0"), test="F")
linearHypothesis(mdl3, c("I(lngpinc^3)=0"), test="F")
  # Both second and third order are relevant to be included in the model. 

ax_mdl3<-lm(resid(mdl3)^2~lngpinc+I(lngpinc^2)+I(lngpinc^3), data=dt)
summary(ax_mdl3)
  # None of the variables have significant relationship with the u^2 of the model.
  # This means that, the model has constant variance.

# HETEROSKEDASTICITY TEST #
  # to prove our claim above, we shall run BP-LM Test

mdl3_rsq<-summary(ax_mdl3)$r.squared
mdl3_LMcrit<-mdl3_rsq*nrow(dt) # 4.483
mdl3_chivalue<-qchisq(0.95, df=3) # 7.814
mdl3_LMcrit > mdl3_chivalue # False. We do not reject the null.
                            # Null hypothesis is that the model is homoscedastic.

  # However, the coefficient of β1 does not make sense

# ROBUST STANDARD ERRORS #
t(sapply(c("const","HC0","HC1","HC2","HC3","HC4"), function(x) sqrt(diag(
  vcovHC(mdl3,type=x)))))

mdl3_robust<-coeftest(mdl3, vcovHC(mdl3, type="HC1"))
mdl3_robust
  # Even after robust standard error, coefficient remains the same

# SIMULATIONS #
b0 <- summary(mdl3)$coefficients[1]
b1 <- summary(mdl3)$coefficients[2]
b2 <- summary(mdl3)$coefficients[3]
b3 <- summary(mdl3)$coefficients[4]

taxrate<-0:200
lntaxrate<-log(taxrate)

#predictcons<-b0+b1*lntaxrate+b2*(lntaxrate^2)+b3*(lntaxrate^3)
#fp_result<-data.frame(tax_rate=taxrate, gasoline_consumption=exp(predictcons))

#plot(x=fp_result$tax_rate,y=fp_result$gasoline_consumption, type="l")

predictcons<-b0+b1*lngpinc+b2*(lngpinc^2)+b3*(lngpinc^3)
predictcons<-b0+b1*lntaxrate+b2*(lntaxrate^2)+b3*(lntaxrate^3) # Simulated Values

fp_result<-data.frame(tax_rate=exp(lntaxrate), gasoline_consumption=exp(predictcons))

plot(x=fp_result$tax_rate,y=fp_result$gasoline_consumption, type="l",
     main="Predicted Gasoline Consumption given Tax Rate",
     ylab="Gasoline Consumption Per Adult", xlab="Tax Rate ($ per gallon)")



# ANTI LOGGING MODEL #
dt$gca<-exp(dt$lngca)
dt$gpinc<-exp(dt$lngpinc)

mdl4<-lm(gca~gpinc+I(gpinc^2)+I(gpinc^3), data=dt)
summary(mdl4)

plot(resid(mdl4), predict(mdl4))
boxplot(resid(mdl4))

# HETEROSKEDASTICITY TEST #
ax_mdl4<-lm(resid(mdl4)^2~gpinc+I(gpinc^2)+I(gpinc^3), data=dt)
summary(mdl4)
  # all regressors have statistically significant relationship with u^2 of the model 4.

t(sapply(c("const","HC0","HC1","HC2","HC3","HC4"), function(x) sqrt(diag(
  vcovHC(mdl4,type=x)))))

mdl4_robust<-coeftest(mdl4, vcovHC(mdl4, type="HC1"))
mdl4_robust
  # Even after robust standard error, coefficient remains the same




# Optional #
mdl5<-lm(lngca~gpinc+I(gpinc^2)+I(gpinc^3), data=dt)
summary(mdl5)
mdl6<-lm(lngca~gpinc+I(gpinc^2), data=dt)
summary(mdl6)

mdl7<-lm(lngca~lngpinc+I(lngpinc^2),data=dt)
summary(mdl7)
linearHypothesis(mdl7, c("lngpinc=0"), test="F")

mdl8<-lm(lngca~I(lngpinc^2), data=dt)
summary(mdl8)

