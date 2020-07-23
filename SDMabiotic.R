setwd("/Users/katebuckeridge/OneDrive - University of Edinburgh/R/UGrass/SDM") 
setwd("C:/Users/kbuckeri/OneDrive - University of Edinburgh/R/UGrass/SDM") #at work

#################  SDM abiotic immobilization ################


#  MIXED MODEL stats for pub

##################################################################
#https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

sdm <- read.csv("SDMabiotic.csv", header=T)

library(lme4)
library(car)
library(multcomp)

### below run first with not transformed data - similar results but higher P values. Data not normal, so stuck with more conservative version despite website above says not to transform

######## C

sdm$log_rc <- log(sdm$RecoveryC)

model.1.2 <- lmer(log_rc ~ Steri + Necro + (1|Plot), data = sdm, REML=FALSE)
summary (model.1.2)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: log_rc ~ Steri + Necro + (1 | Plot)
# Data: sdm
# 
# AIC      BIC   logLik deviance df.resid 
# -11.7      0.1     12.9    -25.7       33 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.2466 -0.6928  0.1419  0.7128  1.5084 
# 
# Random effects:
#   Groups   Name        Variance  Std.Dev.
# Plot     (Intercept) 0.0009058 0.0301  
# Residual             0.0299486 0.1731  
# Number of obs: 40, groups:  Plot, 5
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)       4.13316    0.06265  65.975
# SteriSterile      0.15482    0.05473   2.829
# NecroEcoli        0.17268    0.07739   2.231
# NecroMluteus      0.13889    0.07739   1.795
# NecroScerevisiae  0.32413    0.07739   4.188
# 
# Correlation of Fixed Effects:
#   (Intr) StrStr NcrEcl NcrMlt
# SteriSteril -0.437                     
# NecroEcoli  -0.618  0.000              
# NecroMlutes -0.618  0.000  0.500       
# NecroScervs -0.618  0.000  0.500  0.500
Anova(model.1.2)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: log_rc
# Chisq Df Pr(>Chisq)   
# Steri  8.0031  1  0.0046697 ** 
# Necro 17.7437  3  0.0004967 ***
summary(glht(model.1.2, linfct=mcp(Necro="Tukey")))
# Fit: lmer(formula = log_rc ~ Steri + Necro + (1 | Plot), data = sdm, 
#           REML = FALSE)
# 
# Linear Hypotheses:
# Estimate Std. Error z value Pr(>|z|)    
# Ecoli - Control == 0        0.17268    0.07739   2.231   0.1148    
# Mluteus - Control == 0      0.13889    0.07739   1.795   0.2758    
# Scerevisiae - Control == 0  0.32413    0.07739   4.188   <0.001 ***
# Mluteus - Ecoli == 0       -0.03379    0.07739  -0.437   0.9722    
# Scerevisiae - Ecoli == 0    0.15145    0.07739   1.957   0.2044    
# Scerevisiae - Mluteus == 0  0.18524    0.07739   2.394   0.0783 .   
summary(glht(model.1.2, linfct=mcp(Steri="Tukey"))) #not necessary, same result as Wald's test b/c one level


#since Steri is sig, need to break df into steri and non for figure 
sdmst <- sdm[sdm$Steri == "Sterile",]
sdmn <- sdm[sdm$Steri == "None",]

model.1.3 <- lmer(log_rc ~ Necro + (1|Plot), data = sdmst, REML=FALSE)
summary (model.1.3)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: log_rc ~ Necro + (1 | Plot)
# Data: sdmst
# 
# AIC      BIC   logLik deviance df.resid 
# -7.7     -1.7      9.8    -19.7       14 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.4247 -0.4938  0.4001  0.5134  1.4222 
# 
# Random effects:
#   Groups   Name        Variance  Std.Dev.
# Plot     (Intercept) 0.0007213 0.02686 
# Residual             0.0211970 0.14559 
# Number of obs: 20, groups:  Plot, 5
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)       4.30932    0.06621  65.086
# NecroEcoli        0.12767    0.09208   1.387
# NecroMluteus      0.12565    0.09208   1.365
# NecroScerevisiae  0.29701    0.09208   3.226
# 
# Correlation of Fixed Effects:
#   (Intr) NcrEcl NcrMlt
# NecroEcoli  -0.695              
# NecroMlutes -0.695  0.500       
# NecroScervs -0.695  0.500  0.500
Anova(model.1.3)
# Response: log_rc
# Chisq Df Pr(>Chisq)  
# Necro 10.518  3    0.01464 *
summary(glht(model.1.3, linfct=mcp(Necro="Tukey")))
# Linear Hypotheses:
# Estimate Std. Error z value Pr(>|z|)   
# Ecoli - Control == 0        0.127672   0.092080   1.387  0.50778   
# Mluteus - Control == 0      0.125651   0.092080   1.365  0.52170   
# Scerevisiae - Control == 0  0.297015   0.092080   3.226  0.00683 **
# Mluteus - Ecoli == 0       -0.002021   0.092080  -0.022  1.00000   
# Scerevisiae - Ecoli == 0    0.169343   0.092080   1.839  0.25493   
# Scerevisiae - Mluteus == 0  0.171364   0.092080   1.861  0.24504 

model.1.4 <- lmer(log_rc ~ Necro + (1|Plot), data = sdmn, REML=FALSE) #singular fit
summary (model.1.4)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: log_rc ~ Necro + (1 | Plot)
# Data: sdmn
# 
# AIC      BIC   logLik deviance df.resid 
# 4       10        4       -8       14 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -1.97741 -0.86971  0.06356  1.04123  1.32552 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# Plot     (Intercept) 0.00000  0.0000  
# Residual             0.03923  0.1981  
# Number of obs: 20, groups:  Plot, 5
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)       4.11183    0.08858  46.419
# NecroEcoli        0.21768    0.12527   1.738
# NecroMluteus      0.15212    0.12527   1.214
# NecroScerevisiae  0.35124    0.12527   2.804
# 
# Correlation of Fixed Effects:
#   (Intr) NcrEcl NcrMlt
# NecroEcoli  -0.707              
# NecroMlutes -0.707  0.500       
# NecroScervs -0.707  0.500  0.500
# convergence code: 0
# singular fit
Anova(model.1.4)
# Response: log_rc
# Chisq Df Pr(>Chisq)  
# Necro 8.1462  3    0.04308 *
summary(glht(model.1.4, linfct=mcp(Necro="Tukey")))
# Linear Hypotheses:
#                           Estimate Std. Error z value Pr(>|z|)  
# Ecoli - Control == 0        0.21768    0.12527   1.738   0.3041  
# Mluteus - Control == 0      0.15212    0.12527   1.214   0.6177  
# Scerevisiae - Control == 0  0.35124    0.12527   2.804   0.0258 *
# Mluteus - Ecoli == 0       -0.06556    0.12527  -0.523   0.9535  
# Scerevisiae - Ecoli == 0    0.13356    0.12527   1.066   0.7102  
# Scerevisiae - Mluteus == 0  0.19912    0.12527   1.589   0.3846  

######### N

sdm$log_rn <- log(sdm$RecoveryN)

model.1.5 <- lmer(log_rn ~ Steri + Necro + (1|Plot), data = sdm, REML=FALSE) #singular fit
summary (model.1.5)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: log_rn ~ Steri + Necro + (1 | Plot)
# Data: sdm
# 
# AIC      BIC   logLik deviance df.resid 
# -3.5      8.4      8.7    -17.5       33 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.22357 -0.44454  0.02136  0.68495  2.03275 
# 
# Random effects:
#   Groups   Name        Variance  Std.Dev. 
# Plot     (Intercept) 2.906e-17 5.391e-09
# Residual             3.783e-02 1.945e-01
# Number of obs: 40, groups:  Plot, 5
# 
# Fixed effects:
#                  Estimate Std. Error t value
# (Intercept)      4.1854242  0.0687693  60.862
# SteriSterile     0.0003046  0.0615091   0.005
# NecroEcoli       0.1743890  0.0869870   2.005
# NecroMluteus     0.1555270  0.0869870   1.788
# NecroScerevisiae 0.2458793  0.0869870   2.827
# 
# Correlation of Fixed Effects:
#   (Intr) StrStr NcrEcl NcrMlt
# SteriSteril -0.447                     
# NecroEcoli  -0.632  0.000              
# NecroMlutes -0.632  0.000  0.500       
# NecroScervs -0.632  0.000  0.500  0.500
# convergence code: 0
# singular fit

Anova(model.1.5)
# Response: log_rn
# Chisq Df Pr(>Chisq)  
# Steri 0.0000  1    0.99605  
# Necro 8.5035  3    0.03668 *

summary(glht(model.1.5, linfct=mcp(Necro="Tukey")))
# Linear Hypotheses:
# Estimate Std. Error z value Pr(>|z|)  
# Ecoli - Control == 0        0.17439    0.08699   2.005   0.1860  
# Mluteus - Control == 0      0.15553    0.08699   1.788   0.2790  
# Scerevisiae - Control == 0  0.24588    0.08699   2.827   0.0246 *
# Mluteus - Ecoli == 0       -0.01886    0.08699  -0.217   0.9964  
# Scerevisiae - Ecoli == 0    0.07149    0.08699   0.822   0.8442  
# Scerevisiae - Mluteus == 0  0.09035    0.08699   1.039   0.7267  

#since Steri is NOT sig, no need to break df into steri and non for figure 

####### CN

model.1.6 <- lmer(RecCN ~ Steri + Necro + (1|Plot), data = sdm, REML=FALSE) 
summary (model.1.6)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: RecCN ~ Steri + Necro + (1 | Plot)
# Data: sdm
# 
# AIC      BIC   logLik deviance df.resid 
# 22.0     33.8     -4.0      8.0       33 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.8740 -0.7571 -0.2343  0.8397  1.7369 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# Plot     (Intercept) 0.01283  0.1133  
# Residual             0.06338  0.2518  
# Number of obs: 40, groups:  Plot, 5
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)       3.04035    0.10241  29.687
# SteriSterile      0.53266    0.07961   6.691
# NecroEcoli       -0.02740    0.11259  -0.243
# NecroMluteus     -0.04278    0.11259  -0.380
# NecroScerevisiae  0.17376    0.11259   1.543
# 
# Correlation of Fixed Effects:
#   (Intr) StrStr NcrEcl NcrMlt
# SteriSteril -0.389                     
# NecroEcoli  -0.550  0.000              
# NecroMlutes -0.550  0.000  0.500       
# NecroScervs -0.550  0.000  0.500  0.500

Anova(model.1.6)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: RecCN
# Chisq Df Pr(>Chisq)    
# Steri 44.7646  1  2.222e-11 ***
# Necro  4.7475  3     0.1912  

###### Figure 3 (MBC/CO2/N2O and CUE)

remove(sdm)
remove(sdmst)
remove(sdmn)
sdmcue <- read.csv("SDMcue.csv", header=T) 
# all results below without 2x ML

##### MB Recovery

sdmcue$log_mbc <- log(sdmcue$MBRecovery)

model.2.1 <- lmer(log_mbc ~ Necro + (1|Plot), data = sdmcue, REML=FALSE) #singular fit
summary (model.2.1)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: log_mbc ~ Necro + (1 | Plot)
# Data: sdmcue
# 
# AIC      BIC   logLik deviance df.resid 
# 13.6     18.2     -0.8      1.6       10 
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -2.12446 -0.53128  0.09428  0.41973  2.04072 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# Plot     (Intercept) 0.00000  0.000   
# Residual             0.06452  0.254   
# Number of obs: 16, groups:  Plot, 4
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)        1.0290     0.1270   8.102
# NecroEcoli         0.8252     0.1796   4.594
# NecroMluteus       0.4145     0.1796   2.308
# NecroScerevisiae   0.7161     0.1796   3.987
# 
# Correlation of Fixed Effects:
#   (Intr) NcrEcl NcrMlt
# NecroEcoli  -0.707              
# NecroMlutes -0.707  0.500       
# NecroScervs -0.707  0.500  0.500
# convergence code: 0
# singular fit
Anova(model.2.1)
# Response: log_mbc
# Chisq Df Pr(>Chisq)    
# Necro 25.372  3  1.291e-05 ***
summary(glht(model.2.1, linfct=mcp(Necro="Tukey")))
# Fit: lmer(formula = log_mbc ~ Necro + (1 | Plot), data = sdmcue, REML = FALSE)
# 
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)    
# Ecoli - Control == 0         0.8252     0.1796   4.594   <0.001 ***
# Mluteus - Control == 0       0.4145     0.1796   2.308    0.096 .  
# Scerevisiae - Control == 0   0.7161     0.1796   3.987   <0.001 ***
# Mluteus - Ecoli == 0        -0.4106     0.1796  -2.286    0.101    
# Scerevisiae - Ecoli == 0    -0.1091     0.1796  -0.607    0.930    
# Scerevisiae - Mluteus == 0   0.3015     0.1796   1.679    0.335   

#### CO2

hist(sdmcue$CO2Recovery)
sdmcue$log_co2 <- log(sdmcue$CO2Recovery)
hist(sdmcue$log_co2) #better but still bimodal

model.2.2 <- lmer(log_co2 ~ Necro + (1|Plot), data = sdmcue, REML=FALSE) 
summary (model.2.2)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: log_co2 ~ Necro + (1 | Plot)
# Data: sdmcue
# 
# AIC      BIC   logLik deviance df.resid 
# 10.4     16.3      0.8     -1.6       14 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.9729 -0.2160  0.2255  0.4596  1.1306 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# Plot     (Intercept) 0.02589  0.1609  
# Residual             0.03905  0.1976  
# Number of obs: 20, groups:  Plot, 5
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)        -1.182      0.114  -10.38
# NecroEcoli          1.989      0.125   15.91
# NecroMluteus        1.916      0.125   15.33
# NecroScerevisiae    1.963      0.125   15.71
# 
# Correlation of Fixed Effects:
#   (Intr) NcrEcl NcrMlt
# NecroEcoli  -0.548              
# NecroMlutes -0.548  0.500       
# NecroScervs -0.548  0.500  0.500
Anova(model.2.2)
# Response: log_co2
# Chisq Df Pr(>Chisq)    
# Necro 367.76  3  < 2.2e-16 ***
summary(glht(model.2.2, linfct=mcp(Necro="Tukey")))
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)    
# Ecoli - Control == 0        1.98877    0.12498  15.913   <1e-06 ***
# Mluteus - Control == 0      1.91605    0.12498  15.331   <1e-06 ***
# Scerevisiae - Control == 0  1.96306    0.12498  15.707   <1e-06 ***
# Mluteus - Ecoli == 0       -0.07272    0.12498  -0.582    0.938    
# Scerevisiae - Ecoli == 0   -0.02571    0.12498  -0.206    0.997    
# Scerevisiae - Mluteus == 0  0.04700    0.12498   0.376    0.982  

### N2O

hist(sdmcue$N2ORecovery)
sdmcue$log_n2o <- log(sdmcue$N2ORecovery)
hist(sdmcue$log_n2o) #much better

model.2.3 <- lmer(log_n2o ~ Necro + (1|Plot), data = sdmcue, REML=FALSE) 
summary (model.2.3)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: log_n2o ~ Necro + (1 | Plot)
# Data: sdmcue
# 
# AIC      BIC   logLik deviance df.resid 
# 52.6     58.5    -20.3     40.6       14 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.7750 -0.4858  0.0609  0.6859  1.3784 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# Plot     (Intercept) 0.2847   0.5336  
# Residual             0.3009   0.5485  
# Number of obs: 20, groups:  Plot, 5
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)       -2.7068     0.3422  -7.910
# NecroEcoli         2.5624     0.3469   7.386
# NecroMluteus       2.7680     0.3469   7.979
# NecroScerevisiae   3.1729     0.3469   9.146
# 
# Correlation of Fixed Effects:
#   (Intr) NcrEcl NcrMlt
# NecroEcoli  -0.507              
# NecroMlutes -0.507  0.500       
# NecroScervs -0.507  0.500  0.500
Anova(model.2.3)
# Response: log_n2o
# Chisq Df Pr(>Chisq)    
# Necro 103.34  3  < 2.2e-16 ***
summary(glht(model.2.3, linfct=mcp(Necro="Tukey")))
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)    
# Ecoli - Control == 0         2.5624     0.3469   7.386   <1e-04 ***
# Mluteus - Control == 0       2.7680     0.3469   7.979   <1e-04 ***
# Scerevisiae - Control == 0   3.1729     0.3469   9.146   <1e-04 ***
# Mluteus - Ecoli == 0         0.2055     0.3469   0.592    0.934    
# Scerevisiae - Ecoli == 0     0.6105     0.3469   1.760    0.293    
# Scerevisiae - Mluteus == 0   0.4050     0.3469   1.167    0.648    

#### CUE

model.2.4 <- lmer(CUE ~ Necro + (1|Plot), data = sdmcue, REML=FALSE) #singular
summary (model.2.4)
# Linear mixed model fit by maximum likelihood  ['lmerMod']
# Formula: CUE ~ Necro + (1 | Plot)
# Data: sdmcue
# 
# AIC      BIC   logLik deviance df.resid 
# -21.4    -16.8     16.7    -33.4       10 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.1781 -0.6344 -0.1069  0.1978  2.6319 
# 
# Random effects:
#   Groups   Name        Variance  Std.Dev. 
# Plot     (Intercept) 8.509e-18 2.917e-09
# Residual             7.241e-03 8.510e-02
# Number of obs: 16, groups:  Plot, 4
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)       0.68971    0.04255  16.210
# NecroEcoli       -0.38147    0.06017  -6.340
# NecroMluteus     -0.45401    0.06017  -7.545
# NecroScerevisiae -0.37694    0.06017  -6.264
# 
# Correlation of Fixed Effects:
#   (Intr) NcrEcl NcrMlt
# NecroEcoli  -0.707              
# NecroMlutes -0.707  0.500       
# NecroScervs -0.707  0.500  0.500
# convergence code: 0
# singular fit
Anova(model.2.4)
# Response: CUE
# Chisq Df Pr(>Chisq)    
# Necro 69.733  3   4.87e-15 ***
summary(glht(model.2.4, linfct=mcp(Necro="Tukey")))
# Linear Hypotheses:
#   Estimate Std. Error z value Pr(>|z|)    
# Ecoli - Control == 0       -0.381469   0.060172  -6.340   <1e-04 ***
# Mluteus - Control == 0     -0.454009   0.060172  -7.545   <1e-04 ***
# Scerevisiae - Control == 0 -0.376943   0.060172  -6.264   <1e-04 ***
# Mluteus - Ecoli == 0       -0.072541   0.060172  -1.206    0.623    
# Scerevisiae - Ecoli == 0    0.004526   0.060172   0.075    1.000    
# Scerevisiae - Mluteus == 0  0.077066   0.060172   1.281    0.575    

##############################################
