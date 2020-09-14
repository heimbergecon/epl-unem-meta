rm(list = ls()) #clear list

#automatic installation of required packages
packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
              "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
              "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","plyr","zoo","ggthemes",
              "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table")
ipak(packages)

#load packages
library(xlsx) #Excel-Paket laden
library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
library(sandwich)
library(lmtest)
library(getopt)
library(CausalGAM)
library(ggplot2)
library(reshape2)
library(xts)
library(lattice)
library(gridExtra)
library(gtable)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(tis)
library(foreign)
library(MASS)
library(quantreg)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggplot2)
library(datasets)
library(rio)
library(psych)
library(systemfit)
library(foreign)
library(MatchIt)
library(CRTgeeDR)
library(eurostat)
library(plyr)
library(zoo)
library(ggthemes)
library("robumeta")
library("metafor")
library("dplyr")
library(clubSandwich)
library(Hmisc)
library(metafor)
library(pracma)
library(pkgs)
library(broom)
library(sjPlot)
library(here)
library(data.table)

#load data
dat <- fread(here("data/EPL-UNEM-data-FINAL.csv"))

#calculate the partial correlation coefficient
dat$PartialCorrelationCoefficient <- dat$Tstatistic / (sqrt((dat$Tstatistic^2)+dat$DegreesofFreedom))

dat$PartialCorrelationCoefficient <- dat$PartialCorrelationCoefficient*dat$Transform
dat$Tstatistic <- dat$Tstatistic*dat$Transform

#calculate the standard error of the partial correlation coefficient
dat$StandardErrorPartialCorrelation <- sqrt((1-(dat$PartialCorrelationCoefficient)^2)/dat$DegreesofFreedom)

#Precision
dat$PrecSE <- 1 / dat$StandardErrorPartialCorrelation
#InverseSE
dat$InverseSE <- 1 / dat$StandardError
#Variance 
dat$Variance <- dat$StandardErrorPartialCorrelation^2
#PrecVariance
dat$PrecVariance <- 1 / dat$Variance

#calculate mean year of the data
dat$MeanYearData<- (dat$StartYearData+dat$EndYearData)/2

dat <- escalc(measure="ZCOR", ri=PartialCorrelationCoefficient, ni=Observations, data=dat)

dat_long <- melt(dat, id=1:56)

#subsets of the data
dat_NonFeldmann <- subset(dat_long, NonFeldmann %in% c('1'))
dat_LaggedEPL <- subset(dat_long, LaggedEPL %in% c('1'))
dat_YearAverage <- subset(dat_long, YearAverage %in% c('1'))
dat_NonInteractedEPL <- subset(dat_long, InteractedEPL %in% c('0'))
dat_OECD <- subset(dat_long, OECDMethodologyIndex %in% c('1'))
dat_SurveyBased <- subset(dat_long, SurveyBased %in% c('1'))
dat_CrossSection <- subset(dat_long, CrossSection %in% c('1'))

#Table 1: descriptive statistics for the variables included in the meta data set

#mean
mean(dat_long$PartialCorrelationCoefficient)
mean(dat_long$Unemployment)
mean(dat_long$YouthUnemployment)
mean(dat_long$LongTermUnemployment)
mean(dat_long$FemaleUnemployment)
mean(dat_long$OtherDependentVar)
mean(dat_long$OECDMethodologyIndex)
mean(dat_long$LawBased)
mean(dat_long$EFWIndex)
mean(dat_long$SurveyBased)
mean(dat_long$CrossSection)
mean(dat_long$PostCrisisData)
mean(dat_long$YearAverage)
mean(dat_long$OECDCountriesOnly)
mean(dat_long$NonOECDcountries)
mean(dat_long$MixofCountries)
mean(dat_long$CountryFixedEffects)
mean(dat_long$InteractedEPL)
mean(dat_long$LaggedEPL)
mean(dat_long$GMM)
mean(dat_long$OLS)
mean(dat_long$FGLS)
mean(dat_long$IV)
mean(dat_long$OtherEstimator)
mean(dat_long$StandardErrorPartialCorrelation)
mean(dat_long$LaborJournal)
mean(dat_long$Primary)
mean(dat_long$Crossauthor)
mean(dat_long$Prior)
mean(dat_long$UBR)
mean(dat_long$TaxWedge)
mean(dat_long$GDPgrowth)
mean(dat_long$ACCU)
mean(dat_long$RealInterestRate)
mean(dat_long$ProductMarketRegulation)

#standard deviation
sd(dat_long$PartialCorrelationCoefficient)
sd(dat_long$Unemployment)
sd(dat_long$YouthUnemployment)
sd(dat_long$LongTermUnemployment)
sd(dat_long$FemaleUnemployment)
sd(dat_long$OtherDependentVar)
sd(dat_long$OECDMethodologyIndex)
sd(dat_long$LawBased)
sd(dat_long$EFWIndex)
sd(dat_long$SurveyBased)
sd(dat_long$CrossSection)
sd(dat_long$PostCrisisData)
sd(dat_long$YearAverage)
sd(dat_long$OECDCountriesOnly)
sd(dat_long$NonOECDcountries)
sd(dat_long$MixofCountries)
sd(dat_long$CountryFixedEffects)
sd(dat_long$InteractedEPL)
sd(dat_long$LaggedEPL)
sd(dat_long$GMM)
sd(dat_long$OLS)
sd(dat_long$FGLS)
sd(dat_long$IV)
sd(dat_long$OtherEstimator)
sd(dat_long$StandardErrorPartialCorrelation)
sd(dat_long$LaborJournal)
sd(dat_long$Primary)
sd(dat_long$Crossauthor)
sd(dat_long$Prior)
sd(dat_long$UBR)
sd(dat_long$TaxWedge)
sd(dat_long$GDPgrowth)
sd(dat_long$ACCU)
sd(dat_long$RealInterestRate)
sd(dat_long$ProductMarketRegulation)

#distributional statistics
max(dat_long$PartialCorrelationCoefficient)
min(dat_long$PartialCorrelationCoefficient)
sd(dat_long$PartialCorrelationCoefficient)
mean(dat_long$PartialCorrelationCoefficient)
median(dat_long$PartialCorrelationCoefficient)

#Figure 1

#all estimates, colored based on EPL variable type
plot_funnel <- ggplot(data=dat,
                      aes(x=PartialCorrelationCoefficient, y=PrecSE)) +
  geom_point() +
  xlab("Partial correlation coefficient") +
  ylab("1 / Standard error (precision of the estimate)") +
  geom_point(aes(colour=factor(EPLtype))) +
  ggtitle("Funnel plot of EPL-unemployment\n partial correlations (n=881)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.text.y=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  guides(fill=guide_legend("ABC"))+
  theme(legend.position="bottom")+
  scale_color_discrete(name = "EPL variable")
plot_funnel

#Table 2: Results on publication selection bias

#column (1)
average_effect <- lm(PartialCorrelationCoefficient ~1, weights=PrecVariance, data=dat_long)
summary(average_effect)
coef_test(average_effect, vcov = "CR1", 
          cluster = dat_long$id, test = "naive-t")

#column (2)
pubbias_1 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation, weights=PrecVariance, data=dat_long)
summary(pubbias_1)

#column (3)
coef_test(pubbias_1, vcov = "CR1", 
          cluster = dat_long$id, test = "naive-t")

#column (4)
pubbias_3 <- lm(yi ~ StandardErrorPartialCorrelation, weights=PrecVariance, data=dat_long)
summary(pubbias_3)

coef_test(pubbias_3, vcov = "CR1", 
          cluster = dat_long$id, test = "naive-t")

#column (5)
pubbias_1 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation, weights=PrecVariance, data=dat_OECD)
summary(pubbias_1)

coef_test(pubbias_1, vcov = "CR1", 
          cluster = dat_OECD$id, test = "naive-t")

#column (6)
#LaggedEPL
pubbias_1 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation, weights=PrecVariance, data=dat_LaggedEPL)
summary(pubbias_1)

coef_test(pubbias_1, vcov = "CR1", 
          cluster = dat_LaggedEPL$id, test = "naive-t")

#Table 3: Multivariate meta-regression analysis

#column (1)
pubbias_2_var_est_gts <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + YouthUnemployment + LongTermUnemployment + FemaleUnemployment + OtherDependentVar + LawBased + EFWIndex + SurveyBased + NonOECDcountries + MixofCountries + OLS+ FGLS + IV + OtherEstimator + YearAverage + LaborJournal + ProductMarketRegulation, weights=PrecVariance, data=dat_long)
summary(pubbias_2_var_est_gts)

coef_test(pubbias_2_var_est_gts, vcov = "CR1", 
          cluster = dat_long$id, test = "naive-t")

#column (2)
RE <- rma(PartialCorrelationCoefficient, Variance, mods = ~ StandardErrorPartialCorrelation + YouthUnemployment + LongTermUnemployment + FemaleUnemployment + OtherDependentVar + LawBased + EFWIndex + SurveyBased + NonOECDcountries + MixofCountries + OLS+ FGLS + IV + OtherEstimator + YearAverage + LaborJournal + ProductMarketRegulation, weights=PrecSE, weighted=TRUE, method="REML", data=dat_long) 
summary(RE)

coef_test(RE, vcov = "CR1", 
          cluster = dat_long$id, test = "naive-t")

#column (3)
#Robust regression
Robust <- rlm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + YouthUnemployment + LongTermUnemployment + FemaleUnemployment + OtherDependentVar + LawBased + EFWIndex + SurveyBased + NonOECDcountries + MixofCountries + OLS+ FGLS + IV + OtherEstimator + YearAverage + LaborJournal + ProductMarketRegulation, data=dat_long)
summary(Robust)

#column (4)
#Fisher's z
pubbias_2_var_gts_Fisher <- lm(yi ~ StandardErrorPartialCorrelation + YouthUnemployment + LongTermUnemployment + FemaleUnemployment + OtherDependentVar + LawBased + EFWIndex + SurveyBased + NonOECDcountries + MixofCountries + OLS+ FGLS + IV + OtherEstimator + YearAverage + LaborJournal + ProductMarketRegulation, weights=PrecVariance, data=dat_long)
summary(pubbias_2_var_gts_Fisher)

coef_test(pubbias_2_var_gts_Fisher, vcov = "CR1", 
          cluster = dat_long$id, test = "naive-t")

#column (5)
#NonFeldmann
pubbias_2_var_est_gts_NonFeldmann <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + YouthUnemployment + LongTermUnemployment + FemaleUnemployment + OtherDependentVar + LawBased + EFWIndex + SurveyBased + NonOECDcountries + MixofCountries + OLS+ FGLS + IV + OtherEstimator + YearAverage + LaborJournal + ProductMarketRegulation, weights=PrecVariance, data=dat_NonFeldmann)
summary(pubbias_2_var_est_gts_NonFeldmann)

coef_test(pubbias_2_var_est_gts_NonFeldmann, vcov = "CR1", 
          cluster = dat_NonFeldmann$id, test = "naive-t")

#column (6)
#NonInteractedEPL
pubbias_2_var_est_gts_NonInteractedEPL <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + YouthUnemployment + LongTermUnemployment + FemaleUnemployment + OtherDependentVar + LawBased + EFWIndex + SurveyBased + NonOECDcountries + MixofCountries + OLS+ FGLS + IV + OtherEstimator + YearAverage + LaborJournal + ProductMarketRegulation, weights=PrecVariance, data=dat_NonInteractedEPL)
summary(pubbias_2_var_est_gts_NonInteractedEPL)

coef_test(pubbias_2_var_est_gts_NonInteractedEPL, vcov = "CR1", 
          cluster = dat_NonInteractedEPL$id, test = "naive-t")

#column(7)
#OECD data
pubbias_2_var_est_gts_OECD <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + YouthUnemployment + LongTermUnemployment + FemaleUnemployment + OtherDependentVar+ NonOECDcountries + MixofCountries + OLS+ FGLS + IV + OtherEstimator + YearAverage + LaborJournal + ProductMarketRegulation, weights=PrecVariance, data=dat_OECD)
summary(pubbias_2_var_est_gts_OECD)

coef_test(pubbias_2_var_est_gts_OECD, vcov = "CR1",
          cluster = dat_OECD$id, test = "naive-t")

#stargazer table
ses.WLS.est <- list(coef_test(pubbias_2_var_est_gts, vcov = "CR1", 
                          cluster = dat_long$id, test = "naive-t")[,2])
tvals.WLS.est <- list(coef_test(pubbias_2_var_est_gts, vcov = "CR1", 
                            cluster = dat_long$id, test = "naive-t")[,3]) 

#WLS (Fisher's z) with standard errors clustered at the study level
ses.WLS.Fisher <- list(coef_test(pubbias_2_var_gts_Fisher, vcov = "CR1", 
                                 cluster = dat_long$id, test = "naive-t")[,2]) 
tvals.WLS.Fisher <- list(coef_test(pubbias_2_var_gts_Fisher, vcov = "CR1", 
                                   cluster = dat_long$id, test = "naive-t")[,3]) 

#WLS (Fisher's z) with standard errors clustered at the study level
ses.WLS.OECD <- list(coef_test(pubbias_2_var_est_gts_OECD, vcov = "CR1", 
                                 cluster = dat_OECD$id, test = "naive-t")[,2]) 
tvals.WLS.OECD <- list(coef_test(pubbias_2_var_est_gts_OECD, vcov = "CR1", 
                                   cluster = dat_OECD$id, test = "naive-t")[,3]) 


#WLS (Fisher's z) with standard errors clustered at the study level
ses.WLS.NonFeldmann <- list(coef_test(pubbias_2_var_est_gts_NonFeldmann, vcov = "CR1", 
                                     cluster = dat_NonFeldmann$id, test = "naive-t")[,2]) 
tvals.WLS.NonFeldmann <- list(coef_test(pubbias_2_var_est_gts_NonFeldmann, vcov = "CR1", 
                                       cluster = dat_NonFeldmann$id, test = "naive-t")[,3]) 

#WLS (Fisher's z) with standard errors clustered at the study level
ses.WLS.NonInteractedEPL <- list(coef_test(pubbias_2_var_est_gts_NonInteractedEPL, vcov = "CR1", 
                                      cluster = dat_NonInteractedEPL$id, test = "naive-t")[,2]) 
tvals.WLS.NonInteractedEPL <- list(coef_test(pubbias_2_var_est_gts_NonInteractedEPL, vcov = "CR1", 
                                        cluster = dat_NonInteractedEPL$id, test = "naive-t")[,3]) 

#Robust regression with standard errors clustered at the study level
ses.Robust <- list(coeftest(Robust,vcov=NeweyWest(Robust, verbose=T))[,2]) 
tvals.Robust <- list(coeftest(Robust,vcov=NeweyWest(Robust, verbose=T))[,3]) 

#reproduction of table 3; note that the random effects results are not included in the stargazer output due to compatibility issues; results for column (2) are available above.
stargazer(pubbias_2_var_est_gts, Robust, pubbias_2_var_gts_Fisher, pubbias_2_var_est_gts_NonFeldmann, pubbias_2_var_est_gts_NonInteractedEPL, pubbias_2_var_est_gts_OECD, t=list(unlist(tvals.WLS.est), unlist(tvals.Robust), unlist(tvals.WLS.Fisher), unlist(tvals.WLS.NonFeldmann), unlist(tvals.WLS.NonInteractedEPL), unlist(tvals.WLS.OECD)), se=list(unlist(ses.WLS.est), unlist(ses.Robust), unlist(ses.WLS.Fisher), unlist(ses.WLS.NonFeldmann), unlist(ses.WLS.NonInteractedEPL), unlist(ses.WLS.OECD)))

#Supplementary appendix

#Figure A1
partialvector <- dat_long$PartialCorrelationCoefficient
h<-hist(partialvector, breaks=10, col="red", xlab="partial correlation coefficient", 
        main="Distribution of EPL-unemployment\n partial correlation coefficients") 
xfit<-seq(min(partialvector),max(partialvector),length=40) 
yfit<-dnorm(xfit,mean=mean(partialvector),sd=sd(partialvector)) 
yfit <- yfit*diff(h$mids[1:2])*length(partialvector) 
lines(xfit, yfit, col="blue", lwd=2)

#kernel density plot
d <- density(dat_long$PartialCorrelationCoefficient) # returns the density data 
plot(d) # plots the results

hist(partialvector,breaks = 10, freq=F,main="Distribution of partial correlations:\n EPL and unemployment\n (n=881)",xlab="partial correlation coefficient\n EPL-unemployment",ylab="density", ylim=c(0,3), xlim=c(-1,1))
lines(density(partialvector), col="red", lwd=2) 
#curve(dnorm(x, mean = mean(partialvector), sd = sd(partialvector)), add=TRUE, col="blue", lty="dotted")
density(partialvector)

#Figure A2

#excluding estimates that are not based on using a lagged EPL variable
plot_funnel_LaggedEPL <- ggplot(data=dat_LaggedEPL,
                                aes(x=PartialCorrelationCoefficient, y=PrecSE)) +
  geom_point() +
  xlab("Partial correlation coefficient") +
  ylab("Precision") +
  geom_point(aes(colour=factor(EPLtype))) +
  ggtitle("Lagged EPL variables only (n=262)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.text.y=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  guides(fill=guide_legend("ABC"))+
  theme(legend.position="bottom")+
  scale_color_discrete(name = "EPL variable")
plot_funnel_LaggedEPL

#excluding all estimates based on annual data (i.e. restriction on averaged or grouped data)
plot_funnel_YearAverage <- ggplot(data=dat_YearAverage,
                                  aes(x=PartialCorrelationCoefficient, y=PrecSE)) +
  geom_point() +
  xlab("Partial correlation coefficient") +
  ylab("Precision") +
  geom_point(aes(colour=factor(EPLtype))) +
  ggtitle("Averaged data only (n=150)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.text.y=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  guides(fill=guide_legend("ABC"))+
  theme(legend.position="bottom")+
  scale_color_discrete(name = "EPL variable")
plot_funnel_YearAverage

#excluding all EPL estimates where interaction terms are included
plot_funnel_NonInteractedEPL <- ggplot(data=dat_NonInteractedEPL,
                                       aes(x=PartialCorrelationCoefficient, y=PrecSE)) +
  geom_point() +
  xlab("Partial correlation coefficient") +
  ylab("Precision") +
  geom_point(aes(colour=factor(EPLtype))) +
  ggtitle("Interaction terms excluded (n=784)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.text.y=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  guides(fill=guide_legend("ABC"))+
  theme(legend.position="bottom")+
  scale_color_discrete(name = "EPL variable")
plot_funnel_NonInteractedEPL

#Cross Section only
plot_funnel_CrossSection <- ggplot(data=dat_CrossSection,
                                   aes(x=PartialCorrelationCoefficient, y=PrecSE)) +
  geom_point() +
  xlab("Partial correlation coefficient") +
  ylab("Precision") +
  geom_point(aes(colour=factor(EPLtype))) +
  ggtitle("Cross-section data only (n=35)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.text.y=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  guides(fill=guide_legend("ABC"))+
  theme(legend.position="bottom")+
  scale_color_discrete(name = "EPL variable")
plot_funnel_CrossSection

#OECDMethodologyOnly
plot_funnel_OECD <- ggplot(data=dat_OECD,
                           aes(x=PartialCorrelationCoefficient, y=PrecSE)) +
  geom_point() +
  xlab("Partial correlation coefficient") +
  ylab("Precision") +
  ggtitle("OECD's EPL methodology only (n=667)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.text.y=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  guides(fill=guide_legend("ABC"))+
  theme(legend.position="bottom")+
  scale_color_discrete(name = "EPL variable")
plot_funnel_OECD

#SurveyBasedOnly
plot_funnel_SurveyBased <- ggplot(data=dat_SurveyBased,
                                  aes(x=PartialCorrelationCoefficient, y=PrecSE)) +
  geom_point() +
  xlab("Partial correlation coefficient") +
  ylab("Precision") +
  ggtitle("Survey-based EPL only (n=82)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.text.y=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  guides(fill=guide_legend("ABC"))+
  theme(legend.position="bottom")+
  scale_color_discrete(name = "EPL variable")
plot_funnel_SurveyBased

grid.arrange(plot_funnel_OECD, plot_funnel_SurveyBased, plot_funnel_LaggedEPL, plot_funnel_YearAverage, plot_funnel_NonInteractedEPL, plot_funnel_CrossSection, nrow=3, ncol=2)
