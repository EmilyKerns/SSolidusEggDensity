# Density_analysis_RandomEffects 
# load libraries

library(readxl)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tidyverse)
library(modelr)
library(lme4)
library(lmerTest)
library(viridis)
library(patchwork)
library(ggtext)

# read in data
Density.2 <-read_excel("PATH/Density.plate.eggs_2.xlsx", sheet="Trimmed 2")


#### Egg Density vs Total Eggs
# Is our method of creating density treatments valid?
# We used volume of eggs to create density treatments. For volume to be a good proxy for density, we need to show that higher volume of egg solution is associated with a higher number of eggs. 

# make a plot
plot(Density.2$Density.micro.l,Density.2$`Total`,
     xlab="Egg Solution Volume (µl)",
     ylab="Total Eggs")

# run a linear model to test the effect of volume ("density") on total number of eggs
#add a column of log transformed total eggs
Density.2 <- Density.2%>% 
  mutate(
    Log.Total = log(Total)
  )

hist(Density.2$Total)
hist(Density.2$Log.Total)

ggplot(Density.2,aes(x= Density.micro.l,y= Log.Total)) +
  geom_point() +
  theme_cowplot() +
  geom_smooth(se = TRUE, method = "lm") +
  geom_point(size=2) +
  scale_color_viridis(discrete=TRUE) +
  xlab("Egg Solution Volume (µl)") +
  ylab("Log(Total Eggs)")

AllEggs <- ggplot(Density.2, aes(x=Density.micro.l, y=Log.Total, color = Cross)) +
  geom_point(position =  position_jitterdodge(jitter.width = 0.0001, jitter.height = 0.0001, dodge.width = 1), alpha= 0.1) +
  theme_classic() +
  geom_smooth(method = "lm", alpha = 0.2)+
  xlab("Egg Solution Volume (µl)") +
  ylab("Log(Total Eggs)") +
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "point", shape = "circle",
               size = 2, 
               position = position_dodge(width = 1)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width = .3,
               position = position_dodge(width = 1))+
  scale_color_viridis_d(option = "D") + 
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1, size = 5, fontface = "bold")
AllEggs

model1=lmer(Log.Total~ Density.micro.l+Cross + (1|Recorder),data = Density.2)
summary(model1) 
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Log.Total ~ Density.micro.l + Cross + (1 | Recorder)
#    Data: Density.2
# 
# REML criterion at convergence: 204.2
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.1615 -0.4965  0.1538  0.6460  2.4771 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Recorder (Intercept) 0.0000   0.0000  
#  Residual             0.2515   0.5015  
# Number of obs: 125, groups:  Recorder, 3
# 
# Fixed effects:
#                   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      4.327e+00  1.290e-01  1.190e+02  33.528  < 2e-16 ***
# Density.micro.l  3.800e-03  3.042e-04  1.190e+02  12.494  < 2e-16 ***
# CrossMyvatn3_1A -7.233e-01  1.324e-01  1.190e+02  -5.464 2.59e-07 ***
# CrossMyvatn3_1C -1.240e+00  1.448e-01  1.190e+02  -8.567 4.49e-14 ***
# CrossWalby_23_2  3.706e-01  1.448e-01  1.190e+02   2.560   0.0117 *  
# CrossWalbyBulk4 -8.864e-01  1.591e-01  1.190e+02  -5.573 1.59e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) Dnst.. CM3_1A CM3_1C CW_23_
# Dnsty.mcr.l -0.609                            
# CrssMyv3_1A -0.648  0.057                     
# CrssMyv3_1C -0.561  0.000  0.547              
# CrssWl_23_2 -0.569  0.014  0.548  0.500       
# CrssWlbyBl4 -0.530  0.033  0.500  0.455  0.455
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')


##### Calculate actual densitiy of each well from the model above that used volume as a proxy for number of eggs.

fitted_log_values <- fitted(model1)
fitted_total_eggs <- exp(fitted_log_values)

Density.2$predicted_log_total <- fitted_log_values
Density.2$predicted_total_eggs <- fitted_total_eggs

PredictedEggs <- ggplot(Density.2, aes(x=Density.micro.l, y=predicted_total_eggs, color = Cross)) +
  geom_point(position =  position_jitterdodge(jitter.width = 0.0001, jitter.height = 0.0001, dodge.width = 1), alpha= 0.1) +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Egg Solution Volume (µl)") +
  ylab("Predicted Number of Eggs/Well") +
  geom_smooth(method = "lm", alpha = .2)+
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "point", shape = "circle",
               size = 2, 
               position = position_dodge(width = 1)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width = .3,
               position = position_dodge(width = 1))+
  scale_color_viridis_d(option = "D") + 
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1, size = 5, fontface = "bold")
PredictedEggs


pdf(file = "PATH/Figure3.pdf",
    width = 4.5, 
    height = 6.5)

AllEggs + PredictedEggs + plot_layout(ncol = 1) + plot_annotation(caption = "<b>Figure 3: Density treatments based on egg solution volume.</b><br>\nA) There is a significant positive association between the log transformed total number of eggs counted in each photo to the volume of egg solution used to create density treatments. B) The total number of eggs in each well was calculated using as model \nassessing the number of eggs per photo as a funciton of egg solution volume.")&
  theme(plot.caption = element_textbox_simple(hjust = 0, halign = 0), plot.margin = margin(t = 5, r = 5, b = 20, l = 5, unit = "pt"))

dev.off()


#### Egg Density vs Proportion hatched

## Is density-dependent hatch rates an ancestral trait (i.e. present in both populations)?

# plot the proportion of eggs hatched by egg density
Density <- ggplot(Density.2, aes(x=predicted_total_eggs, y=Proportion.Hatched, color = Population)) +
  geom_point(position =  position_jitterdodge(jitter.width = 0.0001, jitter.height = 0.0001, dodge.width = 1), alpha= 0.1) +
  theme_classic() +
  xlab("Predicted Number of Eggs/Well") +
  ylab("Percent Hatched") +
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "point", shape = "circle",
               size = 2, 
               position = position_dodge(width = 1)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width = .3,
               position = position_dodge(width = 1))+
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1, size = 5, fontface = "bold")

Volume <- ggplot(Density.2, aes(x=Density.micro.l, y=Proportion.Hatched, color = Population)) +
  geom_point(position =  position_jitterdodge(jitter.width = 0.0001, jitter.height = 0.0001, dodge.width = 1), alpha= 0.1) +
  theme_classic() +
  xlab("Egg Solution (uL)") +
  ylab("Percent Hatched") +
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "point", shape = "circle",
               size = 2, 
               position = position_dodge(width = 1)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width = .3,
               position = position_dodge(width = 1))+
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1, size = 5, fontface = "bold")


# run a linear model to test the effect of egg density on hatch rates
df_Density.2 = lmer(Proportion.Hatched ~ predicted_total_eggs*Population +(1|Cross) + (1|Recorder), data = Density.2)
summary(df_Density.2) 
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Proportion.Hatched ~ predicted_total_eggs * Population + (1 |      Cross) + (1 | Recorder)
#    Data: Density.2
# 
# REML criterion at convergence: 883.4
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -2.48280 -0.60313  0.04972  0.53225  2.87734 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Cross    (Intercept) 172.49   13.134  
#  Recorder (Intercept)  29.58    5.439  
#  Residual              59.38    7.706  
# Number of obs: 125, groups:  Cross, 5; Recorder, 3
# 
# Fixed effects:
#                                         Estimate Std. Error         df t value Pr(>|t|)  
# (Intercept)                            53.140206  14.168525   1.844381   3.751   0.0728 .
# predicted_total_eggs                   -0.005158   0.010816 116.329217  -0.477   0.6343  
# PopulationMyvatn                       22.004714  17.811145   2.116119   1.235   0.3362  
# PopulationWalby                       -21.118892  17.138612   1.664919  -1.232   0.3638  
# predicted_total_eggs:PopulationMyvatn  -0.008165   0.024009 116.526355  -0.340   0.7344  
# predicted_total_eggs:PopulationWalby   -0.002827   0.013143 117.459670  -0.215   0.8300  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) prdc__ PpltnM PpltnW p__:PM
# prdctd_ttl_ -0.185                            
# PopltnMyvtn -0.792  0.147                     
# PopultnWlby -0.807  0.153  0.687              
# prdctd__:PM  0.083 -0.450 -0.158 -0.069       
# prdctd__:PW  0.177 -0.823 -0.146 -0.208  0.371

pdf(file = "PATH/Figure4.pdf",
    width = 4.5, 
    height = 6.5)

Volume + Density + plot_layout(ncol = 1) + plot_annotation(caption = "<b>Figure 4: The relationship between egg density and hatching rates.</b><br>\nThere is no relationship between volume of egg solution (A) or predicted density of eggs per well (B) and the proportion of hatched eggs.")&
  theme(plot.caption = element_textbox_simple(hjust = 0, halign = 0), plot.margin = margin(t = 5, r = 5, b = 20, l = 5, unit = "pt"))

dev.off()