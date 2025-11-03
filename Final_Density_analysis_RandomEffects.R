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
library(rsq)
library(emmeans)
library(car)

# read in data
Density.1 <-read_excel("R:/Scistocephalus/EggDensity_SaraEngel/Density.plate.eggs_2.xlsx", sheet="Trimmed 2")

Density.1$Clutch <- Density.1$Cross

#### Egg Density vs Total Eggs
# Is our method of creating density treatments valid?
# We used volume of eggs to create density treatments. For volume to be a good proxy for density, we need to show that higher volume of egg solution is associated with a higher number of eggs. 

# make a plot
plot(Density.1$Density.micro.l,Density.1$`Total`,
     xlab="Egg Solution Volume (µl)",
     ylab="Total Eggs")

# run a glm to test the effect of volume ("density") on total number of eggs

hist(Density.1$Total)

ggplot(Density.1,aes(x= Density.micro.l,y= Total)) +
  geom_point()+
  theme_cowplot() +
  geom_smooth(se = TRUE, method = "lm") +
  geom_point(size=2) +
  scale_color_viridis(discrete=TRUE) +
  xlab("Egg Solution Volume (µl)") +
  ylab("Total Eggs")

Density.2 <- Density.1 %>%
  mutate(
    Clutch = case_when(
      Clutch == "Myvatn3_1A" ~ "Myvatn_Clutch1",
      Clutch == "WalbyBulk4" ~ "Walby_Clutch1",
      Clutch == "Myvatn3_1C" ~ "Myvatn_Clutch2",
      Clutch == "Walby_23_2" ~ "Walby_Clutch2",
      Clutch == "EchoBulk2B" ~ "Echo_Clutch1",
      TRUE ~ "Yer missing something"  # A default value if none of the conditions match
    )
  )

AllEggs <- ggplot(Density.2, aes(x=Density.micro.l, y=log(Total), color = Clutch)) +
  #geom_point(position =  position_jitterdodge(jitter.width = 25, jitter.height = 0.2, dodge.width = 15), alpha= 0.3) +
  theme_classic() +
  geom_smooth(method = "lm", alpha = 0.2, size = .5)+
  xlab("Egg Solution Volume (µl)") +
  ylab("ln(Eggs per photo)") +
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "point", shape = "circle",
               size = 2, 
               position = position_dodge(width = 15)) +
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar", width = .3,
               position = position_dodge(width = 15))+
  scale_color_viridis_d(option = "D") 
  #annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1, size = 5, fontface = "bold")
AllEggs


### Does volume predict total number of eggs for each clutch?

model1<-lmer(log(Total)~ Density.micro.l * Clutch + (1|Recorder),data = Density.2)
plot(model1)
summary(model1) 
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(Total) ~ Density.micro.l * Clutch + (1 | Recorder)
#    Data: Density.2
# 
# REML criterion at convergence: 238.2
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.5775 -0.5834  0.1573  0.6407  2.4111 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Recorder (Intercept) 0.0000   0.0000  
#  Residual             0.2283   0.4778  
# Number of obs: 125, groups:  Recorder, 3
# 
# Fixed effects:
#                                      Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                         3.895e+00  1.859e-01  1.150e+02  20.949  < 2e-16 ***
# Density.micro.l                     5.472e-03  6.127e-04  1.150e+02   8.931 7.98e-15 ***
# ClutchMyvatn_Clutch1                 -7.426e-02  2.512e-01  1.150e+02  -0.296  0.76805    
# ClutchMyvatn_Clutch2                 -7.085e-01  2.629e-01  1.150e+02  -2.695  0.00810 ** 
# ClutchWalby_Clutch1                   7.289e-02  3.144e-01  1.150e+02   0.232  0.81708    
# ClutchWalby_Clutch2                   6.878e-01  2.556e-01  1.150e+02   2.691  0.00819 ** 
# Density.micro.l:ClutchMyvatn_Clutch1 -2.603e-03  8.849e-04  1.150e+02  -2.941  0.00396 ** 
# Density.micro.l:ClutchMyvatn_Clutch2 -2.058e-03  8.664e-04  1.150e+02  -2.375  0.01919 *  
# Density.micro.l:ClutchWalby_Clutch1  -3.858e-03  1.118e-03  1.150e+02  -3.452  0.00078 ***
# Density.micro.l:ClutchWalby_Clutch2  -1.216e-03  8.432e-04  1.150e+02  -1.442  0.15199    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) Dnst.. CrM_C1 CrM_C2 CrW_C1 CrW_C2 D..:CM_C1 D..:CM_C2 D..:CW_C1
# Dnsty.mcr.l -0.851                                                                 
# CrssMyvt_C1 -0.740  0.630                                                          
# CrssMyvt_C2 -0.707  0.602  0.523                                                   
# CrssWlby_C1 -0.591  0.503  0.438  0.418                                            
# CrssWlby_C2 -0.727  0.619  0.538  0.514  0.430                                     
# Dns..:CM_C1  0.589 -0.692 -0.864 -0.417 -0.349 -0.429                              
# Dns..:CM_C2  0.602 -0.707 -0.446 -0.851 -0.356 -0.438  0.490                       
# Dns..:CW_C1  0.467 -0.548 -0.345 -0.330 -0.876 -0.339  0.379     0.388             
# Dns..:CW_C2  0.619 -0.727 -0.458 -0.437 -0.366 -0.842  0.503     0.514     0.398   
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

rsq(model1) # 0.7664177
rsq(model1, adj = TRUE) # 0.7481373
confint(model1)
#                                         2.5 %      97.5 %
# .sig01                              0.000000000  0.1251706131
# .sigma                              0.406897734  0.5216013267
# (Intercept)                         3.542381814  4.2468826185
# Density.micro.l                     0.004311274  0.0066326047
# ClutchMyvatn_Clutch1                 -0.550287629  0.4018952736
# ClutchMyvatn_Clutch2                 -1.206717194 -0.2102108243
# ClutchWalby_Clutch1                  -0.522678934  0.6686147829
# ClutchWalby_Clutch2                   0.203536139  1.1721710650
# Density.micro.l:ClutchMyvatn_Clutch1 -0.004278950 -0.0009260058
# Density.micro.l:ClutchMyvatn_Clutch2 -0.003699450 -0.0004165932
# Density.micro.l:ClutchWalby_Clutch1  -0.005976285 -0.0017407605
# Density.micro.l:ClutchWalby_Clutch2  -0.002813398  0.0003814222

anova(model1)
# Type III Analysis of Variance Table with Satterthwaite's method
#                        Sum Sq Mean Sq NumDF DenDF  F value    Pr(>F)    
# Density.micro.l       29.9486 29.9486     1   115 131.1583 < 2.2e-16 ***
# Clutch                  6.8895  1.7224     4   115   7.5431 1.968e-05 ***
# Density.micro.l:Clutch  3.6663  0.9166     4   115   4.0140  0.004383 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1                 

#If interested in slope differences across clutches
Clutch.vol.int <- emtrends(model1, pairwise ~ Clutch, var = "Density.micro.l")
Clutch.vol.int

#If interested in slope differences across clutches for pearson correlation:
#i.e., clutches with higher densities should have higher clutch*volume slopes.
Clutch.vol.int <- emtrends(model1,  ~ Clutch, var = "Density.micro.l")
Clutch.vol.int
# Clutch         Density.micro.l.trend       SE    df  lower.CL upper.CL
# Echo_Clutch1                 0.00547 0.000613 114.0  0.004258  0.00669
# Myvatn_Clutch1               0.00287 0.000639 114.0  0.001604  0.00413
# Myvatn_Clutch2               0.00341 0.000613 114.0  0.002200  0.00463
# Walby_Clutch1                0.00161 0.000935 114.0 -0.000239  0.00347
# Walby_Clutch2                0.00426 0.000710  44.9  0.002825  0.00569

#intercepts
fixef(model1)
echo.intercept <- fixef(model1)[1] #3.894717
myv1.intercept <- fixef(model1)[3] + echo.intercept #3.820454
myv2.intercept <- fixef(model1)[4] + echo.intercept #3.186182
walby1.intercept <- fixef(model1)[5] + echo.intercept #3.967603
walby2.intercept <- fixef(model1)[6] + echo.intercept #4.582505



slope <- c(0.00547,0.00287,0.00341,0.00161,0.00426)
yint <- c(3.894717,3.820454,3.186182,3.967603,4.582505)
cor.test(slope, yint, method = "spearman")
#############


##### Calculate actual mean number of eggs/photo in each density treatment

Eggs <- Density.2 %>%
  group_by(Density.micro.l, Clutch) %>%
  summarise(
    count = n(),
    mean = mean(Total, na.rm = TRUE),
    sd = sd(Total, na.rm = TRUE),
    se=sd(Total)/sqrt(n()), 
    max = max(Total),
    min = min(Total)
  )
Eggs

Density.3 <- merge.data.frame(Density.2, Eggs, by = c("Clutch", "Density.micro.l"))

#Plot Figure 2
png(file = "Figure2.png",
    width = 4.5, 
    height = 4.5)

AllEggs 

dev.off()


#### Egg Density vs Proportion hatched

new <- select(Density.2, c('Clutch','Density.micro.l','Recorder'))

# predict number of eggs/treatment based on glm

predict_results <- predict(model1, new, type="response", se.fit = TRUE)
Density.2$predict <- predict_results$fit
Density.2$predict_se <- predict_results$se.fit
Density.2$predict_lower <- predict_results$fit - 1.96 * predict_results$se.fit
Density.2$predict_upper <- predict_results$fit + 1.96 * predict_results$se.fit

Density.2 <- Density.2 %>%
  mutate(
    Type = case_when(
      Clutch == "Myvatn_Clutch1" ~ "Paired",
      Clutch == "Walby_Clutch1" ~ "Bulk",
      Clutch == "Myvatn_Clutch2" ~ "Paired",
      Clutch == "Walby_Clutch2" ~ "Paired",
      Clutch == "Echo_Clutch1" ~ "Bulk",
      TRUE ~ "Yer missing something"  # A default value if none of the conditions match
    ),
    Type = factor(Type, levels = c("Paired", "Bulk"))
  )


Density_summary <- Density.2 %>%
  group_by(Density.micro.l, Clutch) %>%
  summarise(
    predict = first(predict),  
    predict_lower = first(predict_lower),  # Fixed: get the actual lower bound
    predict_upper = first(predict_upper),  # Fixed: get the actual upper bound
    mean_prop = mean(Proportion.Hatched, na.rm = TRUE),
    se_prop = sd(Proportion.Hatched, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )
head(Density_summary)

Density_summary.2 <- Density_summary %>%
  mutate(
    Type = case_when(
      Clutch == "Myvatn_Clutch1" ~ "Paired",
      Clutch == "Walby_Clutch1" ~ "Bulk",
      Clutch == "Myvatn_Clutch2" ~ "Paired",
      Clutch == "Walby_Clutch2" ~ "Paired",
      Clutch == "Echo_Clutch1" ~ "Bulk",
      TRUE ~ "Yer missing something"  # A default value if none of the conditions match
    ),
    Type = factor(Type, levels = c("Paired", "Bulk"))
  )

# Does egg density affect hatch rate?
df_Density.only = lmer(Proportion.Hatched ~ predict + (1|Clutch) + (1|Recorder), data = Density.2)
plot(df_Density.only)
summary(df_Density.only) 
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Proportion.Hatched ~ predict + (1 | Clutch) + (1 | Recorder)
#    Data: Density.2
# 
# REML criterion at convergence: 881.7
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -2.44490 -0.69594  0.03117  0.53547  2.90799 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Clutch    (Intercept) 532.16   23.069  
#  Recorder (Intercept)  19.98    4.469  
#  Residual              58.94    7.677  
# Number of obs: 125, groups:  Clutch, 5; Recorder, 3
# 
# Fixed effects:
#             Estimate Std. Error      df t value Pr(>|t|)   
# (Intercept)   57.394     12.146   6.918   4.725  0.00221 **
# predict       -1.299      1.181 120.113  -1.099  0.27378   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#         (Intr)
# predict -0.465

anova(df_Density.only)
# Type III Analysis of Variance Table with Satterthwaite's method
#         Sum Sq Mean Sq NumDF  DenDF F value Pr(>F)
# predict 71.244  71.244     1 120.11  1.2088 0.2738


# Does hatch rate vary by population?
df_Density.pop = lmer(Proportion.Hatched ~ Population + (1|Clutch) + (1|Recorder), data = Density.2)
plot(df_Density.pop)
summary(df_Density.pop) 
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: Proportion.Hatched ~ Population + (1 | Clutch) + (1 | Recorder)
#    Data: Density.2
# 
# REML criterion at convergence: 864
# 
# Scaled residuals: 
#      Min       1Q   Median       3Q      Max 
# -2.56196 -0.71554  0.01809  0.55530  2.98169 
# 
# Random effects:
#  Groups   Name        Variance Std.Dev.
#  Clutch    (Intercept)  57.01    7.550  
#  Recorder (Intercept) 116.00   10.770  
#  Residual              58.81    7.669  
# Number of obs: 125, groups:  Clutch, 5; Recorder, 3
# 
# Fixed effects:
#                  Estimate Std. Error       df t value Pr(>|t|)  
# (Intercept)       49.7780    10.6150   1.7182   4.689   0.0565 .
# PopulationMyvatn  31.6158    13.0583   0.4339   2.421   0.4423  
# PopulationWalby  -18.2347    10.9457   0.2777  -1.666   0.6107  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr) PpltnM
# PopltnMyvtn -0.660       
# PopultnWlby -0.693  0.744

anova(df_Density.pop)

# Type III Analysis of Variance Table with Satterthwaite's method
#            Sum Sq Mean Sq NumDF DenDF F value  Pr(>F)  
# Population 1927.6   963.8     2     2  16.389 0.05751 .

#too few samples for posthoc (n=1 in Walby)

emmeans(df_Density.pop,"Population")
# Population emmean    SE   df lower.CL upper.CL
# Echo         49.8 11.44 2.99    13.30     86.3
# Myvatn       81.4 12.81 2.84    39.26    123.5
# Walby        31.5  8.71 2.84     2.95     60.1
# 
# Degrees-of-freedom method: kenward-roger 
# Confidence level used: 0.95 

#We only had a single population (Walby) where we tested both paired and bulk outcross hatching rates. 
#We do not report formal statistics because n=1 for each category, however we do note a large difference in hatching rate
#based on cross type in Walby eggs.

walby.data <- Density.2 %>% filter(Population == "Walby")
summary(walby.data)
Walby_outcross_summary <- Density.2 %>%
  group_by(Type) %>%
  summarise(  
    mean_prop = mean(Proportion.Hatched, na.rm = TRUE),
    se_prop = sd(Proportion.Hatched, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )
Walby_outcross_summary
# Type   mean_prop se_prop
# <fct>      <dbl>   <dbl>
# 1 Paired      64.7    1.72
# 2 Bulk        36.4    2.72


# plot the proportion of eggs hatched by egg density

Density_summary.2$Clutch <- as.factor(Density_summary.2$Clutch)
Density <- ggplot(Density_summary.2, aes(x=log(predict), y=Proportion.Hatched, shape = Clutch)) +
  theme_classic() +
  xlab("ln(Predicted Egg Density)") +
  ylab("Percent of Eggs Hatched") +
  scale_shape_manual(values = c("Echo_Clutch1" = 0, "Myvatn_Clutch1" = 1, "Myvatn_Clutch2" = 2,
                                "Walby_Clutch1"=5, "Walby_Clutch2"=6))+
  geom_errorbar(data = Density_summary,
                aes(x = predict, y = mean_prop, 
                    ymin = mean_prop - se_prop, 
                    ymax = mean_prop + se_prop),
                width = 0.3,
                position = position_dodge(width = 1)) +
  geom_errorbarh(data = Density_summary,
                aes(y = mean_prop, 
                    xmin = predict_lower, 
                    xmax = predict_upper),
                height = 0.3,
                position = position_dodge(width = 1)) +
  geom_point(data = Density_summary.2, 
             aes(x = predict, y = mean_prop,color = Type),
             size = 2.5,
             position = position_dodge(width = 1)) +
  labs(color = "Mating type") 
  #annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1, size = 5, fontface = "bold")
Density

png(file = "Figure3.png",
    width = 4.5, 
    height = 4.5)

Density 

dev.off()






