library(ggplot2)
library(ggpubr)
library(tidyverse)
library(ggpubr)
library(rstatix)


dat = read.csv("H:/LULC_change/Slope_13_23.csv")
attach(dat)
compare_means(Slope~Year, data = dat, method = "anova")
TukeyHSD(aov(Slope~as.factor(Year)))

stat_pvalue_manual(data =dat, label = NULL)

dat$Year<- factor(dat$Year)

set.seed(123)
dat%>% sample_n_by(Year, size = 2)

stat.test <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,   ~p.adj.signif,
  "2016",     "2013", 0.0000000, "***",
  "2019",     "2013", 0.1395886, "ns",
  "2023",     "2013", 0.0053882, "**",
  "2019",     "2016", 0.0000000, "***",
  "2023",     "2016", 0.0000000, "***",
  "2023",     "2019", 0.6709546, "ns"
)

stat.test

theme_set(
  theme_classic()+
    theme(legend.position = "right")
)

e<-ggplot(dat, aes( x = Year, y = Slope))
e+geom_violin(trim = FALSE, aes(fill = Year))+
  geom_boxplot(width = 0.1, color = "grey", alpha = 0.2)+
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult =1), 
    geom = "pointrange", color = "black")+
  stat_pvalue_manual(stat.test, y.position = 45,
                     step.increase = 0.1,label = "p.adj.signif")
  
