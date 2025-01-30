setwd("/Users/irisflores/Desktop/Variable exp")
d<-read.csv("VariableGrazing2.csv")
library(tidyverse)
library(readxl)
library(ggplot2)
library(sciplot)
library(ggmap)
library(mapdata)
library(readr)
library(dplyr)
install.packages("glmm")
library(glmm)
library(lme4)
library(ggplot2)

str(d)
d1<- d %>%
  mutate(
    K_S1 = as.numeric(K_S1),
    K_E1 = as.numeric(K_E1),
    Urchin_W_weight=as.numeric(Urchin_W_weight),
    Treatment=as.character(Treatment)
  )
str(d1)

d2<-d1 %>% #new column for grazing rate 
  mutate(Grazing1= K_S1-K_E1)

d2$Grazing1<- ifelse(d2$Grazing1< 0, 0,  d2$Grazing1) #makes all negative grazing rates zero

#row 85 urchin ate all remove
d3<-d2[-c(85,226,254,325,517,913,917,1016.1081,1165,1197,1232,1330,1345,1369,1371,1380,1381,1392,1418,1448,1473),]

d4<-d3%>%
  mutate(Graze1W=Grazing1/Urchin_B_Weight)

##DATA SET WITH THE COLUMNS WE NEED----
d5<-d4 %>%
  select(Tank,Header,Treatment, Urchin.ID,Graze1W, Dummy, Period, SubP_Event )
d5$Header <- as.factor(d5$Header)

d6<-d5%>%
  group_by(Header,Dummy,Treatment)%>%
  summarize(MeanGrazingbw=mean(Graze1W),stdGrazingwt=sd(Graze1W))

d6 %>% #plot with grazing rate by size on y and treatment on x-axis
  ggplot(mapping = aes(x=Dummy, y=MeanGrazingbw,color=Treatment))+ geom_point()+ scale_x_continuous(breaks = seq(0, 7, by = 1)) # Change increments

d7<-d6 %>% #plot with grazing rate by size on y and treatment on x-axis
  ggplot(mapping = aes(x=MeanGrazingbw))+ geom_histogram()+
  facet_wrap(~Dummy)
  
d7

library(ggpubr)
ggqqplot(d6$MeanGrazingbw)

#GLMM------
  model1<-glmer(MeanGrazingbw~Treatment+Dummy+ Dummy*Treatment+(1|Tank),data=d6)
  summary(model1)
  anova(model1)
###plot#####
d4%>%
  ggplot(mapping = aes(x=Period, y=Cumulative ,color=Treatment))+ geom_point()+facet_grid(.~Habitat)

  #sum residuals#### sum residuals#####
r1=lm(Cumulative~Treatment,data=m2)
plot(m2$Treatment, m2$Cumulative, pch = 19)
abline(r1)

r1=lm(Cumulative~Treatment,data=m2)
qqnorm(residuals(r1))
qqline(residuals(r1), col = "red")

r1=lm(Cumulative~Treatment,data=m2)
shapiro.test(residuals(r1))

r1=lm(Cumulative~Treatment,data=m2)
plot(residuals(r1) ~ predict(r1))


#use contrast


