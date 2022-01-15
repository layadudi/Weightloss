df = read.csv("weightLoss.csv") #Importing data

#install.packages("plyr")
library(plyr)
library(ggplot2)

#Renaming the columns using (Rename)
df2 = rename(df, c("X"="ID", "wl1" = "WeightLoss_month1", "wl2"="WeightLoss_month2", "wl3"="WeightLoss_month3", "se1"= "SelfEsteem_month1", "se2"="SelfEsteem_month2", "se3" = "SelfEsteem_month3"))
df2

#Melt function in R
library(reshape2)
wl.data = melt(df2,id.vars=c("ID", "group"), measure.vars = c("WeightLoss_month1", "WeightLoss_month2", "WeightLoss_month3" ) )

#Renaming 3rd & 4th column
wl.data = rename(wl.data, c("variable" = "WeightLoss_Month", "value" = "WeightLoss"))
wl.data

#Melt function for selfesteem
we.data = melt(df2, id.vars = c("ID", "group"), measure.vars = c("SelfEsteem_month1", "SelfEsteem_month2", "SelfEsteem_month3"))

#Renaming 3rd & 4th column
we.data = rename(we.data, c("variable" = "SelfEsteem_Month", "value" = "SelfEsteem_Score"))

b = we.data
we.data = we.data[-1] #Removing ID and group as common variables
we.data = we.data[-1]
we.data


#Combined dataset
data.long = cbind(wl.data, we.data)
data.long

#Use the Weight Loss (pounds) as a categorical data and get the weight loss frequencies by groups.
table(data.long$group, data.long$WeightLoss)

#DO HISTOGRAM QN
ggplot(data.long, aes(x=as.factor(WeightLoss), fill=group))+
  labs(x = "Weight in pounds", y = "Count", title = "Weight Loss by Group within 3 months")+
  geom_bar() +
  facet_grid (WeightLoss_Month ~ group)+
  geom_line(aes(y = SelfEsteem_Score, fill=group)) +
  geom_point(aes(y = SelfEsteem_Score, colour = "blue")) +
  theme(legend.position='bottom',panel.grid.major.x = element_blank(), 
               legend.key.size = unit(.7, "cm"))+
                
scale_fill_discrete(guide_legend(title ="Group")) 
               
 
#Use the "weightLoss.data" and the ggplot() function to reproduce the exact same scatterplotbelow.
ggplot(df2, aes(x= WeightLoss_month1, y = SelfEsteem_month1, color = group)) + 
  labs(x = "Weight Loss", y = "Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 1") +
  geom_point() +
  facet_wrap(~ group)
  

#Generate similar scatter plots for "Weight Loss vs Self-Esteem - Month 2" and "Weight Loss vs Self-Esteem - Month 3".
ggplot(df2, aes(x= WeightLoss_month2, y = SelfEsteem_month2, color = group)) + 
  labs(x = "Weight Loss", y = "Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 2") +
  geom_point() +
  facet_wrap(~ group)

ggplot(df2, aes(x= WeightLoss_month3, y = SelfEsteem_month3, color = group)) + 
  labs(x = "Weight Loss", y = "Self-Esteem Score", title = "Weight Loss vs. Self-Esteem - Month 3") +
  geom_point() + 
  facet_wrap(~ group)


#Write the R code that generates the following boxplot (Use ggplot() function)
ggplot(df2) + 
  geom_boxplot(aes(x=WeightLoss_month1, y=group)) + 
  geom_boxplot(aes(x=SelfEsteem_month1, y=group), fill= "chartreuse4") +
  labs(x = "Weight Loss         Self-Esteem Score", y = "Group", title = "Weight Loss vs. Self-Esteem - Month 1")

ggplot(df2) + 
  geom_boxplot(aes(x=WeightLoss_month2, y=group)) + 
  geom_boxplot(aes(x=SelfEsteem_month2, y=group), fill= "chartreuse4") +
  labs(x = "Weight Loss         Self-Esteem Score", y = "Group", title = "Weight Loss vs. Self-Esteem - Month 2")

ggplot(df2) + 
  geom_boxplot(aes(x=WeightLoss_month3, y=group)) + 
  geom_boxplot(aes(x=SelfEsteem_month3, y=group), fill= "chartreuse4") +
  labs(x = "Weight Loss         Self-Esteem Score", y = "Group", title = "Weight Loss vs. Self-Esteem - Month 3")

#How do you interpret the results provided in the graphs that you have generated. Please briefly explain your findings from these graphs.
#We can see a clear relationship between weight loss and self-esteem during the 3-month weightloss program. The groups are showed different patterns regarding self-esteem measurement, more weight loss is associated with higher self esteem.
#The DietEx group lost most weight and control group lost least weight in the first 2 months. Same pattern can be seen in the 3rd month. The longer into the program, lesser is the weight lost. 

#Add a new variable to the long.data, with the subjects's weight in kilograms (kg) (1 kg = 2.204pounds).
data.long$Weight_KG = data.long$WeightLoss*0.45372
data.long
