setwd("~/Documents/University/Projects/Glasshouse/Stats")
library("here")
here()

library("RXKCD")

getXKCD(which = "random", display = TRUE, html = FALSE, saveImg = FALSE)
#NMDS, Aplha and Beta diversity}
library("vegan")
library("dplyr")
library("lme4")
library("car")
library("emmeans")
library("ggplot2")
library("lmerTest")


itsdiv.df<-read.csv("all ftME.csv")
BASE<-read.csv("metadata ME.csv")
names(itsdiv.df)
#34,35,36,37, ,41,42,43,44,45,46,47,48,48,50,51,52,53,54,55,56,57,58
its.names<-itsdiv.df[,c(1:17)]
itsdiv.df<-itsdiv.df[,18:50]


##PREPARE DATA #transpose data
itsdiv.df.t<-as.data.frame(t(itsdiv.df))
#add col names
colnames(itsdiv.df.t)<-its.names$Class
colnames(itsdiv.df.t)
#add row names
rownames(itsdiv.df.t)
itsdiv.df.t$Sample_id<-substr(rownames(itsdiv.df.t),2,6)
itsdiv.df.t$Sample_id


########################
##alpha diversity 
######################
str(itsdiv.df.t)
options(max.print=100000000)
colnames(itsdiv.df.t)
rownames(itsdiv.df.t)

Simpson <- diversity(itsdiv.df.t[,-1096], "simpson")
Shannon <- diversity(itsdiv.df.t[,-1096], index = "shannon")
richness <- specnumber(itsdiv.df.t[,-1096])
richness1 <- exp(diversity(itsdiv.df.t[,-1096], index="shannon")) #evenness
Abundance <- apply(itsdiv.df.t[,-1096],1,sum)

#addto BASE dataset
Simpson<-as.data.frame(Simpson)
Simpson$Sample_id<-substr(rownames(Simpson),2,6)
Simpson$Shannon<-Shannon
Simpson$richness<-richness
Simpson$richness1<-richness1
Simpson$Abundance<-Abundance
summary(Simpson)
#join two datasets
BASE$Sample_id<-as.character(Simpson$Sample_id)
BASE.diversity<-left_join(BASE,Simpson)

#run GLM on new dataset
abund<-BASE.diversity%>%
  group_by(Harvest, Inoculum) %>%
  summarise(
    count=n(),
    mean=mean(Abundance),
    sd=sd(Abundance),
    se=sd(Abundance)/sqrt(length(Abundance))
  )
abund
write.table(abund, file = "~/Library/CloudStorage/OneDrive-TheUniversityofmelbourne/Documents/University/Projects/Glasshouse/Stats//Tax abund sym ME.csv")


rich<-BASE.diversity%>%
  group_by(Harvest, Inoculum) %>%
  summarise(
    count=n(),
    mean=mean(richness),
    sd=sd(richness),
    se=sd(richness)/sqrt(length(richness))
  )
rich
write.table(rich, file = "~/Library/CloudStorage/OneDrive-TheUniversityofmelbourne/Documents/University/Glasshouse/Stats//Tax rich sym ME.csv")

shan<-BASE.diversity%>%
  group_by(Harvest, Inoculum) %>%
  summarise(
    count=n(),
    mean=mean(Shannon),
    sd=sd(Shannon),
    se=sd(Shannon)/sqrt(length(Shannon))
  )
shan
write.table(shan, file = "~/Library/CloudStorage/OneDrive-TheUniversityofmelbourne/Documents/University/Glasshouse/Stats//Shan sym ME.csv")

model1 <-  aov(Simpson$Abundance ~ BASE$Inoculum + BASE$Harvest + BASE$Inoculum:BASE$Harvest)
summary(model1)
par(mfrow=c(2,2))
plot(model1)
emmip(model1, Inoculum ~ Harvest) 
tukey.two.waym1<-TukeyHSD(model1)
tukey.two.waym1

model2 <-  aov(Simpson$richness ~ BASE$Inoculum + BASE$Harvest + BASE$Inoculum:BASE$Harvest)
summary(model2)
plot(model2)
emmip(model2, Inoculum ~ Harvest) 
tukey.two.waym2<-TukeyHSD(model2)
tukey.two.waym2

model3 <- aov(Simpson$Shannon ~ BASE$Inoculum + BASE$Harvest + BASE$Inoculum:BASE$Harvest)
summary(model3)
plot(model3)
emmip(model3, Inoculum ~ Harvest) 
tukey.two.waym3<-TukeyHSD(model3)
tukey.two.waym3

model4 <-  aov(Simpson$richness1 ~ BASE$Inoculum + BASE$Harvest + BASE$Inoculum:BASE$Harvest)
summary(model4)
plot(model4)
emmip(model4, Inoculum ~ Harvest) 
tukey.two.waym4<-TukeyHSD(model4)
tukey.two.waym4



###########################
#beta diversity
###########################

#nmds 
nmds<-metaMDS(itsdiv.df.t[,-9892], distance = "bray")
nmds
stressplot(nmds)
plot(nmds, type="t",scaling = "symmetric")

site.scrs <- as.data.frame(scores(nmds, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Landuse = BASE$Landuse) #add grouping variable "Treatment" to dataframe
site.scrs <- cbind(site.scrs, Depth = BASE$Depth) #add grouping variable of cluster grouping to dataframe
#site.scrs <- cbind(site.scrs, Catchment = BASE.diversity$Catchment)
#site.scrs <- cbind(site.scrs, Site = rownames(site.scrs)) #add site names as variable if you want to display on plot

head(site.scrs)

nmds.plot <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Landuse), 
                 shape = factor(site.scrs$Depth)), size = 8)+ #adds site points to plot, shape determined by Depth, colour determined by Land-use
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Land-use", shape = "Depth")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12), axis.text = element_text(size = 10))+ # add legend at right of plot
  xlim(-0.75,0.75)+
  ylim(-0.75,0.75)


nmds.plot + labs(title = "a) All") + scale_colour_grey() #displays plot

#Catchment ANOSIM
colnames(BASE.diversity)
rownames(BASE.diversity)
str(BASE.diversity)
as.numeric(BASE.diversity[,5:33])
dis <- vegdist(itsdiv.df.t[,-33])
mod <- betadisper(dis, BASE.diversity$Catchment)
mod
anova(mod)
permutest(mod, pairwise = TRUE)
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)
plot(mod)
boxplot(mod)
##ANOSIM##
colnames(itsdiv.df.t)
dis <- vegdist(BASE.diversity[,17:33])
dis <- vegdist(itsdiv.df.t[,-33])
BASE.ano <- with(itsdiv.df.t, anosim(itsdiv.df.t[,-33], BASE.diversity$Treatment_old))
summary(BASE.ano)
plot(BASE.ano)
