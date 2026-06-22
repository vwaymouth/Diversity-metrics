setwd("~/filepath")
library("here")
here()

library("ggplot2")
library("dplyr")
library("ggThemeAssist")
library("scales")
library("ggpubr")
library("devtools")


#fungi.df<-read.csv("diversity1.csv")
fungi.df<-read.csv("diversity2.csv")

pd <- position_dodge(0.5) # move them .05 to the left and right
level_order <- c('P', 'RM')#makes sure graphs always show treatments in this order
font.size<-10*96/72
font.size.2<-4
margin.top<-0.3
margin.left<-0.2
colnames(fungi.df)

AbundEV<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=abundsymEVmean, color=Harvest, group=Harvest))
AbundEV1 <- AbundEV + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw()+ scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 4), 
                                expand = expansion(mult = c(0, 0.05)), 
                                limits = c(0, 100000)) + 
  xlab("Inoculum")+ ylab("Abundance")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=abundsymEVmean-abundsymEVse, ymax=abundsymEVmean+
                      abundsymEVse), position=pd, width=.1)+
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  scale_color_discrete(name="Harvest")+ 
  ggtitle("a)")
print(AbundEV1)

AbundKL<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=abundsymKLmean, color=Harvest, group=Harvest))
AbundKL1 <- AbundKL + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 4), 
                                 expand = expansion(mult = c(0, 0.05)), 
                                 limits = c(0, 100000)) +xlab("Inoculum")+ ylab("Abundance")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=abundsymKLmean-abundsymKLse, 
                    ymax=abundsymKLmean+abundsymKLse), position=pd, width=.1)+
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  scale_color_discrete(name="Harvest")+ 
  ggtitle("b) ")
print(AbundKL1)

AbundME<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=abundsymMEmean,color=Harvest, group=Harvest))
AbundME1 <- AbundME + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +theme_bw() +scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 4), 
                                             expand = expansion(mult = c(0, 0.05)), 
                                             limits = c(0, 105000)) +
  xlab("Inoculum")+ ylab("Abundance")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=abundsymMEmean-abundsymMEse, 
                    ymax=abundsymMEmean+abundsymMEse), 
                position=pd, width=.1)+
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  scale_color_discrete(name="Harvest")+ 
  ggtitle("c) ")
print(AbundME1)

RichEV<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=richsymEVmean, color=Harvest, group=Harvest))
RichEV1 <- RichEV + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 4), 
                                  expand = expansion(mult = c(0, 0.05)), 
                                  limits = c(min(fungi.df$richsymEVmean - fungi.df$richsymEVse) - 0.5, 
                                             max(fungi.df$richsymEVmean + fungi.df$richsymEVse) + 0.5)) +
  xlab("Inoculum")+ ylab("Richness")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=richsymEVmean-richsymEVse, ymax=richsymEVmean+
                      richsymEVse), position=pd, width=.1)+
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  scale_color_discrete(name="Harvest")+ 
  ggtitle("d)")
print(RichEV1)

RichKL<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=richsymKLmean, color=Harvest, group=Harvest))
RichKL1 <- RichKL + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 4), 
                                 expand = expansion(mult = c(0, 0.05)), 
                                 limits = c(min(fungi.df$richsymKLmean - fungi.df$richsymKLse) - 0.5, 
                                            max(fungi.df$richsymKLmean + fungi.df$richsymKLse) + 0.5)) +
  xlab("Inoculum")+ ylab("Richness")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=richsymKLmean-richsymKLse, 
                    ymax=richsymKLmean+richsymKLse), position=pd, width=.1)+
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  scale_color_discrete(name="Harvest")+ 
  ggtitle("e) ")
print(RichKL1)

RichME<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=richsymMEmean,color=Harvest, group=Harvest))
RichME1 <- RichME + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 4), 
                                 expand = expansion(mult = c(0, 0.05)), 
                                 limits = c(min(fungi.df$richsymMEmean - fungi.df$richsymMEse) - 0.5, 
                                            max(fungi.df$richsymMEmean + fungi.df$richsymMEse) + 0.5)) + 
  xlab("Inoculum")+ ylab("Richness")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=richsymMEmean-richsymMEse, 
                    ymax=richsymMEmean+richsymMEse), 
                position=pd, width=.1)+
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  scale_color_discrete(name="Harvest")+ 
  ggtitle("f) ")
print(RichME1)

ShanEV<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=ShansymEVmean, color=Harvest, group=Harvest))
ShanEV1 <- ShanEV + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 4), 
                                 expand = expansion(mult = c(0, 0.05)), 
                                 limits = c(min(fungi.df$ShansymEVmean - fungi.df$ShansymEVse) - 0.1, 
                                            max(fungi.df$ShansymEVmean + fungi.df$ShansymEVse) + 0.1)) + 
  xlab("Inoculum")+ ylab("Shannon Index")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=ShansymEVmean-ShansymEVse, ymax=ShansymEVmean+
                      ShansymEVse), position=pd, width=.1)+
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  scale_color_discrete(name="Harvest")+ 
  ggtitle("g)")
print(ShanEV1)

ShanKL<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=ShansymKLmean, color=Harvest, group=Harvest))
ShanKL1 <- ShanKL + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 4), 
                                 expand = expansion(mult = c(0, 0.05)), 
                                 limits = c(min(fungi.df$ShansymKLmean - fungi.df$ShansymKLse) - 0.1, 
                                            max(fungi.df$ShansymKLmean + fungi.df$ShansymKLse) + 0.1)) + 
  xlab("Inoculum")+ ylab("Shannon Index")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=ShansymKLmean-ShansymKLse, 
                    ymax=ShansymKLmean+ShansymKLse), position=pd, width=.1)+
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  scale_color_discrete(name="Harvest")+ 
  ggtitle("h) ")
print(ShanKL1)

ShanME<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=ShansymMEmean,color=Harvest, group=Harvest))
ShanME1 <- ShanME + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 4), 
                                   expand = expansion(mult = c(0, 0.05)), 
                                   limits = c(min(fungi.df$ShansymMEmean - fungi.df$ShansymMEse) - 0.1, 
                                              max(fungi.df$ShansymMEmean + fungi.df$ShansymMEse) + 0.1)) + 
  xlab("Inoculum")+ ylab("Shannon Index")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=ShansymMEmean-ShansymMEse, 
                    ymax=ShansymMEmean+ShansymMEse), 
                position=pd, width=.1)+
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  scale_color_discrete(name="Harvest")+ 
  ggtitle("i) ")
print(ShanME1)


TM<-ggarrange(AbundEV1,AbundKL1,AbundME1,
              RichEV1, RichKL1, RichME1,
              ShanEV1,ShanKL1, ShanME1, 
              ncol = 3, nrow = 3, common.legend = TRUE, legend="bottom")
print(TM)
ggsave("diversity.tiff", plot = TM, width = 30, height = 24, dpi = 600, units = "cm") #saves 9 plots as .tiff in directory
###########
####Funguild
###########
colnames(fungi.df)

##Diversity by Trophic mode
ATMPath<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=Pathabundm, color=Harvest, group=Harvest))
ATMPath1 <- ATMPath + theme(axis.title.x=element_text(size=8),
                        axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma) +xlab("Inoculumulum")+ ylab("Abundance")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=Pathabundm-Pathabundse, ymax=Pathabundm+
                      Pathabundse), position=pd, width=.1)+
  scale_color_discrete(name="Harvest")  + 
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  ggtitle("a) Pathogens abund")
print(ATMPath1)

RTMPath<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=Pathrichm, color=Harvest, group=Harvest))
RTMPath1 <- RTMPath + theme(axis.title.x=element_text(size=8),
                        axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma) +xlab("Inoculumulum")+ ylab("Richness")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=Pathrichm-Pathrichse, 
                    ymax=Pathrichm+Pathrichse), position=pd, width=.1)+
  scale_color_discrete(name="Harvest")+ 
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  ggtitle("e) Pathogens rich")
print(RTMPath1)

STMPath<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=Pathshanm,color=Harvest, group=Harvest))
STMPath1 <- STMPath + theme(axis.title.x=element_text(size=8),
                        axis.title.y=element_text(size=8)) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +xlab("Inoculumulum")+ ylab("Shannon Index")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=Pathshanm-Pathshanse, ymax=Pathshanm+Pathshanse), 
                position=pd, width=.1)+
  scale_color_discrete(name="Harvest")+ 
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  ggtitle("i) Pathogens shan")
print(STMPath1)

ATMSap<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=Sapabundm, color=Harvest, group=Harvest))
ATMSap1 <- ATMSap + theme(axis.title.x=element_text(size=8),
                                    axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma) +xlab("Inoculumulum")+ ylab("Abundance")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=Sapabundm-Sapabundse, ymax=Sapabundm+
                      Sapabundse), position=pd, width=.1)+
  scale_color_discrete(name="Harvest")  + 
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  ggtitle("b) Saprotroph abund")
print(ATMSap1)

RTMSap<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=Saprichm, color=Harvest, group=Harvest))
RTMSap1 <- RTMSap + theme(axis.title.x=element_text(size=8),
                                    axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma) +xlab("Inoculumulum")+ ylab("Richness")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=Saprichm-Saprichse, 
                    ymax=Saprichm+Saprichse), position=pd, width=.1)+
  scale_color_discrete(name="Harvest")+ 
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  ggtitle("f) Saprotroph rich")
print(RTMSap1)

STMSap<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=Sapshanm,color=Harvest, group=Harvest))
STMSap1 <- STMSap + theme(axis.title.x=element_text(size=8),
                                    axis.title.y=element_text(size=8)) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +xlab("Inoculumulum")+ ylab("Shannon Index")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=Sapshanm-Sapshanse, ymax=Sapshanm+Sapshanse), 
                position=pd, width=.1)+
  scale_color_discrete(name="Harvest")+ 
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  ggtitle("j) Saprotroph")
print(STMSap1)

ATMSym<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=Symabundm, color=Harvest, group=Harvest))
ATMSym1 <- ATMSym + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma) +xlab("Inoculumulum")+ ylab("Abundance")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=Symabundm-Symabundse, ymax=Symabundm+
                      Symabundse), position=pd, width=.1)+
  scale_color_discrete(name="Harvest")  + 
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  ggtitle("c) Symbiotroph")
print(ATMSym1)

RTMSym<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=Symrichm, color=Harvest, group=Harvest))
RTMSym1 <- RTMSym + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +scale_y_continuous(labels = comma) +xlab("Inoculumulum")+ ylab("Richness")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=Symrichm-Symrichse, 
                    ymax=Symrichm+Symrichse), position=pd, width=.1)+
  scale_color_discrete(name="Harvest")+ 
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  ggtitle("g) Symbiotroph")
print(RTMSym1)

STMSym<-ggplot(fungi.df, aes(x=factor(Inoculum, level = level_order ), y=Symshanm,color=Harvest, group=Harvest))
STMSym1 <- STMSym + theme(axis.title.x=element_text(size=8),
                          axis.title.y=element_text(size=8)) + 
  theme_bw() +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +xlab("Inoculumulum")+ ylab("Shannon Index")+
  geom_point(position=pd, size=3, shape=21, fill="white")+
  geom_errorbar(aes(ymin=Symshanm-Symshanse, ymax=Symshanm+Symshanse), 
                position=pd, width=.1)+
  scale_color_discrete(name="Harvest")+ 
  theme_pubr(base_size=font.size,  border=F, margin=F, legend="none")+  
  theme(plot.margin = margin(margin.top, 0, 0, margin.left, "in"))+
  ggtitle("k) Symbiotroph")
print(STMSym1)

## Panels
TM<-ggarrange(ATMPath1, ATMSap1, ATMSym1, AbundEV1,
              RTMPath1, RTMSap1, RTMSym1, AbundKL1,
              STMPath1, STMSap1, STMSym1, AbundME1,
                    ncol = 3, nrow = 3, common.legend = TRUE, legend="bottom")
print(TM)
ggsave("diversity.tiff", plot = TM, width = 30, height = 24, dpi = 600, units = "cm") #saves updated 9 plots as .tiff in directory

AbundComp<-ggarrange(AAsco1, ABasi1, AGlom1, AMuc1, ARoze1, ARare1,
                     ncol = 2, nrow = 3, common.legend = TRUE, legend="bottom")
print(AbundComp)

RichComp<-ggarrange(RAsco1, RBasi1, RGlom1, RMuc1, RRoze1, RRare1,
                    ncol = 2, nrow = 3, common.legend = TRUE, legend="bottom")
print(RichComp)

ShanComp<-ggarrange(SAsco1, SBasi1, SGlom1, SMuc1, SRoze1, SRare1,
                    ncol = 2, nrow = 3, common.legend = TRUE, legend="bottom")
print(ShanComp)
