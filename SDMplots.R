setwd("/Users/katebuckeridge/OneDrive - University of Edinburgh/R/UGrass/SDM")
setwd("C:/Users/kbuckeri/OneDrive - University of Edinburgh/R/UGrass/SDM") #at work

#Note: this a good page too: http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(lattice)
#http://colorbrewer2.org/ for colour schemes
#https://www.r-bloggers.com/high-resolution-figures-in-r/

#### abiotic (for SSP poster and ms)

abiotic <- read.csv("AbioticPoster.csv", header=TRUE, sep=",")

abiotic$Steri <- factor(abiotic$Steri, levels=abiotic$Steri)

#https://www.r-bloggers.com/high-resolution-figures-in-r/

########### ms and poster CN retention

limits <- aes(ymax=prop + se, ymin=prop - se)
pltCposter <- ggplot(data=abiotic, 
                     aes(x=necro, y=prop, fill=necro)) +
  geom_bar(position=position_dodge(width=0.9), 
           stat="identity") + 
  geom_errorbar(limits, 
                position=position_dodge(width=0.05), 
                width=0.2, 
                colour="#969696") +
  scale_fill_manual(name = "Added \n (unlabeled) \n necromass", 
                    labels=c("Control (none)", expression(italic("E.coli")), expression(italic("M.luteus")),expression(italic("S.cerevisiae"))),
                    values = c("#f0f9e8","#bae4bc","#7bccc4","#0868ac")) +
  scale_x_discrete(name=NULL, 
                   breaks=NULL,
                   labels=NULL)+
  scale_y_continuous(name="C retention (% of labeled necromass C)", 
                     limits =c(0,110),
                     breaks=c(0,20,40,60,80,100))+
  theme_bw( )  +
  theme(legend.text.align = 0) +
  facet_grid(.~Steri) 

pltCposter

limitsN <- aes(ymax=propN + seN, ymin=propN - seN)

pltNposter <- ggplot(data=abiotic, 
                     aes(x=necro, y=propN, fill=necro)) +
  geom_bar(position=position_dodge(width=0.9), 
           stat="identity") + 
  geom_errorbar(limitsN, 
                position=position_dodge(width=0.05), 
                width=0.2, 
                colour="#969696") +
  scale_fill_manual(name = "Added \n (unlabeled) \n necromass", 
                    labels=c("Control (none)", expression(italic("E.coli")), expression(italic("M.luteus")),expression(italic("S.cerevisiae"))),
                    values = c("#f0f9e8","#bae4bc","#7bccc4","#0868ac")) +
  scale_x_discrete(name=NULL, 
                   breaks=NULL,
                   labels=NULL)+
  scale_y_continuous(name="N retention (% of labeled necromass N)", 
                     limits =c(0,110),
                     breaks=c(0,20,40,60,80,100))+
  theme_bw( )  +
  theme(legend.text.align = 0) +
  facet_grid(.~Steri) 

pltNposter

Figxy <- ggarrange(pltCposter, pltNposter, 
                   ncol = 2, nrow = 1, widths = c(1,1), common.legend = TRUE, legend = "right", align = "h")

png(file="abioticPoster.png", units="in", width=6, height=3, res=300)
Figxy
dev.off()



##### live plot CO2 and MB and N2O

live <- read.csv("live.csv", header=TRUE, sep=",")
live2 <- live[c(1:12),]
limitsL <- aes(ymax=prop + se, ymin=prop - se)

plt1 <- ggplot(data=live2, 
               aes(x=Necro, y=prop, fill=Necro)) +
  geom_bar(position=position_dodge(width=0.9), 
           stat="identity") + 
  geom_errorbar(limitsL, 
                position=position_dodge(width=0.05), 
                width=0.2, 
                colour="#969696") +
  scale_fill_manual(name = "Added \n (unlabeled) \n necromass", 
                    labels=c("Control (none)", expression(italic("E.coli")), expression(italic("M.luteus")),expression(italic("S.cerevisiae"))),
                    values = c("#f0f9e8","#bae4bc","#7bccc4","#0868ac")) +
  scale_x_discrete(name=NULL, 
                   breaks=NULL,
                   labels=NULL)+
  scale_y_continuous(name="C or N loss or retention \n(% of labeled necromass C or N)", 
                     breaks=c(0,1,2,3,4,5,6,7,8,9,10))+
  #guides(fill=FALSE) +
  theme_bw( )  +
  theme(legend.text.align = 0) +
  facet_grid(.~Live) 
plt1

png(file="live.png", width=4.8, height=4, units='in', res=300)
plt1
dev.off()


#### cue

cue <- live[c(13:16),]
limitsC <- aes(ymax=prop + se, ymin=prop - se)

plt2 <- ggplot(data=cue, 
               aes(x=Necro, y=prop, fill=Necro)) +
  geom_bar(position=position_dodge(width=0.9), 
           stat="identity") + 
  geom_errorbar(limitsC, 
                position=position_dodge(width=0.05), 
                width=0.2, 
                colour="#969696") +
  scale_fill_manual(name = "Added \n (unlabeled) \n necromass", 
                    labels=c("Control (none)", expression(italic("E.coli")), expression(italic("M.luteus")),expression(italic("S.cerevisiae"))),
                    values = c("#f0f9e8","#bae4bc","#7bccc4","#0868ac")) +
  scale_x_discrete(name=NULL, 
                   breaks=NULL,
                   labels=NULL)+
  scale_y_continuous(name="Carbon use efficiency", 
                     breaks=c(0,0.2,0.4,0.6,0.8,1))+
  #guides(fill=FALSE) +
  theme_bw( )  +
  theme(legend.text.align = 0) +
  facet_grid(.~Live) 
plt2

png(file="cue.png", width=2, height=4, units='in', res=300)
plt2
dev.off()

Figxx <- ggarrange(plt1, plt2,
                   ncol = 2, nrow = 1, widths = c(2.25,1), common.legend = TRUE, legend = "right", align = "h")
png(file="liveSDM.png", units="in", width=6, height=3, res=300)
Figxx
dev.off()



