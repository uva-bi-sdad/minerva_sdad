library(dplyr)
#Analyzing the all High and all Low runs separately
#Creat3 the two data sets for analysis

##############################################################################################
#Analyze the all high runs for Number of Participants [0-5]
Temp<-aggregate(player.participate~Session+CT+NK+as.factor(Period)+as.factor(SEQ)+NS, data=High, sum)
names(Temp)<-c("Session","CT","NK","Period","SEQ","NS","Players")
ALLh<-Temp[with(Temp, order(Session,CT,NK,Period,SEQ,NS)), ]
#Create the prior NS variable
PriorNS<-as.factor(c("None",as.character(ALLh$NS)))
Replace<-seq(from=4,to=length(PriorNS)-2,by=3)
PriorNS[Replace]<-"None";PriorNS<-PriorNS[-length(PriorNS)]
#Create the variable for sequence group assigned to each session
SEQ_GRP<-as.factor(rep(c(1,2,1,1,2,2,1,2,1,2,2,1),rep(9,12)))
#Create the variable for the number of Groups evaluating the NS sequences
NO_SEQ_GRPS<-as.factor(c(rep(1:3,3),rep(4:6,3),rep(7:9,3),rep(10:12,3),rep(13:15,3),rep(16:18,3),rep(19:21,3),
                      rep(22:24,3),rep(25:27,3),rep(28:30,3),rep(31:33,3),rep(34:36,3)))
ALLh<-data.frame(ALLh,PriorNS=PriorNS,SEQ_GRP=SEQ_GRP,NO_SEQ_GRPS=NO_SEQ_GRPS,WithinSession=as.factor(c(1:108)))
rm(Temp);rm(PriorNS)

#Analysis for the Session (12 Sessions) Experimental Unit
summary(aov(Players~CT+NK+SEQ_GRP+Error(Session), data=ALLh))
print(model.tables(aov(Players~CT*NK*Session, data=ALLh), "means"), digits=2)

#Analysis for the Number of Sequence Groups (3x12=36 Sequences) Experimental Unit
summary(aov(Players~SEQ+Error(NO_SEQ_GRPS), data=ALLh))
print(model.tables(aov(Players~SEQ+NO_SEQ_GRPS, data=ALLh), "means"), digits=2)

#Analysis for Network Structure (3+3x12=108 Sequences) Experimental Unit
summary(aov(Players~SEQ+Period+NS+PriorNS+Error(WithinSession), data=ALLh))
print(model.tables(aov(Players~SEQ+Period+NS+PriorNS+WithinSession, data=ALLh), "means"), digits=2)

##############################################################################################
#Analyze the all low runs for Number of Participants [0-5]
Temp<-aggregate(player.participate~Session+CT+NK+as.factor(Period)+as.factor(SEQ)+NS, data=Low, sum)
names(Temp)<-c("Session","CT","NK","Period","SEQ","NS","Players")
ALLl<-Temp[with(Temp, order(Session,CT,NK,Period,SEQ,NS)), ]
#Create the prior NS variable
PriorNS<-as.factor(ALLl$NS)
#Create the variable for sequence group assigned to each session
SEQ_GRP<-as.factor(rep(c(1,2,1,1,2,2,1,2,1,2,2,1),rep(9,12)))
#Create the variable for the number of Groups evaluating the NS sequences
NO_SEQ_GRPS<-as.factor(c(rep(1:3,3),rep(4:6,3),rep(7:9,3),rep(10:12,3),rep(13:15,3),rep(16:18,3),rep(19:21,3),
                         rep(22:24,3),rep(25:27,3),rep(28:30,3),rep(31:33,3),rep(34:36,3)))
ALLl<-data.frame(ALLl,PriorNS=PriorNS,SEQ_GRP=SEQ_GRP,NO_SEQ_GRPS=NO_SEQ_GRPS,WithinSession=as.factor(c(1:108)))
rm(Temp);rm(PriorNS)

library(lme4)
library(lmerTest)
anova(lmer(Players~NK*CT+(1|Session)+NS*NK*CT+(1|NO_SEQ_GRPS), data=ALLh))


#Analysis for the Session (12 Sessions) Experimental Unit
summary(aov(Players~CT+NK+SEQ_GRP+Error(Session), data=ALLl))
print(model.tables(aov(Players~CT*NK*Session, data=ALLl), "means"), digits=2)

#Analysis for the Number of Sequence Groups (3x12=36 Sequences) Experimental Unit
summary(aov(Players~SEQ+Error(NO_SEQ_GRPS), data=ALLl))
print(model.tables(aov(Players~SEQ+NO_SEQ_GRPS, data=ALLl), "means"), digits=2)

#Analysis for Network Structure (3+3x12=108 Sequences) Experimental Unit
summary(aov(Players~SEQ+Period+NS+PriorNS+Error(WithinSession), data=ALLl))
print(model.tables(aov(Players~SEQ+Period+NS+PriorNS+WithinSession, data=ALLl), "means"), digits=2)


#Color Blind Palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)

library(ggplot2)
#Box plots for network structure
Combine1<-rbind(ALLl[,c(6,7)],ALLh[,c(6,7)])
PLOT1<-data.frame(Threshold=rep(c("Low","High"),c(108,108)),Combine1)
ggplot(PLOT1, aes(x=NS, y=Players)) + 
  scale_fill_manual(values=cbPalette[c(2,3)]) + scale_colour_manual(values=cbPalette[c(2,3)]) +
  geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=factor(Threshold)), 
              alpha=0.9, cex=3, show.legend=FALSE) +
  geom_boxplot(alpha=0.5, show.legend=FALSE, outlier.size=0, aes(fill=factor(Threshold))) + 
  facet_grid(.~Threshold) +
  labs(x="Network Structure", y="Number of Participants [0-5]") +
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=17, face="bold"), 
        axis.title.y=element_text(size=17, face="bold"),
        strip.text.x=element_text(size=17, face="bold"))

#Box plots for Communication type
Combine2<-rbind(ALLl[,c(2,7)],ALLh[,c(2,7)])
PLOT2<-data.frame(Threshold=rep(c("Low","High"),c(108,108)),Combine2)
ggplot(PLOT2, aes(x=CT, y=Players)) + 
  scale_fill_manual(values=cbPalette[c(2,3)]) + scale_colour_manual(values=cbPalette[c(2,3)]) +
  geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=factor(Threshold)), 
              alpha=0.9, cex=3, show.legend=FALSE) +
  geom_boxplot(alpha=0.5, show.legend=FALSE, outlier.size=0, aes(fill=factor(Threshold))) + 
  facet_grid(.~Threshold) +
  labs(x="Communication Type", y="Number of Participants [0-5]") +
  theme(axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=17, face="bold"), 
        axis.title.y=element_text(size=17, face="bold"),
        strip.text.x=element_text(size=17, face="bold"))

#Box plots for Communication type x Network STructure
PLOT3<-data.frame(PLOT1,CT=Combine2[,1])
CT_NS<-paste(Combine2$CT,":",PLOT1$NS)
PLOT3<-data.frame(PLOT3,CT_NS)
ggplot(PLOT3, aes(x=CT_NS, y=Players)) + 
  scale_fill_manual(values=cbPalette[c(2,3)]) + scale_colour_manual(values=cbPalette[c(2,3)]) +
  geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=factor(Threshold)), 
              alpha=0.9, cex=3, show.legend=FALSE) +
  geom_boxplot(alpha=0.5, show.legend=FALSE, outlier.size=0, aes(fill=factor(Threshold))) + 
  facet_grid(.~Threshold) +
  labs(x="Communication Type:Network Structure", y="Number of Participants [0-5]") +
  theme(axis.text.x=element_text(size=15, angle=45, hjust=0.75), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=17, face="bold"), 
        axis.title.y=element_text(size=17, face="bold"),
        strip.text.x=element_text(size=17, face="bold"))

#Box plots for Communication type x Network STructure
Combine3<-rbind(ALLl[,c(2,3,7)],ALLh[,c(2,3,7)])
PLOT4<-data.frame(Threshold=rep(c("Low","High"),c(108,108)),Combine3)
NK_CT<-paste(Combine3$NK,":",Combine3$CT)
PLOT4<-data.frame(PLOT4,NK_CT)
ggplot(PLOT4, aes(x=NK_CT, y=Players)) + 
  scale_fill_manual(values=cbPalette[c(2,3)]) + scale_colour_manual(values=cbPalette[c(2,3)]) +
  geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=factor(Threshold)), 
              alpha=0.9, cex=3, show.legend=FALSE) +
  geom_boxplot(alpha=0.5, show.legend=FALSE, outlier.size=0, aes(fill=factor(Threshold))) + 
  facet_grid(.~Threshold) +
  labs(x="Network Knowledge:Communication Type", y="Number of Participants [0-5]") +
  theme(axis.text.x=element_text(size=15, angle=45, hjust=0.75), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=17, face="bold"), 
        axis.title.y=element_text(size=17, face="bold"),
        strip.text.x=element_text(size=17, face="bold"))

#Box plots for Network knowledge
ggplot(PLOT4, aes(x=NK, y=Players)) + 
  scale_fill_manual(values=cbPalette[c(2,3)]) + scale_colour_manual(values=cbPalette[c(2,3)]) +
  geom_jitter(position=position_jitter(width=0.3, height=0.2), aes(colour=factor(Threshold)), 
              alpha=0.9, cex=3, show.legend=FALSE) +
  geom_boxplot(alpha=0.5, show.legend=FALSE, outlier.size=0, aes(fill=factor(Threshold))) + 
  facet_grid(.~Threshold) +
  labs(x="Network Knowledge", y="Number of Participants [0-5]") +
  theme(axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=17, face="bold"), 
        axis.title.y=element_text(size=17, face="bold"),
        strip.text.x=element_text(size=17, face="bold"))

 

