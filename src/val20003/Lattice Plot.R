#Construct data set for latice plots that have the number of players on the
#vertical axis and the number of high threshold players on the horizontal axis
Temp1<-HighLow[,c(1:7,26)];
names(Temp1)<-c("Session","CT","NK","Period","SEQ_GRP","SEQ","NS","Players")
Temp2<-aggregate(Players~Session+Period+SEQ+NS, data=Temp1, sum)
FINAL<-Temp2[with(Temp2, order(Session,NS,Period,SEQ)), ]
FINAL<-data.frame(FINAL,High=c(rep(c(5,0,4,3,2),108)))
FINAL$NS<-as.factor(FINAL$NS);FINAL$Session<-as.factor(FINAL$Session)
FINAL$Session<-ordered(FINAL$Session,levels=
                       c("S1","S16","S2","S10","S8","S22","S12","S13","S7","S14","S9","S4"))

#Lattice plot Sessions (12) by Network Structures (3) for 36 plots
library(ggplot2)
LatticePLOT<-ggplot(FINAL, aes(High, Players)) +
  geom_point(shape=20, size=2) +
  facet_wrap(NS~Session, nrow=3) +
  scale_x_continuous("Number of High Thresholds") +
  scale_y_continuous("Number of Players") +
  geom_smooth(method="lm", color="blue") +
  theme(text = element_text(size=17))
LatticePLOT  
 
#Construct data sets to estimate the slopes and intercepts for each of the
#6 Communication Type x Network Knowledge combination for each of the Network
#Structures - a total of 6*3*2=36 parameter estimates
NL<-data.frame(rbind(FINAL[FINAL$Session=="S1",],FINAL[FINAL$Session=="S16",]))
  NL<-data.frame(CT=rep("None",dim(NL)[1]),NK=rep("Local",dim(NL)[1]),NL)
NG<-data.frame(rbind(FINAL[FINAL$Session=="S2",],FINAL[FINAL$Session=="S10",]))
  NG<-data.frame(CT=rep("None",dim(NL)[1]),NK=rep("Global",dim(NL)[1]),NG)
WL<-data.frame(rbind(FINAL[FINAL$Session=="S7",],FINAL[FINAL$Session=="S14",]))
  WL<-data.frame(CT=rep("Wall",dim(NL)[1]),NK=rep("Local",dim(NL)[1]),WL)
WG<-data.frame(rbind(FINAL[FINAL$Session=="S9",],FINAL[FINAL$Session=="S4",]))
  WG<-data.frame(CT=rep("Wall",dim(NL)[1]),NK=rep("Global",dim(NL)[1]),WG)
BL<-data.frame(rbind(FINAL[FINAL$Session=="S8",],FINAL[FINAL$Session=="S22",]))
  BL<-data.frame(CT=rep("Bilateral",dim(NL)[1]),NK=rep("Local",dim(NL)[1]),BL)
BG<-data.frame(rbind(FINAL[FINAL$Session=="S12",],FINAL[FINAL$Session=="S13",]))
  BG<-data.frame(CT=rep("Bilateral",dim(NL)[1]),NK=rep("Global",dim(NL)[1]),BG)
ParamEst<-data.frame(rbind(NL,NG,BL,BG,WL,WG))  
TRMT<-rep(c("NLCircle","NLClique","NLStar","NLCircle","NLClique","NLStar",
            "NGCircle","NGClique","NGStar","NGCircle","NGClique","NGStar",  
            "BLCircle","BLClique","BLStar","BLCircle","BLClique","BLStar",
            "BGCircle","BGClique","BGStar","BGCircle","BGClique","BGStar", 
            "WLCircle","WLClique","WLStar","WLCircle","WLClique","WLStar",
            "WGCircle","WGClique","WGStar","WGCircle","WGClique","WGStar"),
             rep(15,36)) 
ParamEst<-data.frame(TRMT=TRMT,ParamEst)

#analysis of covariance with 18 treatments one for each CT*NK*NS 
#combination to get the root mean square and degrees of freedom
#for the betahat model
   CK<-(lm(Players~-1+High+TRMT+High*TRMT, data=ParamEst))
DFRES<-CK$df.residual
 RMSE<-sqrt(sum(CK$residuals^2)/DFRES)

library(dplyr)
library(broom)
#Calculate the intercept, slope, and standard errors for the  
#18 regression lines
#None/Local
fitted_models<-NL %>% 
  group_by(NS) %>%              
  do(model=lm(Players~High, data = .)) 
NLparams<-fitted_models %>% tidy(model)
#None/Global
fitted_models<-NG %>% 
  group_by(NS) %>%              
  do(model=lm(Players~High, data = .)) 
NGparams<-fitted_models %>% tidy(model)
#Bilateral/Local
fitted_models<-BL %>% 
  group_by(NS) %>%              
  do(model=lm(Players~High, data = .)) 
BLparams<-fitted_models %>% tidy(model)
#Bilateral/Global
fitted_models<-BG %>% 
  group_by(NS) %>%              
  do(model=lm(Players~High, data = .)) 
BGparams<-fitted_models %>% tidy(model)
#Wall/Local
fitted_models<-WL %>% 
  group_by(NS) %>%              
  do(model=lm(Players~High, data = .)) 
WLparams<-fitted_models %>% tidy(model)
#Wall/Global
fitted_models<-WG %>% 
  group_by(NS) %>%              
  do(model=lm(Players~High, data = .)) 
WGparams<-fitted_models %>% tidy(model)

TRMT<-rep(c("NLCircle","NLClique","NLStar",
            "NGCircle","NGClique","NGStar",  
            "BLCircle","BLClique","BLStar",
            "BGCircle","BGClique","BGStar", 
            "WLCircle","WLClique","WLStar",
            "WGCircle","WGClique","WGStar"),rep(2,18)) 
NK<-rep(c("NONE","Bilateral","Wall"),rep(6,3))
CT<-rep(rep(c("Local","Global"),rep(3,2)),3)
NS<-rep(c("Circle","Clique","Star"),6)
  BETAHAT<-data.frame(TRMT=TRMT, rbind(NLparams,NGparams,BLparams,BGparams,WLparams,WGparams))
INTERCEPT<-BETAHAT[BETAHAT$term=="(Intercept)",c(1,4,5)]
INTERCEPT<-data.frame(NK=NK,CT=CT,NS=NS,
                      Upper=qt(0.975,df=28)*INTERCEPT$std.error, 
                      Lower=qt(0.025,df=28)*INTERCEPT$std.error, 
                      INTERCEPT)
    SLOPE<-BETAHAT[BETAHAT$term=="High",c(1,4,5)]
    SLOPE<-data.frame(NK=NK,CT=CT,NS=NS,
                      Upper=qt(0.975,df=28)*SLOPE$std.error, 
                      Lower=qt(0.025,df=28)*SLOPE$std.error, 
                      SLOPE)
    
#Null hypothesis the slopes are equal
bhat(SLOPE$estimate,SLOPE$std.error,rmse=RMSE,dfres=DFRES)
#Null hypothesis the intercepts are equal
bhat(INTERCEPT$estimate,INTERCEPT$std.error,rmse=RMSE,dfres=DFRES)





#Color Blind Palettes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...)
wheel(cbPalette)

library(ggplot2)
#Plot the slopes with a confidence interval
SLOPEci<-SLOPE[,c(1:5,7)]
  SLOPEci<-SLOPEci[with(SLOPEci, order(estimate, decreasing=TRUE)), ]
  ID<-paste(SLOPEci$CT, SLOPEci$NK,SLOPEci$NS)
  SLOPEci<-data.frame(SLOPEci, ID=ordered(ID, levels=ID))

ggplot(SLOPEci, aes(x=ID, y=estimate, colour=NS)) +
  scale_colour_manual(values=cbPalette) +
  geom_point(aes(x=ID, y=estimate), cex=5) +
  geom_hline(yintercept=0, linetype=3, colour="black") +
  geom_linerange(aes(x=ID, ymin=estimate+Lower,
                     ymax=estimate+Upper), colour=cbPalette[1], lwd=3) +
  geom_point(aes(x=ID, y=estimate), cex=5) +
  coord_flip() + theme_bw()  
  
  
  
  zp1 <- zp1 + ggtitle("Comparing several models")
  print(zp1) # The trick to these is position_dodge().
  
  
  
  
  ggplot(SLOPEci, aes(x=ID, y=estimate)) + 
    geom_errorbar(aes(ymin=estimate-ci, ymax=estimate+ci), colour="black", width=.1, position=estimate) +
    geom_line(position=pd) +
    geom_point(position=estimate, size=3) + 
    geom_hline(yintercept=0, linetype=2)
  
#plot the intercept versus the slope
#Circle
plot(INTERCEPT$estimate[seq(1,36,3)],SLOPE$estimate[seq(1,36,3)],
     pch=16,col=cbPalette[c(1:6)],cex=2,xlab="Intercept",ylab="Slope",
     ylim=c(-0.6,0.1),xlim=c(4.3,5.3))
#Clique
points(INTERCEPT$estimate[seq(2,36,3)],SLOPE$estimate[seq(2,36,3)],
     pch=17,col=cbPalette[c(1:6)],cex=2)
#Star
points(INTERCEPT$estimate[seq(3,36,3)],SLOPE$estimate[seq(3,36,3)],
     pch=8,col=cbPalette[c(1:6)],cex=2)

