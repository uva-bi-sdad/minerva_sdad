#1. ID 1o19pz9u: [Session 7] - WALL; LOCAL; Seq. 456; H-L Order PROBLEM WITH NAs in the RESPONSE VARIABLE
#2. ID zjb1wxmz: [Session 4] - WALL; GLOBAL; Seq. 456; H-L Order
#3. ID i2vtto7j: [Session 7] - WALL; LOCAL; Seq. 456; H-L Order
#4. ID jllmf4m8: [Session 14]- WAll; LOCAL; Seq. 123; H-L Order


library(readr)
#Session 4  WALL / GLOBAL / SEQUENCE GROUP 456
S4<-read_csv("~/sdal/projects/minerva/data/2018-06-13-ESSL/test_data_format/result-table-zjb1wxmz.csv")
View(S4)
S4<-S4[,c(3,4,7,9,15,14)] 
  names(S4)<-c("Response","NS","ID","Round","SQ","THR")
  S4<-S4[with(S4, order(Round,SQ)), ]
  S4$Response<-ifelse(S4$Response=="True",1,0)
  S4$ID<-as.character(S4$ID);table(S4$ID)
  S4$THR<-rep(rep(c("H5","H0","H4","H3","H2"), c(15,15,15,15,15)),3)
View(S4)
S4.sum<-data.frame(aggregate(Response~SQ+THR+NS+Round, data=S4, sum))
write.csv(S4.sum, "Session4WallGlobalSG456.csv")

#Session 7 WALL / LOCAL / SEQUENCE GROUP 456
S7<-read_csv("~/sdal/projects/minerva/data/2018-06-13-ESSL/test_data_format/result-table-i2vtto7j.csv")
View(S7)
S7<-S7[,c(3,4,7,9,15,14)] 
names(S7)<-c("Response","NS","ID","Round","SQ","THR")
S7<-S7[with(S7, order(Round,SQ)), ]
S7$Response<-ifelse(S7$Response=="True",1,0)
S7$ID<-as.character(S7$ID);table(S7$ID)
S7$THR<-rep(rep(c("H5","H0","H4","H3","H2"), c(15,15,15,15,15)),3)
View(S7)
S7.sum<-data.frame(aggregate(Response~SQ+THR+NS+Round, data=S7, sum))
write.csv(S7.sum, "Session7WallLocalSG456.csv")

#Session 14 WALL / LOCAL / SEQUENCE GROUP 123
S14<-read_csv("~/sdal/projects/minerva/data/2018-06-13-ESSL/test_data_format/result-table-jllmf4m8.csv")
View(S14)
S14<-S14[,c(3,4,7,9,15,14)] 
names(S14)<-c("Response","NS","ID","Round","SQ","THR")
S14<-S14[with(S14, order(Round,SQ)), ]
S14$Response<-ifelse(S14$Response=="True",1,0)
S14$ID<-as.character(S14$ID);table(S14$ID)
S14$THR<-rep(rep(c("H5","H0","H4","H3","H2"), c(15,15,15,15,15)),3)
View(S14)
S14.sum<-data.frame(aggregate(Response~SQ+THR+NS+Round, data=S14, sum))
write.csv(S14.sum, "Session14WallLocalSG123.csv")

WallGlobal456<-data.frame(S4.sum,Session7=S7.sum$Response)
names(WallGlobal456)<-c("SQ","THR","NS","Round","Session4","Session7")
write.csv(WallGlobal456, "WallGlobal456.csv")
View(WallGlobal456)