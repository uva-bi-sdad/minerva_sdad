library(readxl)
#For sessions 2, 3, 5, 6, 8, 9, 12, 14, 16, 17, 21, 23
##############################################################
#ID jllmf4m8: [Session 14] - WAll; LOCAL; Seq. 123; H-L Order
S14<-read_excel("~/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION14_jllmf4m8/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S14<-S14[S14$participant._current_app_name=="survey_final",]; dim(S14)
S14<-S14[with(S14, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S14",dim(S14)[1]); CT<-rep("Wall",dim(S14)[1]); NK<-rep("Local",dim(S14)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(1,2,3),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5))
S14<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(1,dim(S14)[1])), SEQ=SEQ, NS=NS, S14)
##############################################################
#ID p5pcjkjc: [Session 2] - NONE; GLOBAL; Seq. 123; H-L Order
S2<-read_excel("~/sdal/projects/minerva/data/2018-09-19-VT/SESSION2_p5pcjkjc/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S2<-S2[S2$participant._current_app_name=="survey_final",]; dim(S2)
S2<-S2[with(S2, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S2",dim(S2)[1]); CT<-rep("None",dim(S2)[1]); NK<-rep("Global",dim(S2)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(1,2,3),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5))
S2<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(1,dim(S2)[1])), SEQ=SEQ, NS=NS, S2)
##############################################################
#ID vjny1l7l: [Session 12] - BILATERAL; GLOBAL; Seq. 123; H-L Order
S12<-read_excel("~/sdal/projects/minerva/data/2018-09-20-VT/SESSION12_vjny1l7l/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S12<-S12[S12$participant._current_app_name=="survey_final",]; dim(S12)
S12<-S12[with(S12, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S12",dim(S12)[1]); CT<-rep("Bilateral",dim(S12)[1]); NK<-rep("Global",dim(S12)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(1,2,3),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5))
S12<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(1,dim(S12)[1])), SEQ=SEQ, NS=NS, S12)
##############################################################
#ID 5uwv0c88: [Session 16] - NONE; LOCAL; Seq. 123; H-L Order
S16<-read_excel("~/sdal/projects/minerva/data/2018-09-20-VT/SESSION16_5uwv0c88/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S16<-S16[S16$participant._current_app_name=="survey_final",]; dim(S16)
S16<-S16[with(S16, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S16",dim(S16)[1]); CT<-rep("None",dim(S16)[1]); NK<-rep("Local",dim(S16)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(1,2,3),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5))
S16<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(1,dim(S16)[1])), SEQ=SEQ, NS=NS, S16)
##############################################################
#ID tgfavzdh: [Session 8] - BILATERAL; LOCAL; Seq. 123; H-L Order
S8<-read_excel("~/sdal/projects/minerva/data/2018-09-19-VT/SESSION8_tgfavzdh/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S8<-S8[S8$participant._current_app_name=="survey_final",]; dim(S8)
S8<-S8[with(S8, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S8",dim(S8)[1]); CT<-rep("Bilateral",dim(S8)[1]); NK<-rep("Local",dim(S8)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(1,2,3),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5))
S8<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(1,dim(S8)[1])), SEQ=SEQ, NS=NS, S8)
##############################################################
#ID xsrkblkf: [Session 9] - WALL; GLOBAL; Seq. 123; H-L Order
S9<-read_excel("~/sdal/projects/minerva/data/2018-09-19-VT/SESSION9_xsrkblkf/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S9<-S9[S9$participant._current_app_name=="survey_final",]; dim(S9)
S9<-S9[with(S9, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S9",dim(S9)[1]); CT<-rep("Wall",dim(S9)[1]); NK<-rep("Global",dim(S9)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(1,2,3),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5))
S9<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(1,dim(S9)[1])), SEQ=SEQ, NS=NS, S9)




#For sessions 1, 4, 7, 10, 11, 13, 15, 18, 19, 20, 22, 24
##############################################################
#ID zjb1wxmz: [Session 4] - WALL; GLOBAL; Seq. 456; H-L Order
S4<-read_excel("~/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION4_zjb1wxmz/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S4<-S4[S4$participant._current_app_name=="survey_final",]; dim(S4)
S4<-S4[with(S4, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S4",dim(S4)[1]); CT<-rep("Wall",dim(S4)[1]); NK<-rep("Global",dim(S4)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(4,5,6),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5))
S4<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(2,dim(S4)[1])), SEQ=SEQ, NS=NS, S4)
##############################################################
#ID i2vtto7j: [Session 7] - WALL; LOCAL; Seq. 456; H-L Order
S7<-read_excel("~/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION7_i2vtto7j/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S7<-S7[S7$participant._current_app_name=="survey_final",]; dim(S7)
S7<-S7[with(S7, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S7",dim(S7)[1]); CT<-rep("Wall",dim(S7)[1]); NK<-rep("Local",dim(S7)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(4,5,6),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5))
S7<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(2,dim(S7)[1])), SEQ=SEQ, NS=NS, S7)
##############################################################
#ID j0bmhbzp: [Session 22] - BILATERAL; LOCAL; Seq. 456; H-L Order
S22<-read_excel("~/sdal/projects/minerva/data/2018-08-13-ESSL/SESSION22_j0bmhbzp/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S22<-S22[S22$participant._current_app_name=="survey_final",]; dim(S22)
S22<-S22[with(S22, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S22",dim(S22)[1]); CT<-rep("Bilateral",dim(S22)[1]); NK<-rep("Local",dim(S22)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(4,5,6),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5))
S22<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(2,dim(S22)[1])), SEQ=SEQ, NS=NS, S22)
##############################################################
#ID 2o63t7n4: [Session 13] - BILATERAL; GLOBAL; Seq. 456; H-L Order
S13<-read_excel("~/sdal/projects/minerva/data/2018-08-13-ESSL/SESSION13_2o63t7n4/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S13<-S13[S13$participant._current_app_name=="survey_final",]; dim(S13)
S13<-S13[with(S13, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S13",dim(S13)[1]); CT<-rep("Bilateral",dim(S13)[1]); NK<-rep("Global",dim(S13)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(4,5,6),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5))
S13<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(2,dim(S13)[1])), SEQ=SEQ, NS=NS, S13)
##############################################################
#ID hzopjb8y: [Session 1] - NONE; LOCAL; Seq. 456; H-L Order
S1<-read_excel("~/sdal/projects/minerva/data/2018-08-21-ESSL/SESSION1_hzopjb8y/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S1<-S1[S1$participant._current_app_name=="survey_final",]; dim(S1)
S1<-S1[with(S1, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S1",dim(S1)[1]); CT<-rep("None",dim(S1)[1]); NK<-rep("Local",dim(S1)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(4,5,6),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5))
S1<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(2,dim(S1)[1])), SEQ=SEQ, NS=NS, S1)
##############################################################
#ID 8otz5sb0: [Session 10] - NONE; GLOBAL; Seq. 456; H-L Order
S10<-read_excel("~/sdal/projects/minerva/data/2018-09-15-CGU/SESSION10_8otz5sb0/main.xlsx")

#Make sure the data is ordered by round number (1-15) then participant session id (1-15)
S10<-S10[S10$participant._current_app_name=="survey_final",]; dim(S10)
S10<-S10[with(S10, order(subsession.round_number, group.id_in_subsession)), ]

#Create new variables Communication Type and Network Knowledge variables
Session<-rep("S10",dim(S10)[1]); CT<-rep("None",dim(S10)[1]); NK<-rep("Global",dim(S10)[1])
Period<-rep(c(1:15),rep(15,15))
SEQ<-rep(c(rep(c(4,5,6),c(5,5,5))),15)
NS<-c(rep(c(rep(c("Clique","Star","Circle"),rep(5,3))),5),
      rep(c(rep(c("Circle","Clique","Star"),rep(5,3))),5),
      rep(c(rep(c("Star","Circle","Clique"),rep(5,3))),5))
S10<-data.frame(Session=Session, CT=CT, NK=NK, Period=Period, SEQ_GRP=as.factor(rep(2,dim(S10)[1])), SEQ=SEQ, NS=NS, S10)


#All High-Low Sessions
HighLow<-data.frame(rbind(S1,S2,S4,S7,S8,S9,S10,S12,S13,S14,S16,S22))

High<-filter(HighLow, Period %in% c(1,6,11));High<-High[,c(1:7,26)]
 Low<-filter(HighLow, Period %in% c(2,7,12));Low<-Low[,c(1:7,26)]
write.csv(HighLow, "HighLow.csv")
write.csv(High, "High.csv")
write.csv(Low, "Low.csv")