library(readxl)
library(dplyr)

# Reconstructing Vicki's dataset


#
# Read in all sessions ------------------------------------------------------------------------------------------------------------
#

# 2018-06-13 ESSL
main_i2vtto7j <- read_excel("/home/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION7_i2vtto7j/main.xlsx")
# main_1o19pz9u <- read_excel("/home/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION7_1o19pz9u/main.xlsx")  -- players in this one were advanced, discard
main_zjb1wxmz <- read_excel("/home/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION4_zjb1wxmz/main.xlsx")
main_jllmf4m8 <- read_excel("/home/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION14_jllmf4m8/main.xlsx")

# 2018-08-13 ESSL
main_j0bmhbzp <- read_excel("/home/sdal/projects/minerva/data/2018-08-13-ESSL/SESSION22_j0bmhbzp/main.xlsx")
main_2o63t7n4 <- read_excel("/home/sdal/projects/minerva/data/2018-08-13-ESSL/SESSION13_2o63t7n4/main.xlsx")

# 2018-08-21 ESSL
main_hzopjb8y <- read_excel("/home/sdal/projects/minerva/data/2018-08-21-ESSL/SESSION1_hzopjb8y/main.xlsx")
# main_cddue3e2 <- read_excel("/home/sdal/projects/minerva/data/2018-08-21-ESSL/SESSION10_cddue3e2/main.xlsx")   -- this session was cancelled, file is empty

# 2018-09-15 CGU
main_8otz5sb0 <- read_excel("/home/sdal/projects/minerva/data/2018-09-15-CGU/SESSION10_8otz5sb0/main.xlsx")

# 2018-09-19 VT
main_xsrkblkf <- read_excel("/home/sdal/projects/minerva/data/2018-09-19-VT/SESSION9_xsrkblkf/main.xlsx")
main_tgfavzdh <- read_excel("/home/sdal/projects/minerva/data/2018-09-19-VT/SESSION8_tgfavzdh/main.xlsx")
main_p5pcjkjc <- read_excel("/home/sdal/projects/minerva/data/2018-09-19-VT/SESSION2_p5pcjkjc/main.xlsx")

# 2018-09-20 VT
main_5uwv0c88 <- read_excel("/home/sdal/projects/minerva/data/2018-09-20-VT/SESSION16_5uwv0c88/main.xlsx")
main_vjny1l7l <- read_excel("/home/sdal/projects/minerva/data/2018-09-20-VT/SESSION12_vjny1l7l/main.xlsx")


#
# Add network structure (NS) variables by session before appending ------------------------------------------------------------------------------------------------------------
#

# 123: 1 = (CE-SR-CR), 2 = (SR-CR-CE), 3 = (CR-CE-SR)
# 456: 1 = (CE-CR-SR), 2 = (SR-CE-CR), 3 = (CR-SR-CE)

# SESSION7 i2vtto7j   wall local        456
# SESSION4 zjb1wxmz   wall global       456
# SESSION14 jllmf4m8  wall local        123
# SESSION22 j0bmhbzp  bilateral local   456
# SESSION13 2o63t7n4  bilateral global  456
# SESSION1 hzopjb8y   none local        456
# SESSION10 8otz5sb0  none global       456
# SESSION9 xsrkblkf   wall global       123
# SESSION8 tgfavzdh   bilateral local   123
# SESSION2 p5pcjkjc   none global       123
# SESSION16 5uwv0c88  none local        123
# SESSION12 vjny1l7l  bilateral global  123

# 123
p1 <- rep(c("clique", "star", "circle"), each = 5)
p2 <- rep(c("star", "circle", "clique"), each = 5)
p3 <- rep(c("circle", "clique", "star"), each = 5)

# 456
p4 <- rep(c("clique", "circle", "star"), each = 5)
p5 <- rep(c("star", "clique", "circle"), each = 5)
p6 <- rep(c("circle", "star", "clique"), each = 5)

##### 456 sessions
main_i2vtto7j <- main_i2vtto7j %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p5, p6, p4, p4, p5, p4, p6, p5, p6, p4, p4, p6, p5, p6, p5))

main_zjb1wxmz <- main_zjb1wxmz %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p5, p6, p4, p4, p5, p4, p6, p5, p6, p4, p4, p6, p5, p6, p5))

main_j0bmhbzp <- main_j0bmhbzp %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p5, p6, p4, p4, p5, p4, p6, p5, p6, p4, p4, p6, p5, p6, p5))

main_2o63t7n4 <- main_2o63t7n4 %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p5, p6, p4, p4, p5, p4, p6, p5, p6, p4, p4, p6, p5, p6, p5))

main_hzopjb8y <- main_hzopjb8y %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p5, p6, p4, p4, p5, p4, p6, p5, p6, p4, p4, p6, p5, p6, p5))

main_8otz5sb0 <- main_8otz5sb0 %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p5, p6, p4, p4, p5, p4, p6, p5, p6, p4, p4, p6, p5, p6, p5))

##### 123 sessions
main_jllmf4m8 <- main_jllmf4m8 %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p2, p1, p1, p3, p2, p1, p2, p3, p3, p2, p3, p1, p1, p3, p2))

main_xsrkblkf <- main_xsrkblkf %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p2, p1, p1, p3, p2, p1, p2, p3, p3, p2, p3, p1, p1, p3, p2))

main_tgfavzdh <- main_tgfavzdh %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p2, p1, p1, p3, p2, p1, p2, p3, p3, p2, p3, p1, p1, p3, p2))

main_p5pcjkjc <- main_p5pcjkjc %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p2, p1, p1, p3, p2, p1, p2, p3, p3, p2, p3, p1, p1, p3, p2))

main_5uwv0c88 <- main_5uwv0c88 %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p2, p1, p1, p3, p2, p1, p2, p3, p3, p2, p3, p1, p1, p3, p2))

main_vjny1l7l <- main_vjny1l7l %>%
  arrange(participant.id_in_session, subsession.round_number) %>%
  mutate(NS = c(p2, p1, p1, p3, p2, p1, p2, p3, p3, p2, p3, p1, p1, p3, p2))


#
# Append session main data files & clean ------------------------------------------------------------------------------------------------------------
#

# Append
main <- do.call("rbind", mget(ls(pattern = "^main_*")))

# Clean up workspace
remove(list = c(ls(pattern = "main_")))
remove(list = c(ls(pattern = "p")))

# Remove empty/irrelevant variables
main <- main %>%
  select(-participant.label, -participant._is_bot, -participant._index_in_pages, -participant._max_page_index, -participant._current_app_name, -player.user_name, -player.avatar_id,
         -participant._round_number, -participant._current_page_name, -participant.ip_address, -participant.time_started, -participant.visited, -contains("mturk"), -session.comment, -session.is_demo)

#
# Add communication type (CT) variable, do sanity check ------------------------------------------------------------------------------------------------------------
#

# SESSION7 i2vtto7j   wall local
# SESSION4 zjb1wxmz   wall global
# SESSION14 jllmf4m8  wall local
# SESSION22 j0bmhbzp  bilateral local
# SESSION13 2o63t7n4  bilateral global
# SESSION1 hzopjb8y   none local
# SESSION10 8otz5sb0  none global
# SESSION9 xsrkblkf   wall global
# SESSION8 tgfavzdh   bilateral local
# SESSION2 p5pcjkjc   none global
# SESSION16 5uwv0c88  none local
# SESSION12 vjny1l7l  bilateral global

main$CT <- NA
main$CT[main$session.code %in% c("i2vtto7j", "zjb1wxmz", "jllmf4m8", "xsrkblkf")] <- "wall"
main$CT[main$session.code %in% c("j0bmhbzp", "2o63t7n4", "tgfavzdh", "vjny1l7l")] <- "bilateral"
main$CT[main$session.code %in% c("hzopjb8y", "8otz5sb0", "p5pcjkjc", "5uwv0c88")] <- "none"
main$CT <- ordered(main$CT, c("none", "wall", "bilateral"))

table(main$session.code, main$CT)


#
# Add network knowledge (NK) variable, do sanity check ------------------------------------------------------------------------------------------------------------
#

main$NK <- NA
main$NK[main$session.code %in% c("i2vtto7j", "jllmf4m8", "j0bmhbzp", "hzopjb8y", "tgfavzdh", "5uwv0c88")] <- "local"
main$NK[main$session.code %in% c("zjb1wxmz", "2o63t7n4", "8otz5sb0", "xsrkblkf", "p5pcjkjc", "vjny1l7l")] <- "global"
main$NK <- ordered(main$NK, c("local", "global"))

table(main$session.code, main$NK)



#
# Add session (Session) variable, do sanity check ------------------------------------------------------------------------------------------------------------
#

main$Session <- NA
main$Session[main$session.code %in% c("i2vtto7j")] <- 7
main$Session[main$session.code %in% c("zjb1wxmz")] <- 4
main$Session[main$session.code %in% c("jllmf4m8")] <- 14
main$Session[main$session.code %in% c("j0bmhbzp")] <- 22
main$Session[main$session.code %in% c("2o63t7n4")] <- 13
main$Session[main$session.code %in% c("hzopjb8y")] <- 1
main$Session[main$session.code %in% c("8otz5sb0")] <- 10
main$Session[main$session.code %in% c("xsrkblkf")] <- 9
main$Session[main$session.code %in% c("tgfavzdh")] <- 8
main$Session[main$session.code %in% c("p5pcjkjc")] <- 2
main$Session[main$session.code %in% c("5uwv0c88")] <- 16
main$Session[main$session.code %in% c("vjny1l7l")] <- 12
main$Session <- as.integer(main$Session)

table(main$session.code, main$Session)


#
# Add subsession round number (Period) variable ------------------------------------------------------------------------------------------------------------
#

main$Period <- main$subsession.round_number


#
# Add sequence (SEQ) variable ------------------------------------------------------------------------------------------------------------
#

# 123: 1 = (CE-SR-CR), 2 = (SR-CR-CE), 3 = (CR-CE-SR)
# 456: 1 = (CE-CR-SR), 2 = (SR-CE-CR), 3 = (CR-SR-CE)

# SESSION7 i2vtto7j   wall local        456
# SESSION4 zjb1wxmz   wall global       456
# SESSION14 jllmf4m8  wall local        123
# SESSION22 j0bmhbzp  bilateral local   456
# SESSION13 2o63t7n4  bilateral global  456
# SESSION1 hzopjb8y   none local        456
# SESSION10 8otz5sb0  none global       456
# SESSION9 xsrkblkf   wall global       123
# SESSION8 tgfavzdh   bilateral local   123
# SESSION2 p5pcjkjc   none global       123
# SESSION16 5uwv0c88  none local        123
# SESSION12 vjny1l7l  bilateral global  123

# SEQ_GRP, SEQ, NS
# group.id_in_subsession



#
# Test ------------------------------------------------------------------------------------------------------------

High1<-filter(main, Period %in% c(1,6,11))
             
Temp1<-aggregate(player.participate~Session+CT+NK+as.factor(Period)+NS, data=High1, sum)
names(Temp1)<-c("Session","CT","NK","Period","NS","Players")
ALLh1<-Temp1[with(Temp1, order(Session,CT,NK,Period,NS)), ]

summary(aov(Players~CT+NK+Error(Session), data=ALLh1))
print(model.tables(aov(Players~CT*NK*Session, data=ALLh1), "means"), digits=2)

# Vicki
summary(aov(Players~CT+NK+Error(Session), data=ALLh))
print(model.tables(aov(Players~CT*NK*Session, data=ALLh), "means"), digits=2)
