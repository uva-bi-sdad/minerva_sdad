library(readxl)
library(dplyr)

#
# Read in ---------------------------------------------------------------------
#

# i2vtto7j surveys
clicktracking_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/Clicktracking.xlsx")
instructions_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/instructions.xlsx")
main_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/main.xlsx")
practice_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/practice.xlsx")
surveyfinal_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/survey_final.xlsx")
surveyinitial_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/survey_initial.xlsx")
timespent_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/TimeSpent.xlsx")
wall_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/Wall.xlsx")
welcomeconsent_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/welcome_consent.xlsx")

# jllmf4m8 surveys
clicktracking_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/Clicktracking.xlsx")
instructions_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/instructions.xlsx")
main_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/main.xlsx")
practice_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/practice.xlsx")
surveyfinal_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/survey_final.xlsx")
surveyinitial_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/survey_initial.xlsx")
timespent_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/TimeSpent.xlsx")
wall_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/Wall.xlsx")
welcomeconsent_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/welcome_consent.xlsx")

# zjb1wxmz surveys
clicktracking_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/Clicktracking.xlsx")
instructions_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/instructions.xlsx")
main_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/main.xlsx")
practice_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/practice.xlsx")
surveyfinal_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/survey_final.xlsx")
surveyinitial_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/survey_initial.xlsx")
timespent_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/TimeSpent.xlsx")
wall_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/Wall.xlsx")
welcomeconsent_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/welcome_consent.xlsx")

# 2o63t7n4 surveys
clicktracking_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/Clicktracking.xlsx")
instructions_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/instructions.xlsx")
main_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/main.xlsx")
practice_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/practice.xlsx")
surveyfinal_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/survey_final.xlsx")
surveyinitial_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/survey_initial.xlsx")
timespent_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/TimeSpent.xlsx")
wall_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/Wall.xlsx")
welcomeconsent_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/welcome_consent.xlsx")

# j0bmhbzp surveys
clicktracking_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/Clicktracking.xlsx")
instructions_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/instructions.xlsx")
main_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/main.xlsx")
practice_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/practice.xlsx")
surveyfinal_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/survey_final.xlsx")
surveyinitial_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/survey_initial.xlsx")
timespent_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/TimeSpent.xlsx")
wall_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/Wall.xlsx")
welcomeconsent_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/welcome_consent.xlsx")

# hzopjb8y surveys
clicktracking_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/Clicktracking.xlsx")
instructions_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/instructions.xlsx")
main_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/main.xlsx")
practice_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/practice.xlsx")
surveyfinal_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/survey_final.xlsx")
surveyinitial_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/survey_initial.xlsx")
timespent_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/TimeSpent.xlsx")
wall_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/Wall.xlsx")
welcomeconsent_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/welcome_consent.xlsx")

# 8otz5sb0 surveys
clicktracking_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/Clicktracking.xlsx")
instructions_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/instructions.xlsx")
main_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/main.xlsx")
practice_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/practice.xlsx")
surveyfinal_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/survey_final.xlsx")
surveyinitial_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/survey_initial.xlsx")
timespent_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/TimeSpent.xlsx")
wall_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/Wall.xlsx")
welcomeconsent_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/welcome_consent.xlsx")

# xsrkblkf surveys
clicktracking_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/Clicktracking.xlsx")
instructions_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/instructions.xlsx")
main_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/main.xlsx")
practice_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/practice.xlsx")
surveyfinal_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/survey_final.xlsx")
surveyinitial_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/survey_initial.xlsx")
timespent_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/TimeSpent.xlsx")
wall_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/Wall.xlsx")
welcomeconsent_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/welcome_consent.xlsx")

# tgfavzdh surveys
clicktracking_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/Clicktracking.xlsx")
instructions_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/instructions.xlsx")
main_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/main.xlsx")
practice_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/practice.xlsx")
surveyfinal_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/survey_final.xlsx")
surveyinitial_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/survey_initial.xlsx")
timespent_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/TimeSpent.xlsx")
wall_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/Wall.xlsx")
welcomeconsent_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/welcome_consent.xlsx")

# p5pcjkjc surveys
clicktracking_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/Clicktracking.xlsx")
instructions_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/instructions.xlsx")
main_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/main.xlsx")
practice_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/practice.xlsx")
surveyfinal_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/survey_final.xlsx")
surveyinitial_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/survey_initial.xlsx")
timespent_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/TimeSpent.xlsx")
wall_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/Wall.xlsx")
welcomeconsent_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/welcome_consent.xlsx")

# 5uwv0c88 surveys
clicktracking_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/Clicktracking.xlsx")
instructions_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/instructions.xlsx")
main_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/main.xlsx")
practice_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/practice.xlsx")
surveyfinal_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/survey_final.xlsx")
surveyinitial_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/survey_initial.xlsx")
timespent_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/TimeSpent.xlsx")
wall_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/Wall.xlsx")
welcomeconsent_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/welcome_consent.xlsx")

# vjny1l7l surveys
clicktracking_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/Clicktracking.xlsx")
instructions_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/instructions.xlsx")
main_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/main.xlsx")
practice_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/practice.xlsx")
surveyfinal_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/survey_final.xlsx")
surveyinitial_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/survey_initial.xlsx")
timespent_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/TimeSpent.xlsx")
wall_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/Wall.xlsx")
welcomeconsent_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/welcome_consent.xlsx")

#
# Merge & append (player surveys only) ------------------------------------------------
#

intersect(names(surveyfinal_i2vtto7j), names(surveyinitial_i2vtto7j))

# Merge
player_i2vtto7j <- merge(surveyfinal_i2vtto7j, surveyinitial_i2vtto7j, by = c("participant.id_in_session","participant.code"))
player_jllmf4m8 <- merge(surveyfinal_jllmf4m8, surveyinitial_jllmf4m8, by = c("participant.id_in_session","participant.code"))
player_zjb1wxmz <- merge(surveyfinal_zjb1wxmz, surveyinitial_zjb1wxmz, by = c("participant.id_in_session","participant.code"))
player_2o63t7n4 <- merge(surveyfinal_2o63t7n4, surveyinitial_2o63t7n4, by = c("participant.id_in_session","participant.code"))
player_j0bmhbzp <- merge(surveyfinal_j0bmhbzp, surveyinitial_j0bmhbzp, by = c("participant.id_in_session","participant.code"))
player_hzopjb8y <- merge(surveyfinal_hzopjb8y, surveyinitial_hzopjb8y, by = c("participant.id_in_session","participant.code"))
player_8otz5sb0 <- merge(surveyfinal_8otz5sb0, surveyinitial_8otz5sb0, by = c("participant.id_in_session","participant.code"))
player_xsrkblkf <- merge(surveyfinal_xsrkblkf, surveyinitial_xsrkblkf, by = c("participant.id_in_session","participant.code"))
player_tgfavzdh <- merge(surveyfinal_tgfavzdh, surveyinitial_tgfavzdh, by = c("participant.id_in_session","participant.code"))
player_p5pcjkjc <- merge(surveyfinal_p5pcjkjc, surveyinitial_p5pcjkjc, by = c("participant.id_in_session","participant.code"))
player_5uwv0c88 <- merge(surveyfinal_5uwv0c88, surveyinitial_5uwv0c88, by = c("participant.id_in_session","participant.code"))
player_vjny1l7l <- merge(surveyfinal_vjny1l7l, surveyinitial_vjny1l7l, by = c("participant.id_in_session","participant.code"))

# Examine column name mismatches before appending (there are extra variables from survey_final in August and later experiments)
setdiff(names(player_2o63t7n4), names(player_i2vtto7j))

# Fixing by adding missing variables filled with NA to the four smaller dataframes
player_i2vtto7j$player.trust_1 <- NA
player_i2vtto7j$player.trust_2 <- NA
player_i2vtto7j$player.trust_3 <- NA
player_i2vtto7j$player.trust_4 <- NA
player_i2vtto7j$player.q13b_volunteer <- NA
player_i2vtto7j$player.q14_111_used_sitesXFacebook <- NA
player_i2vtto7j$player.q14_112_used_sitesXTwitter <- NA
player_i2vtto7j$player.q14_113_used_sitesXLinkedIn <- NA
player_i2vtto7j$player.q14_114_used_sitesXInstagram <- NA
player_i2vtto7j$player.q14_115_used_sitesXReddit <- NA
player_i2vtto7j$player.q14_116_used_sitesXWhatsApp <- NA
player_i2vtto7j$player.q14_117_used_sitesXMeetup <- NA
player_i2vtto7j$player.q14_118_used_sitesXNextdoor <- NA
player_i2vtto7j$player.q14_119_used_sitesXSnapchat <- NA
player_i2vtto7j$player.q14_120_used_sitesXWeibo <- NA
player_i2vtto7j$player.q14_121_used_sitesXWeChat <- NA

player_jllmf4m8$player.trust_1 <- NA
player_jllmf4m8$player.trust_2 <- NA
player_jllmf4m8$player.trust_3 <- NA
player_jllmf4m8$player.trust_4 <- NA
player_jllmf4m8$player.q13b_volunteer <- NA
player_jllmf4m8$player.q14_111_used_sitesXFacebook <- NA
player_jllmf4m8$player.q14_112_used_sitesXTwitter <- NA
player_jllmf4m8$player.q14_113_used_sitesXLinkedIn <- NA
player_jllmf4m8$player.q14_114_used_sitesXInstagram <- NA
player_jllmf4m8$player.q14_115_used_sitesXReddit <- NA
player_jllmf4m8$player.q14_116_used_sitesXWhatsApp <- NA
player_jllmf4m8$player.q14_117_used_sitesXMeetup <- NA
player_jllmf4m8$player.q14_118_used_sitesXNextdoor <- NA
player_jllmf4m8$player.q14_119_used_sitesXSnapchat <- NA
player_jllmf4m8$player.q14_120_used_sitesXWeibo <- NA
player_jllmf4m8$player.q14_121_used_sitesXWeChat <- NA

player_zjb1wxmz$player.trust_1 <- NA
player_zjb1wxmz$player.trust_2 <- NA
player_zjb1wxmz$player.trust_3 <- NA
player_zjb1wxmz$player.trust_4 <- NA
player_zjb1wxmz$player.q13b_volunteer <- NA
player_zjb1wxmz$player.q14_111_used_sitesXFacebook <- NA
player_zjb1wxmz$player.q14_112_used_sitesXTwitter <- NA
player_zjb1wxmz$player.q14_113_used_sitesXLinkedIn <- NA
player_zjb1wxmz$player.q14_114_used_sitesXInstagram <- NA
player_zjb1wxmz$player.q14_115_used_sitesXReddit <- NA
player_zjb1wxmz$player.q14_116_used_sitesXWhatsApp <- NA
player_zjb1wxmz$player.q14_117_used_sitesXMeetup <- NA
player_zjb1wxmz$player.q14_118_used_sitesXNextdoor <- NA
player_zjb1wxmz$player.q14_119_used_sitesXSnapchat <- NA
player_zjb1wxmz$player.q14_120_used_sitesXWeibo <- NA
player_zjb1wxmz$player.q14_121_used_sitesXWeChat <- NA

# Appending data (player only)
experiments <- bind_rows(player_i2vtto7j, player_jllmf4m8, player_zjb1wxmz, player_2o63t7n4, player_j0bmhbzp, player_hzopjb8y, player_8otz5sb0, 
                         player_xsrkblkf, player_tgfavzdh, player_p5pcjkjc, player_5uwv0c88, player_vjny1l7l)
names(experiments)
# View(experiments)


#
# Cleaning up global environment ------------------------------------------------
#

remove(list = c(ls(pattern = "i2vtto7j"), ls(pattern = "jllmf4m8"), ls(pattern = "zjb1wxmz"), ls(pattern = "2o63t7n4"), ls(pattern = "j0bmhbzp"), ls(pattern = "hzopjb8y"),
                ls(pattern = "8otz5sb0"), ls(pattern = "xsrkblkf"), ls(pattern = "tgfavzdh"), ls(pattern = "p5pcjkjc"), ls(pattern = "5uwv0c88"), ls(pattern = "vjny1l7l")))