library(readxl)
library(dplyr)
library(ggplot2)

#
# Read in and compile ------------------------------------------------------------------------------------------------------------
#

# Read in
main_s01 <- read_excel("/home/sdal/projects/minerva/data/2018-08-21-ESSL/SESSION1_hzopjb8y/main.xlsx")
main_s07 <- read_excel("/home/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION7_i2vtto7j/main.xlsx")
main_s13 <- read_excel("/home/sdal/projects/minerva/data/2018-08-13-ESSL/SESSION13_2o63t7n4/main.xlsx")
main_s22 <- read_excel("/home/sdal/projects/minerva/data/2018-08-13-ESSL/SESSION22_j0bmhbzp/main.xlsx")

# Appending main data
main <- bind_rows(main_s01, main_s07, main_s13, main_s22)

# Select variables
mainClean <- main %>%
  select(participant.id_in_session, participant.code, -participant.label, -participant._is_bot, -participant._index_in_pages, -participant._max_page_index, -participant._current_app_name, 
         -participant._round_number, -participant._current_page_name, -participant.ip_address, participant.time_started, -participant.visited, -contains("mturk"), participant.payoff, 
         player.id_in_group, player.node_id, player.threshold, player.participate, player.user_name, player.avatar_id, player.round_payoff, player.subsession_id, player.group_id, player.payoff, 
         group.id_in_subsession, group.network_id, group.subsession_id, subsession.round_number, session.code, session.label, session.experimenter_name, -session.comment, -session.is_demo)

# Cleaning up
remove(list = c(ls(pattern = "main_")))

#
# Add relevant variables ------------------------------------------------------------------------------------------------------------
#

## Add communication type variable, do sanity check
# i2vtto7j == wall
# 2o63t7n4 == bilateral
# j0bmhbzp == bilateral
# hzopjb8y == none

mainClean$ct <- NA
mainClean$ct[mainClean$session.code %in% c("i2vtto7j")] <- "wall"
mainClean$ct[mainClean$session.code %in% c("2o63t7n4", "j0bmhbzp")] <- "bilateral"
mainClean$ct[mainClean$session.code %in% c("hzopjb8y")] <- "none"
mainClean$ct <- ordered(mainClean$ct, c("none", "wall", "bilateral"))

table(mainClean$session.code, mainClean$ct)

## Add network knowledge variable, do sanity check
# i2vtto7j == local
# 2o63t7n4 == global
# j0bmhbzp == local
# hzopjb8y == local

mainClean$nk <- NA
mainClean$nk[mainClean$session.code %in% c("i2vtto7j", "j0bmhbzp", "hzopjb8y")] <- "local"
mainClean$nk[mainClean$session.code %in% c("2o63t7n4")] <- "global"
mainClean$nk <- ordered(mainClean$nk, c("local", "global"))

table(mainClean$session.code, mainClean$nk)


#
# Payoff ------------------------------------------------------------------------------------------------------------
#

############ 13 versus 22 (both bilateral, global versus local, 2o63t7n4 = global, j0bmhbzp = local)

## T = 1
pay_1322_t1 <- mainClean %>%
  filter(player.threshold == 1 & (session.code == "2o63t7n4" | session.code == "j0bmhbzp")) %>%
  group_by(session.code, subsession.round_number) %>%
  summarize(paymean = mean(player.round_payoff))
pay_1322_t1

pay_1322_t1plot <- ggplot(pay_1322_t1, aes(x = subsession.round_number, y = paymean, color = session.code)) + 
  geom_line() +
  geom_point() +
  labs(title = "Mean payoff by subsession when T = 1 - S13global vs S22local", x = "Subsession round number", y = "Mean payoff ($)", colour = "Session", caption = "N=30/round")   +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) + 
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  scale_color_manual(labels = c("S13global", "S22local"), values = c("coral", "darkblue"))
pay_1322_t1plot

## T = 3
pay_1322_t3 <- mainClean %>%
  filter(player.threshold == 3 & (session.code == "2o63t7n4" | session.code == "j0bmhbzp")) %>%
  group_by(session.code, subsession.round_number) %>%
  summarize(paymean = mean(player.round_payoff))
pay_1322_t3

pay_1322_t3plot <- ggplot(pay_1322_t3, aes(x = subsession.round_number, y = paymean, color = session.code, caption = "N=30/round")) + 
  geom_line() +
  geom_point() +
  labs(title = "Mean payoff by subsession when T = 3 - S13global vs S22local", x = "Subsession round number", y = "Mean payoff ($)", colour = "Session")   +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) + 
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  scale_color_manual(labels = c("S13global", "S22local"), values = c("coral", "darkblue"))
pay_1322_t3plot


############ 1 versus 7 versus 22 (all local, none versus wall versus bilateral, hzopjb8y = none, i2vtto7j = wall, j0bmhbzp = bilateral)

## T = 1
pay_1722_t1 <- mainClean %>%
  filter(player.threshold == 1 & (session.code == "hzopjb8y" | session.code == "i2vtto7j" | session.code == "j0bmhbzp")) %>%
  group_by(session.code, subsession.round_number) %>%
  summarize(paymean = mean(player.round_payoff))
pay_1722_t1

pay_1722_t1plot <- ggplot(pay_1722_t1, aes(x = subsession.round_number, y = paymean, color = session.code)) + 
  geom_line() +
  geom_point() +
  labs(title = "Mean payoff by subsession when T = 1 - S1none, S7wall, S22bilateral", x = "Subsession round number", y = "Mean payoff ($)", colour = "Session", caption = "N=30/round")   +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) + 
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  scale_color_manual(labels = c("S1none", "S7wall", "S22bilateral"), values = c("blue", "orange", "black"))
pay_1722_t1plot

## T = 3
pay_1722_t3 <- mainClean %>%
  filter(player.threshold == 3 & (session.code == "hzopjb8y" | session.code == "i2vtto7j" | session.code == "j0bmhbzp")) %>%
  group_by(session.code, subsession.round_number) %>%
  summarize(paymean = mean(player.round_payoff))
pay_1722_t3

pay_1722_t3plot <- ggplot(pay_1722_t3, aes(x = subsession.round_number, y = paymean, color = session.code, caption = "N=30/round")) + 
  geom_line() +
  geom_point() +
  labs(title = "Mean payoff by subsession when T = 3 - S1none, S7wall, S22bilateral", x = "Subsession round number", y = "Mean payoff ($)", colour = "Session")   +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) + 
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  scale_color_manual(labels = c("S1none", "S7wall", "S22bilateral"), values = c("blue", "orange", "black"))
pay_1722_t3plot


#
# Participation ------------------------------------------------------------------------------------------------------------
#

############ 13 versus 22 (both bilateral, global versus local, 2o63t7n4 = global, j0bmhbzp = local)

# Participated by round when treshold is 1
part_1322_t1 <- mainClean %>%
  filter(player.threshold == 1 &  (session.code == "2o63t7n4" | session.code == "j0bmhbzp"))  %>%
  group_by(session.code, subsession.round_number) %>%
  summarize(partmean = mean(player.participate))
part_1322_t1

part_1322_t1plot <- ggplot(part_1322_t1, aes(x = subsession.round_number, y = partmean, color = session.code)) + 
  geom_line() +
  geom_point() +
  labs(title = "Mean participation proportion T = 1 by round, S13local v S22global", x = "Round number", y = "Participation proportion", color = "Session")  +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  scale_color_manual(labels = c("S13global", "S22local"), values = c("coral", "darkblue"))
part_1322_t1plot

# Participated by round when treshold is 3
part_1322_t3 <- mainClean %>%
  filter(player.threshold == 3 &  (session.code == "2o63t7n4" | session.code == "j0bmhbzp"))  %>%
  group_by(session.code, subsession.round_number) %>%
  summarize(partmean = mean(player.participate))
part_1322_t3

part_1322_t3plot <- ggplot(part_1322_t3, aes(x = subsession.round_number, y = partmean, color = session.code)) + 
  geom_line() +
  geom_point() +
  labs(title = "Mean participation proportion T = 3 by round, S13local v S22global", x = "Round number", y = "Participation proportion", color = "Session")  +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  scale_color_manual(labels = c("S13global", "S22local"), values = c("coral", "darkblue"))
part_1322_t3plot


############ 1 versus 7 versus 22 (all local, none versus wall versus bilateral, hzopjb8y = none, i2vtto7j = wall, j0bmhbzp = bilateral)

# Participated by round when treshold is 1
part_1722_t1 <- mainClean %>%
  filter(player.threshold == 1 &  (session.code == "hzopjb8y" | session.code == "i2vtto7j" | session.code == "j0bmhbzp"))  %>%
  group_by(session.code, subsession.round_number) %>%
  summarize(partmean = mean(player.participate))
part_1722_t1

part_1722_t1plot <- ggplot(part_1722_t1, aes(x = subsession.round_number, y = partmean, color = session.code)) + 
  geom_line() +
  geom_point() +
  labs(title = "Participation proportion by subsession when T = 3 - S1none, S7wall, S22bilateral", x = "Subsession round number", y = "Mean payoff ($)", colour = "Session")   +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  scale_color_manual(labels = c("S1none", "S7wall", "S22bilateral"), values = c("blue", "orange", "black"))
part_1722_t1plot

# Participated by round when treshold is 3
part_1722_t3 <- mainClean %>%
  filter(player.threshold == 3 &  (session.code == "hzopjb8y" | session.code == "i2vtto7j" | session.code == "j0bmhbzp"))  %>%
  group_by(session.code, subsession.round_number) %>%
  summarize(partmean = mean(player.participate))
part_1722_t3

part_1722_t3plot <- ggplot(part_1722_t3, aes(x = subsession.round_number, y = partmean, color = session.code)) + 
  geom_line() +
  geom_point() +
  labs(title = "Participation proportion by subsession when T = 3 - S1none, S7wall, S22bilateral", x = "Subsession round number", y = "Mean payoff ($)", colour = "Session")   +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  scale_color_manual(labels = c("S1none", "S7wall", "S22bilateral"), values = c("blue", "orange", "black"))
part_1722_t3plot


#
# Cleaning up -------------------------------------------------------------------------------------------------------------
#

remove(list = c(ls(pattern = "part_"), ls(pattern = "pay_")))
