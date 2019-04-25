library(readxl)
library(dplyr)
library(ggplot2)

#
# Read in and compile ------------------------------------------------------------------------------------------------------------
#

# Read in main data
main_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/main.xlsx")
main_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/main.xlsx")
main_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/main.xlsx")
main_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/main.xlsx")
main_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/main.xlsx")
main_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/main.xlsx")
main_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/main.xlsx")
main_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/main.xlsx")
main_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/main.xlsx")
main_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/main.xlsx")
main_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/main.xlsx")
main_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/main.xlsx")

# Appending main data
main <- bind_rows(main_i2vtto7j, main_jllmf4m8, main_zjb1wxmz, main_2o63t7n4, main_j0bmhbzp, main_hzopjb8y, main_8otz5sb0, main_xsrkblkf, main_tgfavzdh, main_p5pcjkjc, main_5uwv0c88, main_vjny1l7l)

# Select relevant variables
main <- main %>%
  select(participant.id_in_session, participant.code, -participant.label, -participant._is_bot, -participant._index_in_pages, -participant._max_page_index, -participant._current_app_name, 
         -participant._round_number, -participant._current_page_name, -participant.ip_address, participant.time_started, -participant.visited, -contains("mturk"), participant.payoff, 
         player.id_in_group, player.node_id, player.threshold, player.participate, player.user_name, player.avatar_id, player.round_payoff, player.subsession_id, player.group_id, player.payoff, 
         group.id_in_subsession, group.network_id, group.subsession_id, subsession.round_number, session.code, session.label, session.experimenter_name, -session.comment, -session.is_demo)

## Add communication type variable, do sanity check
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

## Add network knowledge variable, do sanity check
main$NK <- NA
main$NK[main$session.code %in% c("i2vtto7j", "jllmf4m8", "j0bmhbzp", "hzopjb8y", "tgfavzdh", "5uwv0c88")] <- "local"
main$NK[main$session.code %in% c("zjb1wxmz", "2o63t7n4", "8otz5sb0", "xsrkblkf", "p5pcjkjc", "vjny1l7l")] <- "global"
main$NK <- ordered(main$NK, c("local", "global"))

table(main$session.code, main$NK)

# Cleaning up global environment
remove(list = ls(pattern = "main_"))

#
# Missingness ------------------------------------------------------------------------------------------------------------
#

colSums(is.na(main))
# Some sessions missing experimenter name (not an issue/can fill in retrospectively?)
# player.participate: why? people who were advanced

vis_miss(main)

miss <- main %>% 
  select_if(function(x) any(is.na(x)))
vis_miss(miss) + 
  coord_flip() + 
  labs(title = "Main missingness by variable", y = "N missing", x = "Variable with missingness")


#
# Payoff ------------------------------------------------------------------------------------------------------------
#

# BY SUBSESSION NUMBER
pay_sess <- main %>%
  group_by(subsession.round_number) %>%
  summarize(paymean = mean(player.round_payoff), paysd = sd(player.round_payoff), paymin = min(player.round_payoff), paymax = max(player.round_payoff))
pay_sess

ggplot(pay_sess, aes(x = subsession.round_number, y = paymean)) + 
  geom_col() +
  labs(title = "Mean payoff by subsession round number", x = "Subsession round number", y = "Mean payout ($)")

pay_sess1 <- main %>%
  group_by(session.label, subsession.round_number) %>%
  summarize(paymean = mean(player.round_payoff), paysd = sd(player.round_payoff), paymin = min(player.round_payoff), paymax = max(player.round_payoff))
pay_sess1

ggplot(pay_sess1, aes(x = reorder(session.label, paymean), y = paymean, fill = as.factor(subsession.round_number))) + 
  geom_col() +
  coord_flip() +
  labs(title = "Mean payoff for subsession round by session", x = "Session", y = "Mean payout ($)", fill = "Subsession round number") +
  scale_fill_manual(values = c("#114477", "#4477AA", "#77AADD", "#117755", "#44AA88", "#99CCBB", "#777711", "#AAAA44", "#DDDD77", "#771111", "#AA4444", "#DD7777", "#771144", "#AA4477", "#DD77AA"))
                      
# BY THRESHOLD
pay_trs <- main %>%
  group_by(player.threshold) %>%
  summarize(paymean = mean(player.round_payoff), paysd = sd(player.round_payoff), paymin = min(player.round_payoff), paymax = max(player.round_payoff))
pay_trs

ggplot(pay_trs, aes(x = as.factor(player.threshold), y = paymean)) + 
  geom_col() +
  labs(title = "Mean payoff by threshold", x = "Threshold", y = "Mean payout ($)")

pay_trs1 <- main %>%
  group_by(session.code, player.threshold) %>%
  summarize(paymean = mean(player.round_payoff), paysd = sd(player.round_payoff), paymin = min(player.round_payoff), paymax = max(player.round_payoff))
pay_trs1
pay_trs1$session.code <- ordered(pay_trs1$session.code, levels = c("hzopjb8y", "j0bmhbzp", "2o63t7n4", "jllmf4m8", "zjb1wxmz", "i2vtto7j"))

ggplot(pay_trs1, aes(x = session.code, y = paymean, fill = as.factor(player.threshold))) + 
  geom_col() +
  coord_flip() +
  labs(title = "Mean payoff by threshold and session", x = "Session", y = "Mean payout ($)", fill = "Threshold")

# All together
pay_sess_trs <- main %>%
  group_by(subsession.round_number, player.threshold) %>%
  summarize(paymean = mean(player.round_payoff), paysd = sd(player.round_payoff), paymin = min(player.round_payoff), paymax = max(player.round_payoff))
pay_sess_trs

# Payoff by round when threshold is 1
t1 <- pay_sess_trs %>%
  filter(player.threshold == 1) %>%
  summarize(paymean1 = mean(paymean), paymin = min(paymean), paymax = max(paymean))

ggplot(t1, aes(x = as.factor(subsession.round_number), y = paymean1)) + 
  geom_col() +
  labs(title = "Mean payoff by subsession round when T = 1", x = "Subsession round number", y = "Mean payout ($)") +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) 

# Payoff by round when threshold is 3
t2 <- pay_sess_trs %>%
  filter(player.threshold == 3) %>%
  summarize(paymean1 = mean(paymean), paymin = min(paymean), paymax = max(paymean))

ggplot(t2, aes(x = as.factor(subsession.round_number), y = paymean1)) + 
  geom_col() +
  labs(title = "Mean payoff by subsession round when T = 3", x = "Subsession round number", y = "Mean payout ($)")  +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))

# Plot everything
ggplot(pay_net_t, aes(x = netw, y = paymean)) + 
  geom_col(aes(group = player.threshold)) +
  labs(title = "Mean payoff by network knowledge type and T", x = "Network knowledge type", y = "Mean payout ($)")  +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))

# BY NETWORK KNOWLEDGE
pay_net <- main %>%
  group_by(netw) %>%
  summarize(paymean = mean(player.round_payoff), paysd = sd(player.round_payoff), paymin = min(player.round_payoff), paymax = max(player.round_payoff))
pay_net

ggplot(pay_net, aes(x = netw, y = paymean)) + 
  geom_col() +
  labs(title = "Mean payoff by network knowledge type", x = "Network knowledge type", y = "Mean payout ($)") +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))

# By network knowledge and T
pay_net_t <- main %>%
  group_by(player.threshold, netw) %>%
  summarize(paymean = mean(player.round_payoff), paysd = sd(player.round_payoff), paymin = min(player.round_payoff), paymax = max(player.round_payoff))
pay_net_t

ggplot(pay_net_t, aes(x = as.factor(player.threshold), y = paymean, fill = netw)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Mean payoff by network knowledge type & threshold", x = "Network knowledge type", y = "Mean payout ($)", fill = "Network type")

# BY COMMUNICATION TYPE
pay_comm <- main %>%
  group_by(CT) %>%
  summarize(paymean = mean(player.round_payoff), paysd = sd(player.round_payoff), paymin = min(player.round_payoff), paymax = max(player.round_payoff))
pay_comm

ggplot(pay_comm, aes(x = CT, y = paymean)) + 
  geom_col() +
  labs(title = "Mean payoff by communication type", x = "Communication type", y = "Mean payout ($)") +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))

# By communication type and T
pay_comm_t <- main %>%
  group_by(player.threshold, CT) %>%
  summarize(paymean = mean(player.round_payoff), paysd = sd(player.round_payoff), paymin = min(player.round_payoff), paymax = max(player.round_payoff))
pay_comm_t

ggplot(pay_comm_t, aes(x = as.factor(player.threshold), y = paymean, fill = CT)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Mean payoff by communication type & threshold", x = "Network knowledge type", y = "Mean payout ($)", fill = "Communication type")


#
# Participated ------------------------------------------------------------------------------------------------------------
#

## Participated by threshold
# By subsession number (= practice round)
part_sess <- main %>%
  filter(!is.na(player.participate)) %>%
  group_by(subsession.round_number) %>%
  summarize(partmean = mean(player.participate), partsd = sd(player.participate), partmin = min(player.participate), partmax = max(player.participate))
part_sess

ggplot(part_sess, aes(x = subsession.round_number, y = partmean)) + geom_col()

# By treshold
part_trs <- main %>%
  filter(!is.na(player.participate)) %>%
  group_by(player.threshold) %>%
  summarize(partmean = mean(player.participate), partsd = sd(player.participate), partmin = min(player.participate), partmax = max(player.participate))
part_trs

ggplot(part_trs, aes(x = player.threshold, y = partmean)) + 
  geom_col()  +
  labs(title = "Participation proportion by threshold", x = "Threshold", y = "Participation proportion", caption = "Note: NAs removed.")

# T = 1
part_trs1 <- main %>%
  filter(!is.na(player.participate), player.threshold == 1) %>%
  group_by(session.code) %>%
  summarize(partmean = mean(player.participate), partsd = sd(player.participate), partmin = min(player.participate), partmax = max(player.participate))
part_trs1

ggplot(part_trs1, aes(x = reorder(session.code, partmean), y = partmean)) + 
  geom_col(fill = c("darkred", "darkorange", "yellow", "darkgreen", "darkblue", "purple", "brown")) +
  coord_flip() +
  labs(title = "Mean participation proportion T = 1 by session", x = "Session", y = "Participation proportion", caption = "Note: NAs removed.")  +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1))

# T = 3
part_trs2 <- main %>%
  filter(!is.na(player.participate), player.threshold == 3) %>%
  group_by(session.code) %>%
  summarize(partmean = mean(player.participate), partsd = sd(player.participate), partmin = min(player.participate), partmax = max(player.participate))
part_trs2

ggplot(part_trs2, aes(x = reorder(session.code, partmean), y = partmean)) + 
  geom_col(fill = c("darkorange", "darkred", "yellow", "darkblue", "brown", "darkgreen", "purple")) +
  coord_flip() +
  labs(title = "Mean participation proportion T = 3 by session", x = "Session", y = "Participation proportion", caption = "Note: NAs removed.")  +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1))

# All together
part_sess_trs <- main %>%
  filter(!is.na(player.participate)) %>%
  group_by(subsession.round_number, player.threshold) %>%
  summarize(partmean = mean(player.participate), partsd = sd(player.participate), partmin = min(player.participate), partmax = max(player.participate))
part_sess_trs

# Participated by round when treshold is 1
pp1 <- part_sess_trs %>%
  filter(player.threshold == 1) %>%
  summarize(partmean1 = mean(partmean), paymin = min(partmean), paymax = max(partmean))

ggplot(pp1, aes(x = as.factor(subsession.round_number), y = partmean1)) + 
  geom_col() +
  labs(title = "Mean participation proportion T = 1 by round", x = "Round number", y = "Participation proportion", caption = "Note: NAs removed.")  +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1))

# Participated by round when treshold is 3
pp2 <- part_sess_trs %>%
  filter(player.threshold == 3) %>%
  summarize(partmean1 = mean(partmean), paymin = min(partmean), paymax = max(partmean))

ggplot(pp2, aes(x = as.factor(subsession.round_number), y = partmean1)) + 
  geom_col() +
  labs(title = "Mean participation proportion T = 3 by round", x = "Round number", y = "Participation proportion", caption = "Note: NAs removed.")  +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1))

# BY NETWORK KNOWLEDGE
part_net <- main %>%
  filter(!is.na(player.participate)) %>%
  group_by(netw) %>%
  summarize(partmean = mean(player.participate), partsd = sd(player.participate), partmin = min(player.participate), partmax = max(player.participate))
part_net

ggplot(part_net, aes(x = netw, y = partmean)) + 
  geom_col() +
  labs(title = "Participation proportion by network knowledge", x = "Network knowledge",  y = "Participation proportion", caption = "Note: NAs removed.")

# By network knowledge and T
part_net_t <- main %>%
  filter(!is.na(player.participate)) %>%
  group_by(player.threshold, netw) %>%
  summarize(partmean = mean(player.participate), partsd = sd(player.participate), partmin = min(player.participate), partmax = max(player.participate))
part_net_t

ggplot(part_net_t, aes(x = as.factor(player.threshold), y = partmean, fill = netw)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Participation proportion by network knowledge & threshold", x = "Threshold", y = "Mean participation proportion", fill = "Network knowledge")

# BY COMMUNICATION TYPE
part_comm <- main %>%
  filter(!is.na(player.participate)) %>%
  group_by(CT) %>%
  summarize(partmean = mean(player.participate), partsd = sd(player.participate), partmin = min(player.participate), partmax = max(player.participate))
part_comm

ggplot(part_comm, aes(x = CT, y = partmean)) + 
  geom_col() +
  labs(title = "Participation proportion by communication type", x = "Communication type",  y = "Participation proportion", caption = "Note: NAs removed.")

# By communication type and T
part_comm_t <- main %>%
  filter(!is.na(player.participate)) %>%
  group_by(player.threshold, CT) %>%
  summarize(partmean = mean(player.participate), partsd = sd(player.participate), partmin = min(player.participate), partmax = max(player.participate))
part_comm_t

ggplot(part_comm_t, aes(x = as.factor(player.threshold), y = partmean, fill = CT)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Participation proportion by communication type & threshold", x = "Threshold", y = "Mean participation proportion", fill = "Communication type")


#
# Clean up global environment ------------------------------------------------------------------------------------------------------------
#

remove(list = c(ls(pattern = "[^experiments]")))
