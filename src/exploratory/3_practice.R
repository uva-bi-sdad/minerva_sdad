library(readxl)
library(dplyr)
library(naniar)
library(forcats)


#
# Read and compile ------------------------------------------------------------------------------------------------------------
#

# Reading in
practice_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/practice.xlsx")
practice_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/practice.xlsx")
practice_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/practice.xlsx")
practice_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/practice.xlsx")
practice_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/practice.xlsx")
practice_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/practice.xlsx")

# Appending practice data
practice <- bind_rows(practice_i2vtto7j, practice_jllmf4m8, practice_zjb1wxmz, practice_2o63t7n4, practice_j0bmhbzp, practice_hzopjb8y)

# Select relevant variables
practice_clean <- practice %>%
  select(participant.id_in_session, participant.code, participant.time_started, participant.payoff, player.node_id, player.threshold, player.participate, player.user_name, player.avatar_id, 
         player.continue_practice, player.subsession_id, player.group_id, group.id_in_subsession, group.network_id, group.subsession_id, subsession.round_number, session.code, session.label, 
         session.experimenter_name, player.id_in_group, -session.comment, -session.is_demo, -player.payoff, -player.round_payoff, -participant.label, -participant._is_bot, -participant._index_in_pages, 
         -participant._max_page_index, -participant._current_app_name, -participant._round_number, -participant._current_page_name, -participant.ip_address, -participant.visited, 
         -contains("mturk"))


#
# Missingness ------------------------------------------------------------------------------------------------------------
#

colSums(is.na(practice_clean))
# Some sessions missing experimenter name (not an issue/can fill in retrospectively?)
# player.participate and player.continue_practice: why?

vis_miss(practice_clean)

miss <- practice_clean %>% 
  select_if(function(x) any(is.na(x)))
vis_miss(miss) + 
  coord_flip() + 
  labs(title = "Practice missingness by variable", y = "N missing", x = "Variable with missingness")


#
# Continuing practice ------------------------------------------------------------------------------------------------------------
#

# Continuing practice by subsession number
sess_cont <- practice_clean %>%
  group_by(subsession.round_number) %>%
  summarize(paymean = mean(player.continue_practice), paysd = sd(player.continue_practice), paymin = min(player.continue_practice), paymax = max(player.continue_practice))

contsess <- practice_clean %>%
  filter(!is.na(player.continue_practice)) %>%
  group_by(session.code, subsession.round_number) %>%
  summarize(contcount = sum(player.continue_practice))
contsess$subsession.round_number <- ordered(contsess$subsession.round_number)  

ggplot(contsess, aes(x = session.code, y = contcount, fill = fct_rev(subsession.round_number))) + 
  geom_col() +
  coord_flip() +
  labs(title = "Number of participants continuing practice by session", x = "Session", y = "Continue count", fill = "Subsession round")