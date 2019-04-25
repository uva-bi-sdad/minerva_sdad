library(dplyr)
library(ggplot2)

# Note: get data to link by executing 0_readmerge.R ("experiments" dataframe) and 4_main.R ("main" dataframe)

# IN PROGRESS

#
# Merge demographics and main (many:1) ---------------------------------------------------------------------------------------------------------
#

expmain <- merge(experiments, main, by = "participant.code")

  
#
# Participation trends by demographics ---------------------------------------------------------------------------------------------------------
#

expmain <- expmain %>% group_by(participant.code) %>%
  mutate (nyes = sum(player.participate), 
          propyes = nyes / 15)

pdemo <- expmain %>%
  select(participant.code, propyes, player.q1_birthYear, player.q2a_placeOfBirth_country, player.q6a_sexGender,
         player.q7_maritalStatus, player.q8a_education_overview, player.q9_major, player.q10a_subject_econ, 
         player.q10b_subject_finance, player.q10c_subject_stat, player.q11_military_service) %>%
  group_by(participant.code) %>%
  do(head(., 1))

# Gender
ggplot(pdemo, aes(player.q6a_sexGender, propyes)) +
  geom_boxplot()

# Birth country
ggplot(pdemo, aes(player.q2a_placeOfBirth_country, propyes)) +
  geom_boxplot()

# Marital status
ggplot(pdemo, aes(player.q7_maritalStatus, propyes)) +
  geom_boxplot()

# Education
ggplot(pdemo, aes(player.q8a_education_overview, propyes)) +
  geom_boxplot()

#
# Trust & payoff ---------------------------------------------------------------------------------------------------------
#

# Note: First four sessions do not have trust data.

# Look at proportions
for (i in c("player.trust_1", "player.trust_2", "player.trust_3")) {
  print(round(prop.table(table(expmain[[i]])), 2))
}

# Create composite variable
expmain$trust1 <- (as.numeric(expmain$player.trust_1) - 1)
expmain$trust2 <- (as.numeric(expmain$player.trust_2) - 1)
expmain$trust3 <- (as.numeric(expmain$player.trust_3) - 1)

expmain$trust <- ((expmain$trust1 + expmain$trust2 + expmain$trust3) / 3)
table(expmain$trust)

# Create average payoff
expmain <- expmain %>%
  group_by(participant.code) %>%
  mutate(avgpay = mean(player.round_payoff))

table(expmain$player.round_payoff)

# Get one row per participant
paytrust <- expmain %>%
  select(participant.code, avgpay, trust) %>%
  filter(!is.na(trust)) %>%
  group_by(participant.code) %>%
  do(head(., 1))

# Plot
ggplot(paytrust, aes(x = as.factor(trust), y = avgpay, color = as.factor(trust))) +
  geom_point(size = 3) +
  geom_jitter(width = 0.4, height = 0, size = 3) +
  theme(legend.position = "none") + 
  labs(title = "Average round payoff by trust level", x = "Trust (1 = higher trust)", y = "Average round payoff ($)")







