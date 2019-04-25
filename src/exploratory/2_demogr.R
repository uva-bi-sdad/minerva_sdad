library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(naniar)

# Note: get data by executing merge in 0_readmerge.R 


#
# Missingness ------------------------------------------------------------------------------------------------------------
#

# Tally each variable
colSums(is.na(experiments))

# Show only variables with missingness
experiments %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_all(funs(sum(is.na(.))))

# Visualize
vis_miss(experiments)

miss <- experiments %>% 
  select_if(function(x) any(is.na(x)))
vis_miss(miss) + 
  coord_flip() + 
  labs(title = "Survey missingness by variable", y = "N missing", x = "Variable with missingness")


#
# Demographics tables -----------------------------------------------------------------------------------------------------
#

# Birth year
experiments %>%
  group_by(session.label.x) %>%
  summarise_at("player.q1_birthYear", c(mean, sd, min, max), na.rm = TRUE)

# Everything else
for (i in c("player.q6a_sexGender", "player.q8a_education_overview", "player.q9_major", "player.q10a_subject_econ", 
            "player.q10b_subject_finance", "player.q10c_subject_stat", "player.q11_military_service")) {
  demotable <- print(round(prop.table(table(experiments$session.label.x, experiments[[i]]), 1), 2))
  print(demotable)
}


#
# Demographics plots --------------------------------------------------------------------------------------------------------
#

# Birth year
ggplot(experiments, aes(x = session.label.x, y = player.q1_birthYear)) + 
  geom_boxplot() +
  coord_flip() +
  labs(title = "Birth year by session", x = "Session", y = "Birth year")

ggplot(experiments, aes(x = factor(player.q1_birthYear))) +
  geom_bar() +
  labs(title = "Birth year sample distribution", x = "Birth year", y = "Count")

## Everything else
# Gender
ggplot(experiments, aes(x = player.q6a_sexGender, fill = player.q6a_sexGender)) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  facet_wrap(~session.label.x)  +
  labs(title = "Gender by session", x = "", y = "Count", fill = "Gender")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q6a_sexGender))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Gender composition by session", x = "Session", y = "Count", fill = "Gender")

ggplot(experiments, aes(x = player.q6a_sexGender)) +
  geom_bar() +
  labs(title = "Sample gender composition ", x = "Gender", y = "Count")

# Marital status
ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q7_maritalStatus))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Marital status by session", x = "Session", y = "Count", fill = "Marital status")

# Education
ggplot(experiments, aes(x = session.label.x, fill = player.q8a_education_overview)) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Education by session", x = "Session", y = "Count", fill = "Education")

ggplot(experiments, aes(x = factor(player.q8a_education_overview))) +
  geom_bar() +
  labs(title = "Education level sample distribution", x = "Education level", y = "Count")

# Major
ggplot(experiments, aes(x = session.label.x, fill = player.q9_major)) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Major by session", x = "Session", y = "Count", fill = "Major")

ggplot(experiments, aes(x = fct_rev(fct_infreq(player.q9_major)))) +
  geom_bar() +
  labs(title = "Major sample distribution", x = "Major", y = "Count") + 
  coord_flip()

# Subject: Economics
plot_econ <- ggplot(experiments, aes(x = session.label.x, fill = as.factor(player.q10a_subject_econ))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Taken economics by session", x = "Session", y = "Count", fill = "Taken economics")

ggplot(experiments, aes(x = factor(player.q10a_subject_econ))) +
  geom_bar() +
  labs(title = "Taken econ sample distribution", x = "Taken econ", y = "Count")

# Subject: Finance
plot_fin <-  ggplot(experiments, aes(x = session.label.x, fill = as.factor(player.q10b_subject_finance))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Taken finance by session", x = "Session", y = "Count", fill = "Taken finance")

ggplot(experiments, aes(x = factor(player.q10b_subject_finance))) +
  geom_bar() +
  labs(title = "Taken finance sample distribution", x = "Taken finance", y = "Count")

# Subject: Statistics
plot_stat <- ggplot(experiments, aes(x = session.label.x, fill = as.factor(player.q10c_subject_stat))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Taken statistics by session", x = "Session", y = "Count", fill = "Taken statistics")

ggplot(experiments, aes(x = factor(player.q10c_subject_stat))) +
  geom_bar() +
  labs(title = "Taken stats sample distribution", x = "Taken stats", y = "Count")

grid.arrange(plot_econ, plot_fin, plot_stat)

# Military service
ggplot(experiments, aes(x = session.label.x, fill = as.factor(player.q11_military_service))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Military service by session", x = "Session", y = "Count", fill = "Military service")


#
# Time spent on activities ---------------------------------------------------------------------------------------------
#

# Converting character to factor
activities <- colnames(select(experiments, contains("activit")))
activities

experiments[, activities] <- lapply(experiments[, activities], as.factor)

# Ordering levels and sanity check
for (i in activities) {
  experiments[[i]] <- ordered(experiments[[i]], levels = c("None", "Less than 1 hour", "1-5 hours", "6-10 hours", "11-20 hours", "Over 20 hours"))
  print(is.ordered(experiments[[i]]))
  }

# Tables
for (i in activities) {
  demotable <- print(round(prop.table(table(experiments$session.label.x, experiments[[i]]), 1), 2))
  print(demotable)
}

## Plots 1
plot_activity <- function(whichvar, plottitle) {
  activityplot <- ggplot(experiments, aes_string(x = whichvar)) +
    geom_bar() +
    labs(title = plottitle, x = "Time spent", y = "Count") +
    coord_flip()
  return(activityplot)
}

p1 <- plot_activity(whichvar = 'player.q12a_activities_academic', plottitle = "Academic activities")
p2 <- plot_activity(whichvar = 'player.q12b_activities_exercise', plottitle = "Exercise activities")
p3 <- plot_activity(whichvar = 'player.q12c_activities_sports', plottitle = "Sports activities")
p4 <- plot_activity(whichvar = 'player.q12d_activities_performing', plottitle = "Performing activities")
p5 <- plot_activity(whichvar = 'player.q12e_activites_religious', plottitle = "Religious activities")
p6 <- plot_activity(whichvar = 'player.q12f_activites_membership', plottitle = "Membership activities")
p7 <- plot_activity(whichvar = 'player.q12g_activities_leading', plottitle = "Leadership activities")
p8 <- plot_activity(whichvar = 'player.q12h_activities_socializing', plottitle = "Socializing activities")
p9 <- plot_activity(whichvar = 'player.q12i_activities_socialNetworks', plottitle = "Social network activities")
p10 <- plot_activity(whichvar = 'player.q12j_activities_onlineGames', plottitle = "Online gaming activities")
p11 <- plot_activity(whichvar = 'player.q12l_activities_working', plottitle = "Work activities")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)

## Plots 2
ggplot(experiments, aes(x = player.q12a_activities_academic)) + 
  geom_bar() +
  facet_wrap(~session.label.x)

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12a_activities_academic))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Time spent on academic activities by session", x = "Session", y = "Count", fill = "Hours spent")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12b_activities_exercise))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Exercise activities by session", x = "Session", y = "Count", fill = "Hours spent")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12c_activities_sports))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Sports activities by session", x = "Session", y = "Count", fill = "Hours spent")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12d_activities_performing))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Performing activities by session", x = "Session", y = "Count", fill = "Hours spent")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12e_activites_religious))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Religious activities by session", x = "Session", y = "Count", fill = "Hours spent")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12f_activites_membership))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Membership activities by session", x = "Session", y = "Count", fill = "Hours spent")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12g_activities_leading))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Leadership activities by session", x = "Session", y = "Count", fill = "Hours spent")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12h_activities_socializing))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Socializing activities by session", x = "Session", y = "Count", fill = "Hours spent")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12i_activities_socialNetworks))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Social networking activities by session", x = "Session", y = "Count", fill = "Hours spent")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12j_activities_onlineGames))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Online games activities by session", x = "Session", y = "Count", fill = "Hours spent")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q12l_activities_working))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Work activities by session", x = "Session", y = "Count", fill = "Hours spent")


#
# Ethics ------------------------------------------------------------------------------------------------------------
#

# Converting character to factor and ordering levels
ethics <- colnames(select(experiments, contains("ethics")))
ethics

experiments[, ethics] <- lapply(experiments[, ethics], as.factor)
experiments$player.q15a_ethics_wealth <- ordered(experiments$player.q15a_ethics_wealth, levels = c("Srongly Disagree", "Disagree Somewhat", "Indifferent", "Agree Somewhat", "Strongly Agree", "No Opinion", "Prefer not to answer"))
experiments$player.q15b_ethics_climate <- ordered(experiments$player.q15b_ethics_climate, levels = c("Srongly Disagree", "Disagree Somewhat", "Indifferent", "Agree Somewhat", "Strongly Agree", "No Opinion", "Prefer not to answer"))
experiments$player.q15c_ethics_gunControl <- ordered(experiments$player.q15c_ethics_gunControl, levels = c("Srongly Disagree", "Disagree Somewhat", "Indifferent", "Agree Somewhat", "Strongly Agree", "No Opinion", "Prefer not to answer"))

# Filter by session
s0613_2pm1a <- experiments %>%
  filter(session.label.x == "June 13 - 2PM - session 1A")
s0613_2pm1b <- experiments %>%
  filter(session.label.x == "June 13 - 2PM - session 1B")
s0613_11am <- experiments %>%
  filter(session.label.x == "June 13 - 11AM")
s0613_11am1b <- experiments %>%
  filter(session.label.x == "June 13 - 11AM - session 1B")
s0813_1230pm1as13 <- experiments %>%
  filter(session.label.x == "ESSL Aug 13 12:30pm 1A session 13")
s0813_10am1as22 <- experiments %>%
  filter(session.label.x == "ESSL Aug 13 10am 1A session 22")
s0821_1030am1as10 <- experiments %>%
  filter(session.label.x == "ESSL Aug 21 10:30AM session 10 - 1A")

# Wealth 
table(experiments$session.label.x, experiments$player.q15a_ethics_wealth)

eth_w1 <- ggplot(s0613_2pm1a, aes(x = player.q15a_ethics_wealth)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#999999", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
eth_w2 <- ggplot(s0613_2pm1b, aes(x = player.q15a_ethics_wealth)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#999999", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
eth_w3 <- ggplot(s0613_11am, aes(x = player.q15a_ethics_wealth)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
eth_w4 <- ggplot(s0613_11am1b, aes(x = player.q15a_ethics_wealth)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#999999", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
eth_w5 <- ggplot(s0813_1230pm1as13, aes(x = player.q15a_ethics_wealth)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
eth_w6 <- ggplot(s0813_10am1as22, aes(x = player.q15a_ethics_wealth)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
eth_w7 <- ggplot(s0821_1030am1as10, aes(x = player.q15a_ethics_wealth)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

grid.arrange(eth_w1, eth_w2, eth_w3, eth_w4, eth_w5, eth_w6, eth_w7, nrow = 3)

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q15a_ethics_wealth))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() + 
  scale_fill_manual(values = c("#999999", "#999999", "#0571b0", "#92c5de", "#f7f7f7", "#fdb863", "#e66101"))  +
  labs(title = "Wealthy people should pay more taxes than they do now", x = "Session", y = "Count", fill = "Agreement")

# Climate
table(experiments$session.label.x, experiments$player.q15b_ethics_climate)

cli_w1 <- ggplot(s0613_2pm1a, aes(x = player.q15b_ethics_climate)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
cli_w2 <- ggplot(s0613_2pm1b, aes(x = player.q15b_ethics_climate)) + 
  geom_bar(fill = c("#E69F00", "#E69F00")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
cli_w3 <- ggplot(s0613_11am, aes(x = player.q15b_ethics_climate)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#E69F00")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
cli_w4 <- ggplot(s0613_11am1b, aes(x = player.q15b_ethics_climate)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
cli_w5 <- ggplot(s0813_1230pm1as13, aes(x = player.q15b_ethics_climate)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
cli_w6 <- ggplot(s0813_10am1as22, aes(x = player.q15b_ethics_climate)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
cli_w7 <- ggplot(s0821_1030am1as10, aes(x = player.q15b_ethics_climate)) + 
  geom_bar(fill = c("#E69F00", "#E69F00")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

grid.arrange(cli_w1, cli_w2, cli_w3, cli_w4, cli_w5, cli_w6, cli_w7, nrow = 3)

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q15b_ethics_climate))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() + 
  scale_fill_manual(values = c("#999999", "#999999", "#0571b0", "#92c5de", "#f7f7f7", "#fdb863", "#e66101"))   +
  labs(title = "Addressing global climate change should be a federal priority", x = "Session", y = "Count", fill = "Agreement")

# Gun
table(experiments$session.label.x, experiments$player.q15c_ethics_gunControl)

gun_w1 <- ggplot(s0613_2pm1a, aes(x = player.q15c_ethics_gunControl)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
gun_w2 <- ggplot(s0613_2pm1b, aes(x = player.q15c_ethics_gunControl)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#999999", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
gun_w3 <- ggplot(s0613_11am, aes(x = player.q15c_ethics_gunControl)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
gun_w4 <- ggplot(s0613_11am1b, aes(x = player.q15c_ethics_gunControl)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
gun_w5 <- ggplot(s0813_1230pm1as13, aes(x = player.q15c_ethics_gunControl)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#E69F00", "#E69F00")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
gun_w6 <- ggplot(s0813_10am1as22, aes(x = player.q15c_ethics_gunControl)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00", "#999999")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
gun_w7 <- ggplot(s0821_1030am1as10, aes(x = player.q15c_ethics_gunControl)) + 
  geom_bar(fill = c("#E69F00", "#E69F00", "#E69F00")) +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

grid.arrange(gun_w1, gun_w2, gun_w3, gun_w4, gun_w5, gun_w6, gun_w7, nrow = 3)

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(player.q15c_ethics_gunControl))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() + 
  scale_fill_manual(values = c("#999999", "#999999", "#0571b0", "#92c5de", "#f7f7f7", "#fdb863", "#e66101"))  +
  labs(title = "The federal government should have stricter gun control laws", x = "Session", y = "Count", fill = "Agreement")

# Plots 2
eth1 <- ggplot(experiments, aes(x = factor(player.q15a_ethics_wealth))) +
  geom_bar(fill = c("#0571b0", "#0571b0", "#0571b0", "#0571b0", "#0571b0", "#999999", "#999999")) +
  labs(title = "Wealthy people should pay a larger share of taxes than they do now", x = "Agreement", y = "Count")  +
  coord_flip()

eth2 <- ggplot(experiments, aes(x = factor(player.q15b_ethics_climate))) +
  geom_bar(fill = c("#0571b0", "#0571b0", "#0571b0", "#0571b0", "#0571b0", "#999999", "#999999")) +
  labs(title = "Addressing global climate change should be a federal priority", x = "Agreement", y = "Count")  +
  coord_flip()

eth3 <- ggplot(experiments, aes(x = factor(player.q15c_ethics_gunControl))) +
  geom_bar(fill = c("#0571b0", "#0571b0", "#0571b0", "#0571b0", "#0571b0", "#999999", "#999999")) +
  labs(title = "The federal government should have stricter gun control laws", x = "Agreement", y = "Count")  +
  coord_flip()

grid.arrange(eth1, eth2, eth3, nrow = 3)


#
# Trust ------------------------------------------------------------------------------------------------------------
#

trust <- colnames(select(experiments, contains("trust")))
trust

experiments[, trust] <- lapply(experiments[, trust], as.factor)
experiments$player.trust_1 <- ordered(experiments$player.trust_1, levels = c("Need to be very careful", "Most people can be trusted"))
experiments$player.trust_2 <- ordered(experiments$player.trust_2, levels = c("Most of the time they would try to take advantage", "Most of the time they would try to be fair"))
experiments$player.trust_3 <- ordered(experiments$player.trust_3, levels = c("Most of the time they are just looking out for themselves", "Most of the time people are helpful"))
experiments$player.trust_4 <- ordered(experiments$player.trust_4)

# Plots 1
ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.trust_1))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "People can be trusted?", x = "Session", y = "Count", fill = "Response")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.trust_2))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "People would take advantage?", x = "Session", y = "Count", fill = "Response")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.trust_3))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "People are helpful?", x = "Session", y = "Count", fill = "Response")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.trust_4))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Chances of getting wallet back", x = "Session", y = "Count", fill = "Chance")

# Plots 2
tr1 <- ggplot(data = subset(experiments, !is.na(player.trust_1)), aes(x = player.trust_1)) +
  geom_bar(fill = c("#b35900", "#00802b")) +
  labs(title = "People can be trusted? ", x = "Response", y = "Count")

tr2 <- ggplot(data = subset(experiments, !is.na(player.trust_2)), aes(x = player.trust_2)) +
  geom_bar(fill = c("#b35900", "#00802b")) +
  labs(title = "People would take advantage? ", x = "Response", y = "Count")

tr3 <- ggplot(data = subset(experiments, !is.na(player.trust_3)), aes(x = player.trust_3)) +
  geom_bar(fill = c("#b35900", "#00802b")) +
  labs(title = "People are helpful? ", x = "Response", y = "Count")

grid.arrange(tr1, tr2, tr3, nrow = 3)

ggplot(data = subset(experiments, !is.na(player.trust_4)), aes(x = player.trust_4)) +
  geom_bar() +
  labs(title = "Chance of getting wallet back ", x = "Chance", y = "Count")


#
# Activities in past year ------------------------------------------------------------------------------------------------------------
#

activ <- colnames(select(experiments, contains("player.q13")))
activ

experiments[, activ] <- lapply(experiments[, activ], as.factor)
experiments$player.q13b_volunteer <- ordered(experiments$player.q13b_volunteer, levels = c("Never", "Once a year", "A couple of times a year", "A few times a month", "1-2 times a week", "A couple of times a week", "Once a day", "More than once a day"))
experiments$player.q13c_donations <- ordered(experiments$player.q13c_donations, levels = c("Never", "Once a year", "A couple of times a year", "A few times a month", "1-2 times a week", "A couple of times a week", "Once a day", "More than once a day"))
experiments$player.q13d_discussPolitics <- ordered(experiments$player.q13d_discussPolitics, levels = c("Never", "Once a year", "A couple of times a year", "A few times a month", "1-2 times a week", "A couple of times a week", "Once a day", "More than once a day"))
experiments$player.q13e_communicate <- ordered(experiments$player.q13e_communicate, levels = c("Never", "Once a year", "A couple of times a year", "A few times a month", "1-2 times a week", "A couple of times a week", "Once a day", "More than once a day"))
experiments$player.q13f_demonstrate <- ordered(experiments$player.q13f_demonstrate, levels = c("Never", "Once a year", "A couple of times a year", "A few times a month", "1-2 times a week", "A couple of times a week", "Once a day", "More than once a day"))
experiments$player.q13g_elections <- ordered(experiments$player.q13g_elections, levels = c("Never", "Once a year", "A couple of times a year", "A few times a month", "1-2 times a week", "A couple of times a week", "Once a day", "More than once a day"))
experiments$player.q13h_risk <- ordered(experiments$player.q13h_risk, levels = c("Never", "Once a year", "A couple of times a year", "A few times a month", "1-2 times a week", "A couple of times a week", "Once a day", "More than once a day"))

# Plots 1
ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.q13b_volunteer))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Volunteered in past year", x = "Session", y = "Count", fill = "Frequency")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.q13c_donations))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Donated in past year", x = "Session", y = "Count", fill = "Frequency")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.q13d_discussPolitics))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Discussed politics in past year", x = "Session", y = "Count", fill = "Frequency")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.q13e_communicate))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Publicly communicated in past year", x = "Session", y = "Count", fill = "Frequency")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.q13f_demonstrate))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Demonstrated in past year", x = "Session", y = "Count", fill = "Frequency")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.q13g_elections))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Election participation in past year", x = "Session", y = "Count", fill = "Frequency")

ggplot(experiments, aes(x = session.label.x, fill = forcats::fct_rev(experiments$player.q13h_risk))) + 
  geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(axis.text.x = element_text(hjust = 1, size = 10)) +
  coord_flip() +
  labs(title = "Taken risk in past year", x = "Session", y = "Count", fill = "Frequency")

# Plots 2
pa1 <- ggplot(subset(experiments, !is.na(player.q13b_volunteer)), aes(x = factor(player.q13b_volunteer))) +
  geom_bar() +
  labs(title = "Volunteer", x = "Frequency", y = "Count") +
  coord_flip()
pa2 <- ggplot(experiments, aes(x = factor(player.q13c_donations))) +
  geom_bar() +
  labs(title = "Donate", x = "Frequency", y = "Count")  +
  coord_flip()
pa3 <- ggplot(experiments, aes(x = factor(player.q13d_discussPolitics))) +
  geom_bar() +
  labs(title = "Discuss politics", x = "Frequency", y = "Count")  +
  coord_flip()
pa4 <- ggplot(experiments, aes(x = factor(player.q13e_communicate))) +
  geom_bar() +
  labs(title = "Communicate opinion", x = "Frequency", y = "Count")  +
  coord_flip()
pa5 <- ggplot(experiments, aes(x = factor(player.q13f_demonstrate))) +
  geom_bar() +
  labs(title = "Demonstrate", x = "Frequency", y = "Count")  +
  coord_flip()
pa6 <- ggplot(experiments, aes(x = factor(player.q13g_elections))) +
  geom_bar() +
  labs(title = "Participate elections", x = "Frequency", y = "Count") +
  coord_flip()
pa7 <- ggplot(experiments, aes(x = factor(player.q13h_risk))) +
  geom_bar() +
  labs(title = "Take risk", x = "Frequency", y = "Count")  +
  coord_flip()

grid.arrange(pa1, pa2, pa3, pa4, pa5, pa6, pa7)


#
# Cleaning up global environment ------------------------------------------------
#

remove(list = ls(pattern = "[^experiments]"))