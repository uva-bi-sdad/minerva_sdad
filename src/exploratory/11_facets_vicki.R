
############ 13 versus 22 (both bilateral, global versus local, 2o63t7n4 = global, j0bmhbzp = local)

## T = 1 BOXPLOT
pay_1322_t1_box <- mainClean %>%
  select(participant.code, session.code, player.threshold, subsession.round_number, player.round_payoff) %>%
  filter(player.threshold == 1 & (session.code == "2o63t7n4" | session.code == "j0bmhbzp")) %>%
  group_by(subsession.round_number, session.code)
pay_1322_t1_box

pay_1322_t1_box_plot <- ggplot(pay_1322_t1_box, aes(x = subsession.round_number, y = player.round_payoff, fill = session.code)) + 
  geom_boxplot() +
  labs(title = "Payoff by subsession when T = 1 - S13global vs S22local", x = "Subsession round number", y = "Payoff ($)", colour = "Session", caption = "N=30/round") +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) + 
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  scale_color_manual(labels = c("S13global", "S22local"), values = c("coral", "darkblue")) +
  facet_wrap(~session.code)
pay_1322_t1_box_plot

## T = 3 FACETS
# S13
pay_13_t3_box <- mainClean %>%
  select(participant.code, session.code, player.threshold, subsession.round_number, player.round_payoff) %>%
  filter(player.threshold == 3 & (session.code == "2o63t7n4")) %>%
  group_by(participant.code) %>%
  mutate(facetord = mean(player.round_payoff)) %>%
  ungroup() %>%
  mutate(participant.code = reorder(participant.code, facetord))

pay_13_t3_box_plot <- ggplot(pay_13_t3_box, aes(x = subsession.round_number, y = player.round_payoff)) + 
  geom_line(color = "coral") +
  labs(title = "Payoff by subsession when T = 3 - S13global", x = "Subsession round number", y = "Payoff ($)") +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) + 
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  facet_wrap(~participant.code, nrow = 5)
pay_13_t3_box_plot

#S22
pay_22_t3_box <- mainClean %>%
  select(participant.code, session.code, player.threshold, subsession.round_number, player.round_payoff) %>%
  filter(player.threshold == 3 & (session.code == "j0bmhbzp")) %>%
  group_by(participant.code) %>%
  mutate(facetord = mean(player.round_payoff)) %>%
  ungroup() %>%
  mutate(participant.code = reorder(participant.code, facetord))

pay_22_t3_box_plot <- ggplot(pay_22_t3_box, aes(x = subsession.round_number, y = player.round_payoff)) + 
  geom_line(color = "darkblue") +
  labs(title = "Payoff by subsession when T = 3 - S22local", x = "Subsession round number", y = "Payoff ($)") +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) + 
  scale_x_continuous(breaks = seq(1, 15, by = 1), limits = c(1, 15)) + 
  facet_wrap(~participant.code, nrow = 5)
pay_22_t3_box_plot