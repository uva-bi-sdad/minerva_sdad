library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Note: run 0_readmerge.R to get "experiments" data frame needed to pull session identifiers.


#
# Comparisons ------------------------------------------------------------------------------------------------------------
#

# 1 versus 7 versus 22 all LOCAL
# CT: none versus wall versus bilateral
# [Session 1] Run: Groups, 3; Messaging, none; Network Knowledge, local; Seq, 456; Order, H-L: session hzopjb8y
# [Session 7] Run: Groups, 3; Messaging, wall; Network Knowledge, local; Seq, 456; Order, H-L: session i2vtto7j
# [Session 22] Run: Groups, 3; Messaging, bilateral; Network Knowledge, local; Seq, 456; Order, H-L: session j0bmhbzp

# 13 versus 22 both BILATERAL
# NK: global versus local
# [Session 13] Run: Groups, 3; Messaging, bilateral; Network Knowledge, global; Seq, 456; Order, H-L: session 2o63t7n4
# [Session 22] Run: Groups, 3; Messaging, bilateral; Network Knowledge, local; Seq, 456; Order, H-L: session j0bmhbzp

# 1 versus 10 both NONE -- 8otz5sb0 HAS NOT BEEN RUN YET
# NK: global versus local
# [Session 1] Run: Groups, 3; Messaging, none; Network Knowledge, local; Seq, 456; Order, H-L: session hzopjb8y
# [Session 10] Run: Groups, 3; Messaging, none; Network Knowledge, global; Seq, 456; Order, H-L: session 8otz5sb0 HAS NOT BEEN RUN YET


#
# Read in and compile ------------------------------------------------------------------------------------------------------------
#

# Read in timespent data
time_s01 <- read_excel("/home/sdal/projects/minerva/data/2018-08-21-ESSL/hzopjb8y/TimeSpent.xlsx")
time_s07 <- read_excel("/home/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION7_i2vtto7j/TimeSpent.xlsx")
time_s13 <- read_excel("/home/sdal/projects/minerva/data/2018-08-13-ESSL/2o63t7n4/TimeSpent.xlsx")
time_s22 <- read_excel("/home/sdal/projects/minerva/data/2018-08-13-ESSL/j0bmhbzp/TimeSpent.xlsx")

# Appending timespent data
time <- bind_rows(time_s01, time_s07, time_s13, time_s22)
remove(list = c(ls(pattern = "time_")))


#
# Get session identifier from surveys ------------------------------------------------------------------------------------------------------------
#

# Getting identifier
fromsurvey <- experiments %>%
  select(participant.code, session.code.x) %>%
  filter(session.code.x == "hzopjb8y" | session.code.x == "i2vtto7j" | session.code.x == "2o63t7n4" | session.code.x == "j0bmhbzp")

colnames(time)[colnames(time) == "participant__code"] <- "participant.code"

# Merging
time <- merge(time, fromsurvey, by = "participant.code")


#
# Adding NK and CT variables ------------------------------------------------------------------------------------------------------------
#

## Add communication type variable, do sanity check
# zjb1wxmz == wall
# i2vtto7j == wall
# jllmf4m8 == wall
# 2o63t7n4 == bilateral
# j0bmhbzp == bilateral
# hzopjb8y == none

time$ct <- NA
time$ct[time$session.code.x %in% c("i2vtto7j")] <- "wall"
time$ct[time$session.code.x  %in% c("2o63t7n4", "j0bmhbzp")] <- "bilateral"
time$ct[time$session.code.x  %in% c("hzopjb8y")] <- "none"
time$ct <- ordered(time$ct, c("none", "wall", "bilateral"))

table(time$session.code.x, time$ct)

## Add network knowledge variable, do sanity check
# zjb1wxmz == global
# i2vtto7j == local
# jllmf4m8 == local
# 2o63t7n4 == global
# j0bmhbzp == local
# hzopjb8y == local

time$nk <- NA
time$nk[time$session.code.x %in% c("i2vtto7j", "j0bmhbzp", "hzopjb8y")] <- "local"
time$nk[time$session.code.x %in% c("2o63t7n4")] <- "global"
time$nk <- ordered(time$nk, c("local", "global"))

table(time$session.code.x, time$nk)


#
# Missingness ------------------------------------------------------------------------------------------------------------
#

colSums(is.na(time))


#
# Time spent on main experiment by session comparisons: ALL ------------------------------------------------------------------------------------------------------------
#

# All sessions
timeplot <- time %>%
  filter(app_name == "main") %>%
  group_by(page_index) %>%
  summarize(meant = mean(seconds_on_page))

ggplot(timeplot, aes(x = page_index, y = meant)) + 
  geom_col() +
  labs(title = "Time spent on main experiment by page index", x = "Page index", y = "Mean time (seconds)")

# What is going on with page 36?
whattheck <- time %>%
  filter(app_name == "main", page_index == 36) %>%
  select(session.code.x, participant.code, seconds_on_page)


#
# Time spent on main experiment by session comparisons: 13 VS 22------------------------------------------------------------------------------------------------------------
#

## 13 versus 22 (both bilateral, global versus local, 2o63t7n4 = global, j0bmhbzp = local)

# Version 1: bar
# Data
comp_13 <- time %>%
  filter(app_name == "main", session.code.x == "2o63t7n4") %>%
  group_by(page_index) %>%
  summarize(meant = mean(seconds_on_page))

# Plot
comp_13_plot <- ggplot(comp_13, aes(x = page_index, y = meant)) + 
  geom_col() +
  labs(title = "Time spent on main experiment by page index - S13global", x = "Page index", y = "Mean time (seconds)") +
  scale_y_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 180))

# Data
comp_22 <- time %>%
  filter(app_name == "main", session.code.x == "j0bmhbzp") %>%
  group_by(page_index) %>%
  summarize(meant = mean(seconds_on_page))

# Plot
comp_22_plot <- ggplot(comp_22, aes(x = page_index, y = meant)) + 
  geom_col() +
  labs(title = "Time spent on main experiment by page index - S22local", x = "Page index", y = "Mean time (seconds)")  +
  scale_y_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 180))

# Arrange plot
grid.arrange(comp_13_plot, comp_22_plot)

# Version 2: line
comp_1322 <- time %>%
  filter(app_name == "main" & (session.code.x == "2o63t7n4" | session.code.x == "j0bmhbzp")) %>%
  group_by(page_index, session.code.x) %>%
  summarize(meant = mean(seconds_on_page))

comp_1322_plot <- ggplot(comp_1322, aes(x = page_index, y = meant, color = session.code.x)) + 
  geom_line() +
  labs(title = "Time spent on main experiment by page index - S13global vs S22local", x = "Page index", y = "Mean time (seconds)", colour = "Session")   +
  scale_y_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 180)) + 
  scale_color_manual(labels = c("S13global", "S22local"), values = c("coral", "darkblue"))
comp_1322_plot


#
# Time spent on main experiment by session comparisons: 1 VS 7 VS 22------------------------------------------------------------------------------------------------------------
#

# 1 versus 7 versus 22 (all local, none versus wall versus bilateral, hzopjb8y = none, i2vtto7j = wall, j0bmhbzp = bilateral)

## Version 1: bar
# Data
comp_1 <- time %>%
  filter(app_name == "main", session.code.x == "hzopjb8y") %>%
  group_by(page_index) %>%
  summarize(meant = mean(seconds_on_page))

# Plot
comp_1_plot <- ggplot(comp_1, aes(x = page_index, y = meant)) + 
  geom_col() +
  labs(title = "Time spent on main experiment by page index - S1none", x = "Page index", y = "Mean time (seconds)") +
  scale_y_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 180))

# Data
comp_7 <- time %>%
  filter(app_name == "main", session.code.x == "i2vtto7j") %>%
  group_by(page_index) %>%
  summarize(meant = mean(seconds_on_page))

# Plot
comp_7_plot <- ggplot(comp_7, aes(x = page_index, y = meant)) + 
  geom_col() +
  labs(title = "Time spent on main experiment by page index - S7wall", x = "Page index", y = "Mean time (seconds)")  +
  scale_y_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 180))

# Plot (data from previous)
comp_22a_plot <- ggplot(comp_22, aes(x = page_index, y = meant)) + 
  geom_col() +
  labs(title = "Time spent on main experiment by page index - S22bilateral", x = "Page index", y = "Mean time (seconds)")  +
  scale_y_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 180))

# Arrange plot
grid.arrange(comp_1_plot, comp_7_plot, comp_22a_plot)

# Version 2: line
comp_1722 <- time %>%
  filter(app_name == "main" & (session.code.x == "hzopjb8y" | session.code.x == "i2vtto7j" | session.code.x == "j0bmhbzp")) %>%
  group_by(page_index, session.code.x) %>%
  summarize(meant = mean(seconds_on_page))

comp_1722_plot <- ggplot(comp_1722, aes(x = page_index, y = meant, color = session.code.x)) + 
  geom_line() +
  labs(title = "Time spent on main experiment by page index - S1none vs S7wall vs S22bilateral", x = "Page index", y = "Mean time (seconds)", colour = "Session")   +
  scale_y_continuous(breaks = seq(0, 200, by = 25), limits = c(0, 180)) + 
  scale_color_manual(labels = c("S1none", "S7wall", "S22bilateral"), values = c("coral", "darkblue", "brown"))
comp_1722_plot


#
# Cleaning up------------------------------------------------------------------------------------------------------------
#

# Cleaning up
remove(list = c(ls(pattern = "comp_"), "whattheck"))
