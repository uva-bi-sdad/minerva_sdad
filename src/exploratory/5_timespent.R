library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)


#
# Read in and compile ------------------------------------------------------------------------------------------------------------
#

## Read in timespent data
timespent_i2vtto7j <- read_excel("data/2018-06-13-ESSL/SESSION7_i2vtto7j/TimeSpent.xlsx")
timespent_jllmf4m8 <- read_excel("data/2018-06-13-ESSL/SESSION14_jllmf4m8/TimeSpent.xlsx")
timespent_zjb1wxmz <- read_excel("data/2018-06-13-ESSL/SESSION4_zjb1wxmz/TimeSpent.xlsx")
timespent_2o63t7n4 <- read_excel("data/2018-08-13-ESSL/SESSION13_2o63t7n4/TimeSpent.xlsx")
timespent_j0bmhbzp <- read_excel("data/2018-08-13-ESSL/SESSION22_j0bmhbzp/TimeSpent.xlsx")
timespent_hzopjb8y <- read_excel("data/2018-08-21-ESSL/SESSION1_hzopjb8y/TimeSpent.xlsx")

timespent_8otz5sb0 <- read_excel("data/2018-09-15-CGU/SESSION10_8otz5sb0/TimeSpent.xlsx")
timespent_xsrkblkf <- read_excel("data/2018-09-19-VT/SESSION9_xsrkblkf/TimeSpent.xlsx")
timespent_tgfavzdh <- read_excel("data/2018-09-19-VT/SESSION8_tgfavzdh/TimeSpent.xlsx")
timespent_p5pcjkjc <- read_excel("data/2018-09-19-VT/SESSION2_p5pcjkjc/TimeSpent.xlsx")
timespent_5uwv0c88 <- read_excel("data/2018-09-20-VT/SESSION16_5uwv0c88/TimeSpent.xlsx")
timespent_vjny1l7l <- read_excel("data/2018-09-20-VT/SESSION12_vjny1l7l/TimeSpent.xlsx")

## Appending timespent data
timespent <- bind_rows(timespent_i2vtto7j, timespent_jllmf4m8, timespent_zjb1wxmz, timespent_2o63t7n4, timespent_j0bmhbzp, timespent_hzopjb8y,
                      timespent_8otz5sb0, timespent_xsrkblkf, timespent_tgfavzdh, timespent_p5pcjkjc, timespent_5uwv0c88, timespent_vjny1l7l)


#
# Missingness ------------------------------------------------------------------------------------------------------------
#

colSums(is.na(timespent))


#
# Time spent ------------------------------------------------------------------------------------------------------------
#

## Time spent by page index
time1 <- timespent %>%
  filter(page_index > 1) %>%
  group_by(page_index) %>%
  summarize(meant = mean(seconds_on_page), sdt = sd(seconds_on_page), mint = min(seconds_on_page), maxt = max(seconds_on_page))

ggplot(time1, aes(x = page_index, y = meant)) + 
  geom_col() +
  labs(title = "Time spent by page index", x = "Page index", y = "Mean time (seconds)", caption = "Note: Page index 1 coded out due to large value.")

time1a <- timespent %>%
  filter(page_index > 1) %>%
  group_by(page_index, app_name) %>%
  summarize(meant = mean(seconds_on_page), sdt = sd(seconds_on_page), mint = min(seconds_on_page), maxt = max(seconds_on_page))

ggplot(time1a, aes(x = page_index, y = meant, fill = app_name)) + 
  geom_col() +
  labs(title = "Time spent by page index", x = "Page index", y = "Mean time (seconds)", fill = "Section", caption = "Note: Page index 1 coded out due to large value.")

## Time spent by page type
time2 <- timespent %>%
  filter(page_name != "WelcomePage") %>%
  group_by(page_name) %>%
  summarize(meant = mean(seconds_on_page), sdt = sd(seconds_on_page), mint = min(seconds_on_page), maxt = max(seconds_on_page))

ggplot(time2, aes(x = reorder(page_name, meant), y = meant)) + 
  geom_col() +
  coord_flip() +
  labs(title = "Time spent by page type", x = "Page type", y = "Mean time (seconds)", caption = "Note: Page type WelcomePage coded out due to large value.")

## Time spent by section
time3 <- timespent %>%
  filter(app_name != "welcome_consent") %>%
  group_by(app_name) %>%
  summarize(meant = mean(seconds_on_page), sdt = sd(seconds_on_page), mint = min(seconds_on_page), maxt = max(seconds_on_page))

ggplot(time3, aes(x = reorder(app_name, meant), y = meant)) + 
  geom_col() +
  coord_flip() +
  labs(title = "Time spent by section", x = "Section", y = "Mean time (seconds)", caption = "Note: Section welcome_consent coded out due to large value.")

## By session: Time spent by section
# Getting session identifier from survey files
fromsurvey <- experiments %>%
  select(participant.id_in_session, participant.code, participant.payoff.x, player.subsession_id.x, player.group_id.x, session.code.x, session.label.x, 
         player.q1_birthYear, player.q6a_sexGender, player.q8a_education_overview, player.q9_major, contains("player.q10"), contains("player.q12"), contains("player.trust")) 

fromtime <- timespent %>%
  select(-session_id, -auto_submitted)
colnames(fromtime)[colnames(fromtime) == "participant__code"] <- "participant.code"

# Merging
surveytime <- merge(fromtime, fromsurvey, by = "participant.code")

# Plot
time4 <- surveytime %>%
  filter(app_name != "welcome_consent") %>%
  group_by(session.code.x, app_name) %>%
  summarize(meant = mean(seconds_on_page), sdt = sd(seconds_on_page), mint = min(seconds_on_page), maxt = max(seconds_on_page))

ggplot(time4, aes(x = reorder(session.code.x, meant), y = meant, fill = app_name, stat = "identity")) + 
  geom_col() +
  coord_flip() +
  labs(title = "Time spent on sections by session", x = "Session", y = "Mean time (seconds)", fill = "Section", caption = "Note: Section welcome_consent coded out due to large value.") +
  scale_fill_brewer(palette = "Pastel1")