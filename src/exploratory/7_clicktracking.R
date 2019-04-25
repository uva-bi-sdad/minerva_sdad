library(readxl)
library(dplyr)


#
# Read in and compile ------------------------------------------------------------------------------------------------------------
#

# Reading in files
clicktracking_i2vtto7j <- read_excel("/home/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION7_i2vtto7j/Clicktracking.xlsx")
clicktracking_jllmf4m8 <- read_excel("/home/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION14_jllmf4m8/Clicktracking.xlsx")
clicktracking_zjb1wxmz <- read_excel("/home/sdal/projects/minerva/data/2018-06-13-ESSL/SESSION4_zjb1wxmz/Clicktracking.xlsx")
clicktracking_2o63t7n4 <- read_excel("/home/sdal/projects/minerva/data/2018-08-13-ESSL/SESSION13_2o63t7n4/Clicktracking.xlsx")
clicktracking_j0bmhbzp <- read_excel("/home/sdal/projects/minerva/data/2018-08-13-ESSL/SESSION22_j0bmhbzp/Clicktracking.xlsx")
clicktracking_hzopjb8y <- read_excel("/home/sdal/projects/minerva/data/2018-08-21-ESSL/SESSION1_hzopjb8y/Clicktracking.xlsx")

clicktracking_8otz5sb0 <- read_excel("/home/sdal/projects/minerva/data/2018-09-15-CGU/SESSION10_8otz5sb0/Clicktracking.xlsx")
clicktracking_xsrkblkf <- read_excel("/home/sdal/projects/minerva/data/2018-09-19-VT/SESSION9_xsrkblkf/Clicktracking.xlsx")
clicktracking_tgfavzdh <- read_excel("/home/sdal/projects/minerva/data/2018-09-19-VT/SESSION8_tgfavzdh/Clicktracking.xlsx")
clicktracking_p5pcjkjc <- read_excel("/home/sdal/projects/minerva/data/2018-09-19-VT/SESSION2_p5pcjkjc/Clicktracking.xlsx")
clicktracking_5uwv0c88 <- read_excel("/home/sdal/projects/minerva/data/2018-09-20-VT/SESSION16_5uwv0c88/Clicktracking.xlsx")
clicktracking_vjny1l7l <- read_excel("/home/sdal/projects/minerva/data/2018-09-20-VT/SESSION12_vjny1l7l/Clicktracking.xlsx")

# Appending
clicktracking <- bind_rows(clicktracking_i2vtto7j, clicktracking_jllmf4m8, clicktracking_zjb1wxmz, clicktracking_2o63t7n4, clicktracking_j0bmhbzp, clicktracking_hzopjb8y,
                           clicktracking_8otz5sb0, clicktracking_xsrkblkf, clicktracking_tgfavzdh, clicktracking_p5pcjkjc, clicktracking_5uwv0c88, clicktracking_vjny1l7l)

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

clicktracking$comm <- NA
clicktracking$comm[clicktracking$session__code %in% c("i2vtto7j", "zjb1wxmz", "jllmf4m8", "xsrkblkf")] <- "wall"
clicktracking$comm[clicktracking$session__code %in% c("j0bmhbzp", "2o63t7n4", "tgfavzdh", "vjny1l7l")] <- "bilateral"
clicktracking$comm[clicktracking$session__code %in% c("hzopjb8y", "8otz5sb0", "p5pcjkjc", "5uwv0c88")] <- "none"
clicktracking$comm <- ordered(clicktracking$comm, c("none", "wall", "bilateral"))

table(clicktracking$session__code, clicktracking$comm)

## Add network knowledge variable, do sanity check
# zjb1wxmz == global
# i2vtto7j == local
# jllmf4m8 == local
# 2o63t7n4 == global
# j0bmhbzp == local
# hzopjb8y == local

clicktracking$netw <- NA
clicktracking$netw[clicktracking$session__code %in% c("i2vtto7j", "jllmf4m8", "j0bmhbzp", "hzopjb8y", "tgfavzdh", "5uwv0c88")] <- "local"
clicktracking$netw[clicktracking$session__code %in% c("zjb1wxmz", "2o63t7n4", "8otz5sb0", "xsrkblkf", "p5pcjkjc", "vjny1l7l")] <- "global"
clicktracking$netw <- ordered(clicktracking$netw, c("local", "global"))

#
# Sending messages ------------------------------------------------------------------------------------------------------------
#

# Sending messages by network knowledge
send_net <- clicktracking %>%
  filter(element == "send-message-button")  %>%
  group_by(netw) %>%
  summarize(sendn = n())
send_net

ggplot(send_net, aes(x = netw, y = sendn)) + 
  geom_col() +
  labs(title = "Number of messages sent by network knowledge", x = "Network knowledge", y = "# Messages sent", caption = "Note: Sample size differs between conditions.")

# Sending messages by communication type
send_comm <- clicktracking %>%
  filter(element == "send-message-button")  %>%
  group_by(comm) %>%
  summarize(sendn = n())
send_comm

ggplot(send_comm, aes(x = comm, y = sendn)) + 
  geom_col() +
  labs(title = "Number of messages sent by communication type", x = "Communication type", y = "# Messages sent", caption = "Note: Sample size differs between conditions.")

