# Script for Blog Post "Are We Drowning in Conflict Data?"
# Author: Felix Haass
# Licence: CC BY-SA-NC
# 
# The script is using a modified version of the 

library(ggplot2)
library(Cairo)

source("GScholar_scrape.R")

#######
# JPR #
#######

jpr <- GScholar_Scraper("allintitle: data OR dataset", journal = "\"Journal+of+Peace+Research\"")
jpr$pub <- "JPR"
jpr$count <- 1

# add "empty" years; makes for better plotting
jpr <- rbind(jpr, data.frame(TITLES = NA, 
                             PUBLICATION = NA, 
                             YEAR = c(1990:2014)[!(1990:2014 %in% jpr$YEAR )], 
                             pub = "JPR", count = 0))

##############################
# International Interactions #
##############################

ii <- GScholar_Scraper("allintitle: data OR dataset", journal = "\"International+Interactions\"")
ii$pub <- "II"
ii$count <- 1

# add empty years
ii <- rbind(ii, data.frame(TITLES = NA, 
                           PUBLICATION = NA, 
                           YEAR = c(1990:2014)[!(1990:2014 %in% ii$YEAR )], 
                           pub = "II", count = 0))

########
# CMPS #
########

cmps <- GScholar_Scraper("allintitle: data OR dataset", journal = "\"Conflict+Management+and+Peace+Science\"")
cmps$pub <- "CMPS"
cmps$count <- 1

cmps <- rbind(cmps, data.frame(TITLES = NA, 
                               PUBLICATION = NA, 
                               YEAR = c(1990:2014)[!(1990:2014 %in% cmps$YEAR )], 
                               pub = "CMPS", count = 0))

# combine & aggregate datasets
jpr <- jpr %>% mutate(issues = ifelse(YEAR < 1998, 4, 6))

# manual coding of cmps issues / year
cmps_issues <- data.frame(YEAR = 1990:2014, 
                          issues = c(1, 1, 1, 2, 1, 2,2,0,2,2,1,1,2,2,4,4,4,4,4,5,5,5,5,5,5))

cmps <- merge(cmps, cmps_issues, by = "YEAR", all.x = TRUE)

# international interactions issues / year
ii <- ii %>% mutate(issues = ifelse(YEAR < 2011, 4, 5))

data_articles <- rbind(jpr, ii, cmps)
df_ag <- data_articles %>% 
  filter(YEAR != 2015) %>% 
  group_by(YEAR, pub) %>% 
  summarize(count = sum(count),
            issues = min(issues),
            count_by_issue = count / issues) %>% 
  ungroup() %>% 
  arrange(pub, YEAR)
 
# Plots!
 
plot_df <- ggplot(df_ag, aes(x=YEAR, y = count, fill = pub)) +
  geom_area(aes(fill = pub, group = pub), position = "stack") +
  geom_line(position = "stack", aes(ymax = 26)) + 
  geom_point(position = "stack", type = 5, size = 1.2, aes(ymax = 26)) +
  scale_fill_brewer(name = "Journal", palette = "Set1") + 
  theme_bw() +
  scale_x_continuous(breaks = seq(1990,2014,2)) +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  labs(x = "", y = "No. of Data Articles ", title = "Search for \"data OR dataset\" in article title\n")

CairoPNG(file = "data_trend.png", width = 9.39, height = 4.41, units = "in", dpi = 300)
plot_df
dev.off()

# plot counts by issue

plot_df_issues <- ggplot(df_ag, aes(x=YEAR, y = count_by_issue, fill = pub)) +
  geom_area(aes(fill = pub, group = pub), position = "stack") +
  geom_line(position = "stack", aes(ymax = 26)) + 
  geom_point(position = "stack", type = 5, size = 1.2, aes(ymax = 26)) +
  scale_fill_brewer(name = "Journal", palette = "Set1") + 
  theme_bw() +
  scale_x_continuous(breaks = seq(1990,2014,2)) +
  labs(x = "", y = "No. of Data Articles / issues", title = "Search for \"data OR dataset\" in article title\n")

CairoPNG(file = "data_trend_issue.png", width = 9.39, height = 4.41, units = "in", dpi = 300)
plot_df_issues
dev.off()

############################################
# Search for 'new data' in article + title #
############################################


#######
# JPR #
#######

jpr <- GScholar_Scraper("\"new data\"", journal = "\"Journal+of+Peace+Research\"")
jpr$pub <- "JPR"
jpr$count <- 1

# add "empty" years; makes for better plotting
jpr <- rbind(jpr, data.frame(TITLES = NA, 
                             PUBLICATION = NA, 
                             YEAR = c(1990:2014)[!(1990:2014 %in% jpr$YEAR )], 
                             pub = "JPR", count = 0))

##############################
# International Interactions #
##############################

ii <- GScholar_Scraper("\"new data\"", journal = "\"International+Interactions\"")
ii$pub <- "II"
ii$count <- 1

# add empty years
ii <- rbind(ii, data.frame(TITLES = NA, 
                           PUBLICATION = NA, 
                           YEAR = c(1990:2014)[!(1990:2014 %in% ii$YEAR )], 
                           pub = "II", count = 0))

########
# CMPS #
########

cmps <- GScholar_Scraper("\"new data\"", journal = "\"Conflict+Management+and+Peace+Science\"")
cmps$pub <- "CMPS"
cmps$count <- 1

cmps <- rbind(cmps, data.frame(TITLES = NA, 
                               PUBLICATION = NA, 
                               YEAR = c(1990:2014)[!(1990:2014 %in% cmps$YEAR )], 
                               pub = "CMPS", count = 0))

# add issues
jpr <- jpr %>% mutate(issues = ifelse(YEAR < 1998, 4, 6))

# manual coding of cmps issues / year
cmps_issues <- data.frame(YEAR = 1990:2014, 
                          issues = c(1, 1, 1, 2, 1, 2,2,0,2,2,1,1,2,2,4,4,4,4,4,5,5,5,5,5,5))

cmps <- merge(cmps, cmps_issues, by = "YEAR", all.x = TRUE)

# international interactions issues / year
ii <- ii %>% mutate(issues = ifelse(YEAR < 2011, 4, 5))

# combine & aggregate datasets
data_articles <- rbind(jpr, ii, cmps)


df_ag2 <- data_articles %>% 
  filter(YEAR != 2015) %>% 
  group_by(YEAR, pub) %>% 
  summarize(count = sum(count),
            issues = min(issues),
            count_by_issue = count / issues) %>% 
  ungroup() %>% 
  group_by(pub) %>% 
  mutate(cumsum_count = cumsum(count)) %>% 
  arrange(pub, YEAR)

plot_all_data <- ggplot(df_ag2, aes(x=YEAR, y = count, fill = pub)) +
  geom_area(aes(fill = pub, group = pub), position = "stack") +
  geom_line(position = "stack", aes(ymax = 26)) + 
  geom_point(position = "stack", type = 5, size = 1.2, aes(ymax = 26)) +
  scale_fill_brewer(name = "Journal", palette = "Set1") + 
  theme_bw() +
  scale_x_continuous(breaks = seq(1990,2014,2)) +
  labs(x = "", y = "No. of Data Articles", title = "Search for \"new data\" in article title & body \n" )

CairoPNG(file = "data_trend_all.png", width = 9.39, height = 4.41, units = "in", dpi = 300)
plot_all_data
dev.off()

