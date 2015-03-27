library(ggplot2)

source("")


#######
# JPR #
#######

jpr <- GScholar_Scraper("allintitle: data OR dataset", journal = "Journal+of+Peace+Research")
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

ii <- GScholar_Scraper("allintitle: data OR dataset", journal = "International+Interactions")
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

cmps <- GScholar_Scraper("allintitle: data OR dataset", journal = "Conflict+Management+and+Peace+Science")
cmps$pub <- "CMPS"
cmps$count <- 1

cmps <- rbind(cmps, data.frame(TITLES = NA, 
                               PUBLICATION = NA, 
                               YEAR = c(1990:2014)[!(1990:2014 %in% cmps$YEAR )], 
                               pub = "CMPS", count = 0))

# combine & aggregate datasets
data_articles <- rbind(jpr, ii, cmps)
df_ag <- aggregate(count ~ YEAR + pub, data_articles, sum)

# without 2015
df_ag <- df_ag[df_ag$YEAR != 2015, ]

plot_df <- ggplot(df_ag, aes(x=YEAR, y = count, fill = pub)) +
  geom_area(aes(fill = pub, group = pub), position = "stack") +
  geom_line(position = "stack", aes(ymax = 26)) + 
  geom_point(position = "stack", type = 5, size = 1.2, aes(ymax = 26)) +
  scale_fill_brewer(name = "Journal", palette = "Set1") + 
  theme_bw() +
  scale_x_continuous(breaks = seq(1990,2014,2)) +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  labs(x = "", y = "No. of Data Articles / year")


plot_df

###
# "new data
#


#######
# JPR #
#######

jpr <- GScholar_Scraper("\"new data\"", journal = "Journal+of+Peace+Research")
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

ii <- GScholar_Scraper("\"new data\"", journal = "International+Interactions")
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

cmps <- GScholar_Scraper("\"new data\"", journal = "Conflict+Management+and+Peace+Science")
cmps$pub <- "CMPS"
cmps$count <- 1

cmps <- rbind(cmps, data.frame(TITLES = NA, 
                               PUBLICATION = NA, 
                               YEAR = c(1990:2014)[!(1990:2014 %in% cmps$YEAR )], 
                               pub = "CMPS", count = 0))

# combine & aggregate datasets
data_articles <- rbind(jpr, ii, cmps)
df_ag <- aggregate(count ~ YEAR + pub, data_articles, sum)

# without 2015
df_ag <- df_ag[df_ag$YEAR != 2015, ]

plot_df <- ggplot(df_ag, aes(x=YEAR, y = count, fill = pub)) +
  geom_area(aes(fill = pub, group = pub), position = "stack") +
  geom_line(position = "stack", aes(ymax = 26)) + 
  geom_point(position = "stack", type = 5, size = 1.2, aes(ymax = 26)) +
  scale_fill_brewer(name = "Journal", palette = "Set1") + 
  theme_bw() +
  scale_x_continuous(breaks = seq(1990,2014,2)) +
  scale_y_continuous(breaks = seq(0, 60, 5)) +
  labs(x = "", y = "No. of Data Articles / year")

plot_df
