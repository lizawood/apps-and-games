# PC GAMES Owners and Play Time Analysis
#
# Data is what is available for free from steamspy.com.
# 
# Goal: understand top games by number of owners and which ones are still being played long after release.
# 
# Playtime is the average (median) length of time the game was played in the last two weeks (July 1 - 14, 2019). 
# For current or live games, there should be a lot of games with fairly high playtimes. What about old games? 
# Which ones have stood the test of time? What are we nostaligic for?

library(readxl)
library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(viridis)

# Load in a combine all the data:

file.list <- list.files(pattern='*.xlsx')
df.list <- lapply(file.list, read_excel)
df <- bind_rows(df.list)
df

#----------------
# TIDY THE DATA
#----------------

# Separate Score Rank from User/Metascore:
df <- df %>% 
  separate(`Score rank(Userscore / Metascore)`, into = c("ScoreRank", "Metascore"), sep = " ")
df

# Extract Metascore and remove leading N/A:
df$Metascore <- sapply(df$Metascore, function(x) gsub("\\(N/A/", "", x))
df

# Remove parantheses around Metascore:
df$Metascore <- sapply(df$Metascore, function(x) gsub("\\(", "", x))
df$Metascore <- sapply(df$Metascore, function(x) gsub("\\)", "", x))
df

# Separate Playtime from Median Playtime:
df <- df %>%
  separate(`Playtime (Median)`, into = c("AvgRecentPlaytime","MedianRecentPlaytime"), sep = " ")
df

# Remove parantheses around Median Recent Playtime:
df$MedianRecentPlaytime <- sapply(df$MedianRecentPlaytime, function(x) gsub("\\(", "", x))
df$MedianRecentPlaytime <- sapply(df$MedianRecentPlaytime, function(x) gsub("\\)", "", x))
df

# Rename columns with ``
df <- df %>% rename(number = `#`) 
df <- df %>% rename(ReleaseDate = `Release date`) 
df <- df %>% rename(Developer = `Developer(s)`)
df <- df %>% rename(Publisher = `Publisher(s)`) 
df

# Convert ReleaseDate to date
df$ReleaseDate <- mdy(df$ReleaseDate)
df

# Convert Price to number and replace "Free" with 0
df$Price <- sapply(df$Price, function(x) gsub("Free", 0.00, x))
df$Price <- as.numeric(df$Price)
df

# Convert Metascore to numeric
df$Metascore <- sapply(df$Metascore, function(x) gsub("%", "", x))
df$Metascore <- as.numeric(df$Metascore)
df

# Convert RecentPlaytime and MedianRecentPlaytime to time
df$AvgRecentPlaytime <- lubridate::hm(df$AvgRecentPlaytime)
df$MedianRecentPlaytime <- lubridate::hm(df$MedianRecentPlaytime)
df

#Remove number and ScoreRank columns
df <- df %>% select(-number, -ScoreRank)
df

# Export file with tidy data
write_csv(df, "PCgames.csv")

#---------------------------
# LET'S START VISUALIZING
#---------------------------

pcgames <- df

# How many games were released on Steam each year and how many are still being played?

#Separate out the year
pcgames <- pcgames %>%
  mutate_at(vars(ReleaseDate), funs(year, month, day)) 
pcgames

#Count how many were played in the last two weeks and howmany were released per year.
played <- pcgames %>% 
  group_by(year) %>%
  tally(MedianRecentPlaytime>0, name = "nPlayed") 
played
total <- pcgames %>% count(year, name = "TotalGames")
total
comp1 <- full_join(total, played)
comp1

#Let's see what the charts look like. We're less interested in scale and more interested in shape.
totalplot <- ggplot(data = comp1) +
  geom_bar(mapping = aes(x = year, y = TotalGames), stat = "identity") +
  ggtitle("PC Games Released on Steam \nby Release Year") +
  labs(x="Release Year", y="Number of Games Released")
playedplot <- ggplot(data = comp1) +
  geom_bar(mapping = aes(x = year, y = nPlayed), stat = "identity") +
  ggtitle("PC Games Played 1-15 July 2019 \nby Release Year") +
  labs(x="Release Year", y="Number of Games Played 1-15 July 2019")
plot_grid(totalplot, playedplot)

# We're look for years that have more games played relative to the number released that year.
# Interesting years: 2004, 2009, 2012. What games with >1M owners are being played in order of playtime?

# Filter games with > 1M owners
# NOTE: the Owners column doesn't seem to filter directly

(ownergroups <- pcgames %>% count(Owners))
(ownergroups <- filter(ownergroups, n < 500))
(ogroups <- ownergroups$Owners)

pcgamesg1M <- filter(pcgames, Owners %in% ogroups)
pcgamesg1M

#2004 
four <- filter(pcgamesg1M, year == 2004, MedianRecentPlaytime>0)
four$AvgRecentPlaytimeHrs <- as.duration(four$AvgRecentPlaytime)
four$AvgRecentPlaytimeHrs <- as.numeric(four$AvgRecentPlaytimeHrs, "hours")
ggplot(data = four) +
  geom_bar(mapping = aes(x = Game, y = AvgRecentPlaytimeHrs, fill = Owners), stat = "identity") +
  ggtitle("PC Games Played July 2019 Released in 2004") +
  labs(x = "Game Title", y = "Average 2-Week Playtime (Hours)") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#2009
nine <- filter(pcgamesg1M, year == 2009, MedianRecentPlaytime>0)
nine$AvgRecentPlaytimeHrs <- as.duration(nine$AvgRecentPlaytime)
nine$AvgRecentPlaytimeHrs <- as.numeric(nine$AvgRecentPlaytimeHrs, "hours")
nine$Owners <- as_factor(nine$Owners)
nine$Owners <- factor(nine$Owners, levels = rev(levels(nine$Owners)))

ggplot(data = nine) +
  geom_bar(mapping = aes(x = Game, y = AvgRecentPlaytimeHrs, fill = Owners), stat = "identity") + 
  ggtitle("PC Games Played July 2019 Released in 2009") +
  labs(x = "Game Title", y = "Average 2-Week Playtime (Hours)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "right")
  
#2012
twelve <- filter(pcgamesg1M, year == 2012, MedianRecentPlaytime>0)
twelve$AvgRecentPlaytimeHrs <- as.duration(twelve$AvgRecentPlaytime)
twelve$AvgRecentPlaytimeHrs <- as.numeric(twelve$AvgRecentPlaytimeHrs, "hours")
twelve$Owners <- as_factor(twelve$Owners)
twelve$Owners <- factor(twelve$Owners, levels = rev(levels(twelve$Owners)))

ggplot(data = twelve) +
  geom_bar(mapping = aes(x = Game, y = AvgRecentPlaytimeHrs, fill = Owners), stat = "identity") + 
  ggtitle("PC Games Played July 2019 Released in 2012") +
  labs(x = "Game Title", y = "Average 2-Week Playtime (Hours)") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "right")

# What are all old games with 1M+ owners that are still being played?

# Games with 10M+ owners
(tenM <- filter(ownergroups, n <= 30))
(tenMowners <- tenM$Owners)
tenMgames <- filter(pcgames, Owners %in% tenMowners)
tenMgames

# Games 5+ years old (some are still actively maintained, though)
tenMgames$AvgRecentPlaytimeHrs <- as.duration(tenMgames$AvgRecentPlaytime)
tenMgames$AvgRecentPlaytimeHrs <- as.numeric(tenMgames$AvgRecentPlaytimeHrs, "hours")
tenMplayed <- filter(tenMgames, AvgRecentPlaytimeHrs>1, year < 2014)

ggplot(data = tenMplayed) +
  geom_bar(mapping = aes(x = Game, y = AvgRecentPlaytimeHrs, fill = Metascore), stat = "identity") + 
  scale_fill_viridis(option = "D") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("PC Games Played July 2019 Released Before 2014 with 10M+ Owners") +
  labs(x = "Game Title", y="Average 2-Week Playtime (Hours)")

# Games with 5-10M owners
(fiveM <- filter(ownergroups, n == 50))
(fiveMowners <- fiveM$Owners)
fiveMgames <- filter(pcgames, Owners %in% fiveMowners)

# Games 5+ years old (some are still actively maintained, though)
fiveMgames$AvgRecentPlaytimeHrs <- as.duration(fiveMgames$AvgRecentPlaytime)
fiveMgames$AvgRecentPlaytimeHrs <- as.numeric(fiveMgames$AvgRecentPlaytimeHrs, "hours")
fiveMplayed <- filter(fiveMgames, AvgRecentPlaytimeHrs>1, year < 2014)

ggplot(data = fiveMplayed) +
  geom_bar(mapping = aes(x = Game, y = AvgRecentPlaytimeHrs, fill = Metascore), stat = "identity") + 
  scale_fill_viridis(option = "D") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("PC Games Played July 2019 Released Before 2014 with 5-10M Owners") +
  labs(x = "Game Title", y="Average 2-Week Playtime (Hours)")

# Games with 2-5M owners
(twoM <- filter(ownergroups, n == 211))
(twoMowners <- twoM$Owners)
twoMgames <- filter(pcgames, Owners %in% twoMowners)

# Games 5+ years old and played more than 2 hours (just to shorten the list)
twoMgames$AvgRecentPlaytimeHrs <- as.duration(twoMgames$AvgRecentPlaytime)
twoMgames$AvgRecentPlaytimeHrs <- as.numeric(twoMgames$AvgRecentPlaytimeHrs, "hours")
twoMplayed <- filter(twoMgames, AvgRecentPlaytimeHrs>2, year < 2014)

ggplot(data = twoMplayed) +
  geom_bar(mapping = aes(x = Game, y = AvgRecentPlaytimeHrs, fill = Metascore), stat = "identity") + 
  scale_fill_viridis(option = "D") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("PC Games Played July 2019 Released Before 2014 with 2-5M Owners") +
  labs(x = "Game Title", y="Average 2-Week Playtime (Hours)")

# Games with 1-2M owners
(oneM <- filter(ownergroups, n == 325))
(oneMowners <- oneM$Owners)
oneMgames <- filter(pcgames, Owners %in% oneMowners)

# Games 5+ years old and played more than 2 hours (just to shorten the list)
oneMgames$AvgRecentPlaytimeHrs <- as.duration(oneMgames$AvgRecentPlaytime)
oneMgames$AvgRecentPlaytimeHrs <- as.numeric(oneMgames$AvgRecentPlaytimeHrs, "hours")
oneMplayed <- filter(oneMgames, AvgRecentPlaytimeHrs>2, year < 2014)

ggplot(data = oneMplayed) +
  geom_bar(mapping = aes(x = Game, y = AvgRecentPlaytimeHrs, fill = Metascore), stat = "identity") + 
  scale_fill_viridis(option = "D") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("PC Games Played July 2019 Released Before 2014 with 1-2M Owners") +
  labs(x = "Game Title", y="Average 2-Week Playtime (Hours)")
