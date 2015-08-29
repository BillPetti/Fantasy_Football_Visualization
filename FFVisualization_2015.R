#########################################
# Fantasy Football 2015
# Bill Petti
# August 2015
# This code has been updated from 2014
#########################################

# XML for web scraping
# dplyr and reshape for data munging
# RMySQL for connecting to and querying databases
# reldist for calculating Gini coefficients
# shiny and plotly for visualization

require(XML)
require(dplyr)
require(reshape2)
require(RMySQL)
require(reldist)
require(shiny)
require(plotly)

# scrape one page to determine the structure

test <- readHTMLTable("http://www.fantasypros.com/nfl/rankings/qb-cheatsheets.php", stringsAsFactors = FALSE)
test <- test[[2]]
test$Position <- "QB"

# so the second element of each page is the data we want
# let's write a function that will scrape each page and then add a column with with position for each player

scrape_FFL_rankings <- function(pos) {
  url <- paste0("http://www.fantasypros.com/nfl/rankings/", pos, "-cheatsheets.php")
  data <- readHTMLTable(url, stringsAsFactors = FALSE)
  data <- data[[2]]
  data$Position <- toupper(pos)
  data
}

# creat a list of the positions to use in the function

pos <- c("qb", "rb", "wr", "te")

# use the scrape function to collect all the data for each offensive position

draft_rankings <- lapply(pos, scrape_FFL_rankings)
draft_rankings <- do.call(rbind, draft_rankings)

# now we need to clean up the data set
# remove the ranking column

draft_rankings <- draft_rankings[,-1]

# move the position column to the first position

draft_rankings <- draft_rankings %>% select(Position, everything())

# now we need to parse out the player name, team, and bye week from the Player column

# we can split the column by any spaces, regroup the name across multiple columns, and then merge back into the original data set

# split Player column by the space delimiter 

split <- colsplit(draft_rankings$Player, " ", c("First", "Second", "Team", "Bye", "Extra"))

# any value in the Extra column indicates a player with a XXXXX in their name (e.g. Robert Griffin III). So, let's split the rows into two sets--one for regular records and one for those with a value in Extra

reg<- filter(split, is.na(Extra))
extra <- filter(split, !is.na(Extra))

# let's work on reg first

reg$Player <- paste(reg$First, reg$Second, sep = " ")
reg <- select(reg, Player, everything())
reg <- reg[,-c(2,3,6)]
reg$Team <- gsub(",", replacement = "", reg$Team)

# now extra

extra$Player <- paste(extra$First, extra$Second, extra$Team, sep = " ")
extra <- select(extra, Player, everything())
extra <- extra[,-c(2,3,4)]
extra$Bye <- gsub(",", replacement = "", extra$Bye)
colnames(extra) <- c("Player", "Team", "Bye")

# rbind the two split data sets

split <- rbind(reg, extra)

# then cbind into our main data set and reorder

draft_rankings <- draft_rankings[,-2] %>% cbind(split) %>% select(Position, Player, Team, Bye, everything())

rm(split, extra, reg) 
  
## now that we have the ensemble draft rankings we can calculate the consistency of each player

# SQL code for pulling game by game fantasy points for 2011-2014. Database comes fromt the amazing NFL Armchair Analysts (XXXXXXXXX)

# connect to database

con<-dbConnect(RMySQL::MySQL(), dbname="NFL_ArmcharAnalysts", username = "root", port = 3306)

player_season_data <- dbGetQuery(con, "SELECT CONCAT(p.FNAME,' ',p.LNAME) as 'Player', PNAME, p.POS1 as 'Position', o.Game, o.YEAR, `FPTS` FROM offense o JOIN players p ON o.PLAYER=p.PLAYER WHERE o.YEAR >=2011 AND (p.POS1 = 'QB'OR p.POS1 ='RB' OR p.POS1 ='WR' OR p.POS1 ='TE') GROUP BY o.PLAYER, o.Game, o.YEAR ORDER BY FPTS DESC")

# create player table

players <- player_season_data %>% group_by(Player, Position, YEAR) %>% summarise(Games = n(), `Total FPTS` = sum(FPTS), `Average FPTS` = round((sum(FPTS)/n()),1))

# calculate consistency per player, per season for 2011-2013 using Gini coefficients

fpts_gini <- as.data.frame(aggregate(FPTS ~ Player + YEAR, data = player_season_data, FUN = "gini"))
fpts_gini$Consistency <- round(fpts_gini$FPTS,3)
player_table <- fpts_gini[,-3] %>% left_join(players, ., by = c("Player" = "Player", "YEAR" = "YEAR"))

# let's check that avergae consistency isn't all that different across seasons

ave_con_all <- as.data.frame(aggregate(FPTS ~ YEAR, data = player_season_data, FUN = "gini"))

# YEAR      FPTS
# 2011 0.5819839
# 2012 0.5748124
# 2013 0.5701095
# 2014 0.5704967

# it's pretty close, so maybe we could just go with the straight averages and Consistency scores without adjusting for season

# but what about how it varies by games played? Let's look at a break out at <=5, >=10 games played

con_check_playtime_to10 <- player_table %>% filter(Games < 10) %>% group_by(Position, YEAR) %>% summarise(CON_sd = sd(Consistency, na.rm = TRUE), CON_mean = mean(Consistency, na.rm = TRUE)) 

con_check_playtime_above10 <- player_table %>% filter(Games > 9) %>% group_by(Position, YEAR) %>% summarise(CON_sd = sd(Consistency, na.rm = TRUE), CON_mean = mean(Consistency, na.rm = TRUE))

con_check_playtime <- cbind(con_check_playtime_to10, con_check_playtime_above10[,-c(1:2)]) 

con_check_playtime$diff <- round((con_check_playtime[,6] - con_check_playtime[,4]),3)

# those are pretty big differences, so we need to adjust for them. I'd rather just use Consistency scores for players with a larger body of work, so we will calculate z-scores based on >= 10 games and only match them in when a player exceeded 10 games in a given season

# let's join these values to each individual player and calculate z-scores for them. We'll need to only join for players with >= 10 games, so we can split and then re-bind the data

y <- filter(player_table, Games > 9)
n <- filter(player_table, Games < 10)

# be sure to check and make sure you didn't lose any observations

count(player_table) == count(y) + count(n)

# if not, join away

y <- y %>% left_join(con_check_playtime_above10, by = c("Position" = "Position", "YEAR" = "YEAR")) 

y$`Normalized Consistency` <- round(((y$Consistency - y$CON_mean) / y$CON_sd),3)

player_table <- bind_rows(y, n)

# calculating average Consistency (based off z-score) since 2011 for each player

ave_z_score <- player_table %>% group_by(Player) %>% summarise(`Ave Consistency` = round((mean(`Normalized Consistency`, na.rm = TRUE)),3)) 

# subset by 2014 and then join, so we will have last year's Consistency, z-score, and 4-year average z-score

player_2014 <- filter(player_table, YEAR == 2014) %>% left_join(ave_z_score, by = "Player")

player_2014 <- player_2014[,-c(3,8,9)]
colnames(player_2014) <- c("Player", "Position", "Games (2014)", "FPTS (2014)", "Average FPTS (2014)", "2014 Consistency", "2014 Normalized Consistency", "Normalized Consistency (2011-14)")

## Now we need to scrape data for the actual point projections by player

# let's write a function that will scrape each page and then add a column with with position for each player

scrape_FFL_point_proj <- function(pos) {
  url <- paste0("http://www.fantasypros.com/nfl/projections/", pos, ".php")
  data <- readHTMLTable(url, stringsAsFactors = FALSE)
  data <- data[[2]]
  data$Position <- toupper(pos)
  data
}

# creat a list of the positions to use in the function

pos <- c("qb", "rb", "wr", "te")

# use the scrape function to collect all the data for each offensive position

draft_proj <- lapply(pos, scrape_FFL_point_proj)

# we don't need all the columns, so let's create separte data frames for each position, but with the same columns, then rbind

qb <- as.data.frame(draft_proj[1]) %>% select(Player, Position, FPTS)
rb <- as.data.frame(draft_proj[2]) %>% select(Player, Position, FPTS)
wr <- as.data.frame(draft_proj[3]) %>% select(Player, Position, FPTS)
te <- as.data.frame(draft_proj[4]) %>% select(Player, Position, FPTS)

draft_proj <- rbind(qb, rb, wr, te)

# split Player column by the space delimiter 

split <- colsplit(draft_proj$Player, " ", c("First", "Second", "Team", "Extra"))

extra <- filter(split, Extra > 0)
reg <- filter(split, Extra <=0)

# let's work on reg first

reg$Player <- paste(reg$First, reg$Second, sep = " ")
reg <- select(reg, Player, everything())
reg <- reg[,-c(2,3,5)]

# now extra

extra$Player <- paste(extra$First, extra$Second, extra$Team, sep = " ")
extra <- select(extra, Player, everything())
extra <- extra[,-c(2,3,4)]
colnames(extra) <- c("Player", "Team")

# rbind the two split data sets

split <- rbind(reg, extra)

# then cbind into our main data set and reorder

draft_proj <- draft_proj[,-1] %>% cbind(split) %>% select(Position, Player, Team, everything())

rm(split, reg, extra)

# join the total FPTS in to the draft rankings table

draft_combined <- left_join(draft_proj, draft_rankings[,-11], by = c("Player" = "Player", "Position" = "Position", "Team" = "Team"))

# join the draft_combined table to the player_2014 table

join <- left_join(draft_combined, player_2014, by = c("Player" = "Player", "Position" = "Position"))

# adjust the variable types to numeric

join[,c(4:10)] <- sapply(join[,c(4:10)], as.numeric)

# export the final file 
write.csv(join, file="FF_2015_draft_data.csv", row.names = FALSE)

## now let's build a visual for the data

# we should clean up the data set

ffl_data <- join[,-11]

# let's add a column for Projected PPG 2015

ffl_data$`Projected PPG 2015` <- round(ffl_data$FPTS/16,1)

# and rename a few columns

colnames(ffl_data)[6:10] <- c("Best Draft Rank", "Worst Draft Rank", "Average Draft Rank", "Standard Deviation Draft Rank", "Average Draft Position")

# and export the file again

write.csv(ffl_data, file="FF_2015_draft_data2.csv", row.names = FALSE)
