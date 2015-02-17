# Fantasy Football 2014
# Bill Petti

# code from Boris Chen (https://github.com/borisachen/fftiers/blob/master/src/main.R)
require('mclust')
require('ggplot2')

### Parameters 
thisweek = 0
download = TRUE		# Do we want to download fresh data from fantasypros?
useold = FALSE		# Do we want to use the original version of the charts?

### Set and create input / output directories
mkdir <- function(dir) system(paste("mkdir -p", dir))
datdir = "~/projects/fftiers/dat/2014/"; mkdir(datdir)
outputdir = paste("~/FFL2014/fftiers/out/week", thisweek, "/", sep=""); mkdir(outputdir)
outputdircsv = paste("~/FFL2014/fftiers/out/week", thisweek, "/csv/", sep=""); mkdir(outputdircsv)
outputdirpng = paste("~/FFL2014/fftiers/out/week", thisweek, "/png/", sep=""); mkdir(outputdirpng)
outputdirtxt = paste("~/FFL2014/fftiers/out/week", thisweek, "/txt/", sep=""); mkdir(outputdirtxt)

### Curl data from fantasypros

# Which positions do we want to fetch?
pos.list = c('qb','rb','wr','te','flex','k','dst'),
			# 'ppr-rb','ppr-wr','ppr-te','ppr-flex',
            # 'half-point-ppr-rb','half-point-ppr-wr','half-point-ppr-te','half-point-ppr-flex')
			# 'ros-qb','ros-rb','ros-wr','ros-te','ros-k', 'ros-dst')

if (download == TRUE) {
  # download data for each position
  for (mp in pos.list) {
 	curlstr = paste('curl http://www.fantasypros.com/nfl/rankings/',mp,
				'-cheatsheets.php?export=xls > ~/projects/fftiers/dat/2014/week-', 
				thisweek, '-',mp,'-raw.xls', sep="")
    system(curlstr); Sys.sleep(0.5)
    sedstr = paste("sed '1,4d' ~/projects/fftiers/dat/2014/week-", thisweek, '-',mp,'-raw.xls', 
  			  ' > ~/projects/fftiers/dat/2014/week_', thisweek, '_', mp, '.tsv',sep="")
    system(sedstr); Sys.sleep(0.5)
  }	
  
  # overall rankings download:
  overall.url = 'curl http://www.fantasypros.com/nfl/rankings/consensus-cheatsheets.php?export=xls > ~/projects/fftiers/dat/2014/week-0-all-raw.xls'
  ppr.url = 'curl http://www.fantasypros.com/nfl/rankings/ppr-cheatsheets.php?export=xls > ~/projects/fftiers/dat/2014/week-0-all-ppr-raw.xls'
  half.ppr.url = 'curl http://www.fantasypros.com/nfl/rankings/half-point-ppr-cheatsheets.php?export=xls > ~/projects/fftiers/dat/2014/week-0-all-half-ppr-raw.xls'
  system(overall.url); Sys.sleep(0.5); system(ppr.url); Sys.sleep(0.5); system(half.ppr.url); Sys.sleep(0.5)
  sedstr = paste("sed '1,4d' ~/projects/fftiers/dat/2014/week-", thisweek, '-all-raw.xls', 
  			  ' > ~/projects/fftiers/dat/2014/week_', thisweek, '_', 'all', '.tsv',sep="")
  sedstr2 = paste("sed '1,4d' ~/projects/fftiers/dat/2014/week-", thisweek, '-all-ppr-raw.xls', 
  			  ' > ~/projects/fftiers/dat/2014/week_', thisweek, '_', 'all-ppr', '.tsv',sep="")
  sedstr3 = paste("sed '1,4d' ~/projects/fftiers/dat/2014/week-", thisweek, '-all-half-ppr-raw.xls', 
  			  ' > ~/projects/fftiers/dat/2014/week_', thisweek, '_', 'all-half-ppr', '.tsv',sep="")
  system(sedstr);  system(sedstr2); system(sedstr3);
  
}


### main plotting function

error.bar.plot <- function(pos="NA", low=1, high=24, k=8, format="NA", title="dummy", tpos="QB", dat, adjust=0, XLOW=0, highcolor=360) {
	#if (tpos!='ALL') title = paste("Week ",thisweek," - ",tpos," Tiers", sep="")
	if (tpos!='ALL') title = paste("Pre-draft - ",tpos," Tiers", sep="")
	if (tpos=='ALL') title = paste("Pre-draft Tiers - Top 200", sep="")
	dat$Rank = 1:nrow(dat)
	this.pos = dat
	this.pos = this.pos[low:high,]
	this.pos$position.rank <- low+c(1:nrow(this.pos))-1	
  	this.pos$position.rank = -this.pos$position.rank

	# Find clusters
	df = this.pos[,c(which(colnames(this.pos)=="Ave.Rank"))]
	mclust <- Mclust(df, G=k)
	this.pos$mcluster <-  mclust$class
	
	# Print out names
	fileConn<-file(paste(outputdirtxt,"text_",tpos,".txt",sep=""))
	if ((tpos == 'ALL') | (tpos == 'ALL-PPR')| (tpos == 'ALL-HALF-PPR')) fileConn<-file(paste(outputdirtxt,"text_",tpos,'-adjust', adjust,".txt",sep=""))
	tier.list = array("", k)
	for (i in 1:k) {
      foo <- this.pos[this.pos $cluster==i,]
      foo <- this.pos[this.pos $mcluster==i,]
      es = paste("Tier ",i,": ",sep="")
      if (adjust>0) es = paste("Tier ",i+adjust,": ",sep="")
      for (j in 1:nrow(foo)) es = paste(es,foo$Player.Name[j], ", ", sep="")
      es = substring(es, 1, nchar(es)-2)
      tier.list[i] = es
    }
    writeLines(tier.list, fileConn); close(fileConn)
	this.pos$nchar 	= nchar(as.character(this.pos$Player.Name))
	this.pos$Tier 	= factor(this.pos$mcluster)
	if (adjust>0) this.pos$Tier 	= as.character(as.numeric(as.character(this.pos$mcluster))+adjust)


	bigfont			= c("QB","TE","K","DST", "PPR-TE", "ROS-TE", "0.5 PPR-TE", "ROS-QB")
	smfont			= c("RB", "PPR-RB", "ROS-RB", "0.5 PPR-RB")
	tinyfont		= c("WR","Flex", "PPR-WR", "ROS-WR","PPR-Flex", "0.5 PPR-WR","0.5 PPR-Flex", 'ALL', 'ALL-PPR', 'ALL-HALF-PPR')
	
	if (tpos %in% bigfont) {font = 3.5; barsize=1.5;  dotsize=2;   }
	if (tpos %in% smfont)  {font = 3;   barsize=1.25; dotsize=1.5; }
	if (tpos %in% tinyfont){font = 2.5; barsize=1;    dotsize=1;   }
	if (tpos %in% "ALL")   {font = 2.4; barsize=1;    dotsize=0.8;   }
	
	p = ggplot(this.pos, aes(x=position.rank, y=Ave.Rank))
	p = p + ggtitle(title)
    p = p + geom_errorbar(aes(ymin=Ave.Rank-Std.Dev/2, ymax= Ave.Rank + Std.Dev/2, 
    		width=0.2, colour=Tier), size=barsize*0.8, alpha=0.4)
	p = p + geom_point(colour="grey20", size=dotsize) 
    p = p + coord_flip()
    p = p + annotate("text", x = Inf, y = -Inf, label = "www.borischen.co", hjust=1.1, 
    		vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	if (tpos %in% bigfont)     			
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Ave.Rank - nchar/6 - Std.Dev/1.4), size=font)
	if (tpos %in% smfont)     			
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Ave.Rank - nchar/5 - Std.Dev/1.5), size=font) 
	if (tpos %in% tinyfont)     			
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Ave.Rank - nchar/3 - Std.Dev/1.8), size=font) 
    if ((tpos == 'ALL') | (tpos == 'ALL-PPR'))
    	p = p + geom_text(aes(label=Player.Name, colour=Tier, y = Ave.Rank - nchar/3 - Std.Dev/1.8), size=font) + geom_text(aes(label=Position, y = Ave.Rank + Std.Dev/1.8 + 1), size=font, colour='#888888') 
    p = p + scale_x_continuous("Expert Consensus Rank")
    p = p + ylab("Average Expert Rank")
    p = p + theme(legend.justification=c(1,1), legend.position=c(1,1))
    p = p + scale_colour_discrete(name="Tier")
	p = p + scale_colour_hue(l=55, h=c(0, highcolor))
    maxy = max( abs(this.pos$Ave.Rank)+this.pos$Std.Dev/2) 
	if (tpos!='Flex') p = p + ylim(-4, maxy)
    if (tpos=="Flex") p = p + ylim(4, maxy)
	if ((tpos == 'ALL') | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')) p = p + ylim(low-XLOW, maxy+5)
	outfile = paste(outputdirpng, "week-", thisweek, "-", tpos, ".png", sep="")
	if ((tpos == 'ALL') | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')) outfile = paste(outputdirpng, "week-", thisweek, "-", tpos,'-adjust',adjust, ".png", sep="")
	
	if (useold == TRUE) {
		this.pos$position.rank = -this.pos$position.rank 
		this.pos$Ave.Rank = -this.pos$Ave.Rank 
	  p = ggplot(this.pos, aes(x=position.rank, y=Ave.Rank))
	  p = p + ggtitle(title)
      p = p + geom_errorbar(aes(ymin=Ave.Rank-Std.Dev/2, ymax= Ave.Rank + Std.Dev/2, width=0.2), colour="grey80")
  	  p = p + geom_point(colour="grey20", size=dotsize, alpha=0.5) 
      p = p + annotate("text", x = Inf, y = -Inf, label = "www.borischen.co", hjust=1.1, 
    		vjust=-1.1, col="white", cex=6, fontface = "bold", alpha = 0.8)
	  if (tpos %in% bigfont) p = p + geom_text(aes(label=Player.Name, colour=factor(mcluster), y = Ave.Rank), size=font, angle=15) 
	  if (tpos %in% smfont) p = p + geom_text(aes(label=Player.Name, colour=factor(mcluster), y = Ave.Rank), size=font, angle=15) 
	  if (tpos %in% tinyfont) p = p + geom_text(aes(label=Player.Name, colour=factor(mcluster), y = Ave.Rank), size=font, angle=15) 
      p = p + scale_x_continuous("xpert Consensus Rank")
      p = p + scale_y_continuous("Average Rank")
      p = p + theme(legend.position="none") 
      p = p + scale_colour_hue(l=60, h=c(0, highcolor))
      outfile = paste(outputdir, "week-", thisweek, "-", tpos, "-old.png", sep="")
	}

	# write the table to csv
	outfilecsv = paste(outputdircsv, "week-", thisweek, "-", tpos, ".csv", sep="")
	if ((tpos == 'ALL') | (tpos == 'ALL-PPR') | (tpos == 'ALL-HALF-PPR')) outfilecsv = paste(outputdircsv, "week-", thisweek, "-", tpos,'-adjust',adjust, ".csv", sep="")
	this.pos$position.rank <- this.pos$X <- this.pos$mcluster <- this.pos$nchar <- NULL
	write.csv(this.pos, outfilecsv)
	
    p
    ggsave(file=outfile, width=9.5, height=8, dpi=100)
	return(p)
}

## Wrapper function around error.bar.plot
draw.tiers <- function(pos, low, high, k, adjust=0, XLOW=0, highcolor=360) {
	dat = read.delim(paste(datdir, "week_", thisweek, "_", pos, ".tsv",sep=""), sep="\t")
 	dat <- dat[!dat$Player.Name %in% injured,]
	tpos = toupper(pos); if(pos=="flex")tpos<-"Flex"
	error.bar.plot(low = low, high = high, k=k, tpos=tpos, dat=dat, adjust=adjust, XLOW=XLOW, highcolor=highcolor)
}

## If there are any injured players, list them here to remove them
injured <- c('David Wilson','Sam Bradford')

useold=F
draw.tiers("all", 1, 43, 6, XLOW=5, highcolor=720)
draw.tiers("all", 1, 78, 10, XLOW=5, highcolor=720)
draw.tiers("all", 1, 100, 11, XLOW=5, highcolor=720)

useold=F
draw.tiers("all", 93, 180, 9, adjust=10, XLOW=18, highcolor=540)

draw.tiers("all", 41, 160, 7, adjust=10, XLOW=18, highcolor=720)

draw.tiers("all", 93, 220, 4, adjust=16, XLOW=16, highcolor=500)

draw.tiers("qb", 1, 32, 9)
draw.tiers("rb", 1, 40, 10)
draw.tiers("wr", 1, 60, 10)
draw.tiers("te", 1, 24, 8)
#draw.tiers("flex", 15, 75, 13)
draw.tiers("k", 1, 29, 5)
draw.tiers("dst", 1, 32, 6)

# PPR
draw.tiers("all-ppr", 1, 70, 10, XLOW=5)
draw.tiers("all-ppr", 71, 140, 6, adjust=10, XLOW=16)
draw.tiers("all-ppr", 141, 200, 5, adjust=16, XLOW=30)

draw.tiers("all-half-ppr", 1, 70, 10, XLOW=5)
draw.tiers("all-half-ppr", 71, 140, 6, adjust=10, XLOW=16)
draw.tiers("all-half-ppr", 141, 200, 4, adjust=16, XLOW=30)

draw.tiers("ppr-rb", 1, 40, 10)
draw.tiers("ppr-wr", 1, 60, 10)
draw.tiers("ppr-te", 1, 24, 6)

# pull game by game FPTS for offensive players, 2011-2013

# SQL code for pulling game by game fantasy points for 2011-2013. Database used: NFL Armchair Analysts
SELECT CONCAT(p.LNAME,", ",p.FNAME) as "Player.Name", PNAME, p.PLAYER, POS1 as "POS", o.Game, ROUND(AVG(`FPTS`),1) as "Avg_Fantasy_Points"

FROM offense o
JOIN players p ON o.PLAYER=p.PLAYER

WHERE o.YEAR >=2011

GROUP BY o.PLAYER, o.Game, o.YEAR

ORDER BY ROUND(AVG(`FPTS`),1)  DESC

# load data into R
NFL_gamebygame_player_FPTS <- read.csv("~/FFL2014/Fantasy_Football_Visualization/NFL_gamebygame_player_FPTS.csv", stringsAsFactors=FALSE)
fftiers_all <- read.csv("~/FFL2014/fftiers/out/week0/csv/fftiers_all.csv")
names(fftiers_all)[names(fftiers_all)=="Player.Name"] <- "Player_Name"

# save as "NFL"
NFL<-NFL_gamebygame_player_FPTS

# calculate consistency per player for 2011-2013 using Gini coefficients
# load reldist package
library(reldist)
FPTS_Gini<-aggregate(Avg_Fantasy_Points ~ Player_Name, data = NFL, FUN = "gini")

#Convert FPTS.Gini to a data frame
FPTS_Gini<-data.frame(FPTS_Gini)

#rename Avg_Fantasy_Points Consistency in the Gini output
names(FPTS_Gini)[names(FPTS_Gini)=="Avg_Fantasy_Points"] <- "Consistency"


#Merge Consistency scores and Tier/Consensus Rankings from Boris Chen's data
require(sqldf)
Gini_Tiers <- sqldf("select * from fftiers_all left join FPTS_Gini using (Player_Name)")

## Scrape data for actual point projections by player/position
require(XML)

# scrape QB Projections

Test <- htmlParse("http://www.fantasypros.com/nfl/projections/qb.php")
class(Test)
Test.table<- readHTMLTable(Test,stringsAsFactors = FALSE)

#you want the second table, not the first one
QB <- sapply(Test.table[[2]][,], FUN= function(x) as.character(gsub(",", "", as.character(x), fixed = TRUE) ))	

#convert to table
QB<-as.data.frame(QB, stringsAsFactors=FALSE)

#trim and keep only Player and FPTS columns

QB <- sqldf("select Player, FPTS from QB")

#rename Player column to Player_Name
names(QB)[names(QB)=="Player"] <- "Player_Name"

# scrape RB Projections

Test <- htmlParse("http://www.fantasypros.com/nfl/projections/rb.php")
class(Test)
Test.table<- readHTMLTable(Test,stringsAsFactors = FALSE)

#you want the second table, not the first one
RB <- sapply(Test.table[[2]][,], FUN= function(x) as.character(gsub(",", "", as.character(x), fixed = TRUE) ))	

#convert to table
RB<-as.data.frame(RB, stringsAsFactors=FALSE)

#trim and keep only Player and FPTS columns

RB <- sqldf("select Player, FPTS from RB")

#rename Player column to Player_Name
names(RB)[names(RB)=="Player"] <- "Player_Name"

# scrape WR Projections

Test <- htmlParse("http://www.fantasypros.com/nfl/projections/wr.php")
class(Test)
Test.table<- readHTMLTable(Test,stringsAsFactors = FALSE)

#you want the second table, not the first one
WR <- sapply(Test.table[[2]][,], FUN= function(x) as.character(gsub(",", "", as.character(x), fixed = TRUE) ))	

#convert to table
WR <-as.data.frame(WR, stringsAsFactors=FALSE)

#trim and keep only Player and FPTS columns

WR <- sqldf("select Player, FPTS from WR")

#rename Player column to Player_Name
names(WR)[names(WR)=="Player"] <- "Player_Name"

# scrape TE Projections

Test <- htmlParse("http://www.fantasypros.com/nfl/projections/te.php")
class(Test)
Test.table<- readHTMLTable(Test,stringsAsFactors = FALSE)

#you want the second table, not the first one
TE <- sapply(Test.table[[2]][,], FUN= function(x) as.character(gsub(",", "", as.character(x), fixed = TRUE) ))	

#convert to table
TE <-as.data.frame(TE, stringsAsFactors=FALSE)

#trim and keep only Player and FPTS columns

TE <- sqldf("select Player, FPTS from TE")

#rename Player column to Player_Name
names(TE)[names(TE)=="Player"] <- "Player_Name"

# Merge all position data tables together
Proj <- rbind(QB,RB,WR,TE)

# strip out teams and recreate data table with two columns (Player_Name and FPTS)
Proj2 <- Proj
#pull out teams and create a new column of just playe names
Proj2 <- data.frame(do.call('rbind', strsplit(as.character(Proj2$Player_Name),'(',fixed=TRUE)))
#transform Player_Name to a character class for matching
Proj2$Player_Name <- as.character(Proj2$X1)
Proj$Player_Name <- Proj2$Player_Name
#trim leading and trailing spaces for Player_Names
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
Proj$Player_Name <- trim(Proj$Player_Name)
# transform FPTS to numeric
Proj$FPTS <- as.numeric(Proj$FPTS)

#Merge the new files together, and then merge with Gini_Tiers master file
Gini_Tiers <- sqldf("select * from Gini_Tiers left join Proj using (Player_Name)")

#export Gini_Tiers
write.csv(Gini_Tiers, file="Gini_Tiers.csv")


#Data then used to create an interactive visualization in Tableau. Visualization can be found at: https://public.tableausoftware.com/profile/billpetti#!/vizhome/FantasyFootballProjectionsandConsistency2014/ProjectionsvConsistency




