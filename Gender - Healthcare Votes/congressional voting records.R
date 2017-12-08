# Assembling data files containing voting records, demographic data for Members of Congress
# Voting records downloaded from https://www.govtrack.us/congress/votes#category[]=3,6
# Demographic data compiled from Wikipedia

# Not sure which of packages I need -- maybe not stringr
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
setwd("~/Documents/Learning R!/Congressional Voting Records/")

med <- read.csv("medicaid states.csv")
demo.h <- read.csv("house.csv")
# Attach medicaid column to demographic dataframe
demo.h <- merge(demo.h, med[, c("state", "medicaid")], by="state")
demo.s <-  read.csv("senate.csv")
demo.s <- merge(demo.s, med[, c("state", "medicaid")], by="state")

handle.h <- "congress_votes_115-2017_h"
votes.h <- list("6", "8", "23", "45", "51", "54", "58", "65", "72", "76", "77", "78", "83", "84", "85", "92", "96", "97", "98", "99", "114", "120", "121", "126", "148", "152", "158", "168", "169", "186", "202", "206", "208", "244", "256", "265", "268", "282", "299", "306", "308", "319", "337", "342", "344", "352", "391", "398", "402", "406", "435")
count.h <- length(votes.h)
records.h <- list()
data.h <- list()

handle.s <- "congress_votes_115-2017_s"
votes.s <- list("26", "36", "43", "51", "54", "59", "61", "63", "66", "68", "71", "77", "81", "82", "83", "84", "86", "87", "92", "93", "94", "96", "99", "101", "109", "111", "120", "124", "125", "129", "131", "137", "156", "164", "166", "167", "168", "169", "179", "184")
count.s <- length(votes.s)
records.s <- list()
data.s <- list()

for (i in 1:count.h){
	name.h <- paste(handle.h, votes.h[[i]], ".csv", sep = "")
	records.h[[i]] <- read.csv(name.h, skip = 1)
	data.h[[i]] <- merge(records.h[[i]], demo.h, by = c("state", "district"))
	data.h[[i]]$vote <- as.character(data.h[[i]]$vote)
	data.h[[i]]$vote2 <- ifelse(data.h[[i]]$vote %in% c("Aye", "Yea"), data.h[[i]]$vote2 <- 1, ifelse(data.h[[i]]$vote %in% c("Nay", "No"), data.h[[i]]$vote2 <- 2, data.h[[i]]$vote2 <- 3))
	data.h[[i]]$votenum <- as.numeric(votes.h[[i]])
	data.h[[i]]$percentage <- sum(data.h[[i]]$vote2 == 1)/sum(data.h[[i]]$vote2 != 3)
	data.h[[i]]$party <- as.factor(data.h[[i]]$party.x)
	data.h[[i]]$state <- as.factor(data.h[[i]]$state)
	data.h[[i]]$district <- as.factor(data.h[[i]]$district)
	data.h[[i]]$gender <- as.factor(data.h[[i]]$gender)
}

headers.h <- c("state", "district", "vote", "name", "party", "medicaid", "last_name", "first_name", "up_2018", "up_2020", "up_2022", "gender", "race_ethnicity", "white", "black", "latinx", "asian", "poc", "lgbtq", "votenum", "vote2", "percentage")

fullrec.h <- rbind.fill(data.h[[1]][headers.h], data.h[[2]][headers.h])
for (i in 3:count.h){
	fullrec.h <- rbind.fill(fullrec.h[headers.h], data.h[[i]][headers.h])
}

write.csv(fullrec.h, "fullrec_house.csv", row.names=F)

for (i in 1:count.s){
	name.s <- paste(handle.s, votes.s[[i]], ".csv", sep = "")
	records.s[[i]] <- read.csv(name.s, skip = 1)
	# New line to convert from utf-8 to ascii
	records.s[[i]]$name <- iconv(records.s[[i]]$name, "utf-8", "ascii", sub="")
	records.s[[i]]$name <- gsub("Menndez", "Menendez", records.s[[i]]$name)
	countnames.s <- length(records.s[[i]]$name)
	for (j in 1:countnames.s){
		records.s[[i]]$first_name[[j]] <- word(records.s[[i]]$name[[j]], 2)
		records.s[[i]]$last_name[[j]] <- word(records.s[[i]]$name[[j]], -2)
	}
	data.s[[i]] <- merge(records.s[[i]], demo.s, by = c("state", "first_name", "last_name"))
	data.s[[i]]$vote <- as.character(data.s[[i]]$vote)
	data.s[[i]]$vote2 <- ifelse(data.s[[i]]$vote %in% c("Aye", "Yea"), data.s[[i]]$vote2 <- 1, ifelse(data.s[[i]]$vote %in% c("Nay", "No"), data.s[[i]]$vote2 <- 2, data.s[[i]]$vote2 <- 3))
	data.s[[i]]$votenum <- as.numeric(votes.s[[i]])
	data.s[[i]]$percentage <- sum(data.s[[i]]$vote2 == 1)/sum(data.s[[i]]$vote2 != 3)
	data.s[[i]]$party <- as.factor(data.s[[i]]$party.x)
	# data.s[[i]]$last_name <- data.s[[i]]$last_name.x
	data.s[[i]]$state <- as.factor(data.s[[i]]$state)
	data.s[[i]]$gender <- as.factor(data.s[[i]]$gender)
	data.s[[i]]$district <- as.factor(data.s[[i]]$district.y)
	data.s[[i]]$district.x <- NULL
}

headers.s <- c("state", "vote", "party", "medicaid", "last_name", "first_name", "up_2018", "up_2020", "up_2022", "gender", "race_ethnicity", "white", "black", "latinx", "asian", "poc", "lgbtq", "votenum", "vote2", "percentage")

fullrec.s <- rbind.fill(data.s[[1]][headers.s], data.s[[2]][headers.s])
for (i in 3:count.s){
	fullrec.s <- rbind.fill(fullrec.s[headers.s], data.s[[i]][headers.s])
}

write.csv(fullrec.s, "fullrec_senate.csv", row.names=F)

# Vote lists by topic
votes.h <- list("6", "8", "23", "45", "51", "54", "58", "65", "72", "76", "77", "78", "83", "84", "85", "92", "96", "97", "98", "99", "114", "120", "121", "126", "148", "152", "158", "168", "169", "186", "202", "206", "208", "244", "256", "265", "268", "282", "299", "306", "308", "319", "337", "342", "344", "352", "391", "398", "402", "406", "435")
votes.hc.h <- list("6", "58", "256", "308")
votes.def.h <- list("76", "435")
votes.lands.h <- list("83", "92", "319", "406")
votes.energy.h <- list("398", "402")
votes.enviro.h <- list("78", "98", "206", "208", "282", "352", "391")
votes.immi.h <- list("342", "344")
votes.econ.h <- list("51", "54", "72", "299", "306")
votes.crim.h <- list("265", "268")
votes.labor.h <- list("95", "96", "121", "186", "244")
votes.women.h <- list("65", "99")
votes.tech.h <-list("202")
votes.guns.h <- list("77", "169")
votes.edu.h <- list("84", "85")
votes.welfare.h <- list("97") # Could go in labor
votes.vets.h <- list("168")
votes.legal.h <- list("148", "152", "158", "337") # 337 could go in hc
votes.reg.h <- list("8", "23", "45", "114", "120", "126")

votes.nom.s <- list("36", "54", "59", "61", "63", "68", "71", "77", "86", "96", "109", "111", "124", "129", "131", "137", "156", "164", "166", "184")
votes.hc.s <- list("26", "167", "168", "169", "179")
votes.def.s <- list("81")
votes.lands.s <- list("82")
votes.energy.s <- list()
votes.enviro.s <- list("43", "92", "125")
votes.immi.s <- list()
votes.econ.s <- list("51")
votes.crim.s <- list()
votes.labor.s <- list("87", "93", "99", "120")
votes.women.s <- list("101")
votes.tech.s <-list("94")
votes.guns.s <- list("66")
votes.edu.s <- list ("83", "84")
votes.welfare.s <- list("87")
votes.vets.s <- list()
votes.legal.s <- list()
votes.reg.s <- list()

# Scratch work
name.h <- paste(handle.h, votes.h[[1]], ".csv", sep = "")
records.h[[1]] <- read.csv(name.h, skip = 1)
# records.h[[1]] %>%
	# separate(name, c("title", "first", "last", "party_abb"), " ")
demo.h <- read.csv("house.csv")
data.h[[1]] <- merge(records.h[[1]], demo.h, by = c("state", "district"))
data.h[[1]]$votenum <- as.numeric(votes.h[[1]])

name.s <- paste(handle.s, votes.s[[1]], ".csv", sep = "")
records.s[[1]] <- read.csv(name.s, skip = 1)
records.s[[1]] <- records.s[[1]] %>%
	separate(name, c("title", "first_name", "last_name", "rest"), " ", extra = "drop")
demo.s <-  read.csv("senate.csv")
data.s[[1]] <- merge(records.s[[1]], demo.s, by = c("state", "first_name"))
data.s[[1]]$votenum <- as.numeric(votes.s[[1]])
data.s[[1]]$party <- data.s[[1]]$party.x
data.s[[1]]$last_name <- data.s[[1]]$last_name.x

if (data.h[[i]]$vote == "Aye" || data.h[[i]]$vote == "Yea"){
		data.h[[i]]$vote2 <- 1
	}
	if (data.h[[i]]$vote == "Nay" || data.h[[i]]$vote == "No"){
		data.h[[i]]$vote2 <- 2
	}
	if (data.h[[i]]$vote == "Not Voting" || data.h[[i]]$vote == "Present"){
		data.h[[i]]$vote2 <- 3
	}
	
name.h <- paste(handle.h, votes.h[[1]], ".csv", sep = "")
	records.h[[1]] <- read.csv(name.h, skip = 1)
	data.h[[1]] <- merge(records.h[[1]], demo.h, by = c("state", "district"))
	data.h[[1]]$vote <- as.character(data.h[[1]]$vote)
	data.h[[1]]$vote2 <- ifelse(data.h[[1]]$vote %in% c("Aye", "Yea"), data.h[[1]]$vote2 <- 1, ifelse(data.h[[1]]$vote %in% c("Nay", "No"), data.h[[1]]$vote2 <- 2, data.h[[1]]$vote2 <- 3))
	data.h[[1]]$votenum <- as.numeric(votes.h[[1]])
	data.h[[1]]$percentage <- sum(data.h[[1]]$vote2 == 1)/sum(data.h[[1]]$vote2 != 3)
	data.h[[1]]$party <- as.factor(data.h[[1]]$party.x)
	data.h[[1]]$state <- as.factor(data.h[[1]]$state)
	data.h[[1]]$district <- as.factor(data.h[[1]]$district)
	data.h[[1]]$gender <- as.factor(data.h[[1]]$gender)
	
name.s <- paste(handle.s, votes.s[[1]], ".csv", sep = "")
	records.s[[1]] <- read.csv(name.s, skip = 1)
	records.s[[1]] <- records.s[[1]] %>%
		separate(name, c("title", "first_name", "last_name", "rest"), " ", extra = "drop")
	data.s[[i]] <- merge(records.s[[i]], demo.s, by = c("state", "first_name"))
	data.s[[1]] <- merge(records.s[[1]], demo.s, by = c("state", "first_name"))
	data.s[[i]]$vote <- as.character(data.s[[i]]$vote)
	data.s[[i]]$vote2 <- ifelse(data.s[[i]]$vote %in% c("Aye", "Yea"), data.s[[i]]$vote2 <- 1, ifelse(data.s[[i]]$vote %in% c("Nay", "No"), data.s[[i]]$vote2 <- 2, data.s[[i]]$vote2 <- 3))
	data.s[[i]]$votenum <- as.numeric(votes.s[[i]])
	data.s[[i]]$percentage <- sum(data.s[[i]]$vote2 == 1)/sum(data.s[[i]]$vote2 != 3)
	data.s[[i]]$party <- as.factor(data.s[[i]]$party.x)
	data.s[[i]]$last_name <- data.s[[i]]$last_name.x
	data.s[[i]]$state <- as.factor(data.s[[i]]$state)
	data.s[[i]]$district <- as.factor(data.s[[i]]$district)
	data.s[[i]]$gender <- as.factor(data.s[[i]]$gender)
	
records.s[[i]] <- records.s[[i]] %>%
		separate(name, c("title", "first_name", "last_name", "rest"), " ", extra = "drop")
		
# UGH. Need to figure out OK.
for (i in 1:count.s){
	data.s[[i]] <- data.s[[i]][!duplicated(data.s[[i]][c("last_name")]),]
}		
