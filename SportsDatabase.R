FROM_DATE <- format(Sys.Date() - 40, " %Y%m%d")

querySportsDB <- function(query)
{
  url <- paste('http://api.sportsdatabase.com/ncaabb/player_query.json?jsoncallback=$&output=json&api_key=guest&sdql=', URLencode(query), sep='');
  
  j  <- readLines(url)
  j <- sub('[^\\{]*', '', j) # remove function name and opening parenthesis
  j <- sub('\\);$', '', j) # remove closing parenthesis
  j <- gsub("'", "\"", j)
  
  if(j == "")
  {
    return (NULL);
  } 
  
  res <- fromJSON(j)
  
  result <- NULL;
  for (i in 1:length(res[[1]]))
  {
    result <- cbind(result, res[2]$groups[[1]]$columns[[i]])
  }
  
  colnames(result) <- res[[1]]
  as.data.frame(result)
}

averagePlayerStats <- function(playerName)
{
  playerName <- gsub("[.]", "", playerName)
  
  results <- queryPlayer(playerName)
  if(is.null(results))
    return (NULL);
  
  data.frame(Player=playerName, points = mean(results$points), blocks = mean(results$blocks), assists = mean(results$assists), steals = mean(results$steals), rebounds = mean(results$rebounds), turnovers = mean(results$turnovers), fantasyPoints = mean(results$fantasyPoints), risk = sd(results$fantasyPoints), games=dim(results)[1])
}

queryPlayer <- function(playerName)
{
  result <- querySportsDB(paste("date,points,name,blocks,assists,minutes,steals,rebounds,turnovers@season=2015 and name=", playerName, " and date >= ", FROM_DATE, sep=""))
  if(is.null(result))
  {
    return (NULL);
  }
  result$date <- as.Date(result$date, "%Y%m%d")
  
  nums <- c('points', 'blocks', 'assists', 'minutes', 'steals', 'rebounds', 'turnovers')
  for(num in nums)
  {
    result[num] = as.numeric(result[[num]]);
  }
  
  result <- cbind(result, fantasyPoints=calculateFantasyPoints(result))
  result
}

queryTeam <- function(team)
{
  result <- querySportsDB(paste("date,points,name,blocks,assists,minutes,steals,rebounds,turnovers@season=2015 and team=", team, " and date >= ", FROM_DATE, sep=""))
  result$date <- as.Date(result$date, "%Y%m%d")
  
  nums <- c('points', 'blocks', 'assists', 'minutes', 'steals', 'rebounds', 'turnovers')
  for(num in nums)
  {
    result[num] = as.numeric(result[[num]]);
  }
  
  ##### FIX NAMES
  result$name <- gsub(" IV", "", result$name)
  result$name <- gsub(" III", "", result$name)
  result$name <- gsub(" II", "", result$name)
  result$name <- gsub(" Jr", "", result$name)
  result$name <- gsub("-", "0", result$name)
  result$name <- gsub("Morris Walker", "Morris0Walker", result$name)
  result$name <- gsub("Michel Ofik", "Michel0Ofik", result$name)
  result$name <- gsub("Codi Miller McIntyre", "Codi Miller0McIntyre", result$name)
  result$name <- gsub("Kenny Paul Geno", "Kenny Paul0Geno", result$name)
  result$name <- gsub("Xavier Rathan Mayes", "Xavier Rathan0Mayes", result$name)
  result$name <- gsub("Dorian Finney Smith", "Dorian Finney0Smith", result$name)
  result$name <- gsub("Jean Marc Koumadje", "Jean Marc0Koumadje", result$name)
  result$name <- gsub("Sammy Barnes Thompkins", "Sammy Barnes0Thompkins", result$name)
  result$name <- gsub("Brandone Francis Ramirez", "Brandone Francis", result$name)
  result$name <- gsub("Matthew Fisher Davis", "Matthew Fisher0Davis", result$name)
  result$name <- gsub("Donte Fitzpatrick Dorsey", "Donte Fitzpatrick0Dorsey", result$name)
  result$name <- gsub("Abdul Malik Abu", "Abdul-Malik Abu", result$name)
  result$name <- gsub("Tonny Trocha Morelos", "Tonny Trocha0Morelos", result$name)
  result$name <- gsub("Alonzo Nelson Ododa", "Alonzo Nelson0Ododa", result$name)
  result$name <- gsub("Marcus Georges Hunt", "Marcus Hunt", result$name)
  result$name <- gsub("Tra Deon Hollins", "Tra Deon0Hollins", result$name)
  result$name <- gsub("Parker Jackson Cartwright", "Parker Jackson0Cartwright", result$name)
  result$name <- gsub("DVauntes Smith Rivera", "DVauntes Smith0Rivera", result$name)  
  result$name <- gsub("Keita Bates Diop", "Keita Bates0Diop", result$name)  
  result$name <- gsub("Joey Van Zegeren", "Joey Van0Zegeren", result$name)  
  result$name <- gsub("Matt Van Dyk", "Matt Van0Dyk", result$name)  
  result$name <- gsub("John Robert Simon", "John Robert0Simon", result$name)  
  result$name <- gsub("DeVaughn Akoon Purcell", "DeVaughn Akoon0Purcell", result$name)  
  result$name <- gsub("Jalen Coleman Lands", "Jalen Coleman0Lands", result$name)  
  result$name <- gsub("Doyin Akintobi Adeyeye", "Doyin Akintobi0Adeyeye", result$name)  
  result$name <- gsub("Ivan Cruz Uceda", "Ivan Cruz0Uceda", result$name)  
  
  
#  result$name <- gsub("", "", result$name)  
  
  
  ####### FIX NAMES
  
  
  splitNames <- do.call('rbind', strsplit(result$name,' ',fixed=TRUE))
  result$First.Name <- splitNames[,1]
  result$Last.Name <- splitNames[,2]
    
  result$fantasyPoints <- calculateFantasyPoints(result)
  result
}

calculateFantasyPoints <- function(result)
{
  ((result$points * POINTS) + (result$rebounds * RBS) + (result$assists * ASSISTS) + (result$blocks * BLOCKS) + (result$steals * STEALS) + (result$turnovers * TURNOVERS))
}


queryTeamStats <- function(team)
{
  result <- queryTeam(team)
  totalTeamPoints <- sum(result$points);
  teamPointsPerGame <- totalTeamPoints / length(unique(result$date))
 
  fieldNames <- c("points", "blocks", "assists", "steals", "rebounds", "turnovers", "fantasyPoints", "name", "First.Name", "Last.Name")
  teamStats <- by(result, result$name, function(x) {
    
    data.frame(PPG=mean(x$points), 
               BPG=mean(x$blocks),
               APG=mean(x$assists),
               SPG=mean(x$steals),
               RPG=mean(x$rebounds),
               TOPG=mean(x$turnovers),
               Player=x$name[1],
               First.Name=x$First.Name[1],
               Last.Name=x$Last.Name[1],
               Games=dim(x)[1],
               Risk=sd(x$fantasyPoints)
               )
  });

  teamStats <- do.call(rbind.data.frame, teamStats)
  rownames(teamStats) <- NULL;
  teamStats$teamPointsPerGame <- teamPointsPerGame
  teamStats$teamPointPercentage <- teamStats$PPG / teamPointsPerGame
  teamStats  
  
  #ag <- aggregate(result$fantasyPoints, by=list(result$name), simplify=TRUE, FUN=function(x) { c(mean(x), sd(x)) })
  #data.frame(name=ag$Group, fantasyAve=as.numeric(ag$x[,1]), fantasyStd=as.numeric(ag$x[,2]))
}

queryStatsForTeams <- function(teams)
{
  mustStop <- FALSE
  for(t in teams)
  {
    sportsDbTeam <- TEAM_NAMES[TEAM_NAMES$ABB == t,]$SPORTDB
    if(sportsDbTeam == "" || is.na(sportsDbTeam))
    {
      print(paste("Invalid team ", t))
      mustStop <- TRUE
    }
  }
  if(mustStop)
    stop();
  
  ret <- NULL
  
  for(t in teams)
  {
    sportsDbTeam <- TEAM_NAMES[TEAM_NAMES$ABB == t,]$SPORTDB
    print(paste("Collecting stats for", t))
    teamStats <- queryTeamStats(sportsDbTeam)
    teamStats$Team <- t
    ret <- rbind(ret, teamStats)
    
    Sys.sleep(1)
  }
  
  ret
  
}


