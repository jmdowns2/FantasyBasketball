options(stringsAsFactors = FALSE)

source("./VegasOdds.R")

substituteTeamNames <- function(data, colName)
{
  TEAM_NAMES$TEAM <- toupper(TEAM_NAMES$TEAM)
  for(i in 1:dim(TEAM_NAMES)[1])
  {
    data[data[colName] == TEAM_NAMES$TEAM[i], colName] <- TEAM_NAMES$ABB[i]
  }
  data
}

substituteNames <- function(data, colName)
{
  for(i in 1:dim(NAME_SUBSTITUTIONS)[1])
  {
    data[data[colName] == NAME_SUBSTITUTIONS$NAME[i], colName] <- NAME_SUBSTITUTIONS$REPLACEMENT[i]
  }
  data
}


#rbs <- readHTMLTable('http://www.ncaa.com/stats/basketball-men/d1/current/individual/137')[[1]]

#http://www.cbssports.com/collegebasketball/teams/stats/UK/kentucky-wildcats/regularseason/yearly/DEFENSIVE

getStatsForTeam <- function(team)
{
  cleanTable <- function(url)
  {
    t <- readHTMLTable(url)[[4]]
    names <- t[1,]
    lastLine <- dim(t)[1]-1 # last line in frame is garbage
    t <- t[2:lastLine,]
    colnames(t) <- names
    t
  }
  
  scoring <- cleanTable(paste('http://www.cbssports.com/collegebasketball/teams/stats/', team, sep=""))
  if(dim(scoring)[1] < 3)
  {
    print(paste("Invalid team data", team))
    return(NULL)
  }
  defense <- cleanTable(paste('http://www.cbssports.com/collegebasketball/teams/stats/', team, '/regularseason/yearly/DEFENSIVE', sep=""))
  assist <- cleanTable(paste('http://www.cbssports.com/collegebasketball/teams/stats/', team, '/regularseason/yearly/ASTTO', sep=""))
  ret <- merge(scoring, defense, by=c("No", "Player", "GP"))
  ret <- merge(ret, assist, by=c("No", "Player", "GP"))
  ret <- as.data.frame(sapply(ret, function(x) { gsub('-', '0', x) } ))
  
  fieldsToNumeric <- c('TO', 'TOPG', 'APG', 'No', 'PPG', 'RPG', 'B', 'GP', "BPG", "SPG")
  for(field in fieldsToNumeric)
  {
    ret[field] <- as.numeric(ret[[field]])
  }
  
  teamTotal <- sum(ret$PPG)
  ret$teamPointPercentage <- ret$PPG / teamTotal
  
  ret <- substituteNames(ret, "Player")
  
  firstNames <- NULL
  lastNames <- NULL
  for(i in 1:dim(ret)[1])
  {
    s <- strsplit(ret$Player[i], " ")
    firstNames <- rbind(firstNames, s[[1]][[1]])
    lastNames <- rbind(lastNames, s[[1]][[2]])    
  }
  
  ret <- cbind(ret, First.Name=firstNames, Last.Name=lastNames)
  ret
}

salary <- read.csv("./Data/salary.csv")
TEAM_NAMES <- read.csv("./Data/CBBTeamNames.csv")
NAME_SUBSTITUTIONS = read.csv("./Data/CBBNameSubstitutions.csv")
salary$Last.Name <- gsub(" III", "", salary$Last.Name)
salary$Last.Name <- gsub(" II", "", salary$Last.Name)
salary$Last.Name <- gsub(", Jr.", "", salary$Last.Name)
salary$Last.Name <- gsub("-", "0", salary$Last.Name)

playerStats <- NULL
uniqueTeams <- unique(salary$Team)
for(t in uniqueTeams)
{
  statUrl <- TEAM_NAMES[TEAM_NAMES$ABB == t,]$STATURL 
  if(length(statUrl) == 0)
  {
    print(paste("Invalid team ", t))
  }
  statsForTeam <- getStatsForTeam(statUrl)
  if(!is.null(statsForTeam))
  {
    statsForTeam$Team <- t
    playerStats <- rbind(playerStats, statsForTeam)
  }
}

playerStats <- playerStats[which(playerStats$Team != 'MRQTT' | playerStats$No != '22'),] # Wally Ellenson
salary <- salary[salary$Id != '23258',]

cbbOdds <- getOddsData("ncb")
cbbOdds <- cbbOdds[cbbOdds$Source == "SportsBetting.ag",]
cbbOdds$Spread <- as.numeric(cbbOdds$Spread)
cbbOdds$OU <- as.numeric(cbbOdds$OU)

#cbbOdds <- rbind(cbbOdds, data.frame(Source="test", Home="DUKE", Away="KTCKY", Spread=-2, OU=156.5))
#cbbOdds <- rbind(cbbOdds, data.frame(Source="test", Home="MCHST", Away="KANS", Spread=-5, OU=149.0))

homeScores <- data.frame(Team=cbbOdds$Home, Score=(cbbOdds$OU/2 + cbbOdds$Spread/2))
awayScores <- data.frame(Team=cbbOdds$Away, Score=(cbbOdds$OU/2 - cbbOdds$Spread/2))
expectedTeamScores <- rbind(homeScores, awayScores)
expectedTeamScores$Team <- toupper(expectedTeamScores$Team)

expectedTeamScores <- substituteTeamNames(expectedTeamScores, "Team")

#merge(expectedTeamScores, TEAM_NAMES, by.x="Team", by.y="ABB")
salary <- merge(salary, expectedTeamScores, by="Team", all.x = TRUE)
salary <- merge(salary, playerStats, by=c("Team", "Last.Name"), all.x = TRUE)
salary$expectedScore <- salary$Score * salary$teamPointPercentage

unmatchedTeams <- unique(salary$Team[is.na(salary$Score)])
salary$expectedScore[is.na(salary$expectedScore)] <- 0

POINTS <- 1
RBS <- 1.2
ASSISTS <- 1.5
BLOCKS <- 2
STEALS <- 2
TURNOVERS <- -1
salary$expectedPoints <- (salary$expectedScore * POINTS) + (salary$RPG * RBS) + (salary$APG * ASSISTS) + (salary$BPG * BLOCKS) + (salary$SPG * STEALS) + (salary$TOPG * TURNOVERS)
salary$expectedPoints[is.na(salary$expectedPoints)] <- 0


salary <- salary[salary$Injury.Indicator != 'IR',]
salary <- salary[salary$Injury.Indicator != 'O',]
salary <- salary[salary$Injury.Indicator != 'D',]





num.players <- length(salary$Id)
# objective:
obj <- salary$expectedPoints
# the vars are represented as booleans
var.types <- rep("B", num.players)
# the constraints
matrix <- rbind(as.numeric(salary$Position == "F"),
                as.numeric(salary$Position == "G"),
                salary$Salary)                       # total cost

direction <- c("==", # F
               "==", # G
               "<=" # Salary
)
rhs <- c(5, # F
         4, # G
         60000)

sol <- Rglpk_solve_LP(obj = obj, mat = matrix, dir = direction, rhs = rhs,
                      types = var.types, max = TRUE)

validateNames <- function()
{
  cbind(suggestedTeam$Id, suggestedTeam$Last.Name, suggestedTeam$First.Name.x, suggestedTeam$First.Name.y)
}
  
suggestedTeam <- salary[sol$solution == 1,]
print(salary[sol$solution == 1,])
print(paste("Total Salary =", sum(salary[sol$solution == 1,]$Salary)))
print(paste("Expected Points =", sum(salary[sol$solution == 1,]$expectedPoints)))

print("Unmatched Teams")
print(unmatchedTeams)

