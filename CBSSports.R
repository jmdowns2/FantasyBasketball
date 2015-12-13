
getCBSDefenseRank <- function()
{
  cbsUrl <- 'http://www.cbssports.com/nfl/stats/teamsort/nfl/year-2015-season-regular-category-total-type-defense?print_rows=9999'
  defense <- readHTMLTable(cbsUrl, stringsAsFactors=FALSE)[[3]]
  
  if(dim(defense)[1] != 35)
  {
    stop("Invalid defense")
  }
  
  defense <- defense[3:34,]
  
  colnames(defense) <- c('NAME', 'G',	'PTSG',	'PTS', 'PLYS', 'YDSG', 'YP', 'FDG',	'MD3',	'ATT3',	'PCT3',	'MD4',	'ATT4',	'PCT4',	'PEN',	'PENYDS',	'TOP',	'TF',	'L');
  rownames(defense) <- 1:32
  defense <- cbind(defense, DefRank=1:32)
  defense <- merge(defense, TEAM_NAMES, by.x="NAME", suffixes=c('', ''), all.x = TRUE)

  defense <- as.data.frame(cbind(Defense=defense$TEAM, DefRank=defense$DefRank))
  
  defense
}


mergeDefenseRank <- function(v)
{
  defense <- getCBSDefenseRank();
  merge(v, defense, by.x="Opponent", by.y="Defense", suffixes=c('', ''), all.x = TRUE)
}
