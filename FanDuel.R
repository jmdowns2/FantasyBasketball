

data <- './Data/salary.csv'
salary <- read.csv(data, stringsAsFactors=FALSE)


#salary$Last.Name <- gsub('Griffin III', 'Griffin', salary$Last.Name)
#salary$Last.Name <- gsub('Beckham Jr.', 'Beckham', salary$Last.Name)
#salary$Last.Name <- gsub('Ginn Jr.', 'Ginn', salary$Last.Name)
salary <- substituteNames(salary, "Last.Name")