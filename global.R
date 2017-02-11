library(sqldf)
library(dplyr)

##Loading
df <- read.csv("afdb.csv")
colnames(df) <- c(
   "num","country", "project_id", "title","status","amount",
    "implementing_agency","appraisal_date","approval_date", 
    "start_date","board_presentation","segment", "report"
)

####Count number of Project by status
cf <- sqldf::sqldf("
select segment,status, substr(start_date,7,10) startdate, count(status) cnt
from df where CAST(substr(start_date,7,10) AS SIGNED INTEGER) >= 2010 AND length(board_presentation) = 0 
group by segment, status, substr(start_date,7,10)
UNION
select segment,status, substr(board_presentation,7,10) startdate, count(status) cnt
from df where CAST(substr(board_presentation,7,10) AS SIGNED INTEGER) >= 2010 AND length(start_date) = 0 
group by segment, status, substr(board_presentation,7,10)
")

cft <- sqldf::sqldf("select sum(cnt) cnt from cf")

####Row data include the country
rawcf <- sqldf::sqldf("
select country, segment,status, substr(start_date,7,10) startdate
from df where CAST(substr(start_date,7,10) AS SIGNED INTEGER) >= 2010 AND length(board_presentation) = 0 
UNION
select country, segment,status, substr(board_presentation,7,10) startdate
from df where CAST(substr(board_presentation,7,10) AS SIGNED INTEGER) >= 2010 AND length(start_date) = 0 
")

