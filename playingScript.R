# Reading in the file 
actData <- read.csv(unz("activity.zip", "activity.csv"), header = T)

hist(actData$steps)

today <- as.POSIXlt(actData$date)$mday 

thisMonth <- as.POSIXlt(actData$date)$mon 

# empty flags 
yesterDay <- 0
lastMonth <- 0 

# get each individual day 
# if today != yesterday -------> end samples for that day
if ( yesterDay == today && lastMonth == thisMonth)
{
        var1 <- actData$steps
}



getDay <- function(date)
{
        lastDate <- 0
        if (date )
        {
                lastDate <- 
        }
}






da <- as.POSIXlt(actData$date)$mday

for(i in 1:length(da))
{
        x <- 2
        print(as.POSIXlt(actData$date)$mday[i])
        if (as.POSIXlt(actData$date)$mday[i] == 22)
        {
                x = x + 1
                print(x)
        }
        #print(co)
}
#print(co)







