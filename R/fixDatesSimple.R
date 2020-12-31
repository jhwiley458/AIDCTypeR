"fixDatesSimple" <- function(dirtyData)
{

  dirtyData$End_Date <- as.Date(dirtyData$End_Date, origin = "1970-01-01", format = "%m/%d/%Y")

  return(dirtyData)

}
