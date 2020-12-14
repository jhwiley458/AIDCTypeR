
#clean make names using VID data
#works one row at a time
#assumption is that model year is correct
#' @export
"cleanMakeName" <- function(dirtyRow, vidInput, columnName)
{
  #match VID to using model year
  thisYear <- dirtyRow$YearName[1]
  #assume that the first two characters of the make are correct
  firstThree <- paste0("^", substr(dirtyRow$MakeName[1], 1, 3))

  #minimum number of characters in vehicle string corresponding to columnName, which is usually YearMakeModel
  #minimum is one third the number of characters in the columnName string
  #eg "1988 BMW M3" is 11 characters, divided by 3 and rounded is 4
  nYMM <- round(nchar(dirtyRow[1, columnName]) / 3, 0)

  #inner join the vid to the dirty data row based on the columnName string (typically YearMakeModel)
  #number of characters different is returned in DistanceColumn
  #entries in the VID with more than nYMM characters different will not be joined
  combinedData <- {}
  combinedData <- stringdist_inner_join(dirtyRow[, paste0(c("TxnID", "LotNumber", columnName))]
                                        , vidInput[vidInput$YearName == thisYear
                                                   & !is.na(vidInput$YearName)
                                                   & grepl(firstThree, vidInput$MakeName, ignore.case = TRUE), ]
                                        , by = columnName
                                        , max_dist = nYMM
                                        , distance_col = "DistanceColumn"
                                        , ignore_case = TRUE)
  #if at least one row is returns by the join, sort it so that the VID entries with the fewest differences are first
  if(nrow(combinedData) > 0){
    combinedData <- combinedData[order(combinedData$DistanceColumn), ]
  } else {
    #otherwise try to find a match using grepl without the modelname from the dirty data "1988 BMW 3-series" becomes "1988 BMW"
    combinedData <- vidInput[grepl(paste0(thisYear, " ", dirtyRow$MakeName[1]), vidInput[, columnName], ignore.case = TRUE), ]
  }

  returnThis <- NA
  #if the inner join was successful and has at least one row, return the VID version of the MakeName - or MakeName.y because MakeName.x will be the dirty data
  #else if it matched on just the year and make, return the make name of VID, from the combinedData, which was not joined with the dirty data
  returnThis <- ifelse(grepl(columnName, "MakeName", ignore.case = TRUE) & (nrow(combinedData) > 0), combinedData$MakeName.y[1], combinedData$MakeName[1])

  return(returnThis)


}
