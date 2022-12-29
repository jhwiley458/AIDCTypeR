
#clean and or determine body type name using VID data
#works one row at a time
#assumption is that model year and clean make name and clean model name and clean submodel name are correct
#and that body type name has not already been determined
#' @export
"cleanBodyTypeName" <- function(dirtyRow, vidInput, optionalColumns = NA)
{

  #use clean make and model and submodel and model year
  thisYear <- dirtyRow$YearName[1]
  thisMake <- dirtyRow$CleanMakeName[1]
  thisModel <- dirtyRow$CleanModelName[1]
  thisSubModel <- dirtyRow$CleanSubModelName[1]

  #match against those
  bodyType <- unique(vidInput$BodyTypeName[vidInput$YearName == thisYear
                                           & vidInput$MakeName == thisMake
                                           & vidInput$ModelName == thisModel
                                           & vidInput$SubModelName == thisSubModel
                                           & !is.na(vidInput$BodyTypeName)])

  returnThis <- NA
  if(length(bodyType) == 1)
  {
    returnThis <- bodyType #if only one exists return that
  } else {
    #body type name in dirty data must have a value
    if(length(bodyType) > 1 & !is.na(dirtyRow$BodyTypeName))
    {
      #otherwise join VID on year make model submodel
      #only 1/3 of characters in bodytype name can be wrong
      combinedData <- {}
      combinedData <- stringdist_inner_join(dirtyRow[, c("TxnID", "BodyTypeName")]
                                            , vidInput[vidInput$YearName == thisYear
                                                       & !is.na(vidInput$YearName)
                                                       & vidInput$MakeName == thisMake
                                                       & vidInput$ModelName == thisModel
                                                       & vidInput$SubModelName == thisSubModel, ]
                                            , by = "BodyTypeName"
                                            , max_dist = round(nchar(dirtyRow[1, "BodyTypeName"]) / 3, 0)
                                            , distance_col = "DistanceColumn"
                                            , ignore_case = TRUE)
      returnThis <- NA
      if(nrow(combinedData) > 0){
        combinedData <- combinedData[order(combinedData$DistanceColumn), ] #sort by most accurate
        returnThis <- combinedData$BodyTypeName.y[1]
      }
    }
  }

  #if no match exists yet attempt to match against listing description or URL or vehicle description
  if(is.na(returnThis) & length(bodyType) > 1 & !is.na(dirtyRow$BodyTypeName))
  {
    for(b in bodyType){
      #if(grepl(b, dirtyRow$YearMakeModel, ignore.case = TRUE) | grepl(b, dirtyRow$Listing_Description, ignore.case = TRUE) | grepl(b, dirtyRow$ListingURL, ignore.case = TRUE)){
      #  returnThis <- b
      #}
      if(!is.na(optionalColumns[1])){
        for(thisColumn in optionalColumns){
          if(grepl(b, dirtyRow$YearMakeModel, ignore.case = TRUE) | grepl(b, dirtyRow[, thisColumn], ignore.case = TRUE)){
            returnThis <- b
            break
          }
        }
      } else {
        if(grepl(b, dirtyRow$YearMakeModel, ignore.case = TRUE)){
          returnThis <- b
          break
        }
      }
    }
  }

  #attempt to match against VID using year make and model
  bodyType <- unique(vidInput$BodyTypeName[vidInput$YearName == thisYear
                                           & vidInput$MakeName == thisMake
                                           & vidInput$ModelName == thisModel
                                           & !is.na(vidInput$BodyTypeName)])

  if(length(bodyType) == 1 & is.na(returnThis))
  {
    returnThis <- bodyType #if only one body type matches return it
  } else {
    #if no match exists yet attempt join on just year make and model
    if(length(bodyType) > 1 & !is.na(dirtyRow$BodyTypeName) & is.na(returnThis))
    {
      combinedData <- {}
      combinedData <- stringdist_inner_join(dirtyRow[, c("TxnID", "BodyTypeName")]
                                            , vidInput[vidInput$YearName == thisYear
                                                       & !is.na(vidInput$YearName)
                                                       & vidInput$MakeName == thisMake
                                                       & vidInput$ModelName == thisModel, ]
                                            , by = "BodyTypeName"
                                            , max_dist = round(nchar(dirtyRow[1, "BodyTypeName"]) / 3, 0)
                                            , distance_col = "DistanceColumn"
                                            , ignore_case = TRUE)
      returnThis <- NA
      if(nrow(combinedData) > 0){
        combinedData <- combinedData[order(combinedData$DistanceColumn), ]
        returnThis <- combinedData$BodyTypeName.y[1]
      }
    }
  }
  #if join failed attempt match on vehicle description or listing description or URL
  if(is.na(returnThis) & !is.na(dirtyRow$BodyTypeName) & length(bodyType) > 1 )
  {
    for(b in bodyType){
      #if(grepl(b, dirtyRow$YearMakeModel, ignore.case = TRUE) | grepl(b, dirtyRow$Listing_Description, ignore.case = TRUE) | grepl(b, dirtyRow$ListingURL, ignore.case = TRUE)){
      if(!is.na(optionalColumns[1])){
        for(thisColumn in optionalColumns){
          if(grepl(b, dirtyRow$YearMakeModel, ignore.case = TRUE) | grepl(b, dirtyRow[, thisColumn], ignore.case = TRUE)){
            returnThis <- b
            break
          }
        }
      } else {
        if(grepl(b, dirtyRow$YearMakeModel, ignore.case = TRUE)){
          returnThis <- b
          break
        }
      }
    }
  }

return(returnThis)

}
