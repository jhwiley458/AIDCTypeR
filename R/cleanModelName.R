
#clean model names using VID data
#works one row at a time
#assumption is that model year and clean make name is correct
#' @export
"cleanModelName" <- function(dirtyRow, vidInput, columnName, optionalColumns = NA)
{

   thisYear <- dirtyRow$YearName[1]
    thisMake <- dirtyRow$CleanMakeName[1]
    #assumed that first character of model is correct
    firstOne <- paste0("^", substr(dirtyRow$ModelName[1], 1, 1))

    thisBody <- dirtyRow$BodyTypeName[1]

    #allows for one out of every three characters in the model name to be wrong
    if(columnName == "YearMakeModel"){
      nYMM <- round(nchar(dirtyRow[1, "ModelName"]) / 3, 0)
    } else {
      nYMM <- round(nchar(dirtyRow[1, columnName]) / 2, 0)
    }

    #set of possible model names using VID
    modelName <- unique(vidInput$ModelName[vidInput$YearName == thisYear
                                           & vidInput$MakeName == thisMake
                                           & !is.na(vidInput$ModelName)])

    #order so that longest model names first
    if(length(modelName) > 1)
    {
      modelNameNchar <- sapply(modelName, nchar)
      modelName <- modelName[order(-modelNameNchar)]
    }

    #remove characters such as dashes or spaces
    mnStripped <- removeNonLettersNumbers(dirtyRow$ModelName)

    returnThis <- NA
    if(length(modelName) == 1)
    {
      #if there is only one model for this make and model year then return it
      returnThis <- modelName
    } else {
      if(length(modelName) > 1 & !is.na(dirtyRow$ModelName)){
        #order VID model names by decreasing length so that more specific models will be matched first. "328 GTS" matches before "328"
        modelNamesStripped <- removeNonLettersNumbers(modelName)
        if(mnStripped %in% modelNamesStripped){
          returnThis <- modelName[mnStripped == modelNamesStripped]
        } else {
         for(m in modelName){
          mStripped <- removeNonLettersNumbers(m) #also strip any non letters or numbers from the VID model name
          if(stringdist(mStripped, mnStripped) < (nchar(mnStripped) / 2)){
            #match either those two both ways so that either can be longer or match in description
            #if(grepl(mStripped, mnStripped, ignore.case = TRUE) | grepl(m, dirtyRow$Listing_Description, ignore.case = TRUE) | grepl(mnStripped, mStripped, ignore.case = TRUE)){
            if(!is.na(optionalColumns[1])){
              for(thisColumn in optionalColumns){
                if(grepl(mStripped, mnStripped, ignore.case = TRUE) | grepl(m, dirtyRow[, thisColumn], ignore.case = TRUE) | grepl(mnStripped, mStripped, ignore.case = TRUE)){
                  returnThis <- m
                  break
                }
              }
            } else {
              if(grepl(mStripped, mnStripped, ignore.case = TRUE) | grepl(mnStripped, mStripped, ignore.case = TRUE)){
                returnThis <- m
                break
              }
            }
          }
        }
        }
      }
    }

    #if no match exists attempt to make on listing description or page title or URL or vehicle description string
    if(is.na(returnThis) & length(modelName) > 1)
    {
      modelNamesStripped <- removeNonLettersNumbers(modelName)
      if(mnStripped %in% modelNamesStripped){
        returnThis <- modelName[mnStripped == modelNamesStripped]
      } else {
       for(m in modelName){
         mStripped <- removeNonLettersNumbers(m) #also strip any non letters or numbers from the VID model name
         #if(grepl(m, dirtyRow$YearMakeModelSubModel, ignore.case = TRUE) | grepl(m, dirtyRow$Listing_Description, ignore.case = TRUE) | grepl(m, dirtyRow$Page_Title, ignore.case = TRUE) | grepl(m, dirtyRow$ListingURL, ignore.case = TRUE)){
         if(!is.na(optionalColumns[1])){
           for(thisColumn in optionalColumns){
             if(grepl(mStripped, removeNonLettersNumbers(dirtyRow$YearMakeModelSubModel), ignore.case = TRUE) | grepl(mStripped, removeNonLettersNumbers(dirtyRow[, thisColumn]), ignore.case = TRUE)){
               returnThis <- m
               break
             }
           }
         } else {
           if(grepl(mStripped, removeNonLettersNumbers(dirtyRow$YearMakeModelSubModel), ignore.case = TRUE)){
             returnThis <- m
             break
           }
         }
       }
      }
    }

    #if matching didn't work attempt to join provided that the model name in the dirty data is of sufficient length
    if(is.na(returnThis) & (nchar(mnStripped) > 2) & !is.na(dirtyRow$ModelName)){

      combinedData <- {}
      combinedData <- stringdist_inner_join(dirtyRow[, paste0(c("TxnID", "LotNumber", columnName))]
                                            , vidInput[vidInput$YearName == thisYear
                                                       & !is.na(vidInput$YearName)
                                                       & grepl(firstOne, vidInput$ModelName, ignore.case = TRUE)
                                                       & vidInput$MakeName == thisMake, ]
                                            , by = columnName
                                            , max_dist = nYMM
                                            , distance_col = "DistanceColumn"
                                            , ignore_case = TRUE)
      #combinedData <- combinedData[combinedData$DistanceColumn < minN, ]

      #returnThis <- NA
      if(nrow(combinedData) > 0){
        #sort so that most accurate are first
        combinedData <- combinedData[order(combinedData$DistanceColumn), ]
        returnThis <- combinedData$ModelName[1]
      } else {
        #if no matches exist yet attempt join on make name or page title match
        combinedData <- {}
        if(length(vidInput$ModelName[grepl(paste0(thisYear, " ", dirtyRow$CleanMakeName[1]), vidInput[, columnName], ignore.case = TRUE)]) > 0){
          for(m in unique(vidInput$ModelName[grepl(paste0(thisYear, " ", dirtyRow$CleanMakeName[1]), vidInput[, columnName], ignore.case = TRUE)])){
            mStripped <- removeNonLettersNumbers(m) #also strip any non letters or numbers from the VID model name
            #if(grepl(m, dirtyRow$ModelName, ignore.case = TRUE) | grepl(m, dirtyRow$Page_Title, ignore.case = TRUE)){
            if(!is.na(optionalColumns[1])){
              for(thisColumn in optionalColumns){
                if(grepl(mStripped, removeNonLettersNumbers(dirtyRow$ModelName), ignore.case = TRUE) | grepl(mStripped, removeNonLettersNumbers(dirtyRow[, thisColumn]), ignore.case = TRUE)){
                  combinedData <- rbind(combinedData, stringdist_inner_join(dirtyRow[, paste0(c("TxnID", "LotNumber", "ModelName"))]
                                                                            , vidInput[vidInput$YearName == thisYear
                                                                                       & !is.na(vidInput$YearName)
                                                                                       & vidInput$ModelName == m
                                                                                       & vidInput$MakeName == thisMake, ]
                                                                            , by = "ModelName"
                                                                            , max_dist = nYMM + 20
                                                                            , distance_col = "DistanceColumn"
                                                                            , ignore_case = TRUE))
                }
              }
            } else {
              if(grepl(mStripped, dirtyRow$ModelName, ignore.case = TRUE)){
                combinedData <- rbind(combinedData, stringdist_inner_join(dirtyRow[, paste0(c("TxnID", "LotNumber", "ModelName"))]
                                                                          , vidInput[vidInput$YearName == thisYear
                                                                                     & !is.na(vidInput$YearName)
                                                                                     & vidInput$ModelName == m
                                                                                     & vidInput$MakeName == thisMake, ]
                                                                          , by = "ModelName"
                                                                          , max_dist = nYMM + 20
                                                                          , distance_col = "DistanceColumn"
                                                                          , ignore_case = TRUE))
              }
            }

          }
          if(length(combinedData) > 0){
            #sort so that most accurate are first
            combinedData <- combinedData[order(combinedData$DistanceColumn), ]
            returnThis <- combinedData$ModelName.y[1]
          }
        }
      }
    }

    return(returnThis)

}
