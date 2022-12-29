#clean and or determine sub model names using VID data
#new version
#works one row at a time
#assumption is that model year and clean make name and clean model name are correct
#' @export
"cleanSubModelName" <- function(dirtyRow, vidInput, columnName, optionalColumns = NA)
{

   #use clean make and model and model year
  thisYear <- dirtyRow$YearName[1]
  thisMake <- dirtyRow$CleanMakeName[1]
  thisModel <- dirtyRow$CleanModelName[1]

  #match to VID on those
  subModels <- unique(vidInput$SubModelName[vidInput$YearName == thisYear
                                            & vidInput$MakeName == thisMake
                                            & vidInput$ModelName == thisModel
                                            & !is.na(vidInput$SubModelName)])

  if(length(subModels) > 0){
   subModelsNchar <- sapply(subModels, nchar)
   subModels <- subModels[order(-subModelsNchar)] #most descriptive submodels are first
  }

  #remove characters such as dashes or spaces
  smnStripped <- removeNonLettersNumbers(dirtyRow[, columnName])

  returnThis <- NA
  if(length(subModels) == 1)
  {
    returnThis <- subModels #if just one exists return that
  } else {

    #otherwise attempt join to VID on possible matches. Fewer than 1/3 of characters in submodel can be wrong
    if(length(subModels) > 1 & !is.na(dirtyRow$SubModelName) & !dirtyRow$Review & nchar(dirtyRow$SubModelName) > 3)
    {

      submodelNamesStripped <- removeNonLettersNumbers(subModels)
      if(smnStripped %in% submodelNamesStripped){
        returnThis <- subModels[smnStripped == submodelNamesStripped]
      }
      else {
      combinedData <- {}
      combinedData <- stringdist_inner_join(dirtyRow[, c("TxnID", "SubModelName")]
                                            , vidInput[vidInput$YearName == thisYear
                                                       & !is.na(vidInput$YearName)
                                                       & vidInput$MakeName == thisMake
                                                       & vidInput$ModelName == thisModel, ]
                                            , by = "SubModelName"
                                            , max_dist = round(nchar(dirtyRow[1, "SubModelName"]) / 3, 0)
                                            , distance_col = "DistanceColumn"
                                            , ignore_case = TRUE)
      returnThis <- NA
      if(nrow(combinedData) > 0){
        #sort so that most accurate are first
        combinedData <- combinedData[order(combinedData$DistanceColumn), ]
        returnThis <- combinedData$SubModelName.y[1]
      }
      }
    }
    #}
  }

  #if there is a match for year and make and model but not submodel
  #attempt to match on vehicle description string YearMakeModelSubModel or listing description or Page Title or URL
  if(is.na(returnThis) & length(subModels) > 1 & !dirtyRow$Review)
  {
    subModelsNchar <- sapply(subModels, nchar)
    subModels <- subModels[order(-subModelsNchar)] #most descriptive submodels are first

    for(sm in subModels){
      if(grepl("[.]", sm)){sm <- gsub("\\.", "[.]", sm)}
      smStripped <- removeNonLettersNumbers(sm) #also strip any non letters or numbers from the VID model name
      #if(grepl(sm, dirtyRow$YearMakeModelSubModel, ignore.case = TRUE) | grepl(sm, dirtyRow$Listing_Description, ignore.case = TRUE) | grepl(sm, dirtyRow$Page_Title, ignore.case = TRUE)){
      if(!is.na(optionalColumns[1])){
        for(thisColumn in optionalColumns){
          if(is.na(returnThis) & (grepl(smStripped, smnStripped, ignore.case = TRUE) | (grepl(sm, dirtyRow[, columnName], ignore.case = TRUE) | grepl(sm, dirtyRow[, thisColumn], ignore.case = TRUE)) & !grepl(sm, thisMake, ignore.case = FALSE))){
            returnThis <- sm
            break
          }
        }
      } else {
        if(grepl(sm, dirtyRow[, columnName], ignore.case = TRUE) & !grepl(sm, thisMake, ignore.case = FALSE) & is.na(returnThis)){
          returnThis <- sm
          break
        }
        if(grepl(dirtyRow[, columnName], sm, ignore.case = TRUE) & (nchar(dirtyRow[, columnName]) > 0) & !grepl(sm, thisMake, ignore.case = FALSE) & is.na(returnThis)){
          returnThis <- sm
          break
        }
      }
    }
  }

  #if there is a match for year and make but not model or submodel
  #attempt to match on vehicle description string YearMakeModelSubModel or listing description or Page Title or URL
  if(is.na(returnThis) & length(subModels) == 0 & !dirtyRow$Review)
  {
    subModels <- unique(vidInput$SubModelName[vidInput$YearName == thisYear
                                              & vidInput$MakeName == thisMake
                                              & !is.na(vidInput$SubModelName)])
    subModelsNchar <- sapply(subModels, nchar)
    subModels <- subModels[order(-subModelsNchar)] #most descriptive are first

    for(sm in subModels){
      if(grepl("[.]", sm)){sm <- gsub("\\.", "[.]", sm)}
      #if(grepl(sm, dirtyRow$YearMakeModelSubModel, ignore.case = TRUE) | grepl(sm, dirtyRow$Listing_Description, ignore.case = TRUE) | grepl(sm, dirtyRow$Page_Title, ignore.case = TRUE)){
      if(!is.na(optionalColumns[1])){
        for(thisColumn in optionalColumns){
          if((grepl(sm, dirtyRow[, columnName], ignore.case = TRUE) | grepl(sm, dirtyRow[, thisColumn], ignore.case = TRUE)) & !grepl(sm, thisMake, ignore.case = FALSE)) {
            returnThis <- sm
            break
          }
        }
      } else {
        if(grepl(sm, dirtyRow[, columnName], ignore.case = TRUE) & !grepl(sm, thisMake, ignore.case = FALSE) & is.na(returnThis)){
          returnThis <- sm
          break
        }
        if(grepl(dirtyRow[, columnName], sm, ignore.case = TRUE) & (nchar(dirtyRow[, columnName]) > 0) & !grepl(sm, thisMake, ignore.case = FALSE) & is.na(returnThis)){
          returnThis <- sm
          break
        }
      }
    }
  }

  if(grepl("\\[", returnThis) & grepl("\\]", returnThis))
  {
    returnThis <- gsub("\\[", "", returnThis)
    returnThis <- gsub("\\]", "", returnThis)
  }

  return(returnThis)


}
