
#determine engine type name using VID data
#matching attempts become less specific if none are found
#' @export
"cleanEngineDescription" <- function(dirtyRow, vidInput, optionalColumns = NA)
{

  #match on year make model sub model and body
  thisYear <- dirtyRow$YearName[1]
  thisMake <- dirtyRow$CleanMakeName[1]
  thisModel <- dirtyRow$CleanModelName[1]
  thisSubModel <- dirtyRow$CleanSubModelName[1]
  thisBodyType <- dirtyRow$CleanBodyTypeName[1]

  #list of possible engines in the VID
  #match on year make model and sub model body
  engineType <- unique(vidInput$Engine_Description[vidInput$YearName == thisYear
                                                   & vidInput$MakeName == thisMake
                                                   & vidInput$ModelName == thisModel
                                                   & vidInput$SubModelName == thisSubModel
                                                   & vidInput$BodyTypeName == thisBodyType
                                                   & !is.na(vidInput$Engine_Description)])

  returnThis <- NA
  if(length(engineType) == 1)
  {
    returnThis <- engineType #return this if only one engine type available
  }

  #match on year make model and sub model
  engineType <- unique(vidInput$Engine_Description[vidInput$YearName == thisYear
                                                   & vidInput$MakeName == thisMake
                                                   & vidInput$ModelName == thisModel
                                                   & vidInput$SubModelName == thisSubModel
                                                   & !is.na(vidInput$Engine_Description)])

  if(length(engineType) == 1)
  {
    returnThis <- engineType #return this if only one engine type available
  }

  #match on year make model and
  engineType <- unique(vidInput$Engine_Description[vidInput$YearName == thisYear
                                                   & vidInput$MakeName == thisMake
                                                   & vidInput$ModelName == thisModel
                                                   & !is.na(vidInput$Engine_Description)])

  if(length(engineType) == 1)
  {
    returnThis <- engineType #return this if only one engine type available
  }

  #match on year make model and
  engineOptions <- vidInput[vidInput$YearName == thisYear
                            & vidInput$MakeName == thisMake & vidInput$ModelName == thisModel
                            & !is.na(vidInput$Engine_Description)
                            & !is.na(vidInput$EngineHorsepower)
                            & !is.na(vidInput$Displacement), ]

  engineOptions <- removeDuplicates(engineOptions, "Engine_Description")

  if(nrow(dirtyRow[!is.na(dirtyRow$Engine_Description) | !is.na(dirtyRow$Engine_Displacement) | !is.na(dirtyRow$Engine_Horsepower), ]) > 0){
   if(is.na(returnThis) & length(engineType) > 1 & identical(optionalColumns, c("Engine_Description", "Engine_Displacement", "Engine_Horsepower")))
   {
    if(length(unique(engineOptions$EngineHorsepower)) == length(unique(engineType))){
      for(e in engineType){
        thisHP <- NA
        if(length(engineOptions$EngineHorsepower[engineOptions$Engine_Description == e & !is.na(engineOptions$EngineHorsepower)]) > 0){
         thisHP <- engineOptions$EngineHorsepower[engineOptions$Engine_Description == e & !is.na(engineOptions$EngineHorsepower)]
        }
        #HP must be in Engine_Horsepower or Engine_Description
        if(grepl(thisHP, dirtyRow$Engine_Horsepower, ignore.case = TRUE) | grepl(thisHP, dirtyRow$Engine_Description, ignore.case = TRUE)){
          returnThis <- e
          break
        }
      }
    }
    if(length(unique(engineOptions$Displacement)) == length(unique(engineType)) & is.na(returnThis)){
      for(e in engineType){
        thisDisplacement <- NA
        if(length(engineOptions$Displacement[engineOptions$Engine_Description == e & !is.na(engineOptions$Displacement)]) > 0){
         thisDisplacement <- engineOptions$Displacement[engineOptions$Engine_Description == e & !is.na(engineOptions$Displacement)]
        }
        altDisplacement <- NA
        if(length(engineOptions$AltDisplacement[engineOptions$Engine_Description == e & !is.na(engineOptions$AltDisplacement)]) > 0){
          altDisplacement <- engineOptions$AltDisplacement[engineOptions$Engine_Description == e & !is.na(engineOptions$AltDisplacement)]
        }
        #thisDisplacement or AltDisplacement must be in Engine_Description or Engine_Displacement
        if(!is.na(grepl(thisDisplacement, dirtyRow$Engine_Description, ignore.case = TRUE) | grepl(thisDisplacement, dirtyRow$Engine_Displacement, ignore.case = TRUE) | grepl(altDisplacement, dirtyRow$Engine_Description, ignore.case = TRUE) | grepl(altDisplacement, dirtyRow$Engine_Displacement, ignore.case = TRUE))){
         if(grepl(thisDisplacement, dirtyRow$Engine_Description, ignore.case = TRUE) | grepl(thisDisplacement, dirtyRow$Engine_Displacement, ignore.case = TRUE) | grepl(altDisplacement, dirtyRow$Engine_Description, ignore.case = TRUE) | grepl(altDisplacement, dirtyRow$Engine_Displacement, ignore.case = TRUE)){
           returnThis <- e
           break
        }}
      }
    }
    if(is.na(returnThis)){
     for(e in engineType){
      thisHP <- NA
      if(length(engineOptions$EngineHorsepower[engineOptions$Engine_Description == e & !is.na(engineOptions$EngineHorsepower)]) > 0){
        thisHP <- engineOptions$EngineHorsepower[engineOptions$Engine_Description == e & !is.na(engineOptions$EngineHorsepower)]
      }
      thisDisplacement <- NA
      if(length(engineOptions$Displacement[engineOptions$Engine_Description == e & !is.na(engineOptions$Displacement)]) > 0){
        thisDisplacement <- engineOptions$Displacement[engineOptions$Engine_Description == e & !is.na(engineOptions$Displacement)]
      }
      altDisplacement <- NA
      if(length(engineOptions$AltDisplacement[engineOptions$Engine_Description == e & !is.na(engineOptions$AltDisplacement)]) > 0){
       altDisplacement <- engineOptions$AltDisplacement[engineOptions$Engine_Description == e & !is.na(engineOptions$AltDisplacement)]
      }
      #HP must be in Engine_Horsepower or Engine_Description
      #AND thisDisplacement or AltDisplacement must be in Engine_Description or Engine_Displacement
      if(!is.na(thisHP) & !is.na(thisDisplacement)){
       if((grepl(thisHP, dirtyRow$Engine_Horsepower, ignore.case = TRUE) | grepl(thisHP, dirtyRow$Engine_Description, ignore.case = TRUE)) & (grepl(thisDisplacement, dirtyRow$Engine_Description, ignore.case = TRUE) | grepl(thisDisplacement, dirtyRow$Engine_Displacement, ignore.case = TRUE) | grepl(altDisplacement, dirtyRow$Engine_Description, ignore.case = TRUE) | grepl(altDisplacement, dirtyRow$Engine_Displacement, ignore.case = TRUE))){
          returnThis <- e
          break
       }
      }
    }
    }
  }
  }

  return(returnThis)

}
