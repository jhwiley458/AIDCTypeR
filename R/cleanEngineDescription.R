
#determine engine type name using VID data
#matching attempts become less specific if none are found
#' @export
"cleanEngineDescription" <- function(dirtyRow, vidInput)
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
                                                   & vidInput$BodyTypeName == thisBodyType])

  returnThis <- NA
  if(length(engineType) == 1)
  {
    returnThis <- engineType #return this if only one engine type available
  }

  #match on year make model and sub model
  engineType <- unique(vidInput$Engine_Description[vidInput$YearName == thisYear
                                                   & vidInput$MakeName == thisMake
                                                   & vidInput$ModelName == thisModel
                                                   & vidInput$SubModelName == thisSubModel])

  if(length(engineType) == 1)
  {
    returnThis <- engineType #return this if only one engine type available
  }

  #match on year make model and
  engineType <- unique(vidInput$Engine_Description[vidInput$YearName == thisYear
                                                   & vidInput$MakeName == thisMake
                                                   & vidInput$ModelName == thisModel])

  if(length(engineType) == 1)
  {
    returnThis <- engineType #return this if only one engine type available
  }

  return(returnThis)

}
