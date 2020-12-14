

#merges on year make and model to determine base vehicle ID
#' @export
"addBaseVehicleID" <- function(dirtyData, vidInput)
{
  dirtyData <- merge(dirtyData, vidInput[, c("YearMakeModel", "BaseVehicleID")], all.x = TRUE)
  dirtyData <- removeDuplicates(dirtyData, "TxnID")
  return(dirtyData)
}

#merges on match year make model sub model to determine vehicle ID
#' @export
"addVehicleID" <- function(dirtyData, vidInput)
{
  dirtyData <- merge(dirtyData, vidInput[, c("YearMakeModel", "VehicleID")], all.x = TRUE)
  dirtyData <- removeDuplicates(dirtyData, "TxnID")
  return(dirtyData)
}

#merges on match year make model sub model body engine to determine vehicle body engine ID
#' @export
"addVehicleBodyEngineID" <- function(dirtyData, vidInput)
{
  dirtyData <- merge(dirtyData, vidInput[, c("YMMSMBE", "VehicleBodyEngineID")], all.x = TRUE)
  dirtyData <- removeDuplicates(dirtyData, "TxnID")
  return(dirtyData)
}

#merges on vehicle body engine ID to determine engine ID
#' @export
"addEngineCodeID" <- function(dirtyData, vidInput)
{
  dirtyData <- merge(dirtyData, vidInput[, c("VehicleBodyEngineID", "EngineCodeID")], all.x = TRUE)
  dirtyData <- removeDuplicates(dirtyData, "TxnID")
  return(dirtyData)
}
