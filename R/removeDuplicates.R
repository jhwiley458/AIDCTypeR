
#remove any duplicate rows
#can be used for transactions or a merge
#columnName determines which field identifies duplicates
#often helpful to create concatenation of make date lot price or some variation
#' @export
"removeDuplicates" <- function(inputData, columnName)
{
  if(nrow(inputData) == 0){
    inputData <- {}
  } else {
    inputData$Duplicate <- FALSE
    inputData$Duplicate[duplicated(inputData[, columnName])] <- TRUE
    inputData <- inputData[inputData$Duplicate == FALSE, ]
    inputData <- inputData[, !(names(inputData) %in% c("Duplicate"))]
  }
  return(inputData)

}
