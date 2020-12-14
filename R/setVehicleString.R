
#function allows for comparisons using different combinations of columns depending on what's available in dirty data
#' @export
"setVehicleString" <- function(inputData, columnNames = c("YearName", "MakeName", "ModelName"), rename = NULL)
{

  if(is.null(rename)){
    rename <- paste0(gsub("Name", "", columnNames, ignore.case = TRUE), collapse = "")
  }

  inputData[, rename] <- NA

  combineThese <- function(x) paste0(x, collapse = " ")
  inputData[, rename] <- apply(inputData[, columnNames], 1, combineThese)

  inputData[, rename] <- gsub("Base", "", inputData[, rename], ignore.case = TRUE)
  inputData[, rename] <- gsub(" NA", " ", inputData[, rename], ignore.case = FALSE)
  inputData[, rename] <- gsub("  ", " ", inputData[, rename], ignore.case = TRUE)

  inputData[, rename] <- str_trim(inputData[, rename], side = c("both"))

  return(inputData)
}
