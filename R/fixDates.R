

#fix dates for those with three letter month abbreviations
#' @export
"fixDates" <- function(dirtyData)
{

  dirtyData$End_Date[grepl(" 00:00:00", dirtyData$End_Date, ignore.case = FALSE)] <- gsub(" 00:00:00", "", dirtyData$End_Date[grepl(" 00:00:00", dirtyData$End_Date, ignore.case = FALSE)])

  dirtyData$End_Date[grepl("-Jan-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Jan-", "-01-", dirtyData$End_Date[grepl("-Jan-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-Feb-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Feb-", "-02-", dirtyData$End_Date[grepl("-Feb-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-Mar-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Mar-", "-03-", dirtyData$End_Date[grepl("-Mar-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-Apr-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Apr-", "-04-", dirtyData$End_Date[grepl("-Apr-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-May-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-May-", "-05-", dirtyData$End_Date[grepl("-May-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-Jun-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Jun-", "-06-", dirtyData$End_Date[grepl("-Jun-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-Jul-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Jul-", "-07-", dirtyData$End_Date[grepl("-Jul-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-Aug-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Aug-", "-08-", dirtyData$End_Date[grepl("-Aug-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-Sep-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Sep-", "-09-", dirtyData$End_Date[grepl("-Sep-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-Oct-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Oct-", "-10-", dirtyData$End_Date[grepl("-Oct-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-Nov-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Nov-", "-11-", dirtyData$End_Date[grepl("-Nov-", dirtyData$End_Date, ignore.case = FALSE)])
  dirtyData$End_Date[grepl("-Dec-", dirtyData$End_Date, ignore.case = FALSE)] <- gsub("-Dec-", "-12-", dirtyData$End_Date[grepl("-Dec-", dirtyData$End_Date, ignore.case = FALSE)])

  dirtyData$End_Date[is.na(dirtyData$End_Date)] <- "2020-01-01"

  dirtyData$End_Date <- as.Date(dirtyData$End_Date, format = "%Y-%m-%d", origin = "1970-01-01")

  return(dirtyData)

}
