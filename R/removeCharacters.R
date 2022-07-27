
#remove any characters that are not a letter or a number
#' @export
"removeNonLettersNumbers" <- function(inputString)
{
  inputString <- gsub("[^[:alnum:] ]", "", inputString)
  inputString <- str_replace_all(inputString, " ", "")
  return(inputString)
}


#remove any characters that are not a letter or number but keep spaces
#' @export
"removeNonLettersNumbersKS" <- function(inputString)
{
  inputString <- gsub("[^[:alnum:] ]", "", inputString)
  #inputString <- str_replace_all(inputString, " ", "")
  return(inputString)
}

#remove any characters that are not a number
#useful for cleaning prices
#' @export
"removeNonNumbers" <- function(inputString)
{
    #inputString <- gsub("[^[:digit:] ]", "", inputString)
    inputString <- gsub("[^0-9.-]", "", inputString)
    inputString <- str_replace_all(inputString, " ", "")
    inputString <- as.numeric(inputString)
    inputString <- format(inputString, scientific = FALSE)
    return(inputString)
}
