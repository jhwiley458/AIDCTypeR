
#overwrite common misspellings that the matching algorithm will not catch
#' @export
"fixCommonMisspellings" <- function(inputData, columnName)
{

  inputData[grepl("Chevy", inputData[, columnName], ignore.case = TRUE), columnName] <- "Chevrolet"
  inputData[grepl("AMC", inputData[, columnName], ignore.case = TRUE), columnName] <- "American Motors"
  inputData[grepl("^VW$", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "Volkswagen"
  inputData[grepl("Shelby", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "Shelby"
  inputData[grepl("Dodge", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "Dodge"
  inputData[grepl("International", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "International (IHC)"
  inputData[grepl("IHC", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "International (IHC)"

  #fix models to conform to VID
  inputData[grepl("XKE", inputData[, columnName], ignore.case = TRUE), columnName] <-  "E-Type"
  inputData[grepl("XK-E", inputData[, columnName], ignore.case = TRUE), columnName] <-  "E-Type"
  inputData[grepl("911", inputData[, columnName], ignore.case = TRUE), columnName] <-  "911"
  inputData[grepl("964", inputData[, columnName], ignore.case = TRUE), columnName] <-  "911"
  inputData[grepl("993", inputData[, columnName], ignore.case = TRUE), columnName] <-  "911"
  inputData[grepl("996", inputData[, columnName], ignore.case = TRUE), columnName] <-  "911"
  inputData[grepl("997", inputData[, columnName], ignore.case = TRUE), columnName] <-  "911"

  #Fix convention of using model generations in Bring a Trailer data
  inputData[grepl("^E3-B", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "Bavaria"
  inputData[grepl("^E9$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "CS"
  inputData[grepl("^E12$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "5-series"
  inputData[grepl("^E28$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "5-series"
  inputData[grepl("^E34$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "5-series"
  inputData[grepl("^E39$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "5-series"
  inputData[grepl("^E60$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "5-series"

  inputData[grepl("^E24$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "6-series"
  inputData[grepl("E31", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "8-series"

  inputData[grepl("^E21-E36-E46-E92$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "3-series"
  inputData[grepl("^E30$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "3-series"
  inputData[grepl("^E36$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "3-series"
  inputData[grepl("^E46$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "3-series"
  inputData[grepl("^E46-330I-ZHP$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "330i"

  inputData[grepl("M1", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "M1"
  inputData[grepl("M3", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "M3"
  inputData[grepl("M5", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "M5"
  inputData[grepl("M6", inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "M6"

  inputData[grepl("C-Class", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "C-Class"
  inputData[grepl("E-Class", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "E-Class"
  inputData[grepl("S-Class", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "S-Class"
  inputData[grepl("W124", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "E-Class"
  inputData[grepl("W126", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "S-Class"
  inputData[grepl("W113", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "SL"
  inputData[grepl("R230", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "SL"
  #inputData[grepl("W111", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "S"
  #inputData[grepl("W108", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "S"
  #inputData[grepl("W109", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "S"

  #assign appropriate bodystyle to any records that have term in the model name
  inputData[grepl("Cabriolet", inputData[, "ModelName"], ignore.case = TRUE), "BodyTypeName"] <- "Cabriolet"
  inputData[grepl("Convertible", inputData[, "ModelName"], ignore.case = TRUE), "BodyTypeName"] <- "Convertible"
  inputData[grepl("Coupe", inputData[, "ModelName"], ignore.case = TRUE), "BodyTypeName"] <- "Coupe"
  inputData[grepl("Sedan", inputData[, "ModelName"], ignore.case = TRUE), "BodyTypeName"] <- "Sedan"
  inputData[grepl("Roadster", inputData[, "ModelName"], ignore.case = TRUE), "BodyTypeName"] <- "Roadster"

  #remove body name from model name
  inputData[grepl("Cabriolet", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- gsub("Cabriolet", "", inputData[grepl("Cabriolet", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"], ignore.case = TRUE)
  inputData[grepl("Convertible", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- gsub("Convertible", "", inputData[grepl("Convertible", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"], ignore.case = TRUE)
  inputData[grepl("Coupe", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- gsub("Coupe", "", inputData[grepl("Coupe", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"], ignore.case = TRUE)
  inputData[grepl("Sedan", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- gsub("Sedan", "", inputData[grepl("Sedan", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"], ignore.case = TRUE)
  inputData[grepl("Roadster", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- gsub("Roadster", "", inputData[grepl("Roadster", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"], ignore.case = TRUE)

  #fix Porsche 930 model to conform with VID
  inputData[grepl("930", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Porsche", inputData[, "MakeName"], ignore.case = TRUE), "SubModelName"] <-  "Turbo"
  inputData[grepl("930", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Porsche", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "911"

  #fix Porsche GT3 model to conform with VID
  inputData[grepl("GT3", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Porsche", inputData[, "MakeName"], ignore.case = TRUE), "SubModelName"] <-  "GT3"
  inputData[grepl("GT3", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Porsche", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "911"

  #fix Pontiac Trans Am to be submodel of Firebird
  inputData[grepl("Trans Am", inputData[, "ModelName"], ignore.case = TRUE), "SubModelName"] <-  "Trans Am"
  inputData[grepl("Trans Am", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <-  "Firebird"

  #move Range Rover to be model of Land Rover make
  inputData[grepl("Range Rover", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "Range Rover"
  inputData[grepl("Range Rover", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <-  "Land Rover"

  #move Alfa Romeo models to be submodel
  inputData[grepl("Alfa", inputData[, "MakeName"], ignore.case = TRUE) & grepl("GTV", inputData[, "ModelName"], ignore.case = TRUE), "SubModelName"] <-  "GTV"
  inputData[grepl("Alfa", inputData[, "MakeName"], ignore.case = TRUE)
            & grepl("GTV", inputData[, "ModelName"], ignore.case = TRUE)
            & inputData$YearName %in% c(1969, 1970, 1971), "ModelName"] <-  "1750"
  inputData[grepl("Alfa", inputData[, "MakeName"], ignore.case = TRUE)
            & grepl("GTV", inputData[, "ModelName"], ignore.case = TRUE)
            & inputData$YearName %in% c(1972, 1973, 1974), "ModelName"] <-  "2000"


  inputData$MakeName <- str_trim(inputData$MakeName, side = c("both"))
  inputData$ModelName <- str_trim(inputData$ModelName, side = c("both"))
  inputData$BodyTypeName <- str_trim(inputData$BodyTypeName, side = c("both"))

  return(inputData)

}
