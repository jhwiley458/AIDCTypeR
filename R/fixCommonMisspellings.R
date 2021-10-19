
#overwrite common misspellings that the matching algorithm will not catch
#' @export
"fixCommonMisspellings" <- function(inputData, columnName)
{

  inputData[grepl("Chevy", inputData[, columnName], ignore.case = TRUE), columnName] <- "Chevrolet"
  inputData[grepl("AMC", inputData[, columnName], ignore.case = TRUE), columnName] <- "American Motors"
  inputData[grepl("^VW$", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "Volkswagen"
  inputData[grepl("Shelby", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "Shelby"
  inputData[grepl("Dodge", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "Dodge"
  inputData[grepl("Mercedes", inputData[, "MakeName"], ignore.case = TRUE) & inputData$YearName >= 1926, "MakeName"] <- "Mercedes-Benz"
  inputData[grepl("International", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "International (IHC)"
  inputData[grepl("IHC", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "International (IHC)"

  #fix models to conform to VID
  inputData[grepl("XKE", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <-  "E-Type"
  inputData[grepl("XK-E", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <-  "E-Type"
  inputData[grepl("911", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <-  "911"
  inputData[grepl("964", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <-  "911"
  inputData[grepl("993", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <-  "911"
  inputData[grepl("996", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <-  "911"
  inputData[grepl("997", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <-  "911"

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

  #Remove BMW generation data for Collecting Cars etc 
  for(thisGeneration in c("E9", "E12", "E28", "E34", "E39", "E60", "E24", "E31", "E21", "E30", "E36", "E46", "E92")){
   inputData[grepl(thisGeneration, inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- gsub(thisGeneration, "", inputData[grepl(thisGeneration, inputData[, "ModelName"], ignore.case = TRUE) & grepl("BMW", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"])
  }
  
  inputData[grepl("C-Class", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "C-Class"
  inputData[grepl("E-Class", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "E-Class"
  inputData[grepl("S-Class", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "S-Class"
  inputData[grepl("^W124$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "E-Class"
  inputData[grepl("^W126$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "S-Class"
  inputData[grepl("^W113$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "SL"
  inputData[grepl("^R230$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "SL"
  #inputData[grepl("^W111$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "S"
  #inputData[grepl("^W108$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "S"
  #inputData[grepl("^W109$", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "S"

  #Remove Gullwing from model name and add coupe to body name
  inputData[grepl("300SL", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Gullwing", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "BodyTypeName"] <- "Coupe"
  inputData[grepl("300SL", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Gullwing", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "SubModelName"] <- "Gullwing"
  inputData[grepl("300SL", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Gullwing", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "300SL"

    #Remove Mercedes-Benz generation data for Collecting Cars etc 
  for(thisGeneration in c("W111", "W110", "W113", "R107", "R129", "W201", "W210", "W208", "W123", "W124", "W126", "W108", "W109", "R230", "R170")){
   inputData[grepl(thisGeneration, inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- gsub(thisGeneration, "", inputData[grepl(thisGeneration, inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"])
  }
  
  inputData[grepl("[0-9] SL", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- gsub(" SL", "SL", inputData[grepl("[0-9] SL", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"])
  inputData[grepl("SL [0-9]", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- gsub("SL ", "SL", inputData[grepl("SL [0-9]", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"])
  inputData[grepl("PAGODA", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- gsub("PAGODA", "", inputData[grepl("PAGODA", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Mercedes-Benz", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"])
  
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
  inputData[grepl("Trans", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Pontiac", inputData[, "MakeName"], ignore.case = TRUE), "SubModelName"] <-  "Trans Am"
  inputData[grepl("Trans", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Pontiac", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <-  "Firebird"

  #fix Pontiac GTO pre-1966
  inputData[grepl("GTO", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Pontiac", inputData[, "MakeName"], ignore.case = TRUE) & inputData$YearName < 1966, "SubModelName"] <-  "GTO"
  inputData[grepl("GTO", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Pontiac", inputData[, "MakeName"], ignore.case = TRUE) & inputData$YearName < 1966, "ModelName"] <-  "LeMans"

  #fix Oldsmobile 4-4-2 Cutlass pre-1968 and post-1971
  inputData[grepl("442", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Oldsmobile", inputData[, "MakeName"], ignore.case = TRUE) & (inputData$YearName < 1968 | inputData$YearName > 1972), "SubModelName"] <- "4-4-2"
  inputData[grepl("442", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Oldsmobile", inputData[, "MakeName"], ignore.case = TRUE) & (inputData$YearName < 1968 | inputData$YearName > 1972), "ModelName"] <- "Cutlass"

  #Modern Shelby GT350 GT500
  inputData[grepl("Shelby", inputData[, "MakeName"], ignore.case = TRUE) & grepl("GT350", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2006 & (grepl("GT350", inputData[, "ListingURL"], ignore.case = TRUE) | grepl("GT350", inputData[, "ListingURL"], ignore.case = TRUE)), "SubModelName"] <- "Shelby GT350"
  inputData[grepl("Shelby", inputData[, "MakeName"], ignore.case = TRUE) & grepl("GT350R", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2006 & (grepl("GT350R", inputData[, "ListingURL"], ignore.case = TRUE) | grepl("GT350R", inputData[, "ListingURL"], ignore.case = TRUE)), "SubModelName"] <- "Shelby GT350R"
  inputData[grepl("Shelby", inputData[, "MakeName"], ignore.case = TRUE) & grepl("GT500", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2006 & (grepl("GT500", inputData[, "ListingURL"], ignore.case = TRUE) | grepl("GT500", inputData[, "ListingURL"], ignore.case = TRUE)), "SubModelName"] <- "Shelby GT500"
  inputData[grepl("Shelby", inputData[, "MakeName"], ignore.case = TRUE) & grepl("GT", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2006, "ModelName"] <- "Mustang"
  inputData[grepl("Shelby", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Mustang", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2006, "MakeName"] <- "Ford"

  inputData[grepl("Ford", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Shelby", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2006 & (grepl("GT350", inputData[, "ListingURL"], ignore.case = TRUE) | grepl("GT350", inputData[, "ListingURL"], ignore.case = TRUE)), "SubModelName"] <- "Shelby GT350"
  inputData[grepl("Ford", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Shelby", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2006 & (grepl("GT350R", inputData[, "ListingURL"], ignore.case = TRUE) | grepl("GT350R", inputData[, "ListingURL"], ignore.case = TRUE)), "SubModelName"] <- "Shelby GT350R"
  inputData[grepl("Ford", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Shelby", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2006 & (grepl("GT500", inputData[, "ListingURL"], ignore.case = TRUE) | grepl("GT500", inputData[, "ListingURL"], ignore.case = TRUE)), "SubModelName"] <- "Shelby GT500"
  inputData[grepl("Ford", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Shelby", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2006, "ModelName"] <- "Mustang"
  
  inputData[grepl("Ford", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Mustang", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- "Mustang"  
  inputData[grepl("Ford", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Thunderbird", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "Thunderbird"
  inputData[grepl("Ford", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Mustang", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "Mustang"  
  inputData[grepl("Ford", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Thunderbird", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "Thunderbird"
  
  inputData[grepl("Ford ", inputData[, "MakeName"], ignore.case = TRUE), "MakeName"] <- "Ford"
  
  #Modern Mini Cooper
  inputData[grepl("Mini", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Modern", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2002, "ModelName"] <- "Cooper"
  inputData[grepl("Mini", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Cooper", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName >= 2002, "MakeName"] <- "Mini"

  #Classic Mini Cooper
  inputData[grepl("Mini", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Classic", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName < 2002, "ModelName"] <- "Mini"
  inputData[grepl("Mini", inputData[, "MakeName"], ignore.case = TRUE) & grepl("Mini", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName < 2002, "MakeName"] <- "Austin"

  inputData[grepl("Chevrolet", inputData[, "MakeName"], ignore.case = TRUE) & grepl("3100", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- "Series 3100"
  inputData[grepl("Chevrolet", inputData[, "MakeName"], ignore.case = TRUE) & grepl("3200", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- "Series 3200"
  inputData[grepl("Chevrolet", inputData[, "MakeName"], ignore.case = TRUE) & grepl("3400", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- "Series 3400"
  inputData[grepl("Chevrolet", inputData[, "MakeName"], ignore.case = TRUE) & grepl("3500", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- "Series 3500"
  inputData[grepl("Chevrolet", inputData[, "MakeName"], ignore.case = TRUE) & grepl("3600", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- "Series 3600"
  inputData[grepl("Chevrolet", inputData[, "MakeName"], ignore.case = TRUE) & grepl("3700", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- "Series 3700"
  inputData[grepl("Chevrolet", inputData[, "MakeName"], ignore.case = TRUE) & grepl("3800", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- "Series 3800"
  inputData[grepl("Chevrolet", inputData[, "MakeName"], ignore.case = TRUE) & grepl("3900", inputData[, "ModelName"], ignore.case = TRUE), "ModelName"] <- "Series 3900"
  
   inputData[grepl("Bus", inputData[, "ModelName"], ignore.case = TRUE) & grepl("Volkswagen", inputData[, "MakeName"], ignore.case = TRUE) & (inputData$YearName < 1996 | inputData$YearName > 1950), "ModelName"] <- "Transporter"
  
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
  
  inputData[grepl("Alfa Romeo 105/115 Series Spider", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName %in% c(1966, 1967, 1968)
            & grepl("Alfa Romeo", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "Giulia"
  
    inputData[grepl("Alfa Romeo 105/115 Series Spider", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName %in% c(1969, 1970, 1971)
            & grepl("Alfa Romeo", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "1750"
  
      inputData[grepl("Alfa Romeo 105/115 Series Spider", inputData[, "ModelName"], ignore.case = TRUE) & inputData$YearName %in% c(1972, 1973, 1974, 1975, 1976)
            & grepl("Alfa Romeo", inputData[, "MakeName"], ignore.case = TRUE), "ModelName"] <- "2000"

  inputData$ModelName[grepl("[[]link[]]", inputData$ModelName, ignore.case = TRUE)] <- gsub("[[]link[]]", "", inputData$ModelName[grepl("[[]link[]]", inputData$ModelName, ignore.case = TRUE)])
  
  inputData$MakeName <- str_trim(inputData$MakeName, side = c("both"))
  inputData$ModelName <- str_trim(inputData$ModelName, side = c("both"))
  inputData$BodyTypeName <- str_trim(inputData$BodyTypeName, side = c("both"))

  return(inputData)

}
