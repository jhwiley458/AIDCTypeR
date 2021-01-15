
#query analysis for VID data as guide for cleaning data
#' @export
"getVIDData" <- function()
{

  {
    queryString <- paste0("SELECT LTRIM(RTRIM([YearName])) AS YearName
                                   ,LTRIM(RTRIM([MakeName])) AS MakeName
                                   ,LTRIM(RTRIM([ModelName])) AS ModelName
                                   ,LTRIM(RTRIM([SubModelName])) AS SubModelName
	                                 ,LTRIM(RTRIM([BodyTypeName])) AS BodyTypeName
                                   ,LTRIM(RTRIM(vbref.[Engine_Description])) AS Engine_Description
	                                 ,vbref.[VehicleID]
                                   ,vbref.[BaseVehicleID]
	                                 ,vbref.[VehicleBodyEngineID]
	                                 ,vbref.[EngineCodeID]
	                                 ,vbref.MaintenanceStatusDesc
	                                 ,vg.[VehicleGroupName]
                                   FROM [VehicleInformation].[dbo].[VBEReferenceData] vbref
                                   LEFT JOIN
                                   [VehicleInformation].[dbo].[VehicleGroupToBaseVehicle] vgbv
                                   ON
                                   vgbv.[BaseVehicleID] = vbref.[BaseVehicleID]
                                   LEFT JOIN
                                   [VehicleInformation].[dbo].[VehicleGroup] vg
                                   ON
                                   vgbv.[VehicleGroupID] = vg.[VehicleGroupID]
                                   LEFT JOIN
                                   [VehicleInformation].[dbo].[EngineCode] ec
                                   ON
                                   vbref.EngineCodeID = ec.EngineCodeID
                                   WHERE vbref.MaintenanceStatusDesc LIKE '%Active%'
                                   AND [ModelName] NOT LIKE '% Automatic%'
                                   --AND vg.[VehicleGroupTypeID] = 2
                                   ORDER BY [YearName], [MakeName], [ModelName], SubModelName, [BodyTypeName]")
  }

  vidData <- {}
  dbhandle <- odbcDriverConnect("driver={SQL Server};server=SQLVehicleProd;database=VehicleInformation;trusted_connection=true")
  vidData <- sqlQuery(dbhandle, queryString)

  odbcClose(dbhandle)

  vidData$ModelName[grepl("[(]Truck[)]", vidData$ModelName, ignore.case = TRUE)] <- gsub("[(]Truck[)]", "", vidData$ModelName[grepl("[(]Truck[)]", vidData$ModelName, ignore.case = TRUE)], ignore.case = TRUE)

  vidData$ModelName <- str_trim(vidData$ModelName, side = c("both"))

  vidData$Engine_Description[grepl("N/A", vidData$Engine_Description, ignore.case = TRUE)] <- NA

  #Convert blanks to NAs
  vidData$LEN <- NA
  vidData$LEN <- nchar(vidData$Engine_Description)
  vidData$Engine_Description[vidData$LEN == 0] <- NA
  vidData <- vidData[, !(names(vidData) %in% c("LEN"))]

  get_cylinder_count <- function(x) strsplit(x, '-cyl')[[1]][1]
  vidData$CylinderCount <- sapply(vidData$Engine_Description, get_cylinder_count)
  vidData$CylinderCount <- gsub("Special Order ", "", vidData$CylinderCount, ignore.case = TRUE)
  vidData$CylinderCount <- gsub("Electric Motor", NA, vidData$CylinderCount, ignore.case = TRUE)
  vidData$CylinderCount[grepl("Volt", vidData$CylinderCount, ignore.case = TRUE)] <- NA

  get_displacement <- function(x) strsplit(strsplit(x, '/')[[1]][1], " ")[[1]][2]
  vidData$Displacement <- sapply(vidData$Engine_Description, get_displacement)
  vidData$Displacement <- gsub("Special", NA, vidData$Displacement, ignore.case = TRUE)
  vidData$Displacement <- gsub("Order", NA, vidData$Displacement, ignore.case = TRUE)
  vidData$Displacement <- gsub("Special", NA, vidData$Displacement, ignore.case = TRUE)

  #get units
  vidData$DisplacementUnit[grepl('L', vidData$Displacement, ignore.case = FALSE)] <- 'L'
  vidData$DisplacementUnit[grepl('cc', vidData$Displacement, ignore.case = TRUE)] <- 'cc'
  vidData$DisplacementUnit[grepl('cid', vidData$Displacement, ignore.case = TRUE)] <- 'cid'

  #remove units from displacement field
  vidData$Displacement <- gsub("cid", "", vidData$Displacement, ignore.case = TRUE)
  vidData$Displacement <- gsub("L", "", vidData$Displacement, ignore.case = FALSE)
  vidData$Displacement <- gsub("cc", "", vidData$Displacement, ignore.case = TRUE)

  vidData$Displacement <- as.numeric(vidData$Displacement)

  vidData$AltDisplacementUnit <- NA
  vidData$AltDisplacement <- NA
  vidData$AltDisplacementUnit[grepl('cc', vidData$DisplacementUnit, ignore.case = FALSE)] <- 'L'
  vidData$AltDisplacement[grepl('cc', vidData$DisplacementUnit, ignore.case = FALSE)] <- round(0.001 * vidData$Displacement[grepl('cc', vidData$DisplacementUnit, ignore.case = FALSE)], 1)

  vidData$AltDisplacementUnit[grepl('cid', vidData$DisplacementUnit, ignore.case = FALSE)] <- 'L'
  vidData$AltDisplacement[grepl('cid', vidData$DisplacementUnit, ignore.case = FALSE)] <- round((1/61) * vidData$Displacement[grepl('cid', vidData$DisplacementUnit, ignore.case = FALSE)], 1)

  vidData$Displacement[grepl("Volt", vidData$Displacement, ignore.case = TRUE)] <- NA
  vidData$Displacement[grepl("Electric Motor", vidData$Engine_Description, ignore.case = TRUE)] <- NA

  vidData$Displacement[grepl('^NA$', vidData$Displacement, ignore.case = FALSE)] <- NA

  #horsepower when hp is present
  get_horsepower <- function(x) strsplit(strsplit(x, 'hp')[[1]][1], '/')[[1]][2]
  vidData$EngineHorsepower[grepl("[0-9]hp", vidData$Engine_Description, ignore.case = TRUE)] <- sapply(vidData$Engine_Description[grepl("[0-9]hp", vidData$Engine_Description, ignore.case = TRUE)], get_horsepower)

  vidData$EngineHorsepower <- as.character(vidData$EngineHorsepower)
  vidData$Displacement <- as.character(vidData$Displacement)
  vidData$AltDisplacement <- as.character(vidData$AltDisplacement)

  vidData <- setVehicleString(vidData, c("YearName", "MakeName", "ModelName"))
  vidData <- setVehicleString(vidData, c("YearName", "MakeName", "ModelName", "SubModelName", "BodyTypeName"))

  return(vidData)

}
