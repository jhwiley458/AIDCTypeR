
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
                                   FROM [VehicleInformationAIS].[dbo].[VBEReferenceData] vbref
                                   LEFT JOIN
                                   [VehicleInformationAIS].[dbo].[VehicleGroupToBaseVehicle] vgbv
                                   ON
                                   vgbv.[BaseVehicleID] = vbref.[BaseVehicleID]
                                   LEFT JOIN
                                   [VehicleInformationAIS].[dbo].[VehicleGroup] vg
                                   ON
                                   vgbv.[VehicleGroupID] = vg.[VehicleGroupID]
                                   LEFT JOIN
                                   [VehicleInformationAIS].[dbo].[EngineCode] ec
                                   ON
                                   vbref.EngineCodeID = ec.EngineCodeID
                                   WHERE vbref.MaintenanceStatusDesc LIKE '%Active%'
                                   AND [ModelName] NOT LIKE '% Automatic%'
                                   --AND vg.[VehicleGroupTypeID] = 2
                                   ORDER BY [YearName], [MakeName], [ModelName], SubModelName, [BodyTypeName]")
  }

  vidData <- {}
  dbhandle <- odbcDriverConnect("driver={SQL Server};server=VHDCSQLAIS01;database=VehicleInformationAIS;trusted_connection=true")
  vidData <- sqlQuery(dbhandle, queryString)

  odbcClose(dbhandle)

  #combine BMW model and submodels for better matching
  vidData$ModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                    & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                    & grepl("^i", vidData$SubModelName, ignore.case = TRUE)] <- paste0(vidData$ModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                                                                                                         & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                                                                                                         & grepl("^i", vidData$SubModelName, ignore.case = TRUE)], vidData$SubModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                                                                                                                                                                                        & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                                                                                                                                                                                        & grepl("^i", vidData$SubModelName, ignore.case = TRUE)])

  vidData$ModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                    & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                    & grepl("^e", vidData$SubModelName, ignore.case = TRUE)] <- paste0(vidData$ModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                                                                                                         & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                                                                                                         & grepl("^e", vidData$SubModelName, ignore.case = TRUE)], vidData$SubModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                                                                                                                                                                                        & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                                                                                                                                                                                        & grepl("^e", vidData$SubModelName, ignore.case = TRUE)])

  vidData$ModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                    & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                    & grepl("^c", vidData$SubModelName, ignore.case = TRUE)] <- paste0(vidData$ModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                                                                                                         & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                                                                                                         & grepl("^c", vidData$SubModelName, ignore.case = TRUE)], vidData$SubModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                                                                                                                                                                                        & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                                                                                                                                                                                        & grepl("^c", vidData$SubModelName, ignore.case = TRUE)])
  vidData$ModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                    & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                    & grepl("^x", vidData$SubModelName, ignore.case = TRUE)] <- paste0(vidData$ModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                                                                                                         & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                                                                                                         & grepl("^x", vidData$SubModelName, ignore.case = TRUE)], vidData$SubModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                                                                                                                                                                                        & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                                                                                                                                                                                        & grepl("^x", vidData$SubModelName, ignore.case = TRUE)])



  vidData$SubModelName[grepl("BMW", vidData$MakeName, ignore.case = TRUE)
                       & grepl("^[0-9]", vidData$ModelName, ignore.case = TRUE)
                       & (grepl("^i", vidData$SubModelName, ignore.case = TRUE) | grepl("^c", vidData$SubModelName, ignore.case = TRUE) | grepl("^x", vidData$SubModelName, ignore.case = TRUE))] <- "Base"

  #combine Mercedes-Benz models and submodels for better matching
  vidData$ModelName[grepl("Mercedes-Benz", vidData$MakeName, ignore.case = TRUE)
                    & vidData$YearName >= 1970
                    & !grepl("^SWB$", vidData$SubModelName, ignore.case = FALSE)
                    & !grepl("^LWB$", vidData$SubModelName, ignore.case = FALSE)
                    & !grepl("Pullman$", vidData$SubModelName, ignore.case = FALSE)
                    & !grepl("Lang$", vidData$SubModelName, ignore.case = FALSE)
                    & !grepl("^[0-9][.]", vidData$SubModelName, ignore.case = FALSE)] <- paste0(vidData$ModelName[grepl("Mercedes-Benz", vidData$MakeName, ignore.case = TRUE)
                                                                                                                  & vidData$YearName >= 1970
                                                                                                                  & !grepl("^SWB$", vidData$SubModelName, ignore.case = FALSE)
                                                                                                                  & !grepl("^LWB$", vidData$SubModelName, ignore.case = FALSE)
                                                                                                                  & !grepl("Pullman$", vidData$SubModelName, ignore.case = FALSE)
                                                                                                                  & !grepl("Lang$", vidData$SubModelName, ignore.case = FALSE)
                                                                                                                  & !grepl("^[0-9][.]", vidData$SubModelName, ignore.case = FALSE)], vidData$SubModelName[grepl("Mercedes-Benz", vidData$MakeName, ignore.case = TRUE)
                                                                                                                                                                                                          & vidData$YearName >= 1970
                                                                                                                                                                                                          & !grepl("^SWB$", vidData$SubModelName, ignore.case = FALSE)
                                                                                                                                                                                                          & !grepl("^LWB$", vidData$SubModelName, ignore.case = FALSE)
                                                                                                                                                                                                          & !grepl("Pullman$", vidData$SubModelName, ignore.case = FALSE)
                                                                                                                                                                                                          & !grepl("Lang$", vidData$SubModelName, ignore.case = FALSE)
                                                                                                                                                                                                          & !grepl("^[0-9][.]", vidData$SubModelName, ignore.case = FALSE)])



  vidData$SubModelName[grepl("Mercedes-Benz", vidData$MakeName, ignore.case = TRUE)
                       & vidData$YearName >= 1970
                       & !grepl("WB$", vidData$SubModelName, ignore.case = FALSE)
                       & !grepl("Pullman$", vidData$SubModelName, ignore.case = FALSE)
                       & !grepl("Lang$", vidData$SubModelName, ignore.case = FALSE)
                       & !grepl("^[0-9][.]", vidData$SubModelName, ignore.case = FALSE)] <- "Base"

  vidData <- setVehicleString(vidData, c("YearName", "MakeName", "ModelName"))
  vidData <- setVehicleString(vidData, c("YearName", "MakeName", "ModelName", "SubModelName", "BodyTypeName"))

  return(vidData)

}
