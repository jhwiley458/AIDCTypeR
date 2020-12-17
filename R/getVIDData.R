
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

  vidData <- setVehicleString(vidData, c("YearName", "MakeName", "ModelName"))
  vidData <- setVehicleString(vidData, c("YearName", "MakeName", "ModelName", "SubModelName", "BodyTypeName"))

  return(vidData)

}
