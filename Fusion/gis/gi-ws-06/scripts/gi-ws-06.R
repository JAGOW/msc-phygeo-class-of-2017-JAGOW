# gi-ws-06 example control script 
# MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#
# setup working environment 
# provides LiDAR functionality 
# calculate some basic diversity indices
# see also: https://github.com/logmoc/msc-phygeo-class-of-2017-creuden

######### setup the environment -----------------------------------------------
# library requirements
require(link2GI)
require(gdalUtils)
require(raster)


# define project folder

filepath_base<-"~/lehre/msc/active/msc-2017/msc-phygeo-class-of-2017-creuden/"

# define the actual course session
activeSession<-6

# define the used input file(s)
inputFile<- "geonode-lidar_dem_01m.tif"

# make a list of all functions in the corresponding function folder
sourceFileNames <- list.files(pattern="[.]R$", path=paste0(filepath_base,"fun"), full.names=TRUE)

# source all functions
res<- sapply(sourceFileNames, FUN=source)

# if at a new location create filestructure
createMocFolders(filepath_base,ccourse = "gi",csess = activeSession)

# get the global path variables for the current session
getSessionPathes(filepath_git = filepath_base, sessNo = activeSession,courseCode = "gi")

# set working directory
setwd(pd_gi_run)

######### set extra vars ------------------------------------------------------------
## Fusion binary folder
Fusion<-"wine /home/creu/.wine/dosdevices/c:/FUSION/"


######### initialize the external GIS packages --------------------------------

# check GDAL binaries and start gdalUtils
gdal<- link2GI::linkgdalUtils()

link2GI::linkSAGA(MP = "~/")

######### START of the thematic stuff ----------------------------------------

# Create a list containing the las files in the input folder
las_files<-list.files(pd_gi_input, pattern=".las$", full.names=TRUE,recursive = TRUE) 

cmd <- "wine \home\creu\.wine\dosdevices\c:\FUSION\catalog.exe "
if (Sys.info()["sysname"] == "Linux"){
  file.copy(las_files,pd_gi_run)
  pi<-pd_gi_input
  pr<-pd_gi_run
  p0<-pd_gi_output
  pd_gi_input<-""
  pd_gi_run <- ""
  pd_gi_output <-""
  
}

######### start core script     -----------------------------------------------

### read data 

# Most Fusion tools accept a .txt with a list of files that should be processed   


# write the list into a ASCII file (.txt) 
res<-lapply(las_files, write,paste0(pd_gi_run,"lidar_files.txt"), append=T)

# retrieve meta data from the files using fusion catalog
#cmd<-"wine /home/creu/.wine/dosdevices/c:/FUSION/catalog.exe  lidar_files.txt info_caldern.html"


system(paste(paste0(Fusion, "catalog.exe "),paste0(pd_gi_run, "lidar_files.txt") , paste0(pd_gi_output, "info_caldern.html")))


# Create a .las with groundpoints only 
system(paste0(Fusion, "clipdata.exe"," /class:2 ", pd_gi_input, "lidar_files.txt ", pd_gi_output, "classified_GroundPts.las ", "476000 5630000 479000 5633000")) 

# Create the required PLANS DTM format 
system(paste0(Fusion, "gridsurfacecreate.exe ", pd_gi_input, "caldern_GridSurf.dtm ", "1 M M 1 32 0 0 ",pd_gi_output, "classified_GroundPts.las"))

# normalize heights of las point cloud 
system(paste0(Fusion, "clipdata.exe", " /height /dtm:",mainDir, outDir,"caldern_GridSurf.dtm ", mainDir, inDir, "lidar_files.txt ", mainDir, outDir, "caldern_normalized_point_cloud_LIDAR.las ", "476000 5630000 479000 5633000")) 

# create slice between zmin: 0 and zmax:2# 
system(paste0(Fusion, "clipdata.exe"," /zmin:0 /zmax:2 ", mainDir, outDir, "caldern_normalized_point_cloud_LIDAR.las ", mainDir, outDir, "caldern_normalized_point_cloud_LIDAR_zmin0_zmax2.las ", "476000 5630000 479000 5633000"))

#create point count for 1m pixel raster in PLANE dtm
system(paste0(Fusion, "returndensity.exe ",mainDir,outDir, "point_count_all_point_cloud_LIDAR_zmin0_zmax_2_1m.dtm ", "1 ", mainDir, outDir, "caldern_normalized_point_cloud_LIDAR_zmin0_zmax2.las"))

#convert to ascii 
system(paste0(Fusion, "dtm2ascii.exe ",mainDir,outDir, "point_count_all_point_cloud_LIDAR_zmin0_zmax_2_1m.dtm ", mainDir,outDir, "point_count_all_point_cloud_LIDAR_zmin0_zmax_2_1m.asc"))


