# gi-ws-06 example control script 
# MOC - Advanced GIS 
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
library(rgrass7)


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
ext <- "478000.0000,5624000.0000,478999.9900,5624999.9900"

######### initialize the external GIS packages --------------------------------

# check GDAL binaries and start gdalUtils
gdal<- link2GI::linkgdalUtils()
grass<- link2GI::linkGRASS7(x = paste0(pd_gi_input,inputFile,sep=.Platform$file.sep))




## tmp gisdbase and location



## dem function
dem_grass_lidar <- function(path, 
                            inFN, 
                            outFN,
                            grass_lidar_method,
                            res){
  
  ground_raster <- execGRASS("r.in.lidar",
                             input = paste0(path, inFN),
                             output = outFN,
                             flags = c("e", "n", "v", "overwrite","o"),
                             resolution = res,
                             method = grass_lidar_method,
                             class_filter = 2,
                             intern = TRUE,
                             ignore.stderr = TRUE)
  return(ground_raster)
}



## height classes function
heightClasses_grass_lidar <- function(path, 
                                      inFN, 
                                      outFN,
                                      grass_lidar_method,
                                      dem_base,
                                      height_class,
                                      res){
  
  heightClasses <- execGRASS("r.in.lidar",
                             input = paste0(path, inFN),
                             output = outFN,
                             flags = c("e", "n", "v", "overwrite","o"),
                             resolution = res,
                             method = grass_lidar_method,
                             base_raster = dem_base,
                             zrange = height_class,
                             intern = TRUE,
                             ignore.stderr = TRUE)
  return(heightClasses)
}



### calculate fhd index ###

## run dem function
dem_grass_lidar(path = pd_gi_run, 
                inFN = "U4745630.las", 
                outFN = "U4745630_dem",
                grass_lidar_method = "mean",
                res = 10)


## run heightClasses function
# list of height classes
classesList <- list(c(0,3), c(3,6), c(6,9), c(9,12), c(12,15), c(15,18), c(18,21), c(21,24),
                    c(24,27), c(27,30), c(0,30))

# run function with loop over height classes
for (i in classesList){
  heightClasses_grass_lidar(path = pd_gi_run, 
                            inFN = "U4745630.las", 
                            outFN = paste0("class",i[1],i[2]),
                            grass_lidar_method = "n",
                            dem_base = "U4745630_dem",
                            height_class = i,
                            res = 10)
}




## fhd index
# class nr 11 = sum of all classes (0-30m)
raster_classes <- readRAST(c("class03", "class36", "class69", "class912", "class1215", "class1518",
                             "class1821", "class2124","class2427", "class2730", "class030"))
classes_stack <- stack(raster_classes)

FHD_index <- overlay(classes_stack$class03, classes_stack$class36, classes_stack$class69,
                     classes_stack$class912, classes_stack$class1215, classes_stack$class1518,
                     classes_stack$class1821, classes_stack$class2124, classes_stack$class2427,
                     classes_stack$class2730,
                     
                     fun = function(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10){
                       return(abs(
                         (r1/classes_stack[[11]])*log(r1/classes_stack[[11]]) + 
                           (r2/classes_stack[[11]])*log(r2/classes_stack[[11]]) + 
                           (r3/classes_stack[[11]])*log(r3/classes_stack[[11]]) + 
                           (r4/classes_stack[[11]])*log(r4/classes_stack[[11]]) +
                           (r5/classes_stack[[11]])*log(r5/classes_stack[[11]]) + 
                           (r6/classes_stack[[11]])*log(r6/classes_stack[[11]]) +
                           (r7/classes_stack[[11]])*log(r7/classes_stack[[11]]) + 
                           (r8/classes_stack[[11]])*log(r8/classes_stack[[11]]) +
                           (r9/classes_stack[[11]])*log(r9/classes_stack[[11]]) + 
                           (r10/classes_stack[[11]])*log(r10/classes_stack[[11]])
                         
                       ))})