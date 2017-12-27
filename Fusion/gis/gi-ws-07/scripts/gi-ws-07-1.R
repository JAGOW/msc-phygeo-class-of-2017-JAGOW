# --- gi-ws-07-1 example control script GRASS/lastool
# --- MOC - Advanced GIS 
# --- setup working environment 
# --- setup basic GI API links
# --- provides some LiDAR functionality 
# --- calculate some basic biodiversity indices
#
#   Things to do  
#      - please check and realize the source code
#      - check results against FUSION approach
#      - implement one more diversity index and calculate the 
#        
#
#  NOTE  It may occure that generic windows tools like fusion or lastools 
#        will NOT run under Windows most reasonable because I just tested 
#        the wine emulation on Linux platforms...
#
#        Whenever you run in trouble open an issue
#        https://github.com/logmoc/msc-phygeo-class-of-2017-creuden/issues
#        describe the problem with source code and add your sessionInfo() output
#
# --- codebase: https://github.com/logmoc/msc-phygeo-class-of-2017-creuden
# 
#
### -------------------------- setup the environment --------------------------
# --- Basic idea is to set up a static working environment starting a an 
# --- arbitray point in your folder structure called "projDir". Additionally
# --- you need to provide a so called "rootDir" which should be an subfolder 
# --- of the "projDir" folder. 
# --- Within the classes you have to provide the "courseCode" (gi,rs,da) and the 
# --- "activeSessionFolder"  wich is the number of the current class session
# --- 
# --- That's it

#--> library requirements
devtools::install_github("gisma/link2GI", ref = "master", dependencies = TRUE)
library(link2GI)
require(gdalUtils)
require(rgrass7)
require(raster)
require(mapview)

#--> NOTE point to whereever you want but avoid strange letters as dots etc
#--> the ~ is a substitute for the system variable HOME
#--> projDir is general project folder  basic folder eg. C:/Dokumente/1_semester_MSCGEO/GIS/
projDir<-"~/lehre/msc/active/msc-2017/"
#-->  rootFolder of the github repository 
rootDir<-"msc-phygeo-class-of-2017-creuden"


#--> current class
courseCode<-"gi"
#--> current class session folder
activeSessionFolder<-7

#--> create plots
plotIt <- T

#--> optionally convert laz to las
laz<-FALSE

### ------------------------- end basic pathes and settings -------------------
### -------- that means usually there is no need to change the next section ---
### ---------

### ------------------------- setup entvironmentt -------------------

#--> create full rootDir
rootDir<-paste0(projDir,rootDir)
#--> make a list of all functions in the corresponding function folder and source these functions
res<- sapply(list.files(pattern="[.]R$",path=paste0(rootDir,"/fun"),full.names=TRUE),FUN=source)

#--> set working directory (just if you missed using golbal path variables as a backup)
setwd(gi_run)
if (laz){
  lazfiles<-list.files(gi_input, pattern=".laz$", full.names=TRUE,recursive = TRUE) 
  lasTool(  tool="las2las",dirname(lazfiles)[1])
}

### ------------------------- end environment setup --------------------------
### ------------------
### ------------------
### ---------------------------- Thematic  Settings ----------------------------
# Before starting it makes sense to focus the goal:
#   - we want to calculate some diversity indices based on LiDAR data
#   - we need to identify the indices lets say FHD and VDR
#       * what kind of information as derived by the data do we need?
# + we need to strip the terrain altitudes i.e. normalize/reduce the point cloud data 
#         + FHD all returns and a defined number of horizontally sliced returns
#         + VDR max and median returnhttps://www.sensor-swarm.com/s
#       * what is technically required?
#         + we need to deal with a bunch of data files in a row  
#         + we might deal with more indices so it would fine to split preprocessing from calulation
#
#--> Create a list containing the las files in the input folder
lasfiles<-list.files(paste0(gi_input),pattern=".las$", full.names=FALSE) 


#--> list of height pairs as used for slicing the las data
zrList <- list(c(0,5,10,15,20,50))
zrange<- makenames(zrList)[[2]]
zrnames<- makenames(zrList)[[1]]



#--> list of statistic analysis 
# n: Number of points in cell 
# min: Minimum value of point values in cell 
# max: Maximum value of point values in cell 
# range: Range of point values in cell 
# sum: Sum of point values in cell 
# mean: Mean (average) value of point values in cell 
# stddev: Standard deviation of point values in cell 
# variance: Variance of point values in cell 
# coeff_var: Coefficient of variance of point values in cell 
# median: Median value of point values in cell 
# percentile: pth (nth) percentile of point values in cell 
# skewness: Skewness of point values in cell 
# trimmean: Trimmed mean of point values in cell 
statList <- list("max", "median","range")

# target grid sizec(0,5,10,15,20,50)
gridsize <- 10

proj4="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
### ---------------------------- here we go ----------------------------





#--> do it the GRASS way
# NOTE the used GRASS modules are wrapped using the rgrass7. they can be found as single functions 
#      in the file grassLiDAR.R file. The control structure is written below as a mixture of function 
#      calls and generic R code
#      
#--> basic calculation of DEM from las data set 
# standard approach would be to use the class2 values and then interppolate them to a DEM 
# second best simple methods is to use the "last return" filter
zrSlices<-list()
statLayer<-list()
fhd<-list()
vdr<-list()
for (j in 1:(length(lasfiles))) {
  # create for each las file a temporyry GRASS location
  ext<-lasTool(lasDir = paste0(gi_input, lasfiles[j]))
  result<-link2GI::linkGRASS7(spatial_params = c(ext[2],ext[1],ext[4],ext[3],proj4),resolution = gridsize)
  # create pseudo dem 
  r_in_lidar(input = paste0(gi_input,lasfiles[j]), 
             output = paste0(j, "_dem"),
             method = "min",
             resolution = gridsize,
             class_filter = 2,
             flags = c("e","n","overwrite","o","v"))

  # for each las file slice it to according to zrList
  for ( i in 1:(length(zrnames))) {
    #--> slice the data horizontally according to the provide gi_input = NULL, gi_input = NULL,d heightClassList
    #      Reduction to a base raster is applied
    r_in_lidar(input = paste0(gi_input,lasfiles[j]), 
               output = zrnames[i],
               method = "n",
               base_raster = paste0(j, "_dem"),
               zrange = c(zrange[[i]][1],zrange[[i]][2]),
               resolution=gridsize,
               flags = c("e","n","overwrite","o","v")
    )
  }
  
  # for each las file calculate some statistics
  for (meth in statList ){
    r_in_lidar(input = paste0(gi_input,lasfiles[j]), 
               output = paste0(meth,"_veg"),
               method = meth,
               base_raster = paste0(j, "_dem"),
               resolution = gridsize,
               flags = c("e","n","overwrite","o","v")
    )}

  # for each las file import data to R
  
  #-->  read and stack the data according to the GRASS file names as generated by the heightClassList 
  # using an lapply approach which iterates over the list and returns a list of raster wich is stacked 
  
  zrSlices[[j]] <- stack(lapply(zrnames ,
                              function(x){raster::raster(rgrass7::readRAST(x))
                              }
                     ))
  
  statLayer[[j]] <- stack(lapply(statList ,
                            function(x){raster::raster(rgrass7::readRAST(paste0(x,"_veg"),NODATA=-9999))
                            }
                     ))
  

  
  #  each lasfile Calculate indices
  #--> FHD using the fun_fhd function  provided in diversityindeces.R
  fhd[[j]]<- fun_fhd(zrSlices[[j]])
  
  #--> FHD using the fun_fhd function  provided in diversityindeces.R
  vdr[[j]]<- fun_vdr(statLayer[[j]][[1]],statLayer[[j]][[2]])
  

  
  # if plot is true plot them
  if (plotIt){
    plot(zrSlices[[j]])
    plot(statLayer[[j]])
    plot(fhd[[j]],  col=rev(heat.colors(10)),main="FHD Index")
    plot(vdr[[j]],  col=rev(heat.colors(10)),main="VDR Index")
    # mapview::mapview(fhd[[j]],
    #                  col.regions = rev(heat.colors(10)), 
    #                  legend = TRUE,
    #                  alpha.regions = 0.3,
    #                  layer.name="FHD Index") 
    # mapview::mapview(vdr[[j]],
    #                  col.regions = rev(heat.colors(10)), 
    #                  legend = TRUE,
    #                  alpha.regions = 0.3,
    #                  layer.name="VDR Index") 
  }
  
  
  # create names according to inputdata and methods/ranges
  mn<-paste( unlist(statList), collapse='_')
  zrn<-paste( unlist(zrList), collapse='_')
  lsf<-paste( unlist(tools::file_path_sans_ext(lasfiles)), collapse='_')
  # save the results
  save(zrSlices,file = paste0(gi_output,zrn,lsf,".RData"))
  save(statLayer,file = paste0(gi_output,zrn,mn,".RData"))
  # save the results
  save(fhd,file = paste0(gi_output,zrn,lsf,"_fhd",".RData"))
  save(vdr,file = paste0(gi_output,zrn,mn,"_vdr",".RData"))
}


