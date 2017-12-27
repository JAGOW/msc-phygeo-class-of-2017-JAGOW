# --- gi-ws-07-1 example control script FUSION/lastool
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
plotIt <- FALSE

#--> create full rootDir
rootDir<-paste0(projDir,rootDir)
#--> make a list of all functions in the corresponding function folder and source these functions
res<- sapply(list.files(pattern="[.]R$",path=paste0(rootDir,"/fun"),full.names=TRUE),FUN=source)


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

#--> dirty workaround for platform indepentend  use of fusion
source(paste0(fun,"controlFusion.txt"))

#--> list of height pairs as used for slicing the las data
zrList <- list(c(0.5,5,10,15,20,50))
zrange<- makenames(zrList)[[2]]
zrnames<- makenames(zrList)[[1]]

# target grid size
gridsize <- 10

# for assignment of projection
proj4 = "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

### ---------------------------- here we go ----------------------------



#--> do it the Fusion way
# NOTE the fusion algorithm is completly coded in the function fu_sliceRas(),GroundSurfaceCreate,densityMetrics,
#densityMetrics2asci,readAscGridMetrics. They are all in the fusionTools script.
#      they return path-lists to raster files with the necessary data for FHD index calculation  
#      you may change manually the paramList, only the target gridsize is automatically implemented

# call fusion based slicing funktion
density<-fu_sliceRas(lasFiles = lasfiles,
                     zrange = zrange, 
                     zrnames = zrnames,  
                     paramList = c("10 M M 1 32 0 0 "),
                     res = gridsize)


# call fusion based metrics for Median (31) and Max(7) according to Fusion Manual p.78/79
gridMet<- GridMetrics(lasFiles = lasfiles, 
                      res = gridsize, 
                      heightClassList = unlist(zrList), 
                      metrics = c(7,31), 
                      heightbreak=0.2 )


fhd<-list()
vdr<-list()
for (i in 1:length(gridMet)){
  cat(i)
  #--> FHD using the fun_fhd function  provided in diversityindeces.R
  fhd[[i]]<- fun_fhd(unlist(density[[i]]))
  g<-gridMet[[i]] 
  g[is.na(g[])] <- 0 
  g[is.infinite(g[])] <- 0 
  #--> FHD using the fun_fhd function  provided in diversityindeces.R
  vdr[[i]]<-fun_vdr(g[[1]],g[[2]])
  if (plotIt){
    plot(fhd[[i]],  col=rev(heat.colors(10)),main="FHD Index")
    plot(vdr[[i]],  col=rev(heat.colors(10)),main="VDR Index")
    # mapview::mapview(fhd[[j]],
    #                  col.regions = rev(heat.colors(10)), 
    #                  legend = TRUE,
    #                  alpha.regions = 0.3,
    #                  layer.name="FHD Index") 
    
  }
  
  
}


# create names according to inputdata and methods/ranges
#mn<-paste( unlist(statList), collapse='_')
zrn<-paste( unlist(zrList), collapse='_')
lsf<-paste( unlist(tools::file_path_sans_ext(lasfiles)), collapse='_')
# save the results
save(zrSlices,file = paste0(gi_output,"fu_",zrn,lsf,".RData"))
#save(statLayer,file = paste0(gi_output,"fu_",zrn,mn,".RData"))
# save the results
save(fhd,file = paste0(gi_output,"fu_",zrn,lsf,"_fhd",".RData"))
save(vdr,file = paste0(gi_output,zrn,mn,"_vdr",".RData"))  


