#'@name lasR
#'@title Using a bit of lastools from R
#'
#'@description
#' using lasLib from R
#'
#'@author Chris Reudenbach
#'@param tool default is \code[lasinfo]   additionally xou may choose las2las, lasmerge, lasground_new, las2dem, las2txt
#'@param lasDir  default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbase_path default is \code{NULL} root directory of the project. NOTE the function creates two subfolder named \code{run} and \code{output}
#'@param grid_size  resolution of the DTM raster
#'@param path_lastools directory for the windows lastools
#'@param thin_with_grid default 0.5 meter. Grid stepsize for data thinning 
#'@param keep_class default is 2. Default ground class of las/laz conform data 
#'@param bulge  default is 1.5. 'A parameter to filter spikes it is set to a step_size/10 and then clamped into the range from 1.0 to 2.0
#'@param step_size  default is 25 meter. LAStools key words if \code{city},\code{town},\code{metro},\code{nature},\code{wilderness} or experiment with free values
#'@param sub_size = "8", default is 8 meter. LAStools key words if \code{extra_coarse},\code{coarse},\code{fine},\code{extra_fine},\code{ultra_fine},\code{hyper_fine} or experiment with free values
#'@param cores number of cores that will be used
#'@param proj4  default is EPSG 32632 any valid proj4 string that is assumingly the correct one

#'@return lasR basically returns a DTM
#'
#'
#'@export lasR
#'
#'@examples 
#'\dontrun{
#' lasR(lasDir =  "~/path/to/lasdata",
#'        gisdbase_path = "~/temp5",
#'        thin_with_grid = "0.5",
#'        level_max = "5" ,
#'        grid_size = "0.5")
#'}

lasTool <- function(  tool="lasinfo",
                      lasDir = NULL,
                      thin_with_grid = "1.0",
                      keep_class = "2",
                      bulge = "1.5",
                      step_size = "city",
                      sub_size = "ultra_fine",
                      grid_size = "1.0", 
                      proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
                      cores = "2") {
  path_fun<- fun
  path_run<- gi_run
  path_input<- gi_input
  path_output<- gi_output
  
  
  # some basic checks 
  if (is.null(lasDir)) stop("no directory containing las/laz files provided...\n")
  lasDir <- path.expand(lasDir)
  
  # if not provided take the onboard laslib
  
    if (Sys.info()["sysname"] == "Windows") {
      cmd <- paste0(path_fun, "LAStools", .Platform$file.sep)
    } else {
      cmd <- paste0("wine ", path_fun, "LAStools", .Platform$file.sep)
    }
  
  
  # create cmd strings
  lasinfo       <- paste(cmd,"lasinfo-cli.exe",sep = "/")
  las2las       <- paste(cmd,"las2las-cli.exe",sep = "/")
  lasmerge      <- paste(cmd,"lasmerge-cli.exe",sep = "/")
  lasground_new <- paste(cmd,"lasground_new-cli.exe",sep = "/")
  las2dem       <- paste(cmd,"las2dem-cli.exe",sep = "/")
  las2txt       <- paste(cmd,"las2txt-cli.exe",sep = "/")
  
  # check las / laz files laz will be preferred
  lasFileNames <- list.files(pattern = "[.]las$", path = lasDir, full.names = TRUE)
  lazFileNames <- list.files(pattern = "[.]laz$", path = lasDir, full.names = TRUE)
  #if (length(lazFileNames) > 0 ) extFN <- substr(extension(basename(lazFileNames[1])),2,4)
  #else if (length(lasFileNames) > 0) extFN <- substr(extension(basename(lasFileNames[1])),2,4)
  #else stop("no valid las or laz files found...\n")
  
  #sp_param <- uavRst:::getSpatialLASInfo(lasDir)
  
  # map the las code words
  if (step_size == "city") step <- "25"
  else if (step_size == "town") step <- "10"
  else if (step_size == "metro") step <- "50"
  else if (step_size == "nature") step <- "5"
  else if (step_size == "wilderness") step <- "3"
  if (sub_size == "extra_coarse") sub <- "3"
  else if (sub_size == "coarse") sub <- "4"
  else if (sub_size == "fine") sub <- "6"
  else if (sub_size == "extra_fine") sub <- "7"
  else if (sub_size == "ultra_fine") sub <- "8"
  else if (sub_size == "hyper_fine") sub <- "9"
  
  
  
  # merge all files
  if (tool == "lasmerge"){
    cat("\nNOTE: You are dealing with a huge UAV generated point cloud data set.\n      so take time and keep relaxed... :-)\n")
    
    ret <- system(paste0(lasmerge,
                         " -i ",lasDir,"/*.las",
                         " -olas",
                         " -o ",path_run,"full_point_cloud.las"),
                  intern = TRUE, 
                  ignore.stderr = TRUE
    )
    # get extent of merged file  
    sp_param <- uavRst:::getSpatialLASInfo(lasinfo,paste0(path_run,"full_point_cloud.las"))
    
    # add proj4 string manually
    sp_param[5] <- proj4
  }
  
  if (tool=="las2las"){
    ### reduce data amount
    cat("\n:: converting laz to las..\n")
    ret <- system(paste0(las2las,
                         " -i ",lasDir,
#                         " -odix  ",
                         " -odir ",path_run,
                         " -o","las",
                         " -keep_class ",keep_class
#                         " -thin_with_grid ",thin_with_grid
), 
                  intern = TRUE, 
                  ignore.stderr = TRUE
    )
  }
  
  #### starting lastools classification 
  
  if (tool == "lasground"){ 
    cat(":: classify ground points (LAStools) ...\n")
    ret <- system(paste0(lasground_new,
                         " -i ",path_run,"out.",extFN,
                         " -all_returns ",
                         " -bulge ", bulge,
                         " -skip_files",
                         " -step ", step,
                         " -sub ", sub,
                         " -odix g -o",extFN,
                         " -cores ",cores),
                  intern = FALSE,
                  ignore.stderr = TRUE
    )
  }
  if (tool == "las2dem"){ 
    # create lastools  DTM
    ret <- system(paste0(las2dem,
                         " -i ",path_run,"*g.",extFN,
                         " -keep_class 2",
                         " -extra_pass",
                         " -step ",grid_size,
                         " -ocut 3 ",
                         " -odix _dtm ",
                         " -otif ",
                         " -odir ",path_output,
                         " -cores ",cores),
                  intern = TRUE, 
                  ignore.stderr = TRUE
    )
  }
  if (tool == "las2txt"){ 
    # export las file to text file
    ret <- system(paste0(las2txt,
                         " -i ",path_run,"*g.",extFN,
                         " -parse xyzrRGB",
                         " -sep komma"),
                  intern = TRUE, 
                  ignore.stderr = TRUE
    )
  }
  if (tool == "lasinfo"){  
    ret <- system(paste0(lasinfo,
                         " -i ",lasDir,
                         " -no_check  -stdout"),intern = TRUE)
    paste0("wine ",fun,"LASTools/lasinfo-cli.exe ")
    spatial_params<- list() 
    
    tmp <- grep(pattern = "min x y z", ret, value = TRUE)
    tmp <- unlist(strsplit(tmp, ":"))
    tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
    spatial_params[1] <- tmp[1]
    spatial_params[2] <- tmp[2]
    tmp <- grep(pattern = "max x y z", ret, value = TRUE)
    tmp <- unlist(strsplit(tmp, ":"))
    tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
    spatial_params[3] <- tmp[1]
    spatial_params[4] <- tmp[2]
    #spatial_params[5] <- grep(pattern = "+proj", ret, value = TRUE)
    
    return(unlist(spatial_params))}
  
  
}

getSpatialLASInfo <- function(lasinfo,lasFN){
  
  ret <- system(paste0(lasinfo,
                       " -i ",lasFN,
                       " -no_check  -stdout"),intern = TRUE)
  paste0("wine ",fun,"LASTools/lasinfo-cli.exe ")
  spatial_params<- list() 
  
  tmp <- grep(pattern = "min x y z", ret, value = TRUE)
  tmp <- unlist(strsplit(tmp, ":"))
  tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
  spatial_params[1] <- tmp[1]
  spatial_params[2] <- tmp[2]
  tmp <- grep(pattern = "max x y z", ret, value = TRUE)
  tmp <- unlist(strsplit(tmp, ":"))
  tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
  spatial_params[3] <- tmp[1]
  spatial_params[4] <- tmp[2]
  #spatial_params[5] <- grep(pattern = "+proj", ret, value = TRUE)
  
  return(unlist(spatial_params))
}

rescaleLas<- function(lasinfo,lasFN){
ret <- system(paste0("wine ",fun,"LASTools/las2las-cli.exe ",
                     " -i ",lasFN,
                     "-rescale 0.01 0.01 0.01 ", 
                     "-auto_reoffset " ,
                     "-o ", lasFN,"_fixed.laz " ), 
              intern = TRUE, 
              ignore.stderr = TRUE
)}