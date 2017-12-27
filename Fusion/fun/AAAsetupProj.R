# rs-ws-05-1
#' @description  MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#' getSessionPathes
#'@return 
#' defines and creates (if necessary) all folders variables
#' set the SAGA path variables and other system variables
#' exports all variables to the global environment
#'
#'@param filepath_git  project github root directory (your github name)
#'@param csess= current session "01",
#'@param courseCode current course options are "gi", "rs", "da" you may only use one course per session
#'@param moc=TRUE creates a folder structure according to the needs of the MOC courses, FALSE creates a simple project structure
#'@param dataWorkingFolder default c("/scripts/", "/rmds/"),
#'@param sessionWorkingFolder default ("RData/","temp/","run/","input/","output/"))
#'
#'\preformatted{
#'   If moc=TRUE the following folderstructure is exported. If folders do not exist thesy will be created.
#'.
#'├── data
#'│   ├── data_analysis
#'│   │   ├── csv
#'│   │   └── raw
#'│   ├── gis
#'│   │   ├── input
#'│   │   ├── output
#'│   │   ├── RData
#'│   │   ├── run
#'│   │   └── temp
#'│   └── remote_sensing
#'│       ├── aerial
#'│       ├── aerial_croped
#'│       ├── aerial_merged
#'│       ├── input
#'│       ├── RData
#'│       ├── run
#'│       └── temp
#'└── MOC
#'    ├── data_analysis
#'    │   └── da-ws-01
#'    │       └── rmds
#'    │       └── scripts
#'    ├── fun
#'    ├── gis
#'    │   └── gi-ws-01
#'    │       └── rmds
#'    │       └── scripts
#'    └── remote_sensing
#'        └── rs-ws-01
#'    │       └── rmds
#'            └── scripts
#'   } 
#'
#'@author Thomas Nauss, Chris Reudenbach
#'
#'@return  getSessionPathes< creates if necessary the directories and export the corresponding pathes as global variables\cr

getSessionPathes<- function(filepath_git,
                            filepath_data,
                            sessNo=1,
                            courseCode="gi",
                            sessionWorkingFolder=c("/scripts/", "/rmds/"),
                            dataWorkingFolder=c("RData/","temp/","run/","input/","output/")) {
  
  # switch backslash to slash and expand path to full path
  filepath_git<-gsub("\\\\", "/", path.expand(filepath_git))  
  filepath_data<-gsub("\\\\", "/", path.expand(filepath_data))  
  # check  tailing / and if not existing append
  if (substr(filepath_git,nchar(filepath_git),nchar(filepath_git)) != "/") {
    filepath_git<-paste0(filepath_git,"/")
  }
  if (substr(filepath_data,nchar(filepath_data),nchar(filepath_data)) != "/") {
    filepath_data<-paste0(filepath_data,"/")
  }
  
  # script and function folder for each course session can be adapted 
  sessionWorkingFolder<-sessionWorkingFolder
  # currently implemented data folders can be adapted 
  dataWorkingFolder<-dataWorkingFolder
                       
  

    # static course structure - better keep the below folders
    proj_root_git<-c(path.expand(filepath_git))
    proj_root_data<-filepath_data
    #proj_root_data<-paste0(substr(proj_root_git,1,gregexpr(pattern ='/',proj_root_git)[[1]][as.numeric(lengths(gregexpr(pattern ='/',proj_root_git))[[1]]-2)]),"data/")
    
    if (courseCode == "rs") {
    sub_root<-c("remote_sensing/")
    session_ID<-c("rs-ws-")
    
    } else if (courseCode == "gi") {
    sub_root<-c("gis/")
    session_ID<-c("gi-ws-")
    
    } else if  (courseCode == "da") {
    sub_root<-c("data_analysis/")
    session_ID<-c("da-ws-")
    
    }
    # create sessionstring
    
    session_number<- sapply(sessNo, function(no){
      if (no<10) {no<-paste0("0",no)}
      return(no)
    })
    
    # create folder and varibales 
    # function folder for all courses
    name<-paste0("fun")
    value<-paste0(filepath_git,"fun/")
    makGlobalVar(name, value)
    # and the rest
      
    for (i in 1:length(proj_root_git)) {
#      for (j in 1:length(sub_root)) {
        #for (k in 1:length(session_ID)) {
          for (l in 1:length(session_number)) {
            for (m in 1:length(sessionWorkingFolder)) {
              name<-paste0( substr(session_ID,1,2),"_",as.character(gsub("/", "", session_number[l])),"_",as.character(gsub("/", "",sessionWorkingFolder[m])))
              value<- paste0(proj_root_git[i],sub_root,session_ID,session_number[l],sessionWorkingFolder[m])
               makGlobalVar(name, value)
              }
            }
#          }
#        }sub_root<-3
      }
    
    
    # data structure NOTE it is outside the proj_root_git folder
    for (i in 1:length(proj_root_data)){
#      for (j in 1:length(sub_root)) {
        for (k in 1:length(dataWorkingFolder)) {
          name<-paste0(substr(session_ID,1,2),"_",as.character(gsub("/", "",dataWorkingFolder[k]))) #add prefix
          value<- paste0(proj_root_data[i],sub_root,dataWorkingFolder[k])
           makGlobalVar(name, value)
          if (courseCode==substr(session_ID,1,2) && dataWorkingFolder[k]=="run/"){
            path_temp<- value
          }
        }
#      }
  } # end of moc=TRUE

}

#'@title Generates a variable with a certain value in the R environment
#'@name makGlobalVar
#' @description  Generates a variable with a certain value in the R environment
#' @param name character string name of the variable
#' @param value character string value of the variable
#'@export makGlobalVar 
#'@examples
#' \dontrun{
#'
#' # creates the global var \code{pathToData} with the value \code{~/home/data}
#' makGlobalVar("pathToData","~/home/data") 
#' 
#' }
#' 
makGlobalVar <- function(name,value) {
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv())  
  if (exists(name, envir = GiEnv)) {
    #warning(paste0("The variable '", name,"' already exist in .GlobalEnv"))
    assign(name, value, envir = GiEnv, inherits = TRUE)
    #cat("add variable ",name,"=",value," to global GiEnv\n")
  } else {
    assign(name, value, envir = GiEnv, inherits = TRUE)
    #cat("add variable ",name,"=",value," to global GiEnv\n")
  } 
}

# rs-ws-05-1
#' @description  MOC - Advanced GIS (T. Nauss, C. Reudenbach)
#' createMocFolders
#'@return 
#' defines and creates (if necessary) all folders variables
#' set the SAGA path variables and other system variables
#' exports all variables to the global environment
#'
#'@param filepath_git  project github root directory (your github name)
#'@param csess= current session "01",
#'@param ccourse current course options are "gi", "rs", "da"
#'@param moc=TRUE creates a folder structure according to the needs of the MOC courses, FALSE creates a simple project structure
#'@param moclist = 17 defines the  folder structure
#'\preformatted{
#'   If moc=TRUE the following folderstructure is exported. If folders do not exist thesy will be created.
#'.
#'├── data
#'│   ├── data_analysis
#'│   │   ├── csv
#'│   │   └── raw
#'│   ├── gis
#'│   │   ├── input
#'│   │   ├── output
#'│   │   ├── RData
#'│   │   ├── run
#'│   │   └── temp
#'│   └── remote_sensing
#'│       ├── aerial
#'│       ├── aerial_croped
#'│       ├── aerial_merged
#'│       ├── input
#'│       ├── RData
#'│       ├── run
#'│       └── temp
#'└── MOC
#'    ├── data_analysis
#'    │   └── da-ws-01
#'    │       └── rmds
#'    │       └── scripts
#'    ├── fun
#'    ├── gis
#'    │   └── gi-ws-01
#'    │       └── rmds
#'    │       └── scripts
#'    └── remote_sensing
#'        └── rs-ws-01
#'    │       └── rmds
#'            └── scripts
#'
#' 
#' ############
#' 
#' if moc=FALSE
#' .
#' └── project1
#'     ├── control
#'     │   └── log
#'     ├── data
#'     │   ├── input
#'     │   └── output
#'     ├── run
#'     └── src
#'         └── fun
#'   } 
#'
#'@author Thomas Nauss, Chris Reudenbach
#'
#'@return  createMocFolders< creates if necessary the directories and export the corresponding pathes as global variables\cr

createMocFolders<- function(filepath_git,
                            csess=15,
                            ccourse="gi", 
                            moc=TRUE, 
                            moclist=17) {
  
  # switch backslash to slash and expand path to full path
  filepath_git<-gsub("\\\\", "/", path.expand(filepath_git))  
  
  # check  tailing / and if not existing append
  if (substr(filepath_git,nchar(filepath_git)-1,nchar(filepath_git)) != "/") {
    filepath_git<-paste0(filepath_git,"/")
  }
  
  ### moc = FALSE feel free to adapt 
  default_folders<- c(paste0(filepath_git,"src/"),
                      paste0(filepath_git,"src/fun/"),
                      paste0(filepath_git,"data/input/"),
                      paste0(filepath_git,"data/output/"),
                      paste0(filepath_git,"control/log/"),
                      paste0(filepath_git,"run/"))
  
  ### moc=TRUE
  # script and function folder for each course session can be adapted 
  session_working_folder<-c("/scripts/", "/rmds/")
  # currently implemented data folders can be adapted 
  if (moclist=="16")
  data_working_folder<-list(list("aerial/","aerial_merged/","aerial_croped/","RData/","temp/","run/","input/","filter/","aerial_rgbi/","aerial_classified/","otb_results","aerial_texture"),
                            list("RData/","temp/","run/","input/","output/"),
                            list("csv/","raw/"))  
  else if (moclist=="17")
    data_working_folder<-list(list("run/","input/","output/"),
                              list("run/","input/","output/","GRASS7"),
                              list("data","csv/","raw/"))  
  if (moc) {
    # static course structure - better keep the below folders
    proj_root_git<-c(path.expand(filepath_git))
    proj_root_data<-paste0(substr(proj_root_git,1,gregexpr(pattern ='/',proj_root_git)[[1]][as.numeric(lengths(gregexpr(pattern ='/',proj_root_git))[[1]]-2)]),"data/")
    sub_root<-c("remote_sensing/","gis/","data_analysis/")
    session_ID<-c("rs-ws-","gi-ws-","da-ws-")
    
    # create sessionstring
    ns<-1:csess
    session_number<- sapply(ns, function(ns){
      if (ns<10) {ns<-paste0("0",ns)}
      return(ns)
    })
    
    # create folder and varibales 
    # function folder for all courses
    # and the rest
    if (!file.exists(file.path(paste0(filepath_git,"/fun/")))) {
      dir.create(file.path(paste0(filepath_git,"/fun/")), recursive = TRUE)
    }  
    for (i in 1:length(proj_root_git)) {
      for (j in 1:length(sub_root)) {
        for (k in 1:length(session_ID)) {
          for (l in 1:length(session_number)) {
            for (m in 1:length(session_working_folder)) {
              if (!file.exists(file.path(paste0(proj_root_git[i],sub_root[j],session_ID[j],session_number[l],session_working_folder[m])))) {
                dir.create(file.path(paste0(proj_root_git[i],sub_root[j],session_ID[j],session_number[l],session_working_folder[m])), recursive = TRUE)
              }
            }
          }
        }
      }
    }
    
    # data structure NOTE it is outside the proj_root_git folder
    for (i in 1:length(proj_root_data)){
      for (j in 1:length(sub_root)) {
        for (k in 1:length(data_working_folder[[j]])) {
          if (ccourse==substr(session_ID[j],1,2) && data_working_folder[[j]][k]=="run/"){
          }
          if (!file.exists(file.path(paste0(proj_root_data[i],sub_root[j],data_working_folder[[j]][k])))) {
            dir.create(file.path(paste0(proj_root_data[i],sub_root[j],data_working_folder[[j]][k])), recursive = TRUE)
            
          }
        }
      }
    }
  } # end of moc=TRUE
  # create a default project structure
  else {
    # create directories if needed
    path_temp<-paste0(filepath_git,"run/")
    for(folder in default_folders){
      if (!file.exists(file.path(folder))) {
        dir.create(file.path(folder), recursive = TRUE)
      }
    }
  }
}

# create project structure and export global pathes
#--> create/add new folder(s) 
createMocFolders(rootDir, 
                 ccourse = "gi", 
                 csess = activeSessionFolder
)

#--> create *global* path variables for the current session (they won't work in doParallel or foreach)
getSessionPathes(filepath_git = rootDir, 
                 filepath_data = paste0(projDir,"data"),
                 sessNo = activeSessionFolder,
                 courseCode = courseCode,
                 dataWorkingFolder=c("run/","input/","output/","GRASS7/","GRASS7"))

# create GRASS7 link
#--> for a straightforward initialisation of GRASS you need to provide an referenced input file
# grass<- link2GI::linkGRASS7(x = paste0(gi_input,inputFile),
#                             gisdbase = paste0(projDir,"data/gis/GRASS7"), 
#                             location = "OFM")

# check GDAL binaries and start gdalUtils
gdal<- link2GI::linkgdalUtils()


