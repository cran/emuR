is_relativeFilePath<-function(nativeFilePathStr, forRunningPlatform=FALSE){
  if(forRunningPlatform){
    if(.Platform[['OS.type']]=='unix'){
      if(.Platform[['file.sep']]==substr(nativeFilePathStr,1,1)){
        # UNIX: "/dir/file"
        # absolute path
        return(FALSE)
      }
    }else if(.Platform[['OS.type']]=='windows'){
      #See http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
      if(substr(nativeFilePathStr,1,2)=='\\'){
        # fully qualified MS UNC path (is this supported with R?): \\samba\bla
        return(FALSE)
      }else if(grepl('^[A-Z,a-z][:]',nativeFilePathStr)){
        # fully qualified drive path: C:\Users\bla
        return(FALSE)
      }else if(.Platform[['file.sep']]==substr(nativeFilePathStr,1,1)){
        # Windows: "\dir\file"
        # absolute path
        return(FALSE)
      }
    }
  }else{
    if(grepl('^[A-Z,a-z][:]',nativeFilePathStr)){
      return(FALSE)
    }
    if(grepl('^[\\]',nativeFilePathStr)){
      return(FALSE)
    }
    if(grepl('^/',nativeFilePathStr)){
      return(FALSE)
    }
    
  }
  return(TRUE)
}

##' Import media files to emuDB
##' 
##' Import new recordings (media files) to emuDB and create bundles.
##' Looks for files with the defined mediafile extension of the emuDB 
##' (see \code{mediaFileExtension} in vignette \code{emuDB}) in \code{dir}
##' or in sub-directories thereof (interpreted as sessions), for each mediafile
##' create a bundle directory
##' named as the basename of the mediafile in the specified session, and copies 
##' the mediafile into the bundle. If not already present, adds 'OSCI' and 
##' 'SPEC' perspectives to the emuDB config file.
##'
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param dir directory containing mediafiles or session directories
##' @param targetSessionName name of session in which to create the new bundles 
##' @param verbose display infos & show progress bar
##' @import stringr
##' @keywords emuDB database Emu
##' @export
##' @examples
##' \dontrun{
##' ## Add mediafiles from directory
##' 
##'  import_mediaFiles(myEmuDB,dir="/data/mymedia/")
##' 
##' }
import_mediaFiles<-function(emuDBhandle,
                            dir,
                            targetSessionName = '0000', 
                            verbose = TRUE){
  check_emuDBhandle(emuDBhandle)
  dbCfg = load_DBconfig(emuDBhandle)
  if(is.null(dbCfg[['mediafileExtension']])){
    pattern = NULL
    #stop("The DB has no media file extension defined.")
  }else{
    pattern = paste0('.*[.]', dbCfg[['mediafileExtension']],'$')
  }
  if(!dir.exists(dir)){
    stop(dir, " doesn't exist!")
  }
  
  mfList = list.files(dir, pattern = pattern)
  if(length(mfList) == 0){
    stop("no media files found in ", dir)
  } else {
    # create session dir and session list object if required
    sessDir = file.path(emuDBhandle$basePath, 
                        paste0(targetSessionName, session.suffix))
    if(!file.exists(sessDir)){
      created = dir.create(sessDir)
      if(!created){
        stop("Couldn't create ", sessDir)
      }
    }
    
    qSessSql = paste0("SELECT * ",
                      "FROM session ",
                      "WHERE db_uuid='",emuDBhandle$UUID,"' ",
                      " AND name='",targetSessionName,"'")
    sessDf <- DBI::dbGetQuery(emuDBhandle$connection,qSessSql)
    if(nrow(sessDf) == 0){
      add_sessionDBI(emuDBhandle, sessionName = targetSessionName)
    }
    
  }
  mediaAdded = FALSE
  
  progress = 0
  if(verbose){
    cat("INFO: Importing ", length(mfList), " media files...\n")
    pb = utils::txtProgressBar(min = 0, 
                               max = length(mfList), 
                               initial = progress, 
                               style = 3)
    utils::setTxtProgressBar(pb, progress)
  }
  
  for(mf in mfList){
    mfFullPath = file.path(dir,mf)
    bundleName = sub('[.][^.]*$','',mf)
    
    bundleDir=file.path(sessDir,paste0(bundleName, bundle.dir.suffix))
    created = dir.create(bundleDir)
    if(!created){
      stop("Couldn't create ", bundleDir)
    }
    newMediaFileFullPath = file.path(bundleDir,mf)
    file.copy(from = mfFullPath, to = newMediaFileFullPath)
    
    pfAssp = wrassp::read.AsspDataObj(newMediaFileFullPath, 0, 4000)
    sampleRate = attr(pfAssp,'sampleRate')
    b = list(name = bundleName, 
             annotates = mf, 
             sampleRate = sampleRate, 
             levels = list(),
             links = list())
    
    # add empty levels
    for(ld in dbCfg[['levelDefinitions']]){
      b$levels[[length(b$levels) + 1]] = list(name=ld[['name']],
                                              type = ld[['type']],
                                              items = list())
    }
    
    # write to file
    annotJSONchar = jsonlite::toJSON(b, auto_unbox = TRUE, pretty = TRUE)
    newAnnotFileFullPath = file.path(bundleDir, 
                                     paste0(bundleName, bundle.annotation.suffix, ".json"))
    writeLines(annotJSONchar, newAnnotFileFullPath, useBytes = TRUE)
    
    # calculate MD5 sum of bundle annotJSON
    MD5annotJSON = tools::md5sum(newAnnotFileFullPath)
    
    add_bundleDBI(emuDBhandle, 
                  sessionName = targetSessionName, 
                  name = bundleName, 
                  annotates = mf, 
                  sampleRate = sampleRate, 
                  MD5annotJSON = MD5annotJSON)
    
    # update pb
    progress = progress + 1
    if(verbose){
      utils::setTxtProgressBar(pb, progress)
    }
    mediaAdded = TRUE
  }
  
  # create an EMUwebapp default perspective if media has been added 
  perspectives = dbCfg[['EMUwebAppConfig']][['perspectives']]
  if(mediaAdded & (is.null(perspectives) | length(perspectives) == 0)){
    sc = list(order = c("OSCI","SPEC"), assign = list(), contourLims = list())
    defPersp=list(name = 'default', 
                  signalCanvases = sc, 
                  levelCanvases = list(order = list()), 
                  twoDimCanvases = list(order = list()))
    dbCfg[['EMUwebAppConfig']][['perspectives']] = list(defPersp)
    store_DBconfig(emuDBhandle, dbConfig = dbCfg)
  }
  return(invisible(NULL))
}




###################################################
# CRUD operations for files


##' Add files to emuDB
##' 
##' Add files to existing bundles of specified session of emuDB.
##' Do not use this function to import new recordings (media files) and create bundles; 
##' see \code{?import_mediaFiles} to import new recordings.
##' The files that are found in \code{dir} that have the extension 
##' \code{fileExtension} will be copied into the according bundle
##' folder that have the same basename as the file. Note that the 
##' same bundle name may appear in different sessions, therefore you must 
##' specify the session in \code{targetSessionName}. For 
##' more information on the structural elements of an emuDB 
##' see \code{vignette{emuDB}}.
##' Note that adding files does not mean the emuDB is automatically using these, unless
##' you have defined the usage of these files (e.g. by ssffTrackDefinitions).
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param dir directory containing files to be added
##' @param fileExtension file extension of files to be added. If no . (dot) is found 
##' in this string (e.g. "zcr") then the bundle name matching is performed by removing 
##' \code{paste0(".", fileExtension)} from the files ("/path/to/msajc003.zcr" will become "msajc003") 
##' and the according bundle name will be searched. If a . (dot) if found within this string 
##' (e.g. "_annot.json") then the entire string is remove without prepending a . (dot) ("/path/to/msajc003_annot.json" will then become "msajc003")
##' @param targetSessionName name of sessions containing 
##' bundles that the files will be added to
##' @export
##' @keywords emuDB database Emu 
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # specify path to folder containing the following
##' # files we wish to add to: 
##' # msajc003.zcr, msajc010.zcr, msajc012.zcr, msajc015.zcr, 
##' # msajc022.zcr, msajc023.zcr and msajc057.zcr 
##' path2dir = "/path/to/dir/"
##' 
##' # add the files to session "0000" of the "ae" emuDB
##' add_files(emuDBhandle = ae,
##'           dir = path2dir,
##'           fileExtension = "zcr",
##'           targetSessionName = "0000")
##' 
##' }
add_files <- function(emuDBhandle, 
                      dir, 
                      fileExtension, 
                      targetSessionName = '0000'){
  
  check_emuDBhandle(emuDBhandle)
  bndls = list_bundles(emuDBhandle, 
                       sessionPattern = paste0("^", targetSessionName))
  
  if(nrow(bndls) == 0){
    stop("No bundles found in session! Make sure to specify an existing session that contains bundles!")
  }
  
  sourcePaths = list.files(dir, 
                           pattern = paste0("\\.",fileExtension, '$'), 
                           full.names = TRUE)
  
  destDirs = file.path(emuDBhandle$basePath, 
                       paste0(bndls$session, '_ses'), 
                       paste0(bndls$name, '_bndl'))
  
  if(length(sourcePaths) == 0){
    stop("no files found in 'dir' that match the provided 'fileExtension'")
  }
  
  # copy files
  for (i in 1:length(sourcePaths)){
    # if fileExtension doesn't contains . -> split at .
    if(!stringr::str_detect(fileExtension, pattern = "\\.")){
      cbn = basename(stringr::str_remove(sourcePaths[i], 
                                         paste0("\\.", fileExtension, "$")))
    } else {
      # remove from back
      cbn = basename(stringr::str_remove(sourcePaths[i], 
                                         paste0(fileExtension, "$")))
    }
    cbndl = bndls[bndls$name == cbn, ]
    # check that only one bundle folder
    if(nrow(cbndl) != 1){
      if(nrow(cbndl) == 0){
        stop(paste0("no bundle found that matches the base name (",cbn,") of the file '", sourcePaths[i], "'"))
      } else {
        stop(paste0("more than one bundle found (found = ",nrow(cbndl),") that matches the base name (",cbn,") of the file '", sourcePaths[i], "'"))
      }
    }
    
    destDir = file.path(emuDBhandle$basePath, paste0(cbndl$session, '_ses'), paste0(cbndl$name, '_bndl'))
    file.copy(sourcePaths[i], destDir)
  }
}

##' List files of emuDB
##' 
##' List files belonging to emuDB. For 
##' more information on the structural elements of an emuDB 
##' see \code{vignette{emuDB}}.
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param fileExtension file extension of files
##' @param sessionPattern A (RegEx) pattern matching sessions to be searched from the database
##' @param bundlePattern A (RegEx) pattern matching bundles to be searched from the database
##' @return file paths as character vector
##' @export
##' @importFrom rlang .data
##' @keywords emuDB database schema Emu 
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB
##' # (see ?load_emuDB for more information)
##' 
##' # list all files of ae emuDB
##' list_files(emuDBhandle = ae)
##'
##' # list all files of ae emuDB in bundles ending with '3'
##' list_files(emuDBhandle = ae, bundlePattern=".*3$") 
##' 
##' }
##' 
list_files <- function(emuDBhandle,
                       fileExtension = ".*",
                       sessionPattern = ".*",
                       bundlePattern = ".*"){
  
  check_emuDBhandle(emuDBhandle)
  
  fileList = list.files(path = file.path(emuDBhandle$basePath),
                        recursive = TRUE,
                        pattern = paste0(".*[.]", fileExtension, "$")) %>%
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = "value",
                    into = c("session", "bundle", "file"),
                    sep = .Platform$file.sep,
                    extra = "drop",
                    fill = "right")  %>%
    dplyr::filter(!is.na(.data$session)) %>%
    dplyr::filter(!is.na(.data$bundle)) %>%
    dplyr::filter(!is.na(.data$file)) %>%
    
    dplyr::filter (endsWith(.data$session, "_ses")) %>%
    dplyr::filter (endsWith(.data$bundle, "_bndl")) %>%
    
    dplyr::mutate(session = stringr::str_remove(.data$session, "_ses$")) %>%
    dplyr::mutate(bundle = stringr::str_remove(.data$bundle, "_bndl$")) %>%
    
    dplyr::filter (stringr::str_detect(.data$session, sessionPattern)) %>%
    dplyr::filter (stringr::str_detect(.data$bundle, bundlePattern)) %>%
    
    dplyr::mutate (absolute_file_path = file.path(emuDBhandle$basePath,
                                                  paste0(.data$session, "_ses"),
                                                  paste0(.data$bundle, "_bndl"),
                                                  file))
  
  return (fileList)
}

modify_files <- function(){
  stop('not implemented yet')
}

remove_files <- function(){
  stop('not implemented yet')
}


#########################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_emuR-database.files.R')
