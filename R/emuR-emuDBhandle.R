# constructor function for emuDBhandle
emuDBhandle = function(dbName, 
                       basePath, 
                       UUID, 
                       connectionPath, 
                       connection = NULL){
  
  if(is.null(connection)){
    con <- DBI::dbConnect(RSQLite::SQLite(), 
                          connectionPath)
  }else{
    con = connection
  }
  
  handle = list(dbName = dbName,
                basePath = basePath,
                UUID = UUID,
                connection = con)
  
  class(handle) = "emuDBhandle"
  
  if(inherits(handle$connection, "SQLiteConnection")){
    setSQLitePragmas(handle$connection)
    # init regex
    RSQLite::initRegExp(handle$connection)
  }
  
  if(connectionPath == ":memory:" 
     || file.exists(file.path(basePath, 
                              paste0(dbName, database.cache.suffix))) 
     || !is.null(connection)){
    initialize_emuDbDBI(handle)
  }
  
  
  
  return(handle)
}

setSQLitePragmas <- function(con){
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  DBI::dbExecute(con, "PRAGMA temp_store = 2;")
}

##' @export
print.emuDBhandle = function(x, ...){
  check_emuDBhandle(x)
  print(paste0("<emuDBhandle> (dbName = '", x$dbName, "', basePath = '", x$basePath, "')"))
}

# function to check if a emuDBhandle 
# seems to be valid. It only does some shallow 
# checks and doesn't do any deep inspection
check_emuDBhandle <- function(emuDBhandle, checkCache = TRUE){
  # check if dir and cache actually exist
  if(!dir.exists(emuDBhandle$basePath)){
    stop(paste0("emuDBhandle is invalid as emuDBhandle$basePath ",
                "doesn't exist! Please reload the emuDB."))
  }
  if(!DBI::dbIsValid(emuDBhandle$connection)){
    stop(paste0("emuDBhandle is invalid as emuDBhandle$connection is not ",
                "a valid DBI connection! Please reload the emuDB."))
  }
  # from basePath extract dbName and see if DB
  dbName = stringr::str_replace(basename(emuDBhandle$basePath), 
                                pattern = '_emuDB$', 
                                replacement = '')
  
  if(!file.exists(file.path(emuDBhandle$basePath, paste0(dbName, database.schema.suffix)))){
    stop(paste0("emuDBhandle is invalid as the directory emuDBhandle$basePath doesn't ",
                "contain the _DBconfig.json file '", dbName, "_DBconfig.json'. Note that ",
                "the emuDB directory has to have the same prefix / name as the _DBconfig.json."))
  }
  if(checkCache){
    if(!file.exists(file.path(emuDBhandle$basePath, paste0(dbName, database.cache.suffix)))){
      stop(paste0("emuDBhandle is invalid as the directory emuDBhandle$basePath doesn't contain ",
                  "a _emuDBcache.sqlite file! Please reload the emuDB to recreate the emuDBcache."))
    }
  }
}

##' Print summary of loaded EMU database (emuDB).
##' @description Gives an overview of an EMU database.
##' Prints database name, UUID, base directory path, session and bundle 
##' count and informations about signal track, annotation level, attribute and link definitions.
##' @param object emuDBhandle as returned by \code{\link{load_emuDB}}
##' @param ... additional arguments affecting the summary produced.
##' @export
summary.emuDBhandle = function(object, ...){
  
  check_emuDBhandle(object)
  
  cli::cli_h1("Summary of emuDB")
  
  cat("Name:\t", object$dbName, "\n")
  cat("UUID:\t", object$UUID, "\n")
  cat("Directory:\t", object$basePath, "\n")
  sess = list_sessions(object)
  cat("Session count:", nrow(sess), "\n")
  bndls = list_bundles(object)
  cat("Bundle count:", nrow(bndls), "\n")
  
  itCntQ = paste0("SELECT count(*) FROM items WHERE db_uuid='", object$UUID, "'")
  itCntDf = DBI::dbGetQuery(object$connection, itCntQ)
  itemCnt = itCntDf[[1]]
  labCntQ = paste0("SELECT count(*) FROM labels WHERE db_uuid='", object$UUID, "'")
  labCntDf = DBI::dbGetQuery(object$connection, labCntQ)
  labCnt = labCntDf[[1]]
  liCntQ = paste0("SELECT count(*) FROM links WHERE db_uuid='", object$UUID, "'")
  liCntDf = DBI::dbGetQuery(object$connection, liCntQ)
  linkCnt = liCntDf[[1]]
  cat("Annotation item count: ", itemCnt, "\n")
  cat("Label count: ", labCnt, "\n")
  cat("Link count: ", linkCnt, "\n")
  cli::cli_h1("Database configuration")
  
  dbConfig = load_DBconfig(object)
  cli::cli_h2("SSFF track definitions")
  ssffTrackDefs = list_ssffTrackDefinitions(object)
  pr <- print.data.frame(ssffTrackDefs, right = FALSE, row.names = FALSE)
  cat("\n")
  cli::cli_h2("Level definitions")
  levelDefs = list_levelDefinitions(object)
  pr <- print.data.frame(levelDefs, right = FALSE, row.names = FALSE)
  cat("\n")
  lblGrps = list_labelGroups(object)
  if(nrow(lblGrps) > 0){
    cli::cli_h2("Database label group definitions")
    pr <- print.data.frame(lblGrps, right = FALSE, row.names = FALSE)
    cat("\n")
  }
  cli::cli_h2("Link definitions")
  linkDefs = list_linkDefinitions(object)
  pr <- print.data.frame(linkDefs, right = FALSE, row.names = FALSE)
}

##########################
# FOR DEVELOPMENT
# handle = emuDBhandle(dbName = "test12", basePath = "/path/2/emuDB", UUID = "3412D5E3-E0EA-4E81-9F1C-E0A864D0D403", ":memory:")
# ae1 = load_emuDB("~/Desktop/emuR_demoData/ae", inMemoryCache = TRUE)
# ae2 = load_emuDB("~/Desktop/emuR_demoData/ae", inMemoryCache = FALSE)
# print(summary(ae1))
