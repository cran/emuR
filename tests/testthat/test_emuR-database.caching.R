##' testthat tests for autobuild
##'
context("testing caching functions")

suppressMessages(require('jsonlite'))

dbName = "ae"

path2orig = file.path(tempdir(), 
                      "emuR_demoData", 
                      paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), 
                          "emuR_testthat")
path2db = file.path(path2testData, 
                    paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", 
                   envir = .emuR_pkgEnv)

###########################
test_that("update_cache works", {
  
  # delete, copy and load
  unlink(path2db, 
         recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)

  ################################
  # 
  test_that("new bundle in new session is re-cached", {
    dir.create(file.path(path2db, 'new_ses'))
    file.copy(from = file.path(path2db, '0000_ses', 'msajc010_bndl'), 
              to = file.path(path2db, 'new_ses'),
              recursive = TRUE)
    
    update_cache(ae, verbose=FALSE)
    
    l = list_sessionsDBI(ae)
    expect_true("new" %in% l$name)
    b = list_bundlesDBI(ae)
    expect_true(any(b$session == "new" & b$name == 'msajc010'))
    
    sl = query(ae, "Phonetic == n")
    expect_true(any(sl$session == "new"))
  })
  
  ################################
  # 
  test_that("change in _annot.json is re-cached", {
    # change entry
    annotJson = jsonlite::fromJSON(readLines(file.path(path2db, 
                                                       "new_ses", 
                                                       "msajc010_bndl", 
                                                       "msajc010_annot.json")), 
                                   simplifyVector = TRUE)
    
    annotJson$levels$items[[1]]$id = 666666
    
    pbpJSON = jsonlite::toJSON(annotJson, 
                               auto_unbox = TRUE, 
                               force = TRUE, 
                               pretty = TRUE)
    writeLines(pbpJSON,file.path(path2db, 
                                 "new_ses", 
                                 "msajc010_bndl", 
                                 "msajc010_annot.json"), 
               useBytes = TRUE)
    
    update_cache(ae, verbose = FALSE)
    
    res = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                                "FROM items ",
                                                "WHERE db_uuid = '", ae$UUID, "' ",
                                                " AND session = 'new' ",
                                                " AND bundle = 'msajc010' ",
                                                " AND level = 'Utterance'"))$item_id

    expect_true(res == 666666)
    
  })
  
  
  ################################
  # 
  test_that("deleted bundle is re-cached", {
    unlink(file.path(path2db, 
                     'new_ses', 
                     'msajc010_bndl'), 
           recursive = TRUE)
    
    update_cache(ae, verbose = FALSE)
    
    res = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                                "FROM items ",
                                                "WHERE db_uuid = '", ae$UUID, "' ",
                                                " AND session = 'new' ",
                                                " AND bundle = 'msajc010'"))
    
    expect_true(nrow(res) == 0)
    
    bndls = list_bundles(ae)
    expect_false(any(bndls$session == "new"))
    
    
  })
  
  ################################
  # 
  test_that("deleted session is re-cached", {
    unlink(file.path(path2db, 'new_ses'), recursive = TRUE)
    ses = list_sessionsDBI(ae)
    expect_true(any(ses$name == "new"))
    
    update_cache(ae, verbose = FALSE)
    ses = list_sessionsDBI(ae)

    expect_false(any(ses$name == "new"))
    
  })
  
  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  unlink(path2db, recursive = TRUE)

})

