##' testthat tests for export_TextGridCollection
##'
context("testing export_TextGridCollection")

dbName = "ae"
path2orig = file.path(tempdir(), 
                      "emuR_demoData", 
                      paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, 
                    paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", 
                   envir = .emuR_pkgEnv)

test_that("export_TextGridCollection works correctly", {
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  test_that("errors are thrown correctly", {
    # preclean just in case
    unlink(file.path(path2testData, "tgCol"), 
           recursive = TRUE)
    # bad attr. name
    expect_error(export_TextGridCollection(ae, 
                                           targetDir = file.path(path2testData, "tgCol"), 
                                           attributeDefinitionNames = "badName"), 
                 verbose = FALSE)
  })
  
  test_that("exporting every level works", {
    # preclean just in case
    unlink(file.path(path2testData, "tgCol"), 
           recursive = TRUE)
    
    export_TextGridCollection(ae, 
                              targetDir = file.path(path2testData, "tgCol"), 
                              verbose = FALSE)
    
    tgLines = readLines(file.path(path2testData, 
                                  "tgCol", 
                                  "0000", 
                                  "msajc003.TextGrid"))
    # check header
    expect_match(tgLines[1], ".*ooTextFile.*")
    expect_match(tgLines[2], ".*TextGrid.*")
    expect_match(tgLines[4], ".*xmin = 0.*")
    
    # check number of levels by name
    expect_equal(length(grep("name", tgLines)), 11)
    
    #check same number of items are present as in items X label
    qr = DBI::dbGetQuery(ae$connection, 
                         paste0("SELECT * ",
                                "FROM items AS it, ",
                                " labels AS l ",
                                "WHERE it.db_uuid = l.db_uuid ",
                                " AND it.session = l.session ",
                                " AND it.bundle = 'msajc003' ",
                                " AND l.bundle = 'msajc003' ",
                                " AND it.item_id = l.item_id"))
    
    # 10*2 = left right padding for 10 tiers (from 10 attr. defs.) 
    # that are not of type EVENT mergers (containing "->") are deducted
    expect_equal(length(grep("text|mark",tgLines)), 
                 nrow(qr) + 10 * 2 - length(grep("->", tgLines))) 
    
    
    # clean up
    unlink(file.path(path2testData, "tgCol"), 
           recursive = TRUE)
  })
  
  test_that("exporting only msajc003 works", {
    # preclean just in case
    unlink(file.path(path2testData, "tgCol"), 
           recursive = TRUE)
    
    export_TextGridCollection(ae, 
                              targetDir = file.path(path2testData, "tgCol"), 
                              bundlePattern = "msajc003", 
                              verbose = FALSE)
    list.files(file.path(path2testData, "tgCol", "0000"))
    
    expect_equal(length(list.files(file.path(path2testData, 
                                             "tgCol", 
                                             "0000"))), 
                 2)
    
    
    # clean up
    unlink(file.path(path2testData, "tgCol"), 
           recursive = TRUE)
  })
  
  test_that("empty time segments are treated correctly", {
    # preclean just in case
    unlink(file.path(path2testData, "tgCol"), 
           recursive = TRUE)
    
    export_TextGridCollection(ae, 
                              targetDir = file.path(path2testData, "tgCol"), 
                              bundlePattern = "msajc010", 
                              verbose = FALSE)
    
    tgLines = readLines(file.path(path2testData, 
                                  "tgCol", 
                                  "0000", 
                                  "msajc010.TextGrid"))
    
    qr = DBI::dbGetQuery(ae$connection, 
                         paste0("SELECT * ",
                                "FROM items AS it, ",
                                " labels AS l ",
                                "WHERE it.db_uuid = l.db_uuid ",
                                " AND it.session = l.session ",
                                " AND it.bundle = 'msajc010' ",
                                " AND l.bundle = 'msajc010' ",
                                " AND it.item_id = l.item_id"))
    
    # 10*2 = left right padding for 10 tiers 
    # (from 10 attr. defs.) that are not of type EVENT 
    expect_equal(length(grep("text|mark",tgLines)), 
                 nrow(qr) + 10 * 2 + 3 - length(grep("->",tgLines)))
    
    # clean up
    unlink(file.path(path2testData, "tgCol"), 
           recursive = TRUE)
  })
  
  test_that("only exporting Phonetic works", {
    # preclean just in case
    unlink(file.path(path2testData, "tgCol"), 
           recursive = TRUE)
    
    export_TextGridCollection(ae, 
                              targetDir = file.path(path2testData, "tgCol"), 
                              bundlePattern = "msajc010", 
                              attributeDefinitionNames = "Phonetic", 
                              verbose = FALSE)
    
    tgLines = readLines(file.path(path2testData, 
                                  "tgCol", 
                                  "0000", 
                                  "msajc010.TextGrid"))
    
    expect_equal(length(grep("Phonetic", tgLines)), 1)
  })
  
  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  unlink(path2db, recursive = TRUE)
  
  
})
