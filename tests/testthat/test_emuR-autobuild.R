##' testthat tests for autobuild
##'
context("testing autobuild functions")

dbName = "ae"

path2orig = file.path(tempdir(), 
                      "emuR_demoData", 
                      paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), 
                          "emuR_testthat")
path2db = file.path(path2testData, 
                    paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

test_that("autobuild_linkFromTimes works correctly", {
  skip_on_cran()
  ###########################
  test_that("bad calls to autobuild_linkFromTimes", {

    # delete, copy and load
    unlink(path2db, recursive = TRUE)
    file.copy(path2orig, 
              path2testData, 
              recursive = TRUE)
    ae = load_emuDB(path2db, 
                    inMemoryCache = internalVars$testingVars$inMemoryCache, 
                    verbose = FALSE)


    expect_error(autobuild_linkFromTimes(ae, 'Phoneti', 'Tone'))
    expect_error(autobuild_linkFromTimes(ae, 'Phonetic', 'Ton'))
    expect_error(autobuild_linkFromTimes(ae, 'Phonetic', 'Tone'))
    expect_error(autobuild_linkFromTimes(ae, 'Phoneme', 'Phonetic'), 
                 regexp = "The super level type and.*") # super = ITEM
    expect_error(autobuild_linkFromTimes(ae, 'Syllable', 'Phoneme'), 
                 regexp = "The super level type and.*") # super = ITEM

    # clean up
    DBI::dbDisconnect(ae$connection)
    ae = NULL
  })


  ##############################
  test_that("correct links are present after autobuild_linkFromTimes with EVENTS", {

    # delete, copy and load
    unlink(path2db, recursive = TRUE)
    file.copy(path2orig, 
              path2testData, 
              recursive = TRUE)
    ae = load_emuDB(path2db, 
                    inMemoryCache = internalVars$testingVars$inMemoryCache, 
                    verbose = FALSE)

    # add linkDef.
    add_linkDefinition(ae, 
                       "ONE_TO_MANY", 
                       superlevelName = "Phonetic", 
                       sublevelName = "Tone")

    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Tone', 
                            FALSE, 
                            verbose = FALSE)

    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND from_id = 149 ",
                                               " AND to_id = 181"))
    #
    expect_equal(dim(qr)[1], 1)
    expect_equal(qr$session, '0000')
    expect_equal(qr$bundle, 'msajc003')
    expect_equal(qr$from_id, 149)
    expect_equal(qr$to_id, 181)

    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND from_id = 156 ",
                                               " AND to_id = 182"))

    expect_equal(qr$session, '0000')
    expect_equal(qr$bundle, 'msajc003')
    expect_equal(qr$from_id, 156) # redundant
    expect_equal(qr$to_id, 182) # redundant

    # clean up
    DBI::dbDisconnect(ae$connection)
    ae = NULL

  })

  #############################
  test_that("no duplicates are present after autobuild_linkFromTimes with EVENTs", {

    # delete, copy and load
    unlink(path2db, recursive = TRUE)
    file.copy(path2orig, 
              path2testData, 
              recursive = TRUE)
    ae = load_emuDB(path2db, 
                    inMemoryCache = internalVars$testingVars$inMemoryCache, 
                    verbose = FALSE)

    # add linkDef.
    add_linkDefinition(ae, 
                       "ONE_TO_MANY", 
                       superlevelName = "Phonetic", 
                       sublevelName = "Tone")

    # addlink that will also be automatically linked
    DBI::dbExecute(ae$connection, paste0("INSERT INTO links ",
                                         "VALUES ('", ae$UUID, "', '0000', 'msajc003', 140, 181, NULL)"))

    autobuild_linkFromTimes(ae, 'Phonetic', 'Tone', FALSE, verbose = FALSE)

    # extract only one link to be present
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND from_id = 149 ",
                                               " AND to_id = 181"))

    # extract only one link to be present
    expect_equal(dim(qr)[1], 1)

    # if re-run nothing should change (duplicate links)
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Tone', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' "))

    expect_equal(dim(qr)[1], 840)

    # clean up
    DBI::dbDisconnect(ae$connection)
    ae = NULL

  })


  ##############################
  test_that("correct links are present after autobuild_linkFromTimes with SEGMENTS linkDef type ONE_TO_MANY", {

    # delete, copy and load
    unlink(path2db, recursive = TRUE)
    file.copy(path2orig, 
              path2testData, 
              recursive = TRUE)
    ae = load_emuDB(path2db, 
                    inMemoryCache = internalVars$testingVars$inMemoryCache, 
                    verbose = FALSE)

    # add levelDef.
    add_levelDefinition(ae, 
                        "Phonetic2", 
                        "SEGMENT", 
                        verbose = FALSE)
    # add linkDef.
    add_linkDefinition(ae, 
                       "ONE_TO_MANY", 
                       superlevelName = "Phonetic", 
                       sublevelName = "Phonetic2")


    # add item to Phonetic2 = left edge
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 980, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3749, ",
                                         " 10)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND to_id = 980"))
    expect_equal(dim(qr)[1], 1)
    expect_equal(qr$from_id, 147)
    expect_equal(qr$to_id, 980)

    # add item to Phonetic2 = exact match
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 981, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3749, ",
                                         " 1389)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND to_id = 981"))
    expect_equal(dim(qr)[1], 1)
    expect_equal(qr$from_id, 147)
    expect_equal(qr$to_id, 981)

    # add item to Phonetic2 = completely within
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 982, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3800, ",
                                         " 200)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND to_id = 982"))
    expect_equal(dim(qr)[1], 1)
    expect_equal(qr$from_id, 147)
    expect_equal(qr$to_id, 982)


    # add item to Phonetic2 = left overlap
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 983, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3500, ",
                                         " 1000)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"'",
                                               " AND to_id = 983"))
    expect_equal(dim(qr)[1], 0)


    # add item to Phonetic2 = right overlap
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 984, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3800, ",
                                         " 2000)"))
    autobuild_linkFromTimes(ae, 'Phonetic', 'Phonetic2', FALSE, verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND to_id = 984"))
    expect_equal(dim(qr)[1], 0)

    # clean up
    DBI::dbDisconnect(ae$connection)
    ae = NULL

  })

  ##############################
  test_that("correct links are present after autobuild_linkFromTimes with SEGMENTS linkDef type MANY_TO_MANY", {

    #delete, copy and load
    unlink(path2db, recursive = TRUE)
    file.copy(path2orig, 
              path2testData, 
              recursive = TRUE)
    ae = load_emuDB(path2db, 
                    inMemoryCache = internalVars$testingVars$inMemoryCache, 
                    verbose = FALSE)

    # add levelDef.
    add_levelDefinition(ae, 
                        "Phonetic2", 
                        "SEGMENT", 
                        verbose = FALSE)
    # add linkDef.
    add_linkDefinition(ae, 
                       "MANY_TO_MANY", 
                       superlevelName = "Phonetic", 
                       sublevelName = "Phonetic2")

    # add item to Phonetic2 = completely within
    #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3800, 200)
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 980, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3800, ",
                                         " 200)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"'",
                                               " AND to_id = 980"))
    expect_equal(dim(qr)[1], 1)
    expect_equal(qr$from_id, 147)
    expect_equal(qr$to_id, 980)

    # add item to Phonetic2 = left overlap
    #     ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3500, 1000)
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 981, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         "NULL, ",
                                         " 3500, ",
                                         " 1000)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"'",
                                               " AND to_id = 981"))
    expect_equal(dim(qr)[1], 1)
    expect_equal(qr$from_id, 147)
    expect_equal(qr$to_id, 981)

    # add item to Phonetic2 = right overlap
    #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3800, 2000)
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 982, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3800, ",
                                         " 2000)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND to_id = 982"))
    expect_equal(dim(qr)[1], 2)
    expect_equal(qr$from_id, c(147, 148))
    expect_equal(qr$to_id, c(982, 982))


    # add item to Phonetic2 = left and right overlap
    #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3500, 2000)
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 983, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3500, ",
                                         " 2000)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND to_id = 983"))
    expect_equal(dim(qr)[1], 2)
    expect_equal(qr$from_id, c(147, 148))
    expect_equal(qr$to_id, c(983, 983))


    # add item to Phonetic2 = not within
    #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 200, 200)
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         "'msajc003', ",
                                         " 984, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 200, ",
                                         " 200)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND to_id = 984"))
    expect_equal(dim(qr)[1], 0)

    # clean up
    DBI::dbDisconnect(ae$connection)
    ae = NULL

  })

  ##############################
  test_that("correct links are present after autobuild_linkFromTimes with SEGMENTS linkDef type ONE_TO_ONE", {

    # delete, copy and load
    unlink(path2db, recursive = TRUE)
    file.copy(path2orig, 
              path2testData, 
              recursive = TRUE)
    ae = load_emuDB(path2db, 
                    inMemoryCache = internalVars$testingVars$inMemoryCache, 
                    verbose = FALSE)

    # add levelDef.
    add_levelDefinition(ae, 
                        "Phonetic2", 
                        "SEGMENT", 
                        verbose = FALSE)
    # add linkDef.
    add_linkDefinition(ae, 
                       "ONE_TO_ONE", 
                       superlevelName = "Phonetic", 
                       sublevelName = "Phonetic2")


    # add item to Phonetic2 = exact match
    #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3749, 1389)
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 980, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3749, ",
                                         " 1389)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"'",
                                               " AND to_id = 980"))
    expect_equal(dim(qr)[1], 1)
    expect_equal(qr$from_id, 147)
    expect_equal(qr$to_id, 980)

    # add item to Phonetic2 = left overlap
    #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3748, 1389)
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 981, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3748, ",
                                         " 1389)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"'",
                                               " AND to_id = 981"))
    expect_equal(dim(qr)[1], 0)

    # add item to Phonetic2 = right overlap
    #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3749, 1390)
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 982, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3749, ",
                                         " 1390)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"'",
                                               " AND to_id = 982"))
    expect_equal(dim(qr)[1], 0)



    # add item to Phonetic2 = within
    #   ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 3750, 200)
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 983, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3750, ",
                                         " 200)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND to_id = 983"))
    expect_equal(dim(qr)[1], 0)



    # add item to Phonetic2 = not within
    #     ae$items[737, ] = c('ae_0000_msajc003_999', '0000', 'msajc003', 'Phonetic2', 999, 'SEGMENT', 1, 20000, NA, 200, 200)
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 984, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 200, ",
                                         " 200)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            FALSE, 
                            verbose = FALSE)
    qr = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '", ae$UUID,"' ",
                                               " AND to_id = 984"))
    expect_equal(dim(qr)[1], 0)

    # clean up
    DBI::dbDisconnect(ae$connection)
    ae = NULL

  })

  ##############################
  test_that("backup works correctly", {

    # delete, copy and load
    unlink(path2db, recursive = TRUE)
    file.copy(path2orig, 
              path2testData, 
              recursive = TRUE)
    ae = load_emuDB(path2db, 
                    inMemoryCache = internalVars$testingVars$inMemoryCache, 
                    verbose = FALSE)

    # add levelDef.
    add_levelDefinition(ae, 
                        "Phonetic2", 
                        "SEGMENT", 
                        verbose = FALSE)
    # add linkDef.
    add_linkDefinition(ae, 
                       "ONE_TO_ONE", 
                       superlevelName = "Phonetic",
                       sublevelName = "Phonetic2")


    # add item to Phonetic2 = exact match
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         " '0000', ",
                                         " 'msajc003', ",
                                         " 980, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3749, ",
                                         " 1389)"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            TRUE, 
                            TRUE, 
                            verbose = FALSE)



    qr1 = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                                "FROM items ",
                                                "WHERE db_uuid = '", ae$UUID,"' ",
                                                " AND level = 'Phonetic'"))
    qr2 = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                                "FROM items ",
                                                "WHERE db_uuid = '", ae$UUID,"' ",
                                                " AND level = 'Phonetic-autobuildBackup'"))
    # same amount of of items
    expect_equal(dim(qr1), dim(qr2))
    # cols that should be the same are
    expect_equal(qr1$session, qr2$session)
    expect_equal(qr1$bundle, qr2$bundle)
    expect_equal(qr1$seqIdx, qr2$seqIdx)
    expect_equal(qr1$sampleRate, qr2$sampleRate)


    qr1 = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                                "FROM labels ",
                                                "WHERE db_uuid = '", ae$UUID,"' ",
                                                " AND name='Phonetic'"))
    qr2 = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                                "FROM labels ",
                                                "WHERE db_uuid = '", ae$UUID,"' ",
                                                " AND name = 'Phonetic-autobuildBackup'"))
    # same labels
    expect_equal(dim(qr1), dim(qr2))
    expect_equal(dim(qr1$label), dim(qr2$label))


    # itemIDs are the same in items and labels table
    qr1 = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                                "FROM items ",
                                                "WHERE db_uuid = '", ae$UUID,"' ",
                                                " AND level = 'Phonetic-autobuildBackup'"))
    qr2 = DBI::dbGetQuery(ae$connection, paste0("SELECT * ",
                                                "FROM labels ",
                                                "WHERE db_uuid = '", ae$UUID,"' ",
                                                "AND name = 'Phonetic-autobuildBackup'"))
    expect_equal(qr1$itemID, qr2$itemID)


    # new levelDefinition is present
    dbConfig = load_DBconfig(ae)
    expect_equal(dbConfig$levelDefinitions[[length(dbConfig$levelDefinitions)]]$name, 'Phonetic-autobuildBackup')
    expect_equal(dbConfig$levelDefinitions[[length(dbConfig$levelDefinitions)]]$type, 'SEGMENT')

    # clean up
    DBI::dbDisconnect(ae$connection)
    ae = NULL

  })

  ##############################
  test_that("rewrite works correctly", {

    # delete, copy and load
    unlink(path2db, recursive = TRUE)
    file.copy(path2orig, 
              path2testData, 
              recursive = TRUE)
    ae = load_emuDB(path2db, 
                    inMemoryCache = internalVars$testingVars$inMemoryCache, 
                    verbose = FALSE)

    # add levelDef.
    add_levelDefinition(ae, 
                        "Phonetic2", 
                        "SEGMENT", 
                        verbose = FALSE)
    # add linkDef.
    add_linkDefinition(ae, 
                       "ONE_TO_ONE", 
                       superlevelName = "Phonetic", 
                       sublevelName = "Phonetic2")


    # add item to Phonetic2
    DBI::dbExecute(ae$connection, paste0("INSERT INTO items ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         "'0000', ",
                                         " 'msajc003', ",
                                         " 980, ",
                                         " 'Phonetic2', ",
                                         " 'SEGMENT', ",
                                         " 1, ",
                                         " 20000, ",
                                         " NULL, ",
                                         " 3750, ",
                                         " 200)"))

    # add label to Phonetic2
    DBI::dbExecute(ae$connection, paste0("INSERT INTO labels ",
                                         "VALUES (",
                                         " '", ae$UUID, "', ",
                                         "'0000', ",
                                         "'msajc003', ",
                                         " 980, ",
                                         " 0, ",
                                         " 'Phonetic2', ",
                                         " 'testLabel12')"))

    set_levelCanvasesOrder(ae, 
                           "default", 
                           c("Phonetic", "Phonetic2", "Tone"))
    autobuild_linkFromTimes(ae, 
                            'Phonetic', 
                            'Phonetic2', 
                            TRUE, 
                            TRUE, 
                            verbose = FALSE)

    # check if levelCanvasOrder entry was removed
    expect_false("Phonetic" %in% get_levelCanvasesOrder(ae, "default"))


    # _DBconfig.json has new definitions
    dbConfig = load_DBconfig(ae)
    expect_equal(dbConfig$levelDefinitions[[11]]$name, 
                 "Phonetic-autobuildBackup")
    expect_equal(dbConfig$linkDefinitions[[10]]$type, 
                 "ONE_TO_ONE")
    expect_equal(dbConfig$linkDefinitions[[10]]$superlevelName, 
                 "Phonetic")
    expect_equal(dbConfig$linkDefinitions[[10]]$sublevelName, 
                 "Phonetic2")

    # annot.jsons has new fields
    testAnnoFilePath = file.path(path2db, 
                                 "0000_ses", 
                                 "msajc003_bndl", 
                                 "msajc003_annot.json")
    annotJson = fromJSON(testAnnoFilePath, 
                         simplifyVector = FALSE)
    lastLvlName=annotJson$levels[[11]]$name
    expect_equal(lastLvlName, "Phonetic-autobuildBackup")

    # clean up
    DBI::dbDisconnect(ae$connection)
    ae = NULL

  })

  ##############################
  test_that("autobuild of converted TGcol works", {

    path2tgCol = file.path(tempdir(), 
                           "emuR_demoData", 
                           "TextGrid_collection")

    # convert TextGridCollection to the emuDB format
    convert_TextGridCollection(path2tgCol, 
                               dbName = "tgCol",
                               targetDir = path2testData, 
                               verbose = FALSE)

    tgCol = load_emuDB(file.path(path2testData, 
                                 paste0("tgCol", emuDB.suffix)), 
                       verbose = FALSE)

    add_linkDefinition(tgCol, 
                       "ONE_TO_MANY", 
                       superlevelName = "Utterance", 
                       sublevelName = "Intonational")
    # this autobuild causes the warning
    autobuild_linkFromTimes(tgCol, 
                            "Utterance", 
                            "Intonational", 
                            verbose = FALSE)
    
    expect_true(TRUE) # to avoid skip message
    
    test_that("MD5 sums are updated",{
      annotJSONpath = file.path(path2testData, 
                                paste0("tgCol", emuDB.suffix),
                                paste0("0000", session.suffix),
                                paste0("msajc003", bundle.dir.suffix),
                                paste0("msajc003", bundle.annotation.suffix, ".json"))

      curMd5sum = tools::md5sum(annotJSONpath)
      names(curMd5sum) = NULL
      bundleDF = DBI::dbGetQuery(tgCol$connection, paste0("SELECT * ",
                                                          "FROM bundle ",
                                                          "WHERE name = 'msajc003'"))

      expect_equal(curMd5sum, bundleDF$md5_annot_json)

    })

    # clean up
    DBI::dbDisconnect(tgCol$connection)
    tgCol = NULL

  })
  
})
