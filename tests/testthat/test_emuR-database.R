context("testing database functions")

aeSampleRate = 20000

dbName = "ae"
path2demoData = file.path(tempdir(),
                          "emuR_demoData")
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

test_that("database functions work", {
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  unlink(file.path(path2testData, "fromLegacy"), 
         recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  # convert and load legacy database
  convert_legacyEmuDB(emuTplPath = file.path(path2demoData, "legacy_ae", "ae.tpl"),
                      targetDir = file.path(path2testData, "fromLegacy"), 
                      dbUUID = ae$UUID, 
                      verbose = FALSE)
  aeFromLegacy = load_emuDB(file.path(path2testData, "fromLegacy", 
                                      paste0(dbName, emuDB.suffix)), 
                            verbose = FALSE)
  
  test_that("function get_legacyFilePath()",{
    primaryTrackFilePath = get_legacyFilePath("/path/to/db",
                                              'BLOCK*/SES*',
                                              c('BLOCK30', 'SES3042', '0001abc'), 
                                              'wav')
    expect_equal(primaryTrackFilePath, 
                 "/path/to/db/BLOCK30/SES3042/0001abc.wav")
    
    signalTrackFilePath = get_legacyFilePath("/path/to/db",
                                             'F0',
                                             c('BLOCK30', 'SES3042', '0001abc'), 
                                             'f0')
    expect_equal(signalTrackFilePath,
                 "/path/to/db/F0/0001abc.f0")
  })
  
  
  test_that("Converted emuDB is equal to original",{
    expect_equal(ae$dbName, aeFromLegacy$dbName)
    expect_equal(ae$UUI, aeFromLegacy$UUID)
    
    origItems = DBI::dbReadTable(ae$connection, "items")
    convItems = DBI::dbReadTable(aeFromLegacy$connection, "items")
    expect_equal(origItems, convItems)
    
    origLabels = DBI::dbReadTable(ae$connection, "labels")
    convLabels = DBI::dbReadTable(aeFromLegacy$connection, "labels")
    expect_equal(origLabels, convLabels)
    
  })
  
  test_that("properties of ae are correct",{
    bp = file.path(path2testData, 'ae_emuDB')
    nbp = normalizePath(path2db)
    
    expect_that(ae$dbName, is_equivalent_to('ae'))
    expect_that(ae$basePath, is_equivalent_to(nbp))
    sesss = list_sessionsDBI(ae)
    expect_that(nrow(sesss), is_equivalent_to(1))
    bndls = list_bundlesDBI(ae)
    expect_that(nrow(bndls), is_equivalent_to(7))
    itCntQ = paste0("SELECT count(*) ",
                    "FROM items ",
                    "WHERE db_uuid = '",ae$UUID,"'")
    
    itCntDf = DBI::dbGetQuery(ae$connection,itCntQ)
    itemCnt = itCntDf[[1]]
    liCntQ = paste0("SELECT count(*) ",
                    "FROM links ",
                    "WHERE db_uuid = '",ae$UUID,"'")
    liCntDf = DBI::dbGetQuery(ae$connection, liCntQ)
    linkCnt = liCntDf[[1]]
    expect_that(itemCnt, is_equivalent_to(736))
    expect_that(linkCnt, is_equivalent_to(785))
  })
  
  test_that("Create emuDB from scratch works",{
    create_emuDB('create_emuDB_test1', path2testData)
    t1BasePath = file.path(path2testData, 'create_emuDB_test1_emuDB')
    t1 = load_emuDB(t1BasePath)
    expect_that(t1$dbName, is_equivalent_to('create_emuDB_test1'))
    t1 = NULL
    unlink(t1BasePath, recursive = TRUE)
  })
  
  test_that("Data types are correct",{
    items = DBI::dbReadTable(ae$connection, 'items')
    
    expect_that(class(items[['seq_idx']]), is_equivalent_to('integer'))
    expect_that(class(items[['item_id']]), is_equivalent_to('integer'))
    expect_that(class(items[['sample_rate']]), is_equivalent_to('numeric'))
    expect_that(class(items[['sample_point']]), is_equivalent_to('integer'))
    expect_that(class(items[['sample_start']]), is_equivalent_to('integer'))
    expect_that(class(items[['sample_dur']]), is_equivalent_to('integer'))
    
    labels = DBI::dbReadTable(ae$connection, 'labels')
    expect_that(class(labels[['label_idx']]), is_equivalent_to('integer'))
    
    links = DBI::dbReadTable(ae$connection,'links')
    expect_that(class(links[['from_id']]), is_equivalent_to('integer'))
    expect_that(class(links[['to_id']]), is_equivalent_to('integer'))
  })
  
  test_that("Test ae samples",{
    
    # aeB1=get.bundle(sessionName='0000',bundleName='msajc003',dbUUID=.test_emu_ae_db_uuid)
    
    bundleAnnotDFs = load_bundleAnnotDFsDBI(ae, "0000", "msajc003")
    aeB1char = bundleAnnotDFsToAnnotJSONchar(ae, bundleAnnotDFs)
    aeB1 = jsonlite::fromJSON(aeB1char, simplifyVector = FALSE)
    
    expect_equivalent(aeB1[['sampleRate']], aeSampleRate)
    
    halfSample = 0.5 / aeSampleRate
    msajc015_lab_values = c(0.300000, 0.350276, 0.425417, 0.496601, 0.558601, 0.639601,
                            0.663601, 0.706601, 0.806601, 1.006101, 1.085101, 1.097601,
                            1.129101, 1.160101, 1.213101, 1.368101, 1.413095, 1.449550,
                            1.464601, 1.500731, 1.578583, 1.623228, 1.653718, 1.717601,
                            1.797463, 1.828601, 1.903635, 2.070101, 2.104101, 2.154601,
                            2.200911, 2.226601, 2.271132, 2.408601, 2.502214, 2.576618,
                            2.606558, 2.693704, 2.749004, 2.780766, 2.798504, 2.876593,
                            2.958101, 3.026668, 3.046168, 3.067703, 3.123168, 3.238668,
                            3.297668,3.456899) 
    msajc015_tone_events = c(0.531305, 1.486760, 1.609948, 2.445220, 2.910929, 3.110782, 
                             3.262078)
    lvCnt = length(msajc015_lab_values)
    teCnt = length(msajc015_tone_events)
    #msajc015_phonetic=ae[['items']][ae[['items']][['bundle']]=="msajc015" & ae[['items']][['level']]=='Phonetic',]
    msajc015_phonetic = DBI::dbGetQuery(ae$connection, 
                                        paste0("SELECT * ",
                                               "FROM items ",
                                               "WHERE db_uuid = '", ae$UUID, "' ",
                                               " AND session='0000' ",
                                               " AND bundle='msajc015' ",
                                               " AND level='Phonetic'"))
    rc = nrow(msajc015_phonetic)
    expect_equivalent(rc + 1, lvCnt)
    # order by sequence index
    msajc015_phonetic_ordered = msajc015_phonetic[order(msajc015_phonetic[['seq_idx']]),]
    rc = nrow(msajc015_phonetic_ordered)
    expect_equivalent(rc + 1, lvCnt)
    
    #msajc015_tone=ae[['items']][ae[['items']][['bundle']]=="msajc015" & ae[['items']][['level']]=='Tone',]
    msajc015_tone = DBI::dbGetQuery(ae$connection, 
                                    paste0("SELECT * ",
                                           "FROM items ",
                                           "WHERE db_uuid = '", ae$UUID, "' ",
                                           " AND session = '0000' ",
                                           " AND bundle = 'msajc015' ",
                                           " AND level = 'Tone'"))
    msajc015_tone_ordered = msajc015_tone[order(msajc015_tone[['seq_idx']]),]
    lvSq = 1:rc
    
    # check sequence
    for(i in lvSq){
      
      poSampleStart = msajc015_phonetic_ordered[i, 'sample_start']
      poSampleDur=msajc015_phonetic_ordered[i,'sample_dur']
      if(i < rc){
        poNextSampleStart = msajc015_phonetic_ordered[i + 1, 'sample_start']
        # TODO
        expect_equivalent(poNextSampleStart, poSampleStart + poSampleDur + 1)
        #expect_equivalent(poNextSampleStart,poSampleStart+poSampleDur+1)
      }
    }
    # check segment boundaries
    for(i in lvSq){
      lv = msajc015_lab_values[i]
      poSampleStart = msajc015_phonetic_ordered[i, 'sample_start']
      poSampleDur = msajc015_phonetic_ordered[i, 'sample_dur']
      poStart = (poSampleStart + 0.5) / aeSampleRate
      absFail = abs(poStart - lv)
      # accept deviation of at least half a sample
      expect_lt(absFail, halfSample)
    }
    # and the last value
    lv = msajc015_lab_values[lvCnt]
    poSampleEnd = msajc015_phonetic_ordered[rc, 'sample_start'] + msajc015_phonetic_ordered[rc, 'sample_dur'] + 1
    poEnd = (poSampleEnd + 0.5) / aeSampleRate
    absFail = abs(poEnd - lv)
    # accept deviation of at least half a sample
    expect_lt(absFail, halfSample)
    
    # check tone events
    teS = 1:teCnt
    for(i in teS){
      teTime = msajc015_tone_events[i]
      teLSample = msajc015_tone_ordered[i, 'sample_point']
      teLTime = teLSample / aeSampleRate
      absFail = abs(teLTime - teTime)
      expect_lt(absFail, halfSample)
    }
    
  })
  
  test_that("Test ae modify",{
    orgItems = DBI::dbGetQuery(ae$connection, 
                               paste0("SELECT * ",
                                      "FROM items ",
                                      "WHERE db_uuid = '",ae$UUID,"'"))
    orgLabels = DBI::dbGetQuery(ae$connection,
                                paste0("SELECT * ",
                                       "FROM labels ",
                                       "WHERE db_uuid = '",ae$UUID,"'"))
    orgLinks = DBI::dbGetQuery(ae$connection,
                               paste0("SELECT * ",
                                      "FROM links ",
                                      "WHERE db_uuid = '",ae$UUID,"'"))
    
    expect_equivalent(nrow(orgItems),736)
    expect_equivalent(nrow(orgLinks),785)
    # b015=get.bundle(sessionName='0000',bundleName = 'msajc015',dbUUID = .test_emu_ae_db_uuid)
    bundleAnnotDFs = load_bundleAnnotDFsDBI(ae, 
                                            "0000", 
                                            "msajc015")
    b015char = bundleAnnotDFsToAnnotJSONchar(ae, bundleAnnotDFs)
    b015 = jsonlite::fromJSON(b015char, 
                              simplifyVector = FALSE, 
                              na = 'null')
    
    # select arbitrary item
    b015m = b015
    phoneticLvlIt10 = b015m[['levels']][[7]][['items']][[10]]
    lblOrg = phoneticLvlIt10[['labels']][[1]][['value']]
    b015m[['levels']][[7]][['items']][[10]][['labels']][[1]][['value']] = 'test!!'
    
    # convert to bundleAnnotDFs
    b015mChar = jsonlite::toJSON(b015m, 
                                 auto_unbox = TRUE, 
                                 pretty = TRUE)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015mChar[1])
    remove_bundleAnnotDBI(ae, 
                          "0000", 
                          bundleName = "msajc015")
    store_bundleAnnotDFsDBI(ae, 
                            bundleAnnotDFs, 
                            "0000", 
                            "msajc015")
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m)
    
    modItems = DBI::dbGetQuery(ae$connection, 
                               paste0("SELECT * ",
                                      "FROM items ",
                                      "WHERE db_uuid = '",ae$UUID,"'"))
    modLabels = DBI::dbGetQuery(ae$connection, 
                                paste0("SELECT * ",
                                       "FROM labels ",
                                       "WHERE db_uuid = '",ae$UUID,"'"))
    modLinks = DBI::dbGetQuery(ae$connection, 
                               paste0("SELECT * ",
                                      "FROM links ",
                                      "WHERE db_uuid = '",ae$UUID,"'"))
    
    expect_equivalent(nrow(modItems), 736)
    expect_equivalent(nrow(modLinks), 785)
    
    # change only affects labels
    # items should be equal
    expect_equal(orgItems, modItems)
    # labels not
    cmLbls1 = compare::compare(orgLabels, 
                               modLabels, 
                               allowAll = TRUE)
    expect_false(cmLbls1$result)
    # links are not changed, should be equal to original
    expect_equal(orgLinks, modLinks)
    
    b015m[['levels']][[7]][['items']][[10]][['sampleDur']] = 99
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m)
    # convert to bundleAnnotDFs
    b015mChar = jsonlite::toJSON(b015m, 
                                 auto_unbox = TRUE, 
                                 pretty = TRUE)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015mChar[1])
    remove_bundleAnnotDBI(ae, 
                          "0000", 
                          bundleName = "msajc015")
    store_bundleAnnotDFsDBI(ae, 
                            bundleAnnotDFs, 
                            "0000", 
                            "msajc015")
    
    mod2Items = DBI::dbGetQuery(ae$connection,
                                paste0("SELECT * ",
                                       "FROM items ",
                                       "WHERE db_uuid = '",ae$UUID,"'"))
    mod2Labels = DBI::dbGetQuery(ae$connection,
                                 paste0("SELECT * ",
                                        "FROM labels ",
                                        "WHERE db_uuid = '",ae$UUID,"'"))
    mod2Links = DBI::dbGetQuery(ae$connection,
                                paste0("SELECT * ",
                                       "FROM links ",
                                       "WHERE db_uuid = '",ae$UUID,"'"))
    
    expect_equivalent(nrow(mod2Items), 736)
    expect_equivalent(nrow(mod2Links), 785)
    #   
    #   # should all be equal to original 
    cm2 = compare::compare(orgItems, 
                           mod2Items,
                           allowAll = TRUE)
    expect_false(cm2$result)
    cmLbls2 = compare::compare(orgLabels,
                               mod2Labels,
                               allowAll = TRUE)
    expect_false(cmLbls2$result)
    cml2 = compare::compare(orgLinks,
                            mod2Links,
                            allowAll = TRUE)
    expect_true(cml2$result)
    
    # remove link
    b015Lks = b015m[['links']]
    b015LksM = list()
    for(b015Lk in b015Lks){
      if(!(b015Lk[['fromID']] == 177 & b015Lk[['toID']] == 224)){
        b015LksM[[length(b015LksM) + 1]] = b015Lk
      }
    }
    b015m2 = b015m
    b015m2[['links']] = b015LksM
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m2)
    # convert to bundleAnnotDFs and store
    b015m2Char = jsonlite::toJSON(b015m2, 
                                  auto_unbox = TRUE, 
                                  pretty = TRUE)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015m2Char[1])
    remove_bundleAnnotDBI(ae, 
                          "0000", 
                          bundleName = "msajc015")
    store_bundleAnnotDFsDBI(ae, 
                            bundleAnnotDFs, 
                            "0000", 
                            "msajc015")
    
    mod3Items = DBI::dbGetQuery(ae$connection,
                                paste0("SELECT * ",
                                       "FROM items ",
                                       "WHERE db_uuid = '",ae$UUID,"'"))
    mod3Labels = DBI::dbGetQuery(ae$connection,
                                 paste0("SELECT * ",
                                        "FROM labels ",
                                        "WHERE db_uuid = '",ae$UUID,"'"))
    mod3Links = DBI::dbGetQuery(ae$connection,
                                paste0("SELECT * ",
                                       "FROM links ",
                                       "WHERE db_uuid = '",ae$UUID,"'"))
    
    expect_equivalent(nrow(mod3Items), 736)
    expect_equivalent(nrow(mod3Links), 784)
    
    cm3 = compare::compare(mod3Items,
                           mod2Items,
                           allowAll = TRUE)
    expect_true(cm3$result)
    cmLbls3 = compare::compare(mod2Labels,
                               mod3Labels,
                               allowAll = TRUE)
    expect_true(cmLbls3$result)
    cml3 = compare::compare(mod3Links,
                            mod2Links,
                            allowAll = TRUE)
    expect_false(cml3$result)
    
    # insert the link again
    # b015m3=get.bundle(dbUUID = .test_emu_ae_db_uuid,sessionName = '0000',bundleName = 'msajc015')
    bundleAnnotDFs = load_bundleAnnotDFsDBI(ae, 
                                            "0000", 
                                            "msajc015")
    b015m3char = bundleAnnotDFsToAnnotJSONchar(ae, bundleAnnotDFs)
    b015m3 = jsonlite::fromJSON(b015m3char, 
                                simplifyVector = FALSE, 
                                na = 'null')
    
    b015m3Lks = b015m3[['links']]
    b015m3Lks[[length(b015m3Lks) + 1]] = list(fromID = 177,
                                              toID = 224)
    b015m3[['links']] = b015m3Lks
    
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015m3)
    # convert to bundleAnnotDFs and store
    b015m3Char = jsonlite::toJSON(b015m3, 
                                  auto_unbox = TRUE, 
                                  pretty = TRUE)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015m3Char[1])
    remove_bundleAnnotDBI(ae, 
                          "0000", 
                          bundleName = "msajc015")
    store_bundleAnnotDFsDBI(ae, 
                            bundleAnnotDFs, 
                            "0000", 
                            "msajc015")
    
    mod4Links=DBI::dbGetQuery(ae$connection, 
                              paste0("SELECT * ",
                                     "FROM links ",
                                     "WHERE db_uuid = '",ae$UUID,"'"))
    cml3 = compare::compare(orgLinks,
                            mod4Links,
                            allowAll = TRUE)
    expect_true(cml3$result)
    
    #   
    #   # TODO move segment boundaries, change links,etc...
    #   
    #   
    
    #   # store original bundle
    
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015)
    # convert to bundleAnnotDFs and store
    b015Char = jsonlite::toJSON(b015, 
                                auto_unbox = TRUE, 
                                pretty = TRUE)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015Char[1])
    remove_bundleAnnotDBI(ae, 
                          "0000", 
                          bundleName = "msajc015")
    store_bundleAnnotDFsDBI(ae, 
                            bundleAnnotDFs, 
                            "0000", 
                            "msajc015")
    
    #   
    modOrgItems = DBI::dbGetQuery(ae$connection,
                                  paste0("SELECT * ",
                                         "FROM items ",
                                         "WHERE db_uuid = '",ae$UUID,"'"))
    modOrgLabels = DBI::dbGetQuery(ae$connection,
                                   paste0("SELECT * ",
                                          "FROM labels ",
                                          "WHERE db_uuid = '",ae$UUID,"'"))
    modOrgLinks = DBI::dbGetQuery(ae$connection,
                                  paste0("SELECT * ",
                                         "FROM links ",
                                         "WHERE db_uuid = '",ae$UUID,"'"))
    
    expect_equivalent(nrow(modOrgItems), 736)
    expect_equivalent(nrow(modOrgLinks), 785)
    
    #   
    #   # should all be equal to original 
    cm2 = compare::compare(orgItems,
                           modOrgItems,
                           allowAll = TRUE)
    expect_true(cm2$result)
    cmLbls2 = compare::compare(dplyr::arrange(orgLabels, 
                                              bundle, 
                                              item_id), 
                               dplyr::arrange(modOrgLabels, 
                                              bundle, 
                                              item_id))
    compare::compare(orgLabels, 
                     modOrgLabels,
                     allowAll = TRUE)
    expect_true(cmLbls2$result)
    cml2 = compare::compare(orgLinks,
                            modOrgLinks,
                            allowAll = TRUE)
    expect_true(cml2$result)
    
    b015ModInsrt = b015
    # insert segment
    its = b015ModInsrt[['levels']][[7]][['items']]
    
    #b$levels[['Phonetic']][['items']][[9]]
    #$id
    #[1] 193
    #
    #$sampleStart
    #[1] 16132
    #  
    #$sampleDur
    #[1] 3989
    #
    #$labels
    #$labels[[1]]
    #$labels[[1]]$name
    #[1] "Phonetic"
    #
    #$labels[[1]]$value
    #[1] "ai"
    
    # split this item:
    # shrink item to length 3500
    b015ModInsrt[['levels']][[7]][['items']][[9]]$sampleDur = 3500
    b015ModInsrt[['levels']][[7]][['items']][[9]]$labels[[1]]$value = 'a'
    
    # shift items to the right to free index 10
    itCnt = length(b015ModInsrt[['levels']][[7]][['items']])
    shiftSeq = itCnt:10
    
    for(itIdx in shiftSeq){
      b015ModInsrt[['levels']][[7]][['items']][[itIdx+1]] = b015ModInsrt[['levels']][[7]][['items']][[itIdx]]
    }
    
    # insert item at index 10
    itLbl = list(name = 'Phonetic',
                 value = 'i')
    itLbls = list(itLbl)
    insertIt = list(id = 999,
                    sampleStart = 19633,
                    sampleDur = 488,
                    labels = itLbls)
    b015ModInsrt[['levels']][[7]][['items']][[10]] = insertIt
    
    # store.bundle.annotation(dbUUID=.test_emu_ae_db_uuid,bundle=b015ModInsrt)
    # convert to bundleAnnotDFs and store
    b015ModInsrtChar = jsonlite::toJSON(b015ModInsrt, 
                                        auto_unbox = TRUE, 
                                        pretty = TRUE)
    bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(b015ModInsrtChar[1])
    remove_bundleAnnotDBI(ae, 
                          "0000", 
                          bundleName = "msajc015")
    store_bundleAnnotDFsDBI(ae, 
                            bundleAnnotDFs, 
                            "0000", 
                            "msajc015")
    
    # read again
    # b015Read=get.bundle(sessionName='0000',bundleName = 'msajc015',dbUUID = .test_emu_ae_db_uuid)
    bundleAnnotDFs = load_bundleAnnotDFsDBI(ae, 
                                            "0000", 
                                            "msajc015")
    b015ReadChar = bundleAnnotDFsToAnnotJSONchar(ae, bundleAnnotDFs)
    b015Read = jsonlite::fromJSON(b015ReadChar, 
                                  simplifyVector = FALSE, 
                                  na = 'null')
    rItCnt = length(b015Read[['levels']][[7]][['items']])
    # check insert sequence
    for(itIdx in 1:9){
      expect_equal(b015[['levels']][[7]][['items']][[itIdx]][['id']], 
                   b015Read[['levels']][[7]][['items']][[itIdx]][['id']])
    }
    expect_equivalent(b015Read[['levels']][[7]][['items']][[10]]$id, 999)
    
    for(itIdx in 11:rItCnt){
      expect_equal(b015[['levels']][[7]][['items']][[itIdx-1]][['id']],
                   b015Read[['levels']][[7]][['items']][[itIdx]][['id']])
    }
    
    
  })
  ######################
  # cleanup
  
  # delete vars to be safe (& to diconnect)
  DBI::dbDisconnect(ae$connection)
  DBI::dbDisconnect(aeFromLegacy$connection)
  ae = NULL
  aeFromLegacy = NULL
  
  unlink(file.path(path2testData, "fromLegacy"), recursive = TRUE)
  unlink(unlink(path2db, recursive = TRUE), recursive = TRUE)
})

test_that("store works correctly",{
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  unlink(file.path(path2testData, "fromStore"), 
         recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  
  newFolderPath = file.path(path2testData, "fromStore")
  unlink(newFolderPath, recursive = TRUE)
  store(ae, 
        targetDir = newFolderPath, 
        verbose = FALSE)
  aeStored = load_emuDB(file.path(newFolderPath, "ae_emuDB"), 
                        verbose = FALSE)
  
  aeItems=DBI::dbGetQuery(ae$connection,paste0("SELECT * ",
                                               "FROM items ",
                                               "WHERE db_uuid = '",ae$UUID,"'"))
  aeLabels=DBI::dbGetQuery(ae$connection,paste0("SELECT * ",
                                                "FROM labels ",
                                                "WHERE db_uuid = '",ae$UUID,"'"))
  aeLinks=DBI::dbGetQuery(ae$connection,paste0("SELECT * ",
                                               "FROM links ",
                                               "WHERE db_uuid = '",ae$UUID,"'"))
  
  aeStoredItems=DBI::dbGetQuery(aeStored$connection,paste0("SELECT * ",
                                                           "FROM items ",
                                                           "WHERE db_uuid = '",aeStored$UUID,"'"))
  aeStoredLabels=DBI::dbGetQuery(aeStored$connection,paste0("SELECT * ",
                                                            "FROM labels ",
                                                            "WHERE db_uuid = '",aeStored$UUID,"'"))
  aeStoredLinks=DBI::dbGetQuery(aeStored$connection,paste0("SELECT * ",
                                                           "FROM links ",
                                                           "WHERE db_uuid = '",aeStored$UUID,"'"))
  
  # check that all tabels are the same
  # expect_equal(aeItems, aeStoredItems) # new index on items table results in different seq. of items hence:
  cres = compare::compare(aeItems, 
                          aeStoredItems, 
                          allowAll = TRUE)
  expect_true(cres$result)
  expect_equal(aeLabels, aeStoredLabels)
  expect_equal(aeLinks, aeStoredLinks)
  
  ####################
  # cleanup
  
  # delete vars to be safe (& to diconnect  ae = NULL
  DBI::dbDisconnect(aeStored$connection)
  DBI::dbDisconnect(ae$connection)
  aeStored = NULL
  ae = NULL
  unlink(file.path(path2testData, "fromStore"), recursive = TRUE)
  unlink(path2db, recursive = TRUE)
})

test_that("rename emuDB works correctly",{
  skip_on_cran() # for some reason fails on cran’s windows machines, but I confirmed that it works on windows. So skip.
  skip_on_os("windows")
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  unlink(file.path(path2testData, "fromStore"), 
         recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  
  rename_emuDB(path2db, "aeRename")
  newPath = file.path(path2testData, 
                      paste0("aeRename", "_emuDB"))
  
  DBconfig = jsonlite::fromJSON(file.path(newPath, "aeRename_DBconfig.json"), 
                                simplifyVector = FALSE)
  expect_equal(DBconfig$name, "aeRename")
  
  expect_true("aeRename_emuDB" %in% list.files(path2testData))
  expect_true("aeRename_emuDBcache.sqlite" %in% list.files(newPath))
  
  # cleanup
  unlink(newPath, recursive = TRUE)
})

test_that("rename bundles works correctly",{
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  unlink(file.path(path2testData, "fromStore"), 
         recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  
  db = load_emuDB(path2db, verbose = FALSE)
  bundles = list_bundles(db)
  # missing col
  expect_error(rename_bundles(db, bundles))
  bundles$name_new = paste0(bundles$name, "XXX")
  # bad bundle/session names
  bad_bundles = bundles
  bad_bundles[1,]$name = "bad_bundle_name"
  expect_error(rename_bundles(db, bad_bundles))
  bad_bundles = bundles
  bad_bundles[1,]$session = "bad_session_name"
  expect_error(rename_bundles(db, bad_bundles))
  
    
  rename_bundles(db, bundles)
  
  new_bundles = list_bundles(db)
  
  expect_true(all(stringr::str_detect(new_bundles$name, "XXX"))) 
  
  new_bundles = list_bundlesDBI(db)
  
  expect_true(all(stringr::str_detect(new_bundles$name, "XXX"))) 
  
  files = list_files(db)
  
  expect_true(all(stringr::str_detect(files$bundle, "XXX"))) 
  expect_true(all(stringr::str_detect(files$file, "XXX")))
  expect_true(all(stringr::str_detect(files$absolute_file_path, "XXX"))) 
  
  # cleanup
  DBI::dbDisconnect(db$connection)
  db = NULL
  unlink(path2db, recursive = TRUE)
})


test_that("load of read only emuDB works",{
  skip_on_cran() # probably won't work on windows (because of mode) so skip on cran
  skip_on_os("windows")
  
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  unlink(file.path(path2testData, "fromStore"), 
         recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  
  # change emuDB folder to r-x only for everyone
  Sys.chmod(path2db, mode = "555")
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  sl = query(ae, "Phonetic == n")
  expect_true("tbl_df" %in% class(sl))
  Sys.chmod(path2db, mode = "755") # change back
  
  # change emuDBcache.sqlite to 
  Sys.chmod(file.path(path2db, "ae_emuDBcache.sqlite"), mode = "555")
  ae = load_emuDB(path2db, 
                  inMemoryCache = internalVars$testingVars$inMemoryCache, 
                  verbose = FALSE)
  sl = query(ae, "Phonetic == n")
  expect_true("tbl_df" %in% class(sl))
  Sys.chmod(path2db, mode = "755") # change back
  
  # cleanup
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  unlink(path2db, recursive = TRUE)
})

