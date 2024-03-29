##' create emuRtrackdata object
##' 
##' Joins \code{\link{emuRsegs}} and \code{\link{trackdata}} objects
##' to create an \code{\link{emuRtrackdata}} object that is a sub-class of
##' a \code{\link{data.frame}} object. This object 
##' can be viewed as a flat version of a \code{\link{trackdata}} object that also 
##' contains all the information of a \code{\link{emuRsegs}} object. It is meant to
##' ease integration with other packages as it is based on the well known 
##' \code{\link{data.frame}} object.
##' @param sl seglist of class \code{\link{emuRsegs}}
##' @param td \code{\link{trackdata}} object generated from sl
##' @return emuRtrackdata object
##' @export
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # query emuDB (to get object of class emuRsegs)
##' sl = query(emuDBhandle = ae, 
##'            query = "Phonetic == i:")
##'            
##' # get formats for SEGMENTs in sl (to get object of class trackdata)
##' td = get_trackdata(emuDBhandle = ae, 
##'                    seglist = sl,
##'                    onTheFlyFunctionName = "forest")
##' 
##' # create emuRtrackdata object
##' create_emuRtrackdata(sl = sl, td = td)
##' 
##' }
create_emuRtrackdata <- function(sl, td){
  
  ########################
  # check parameters
  # check correct classes
  if((!inherits(sl, "emuRsegs") 
      & !inherits(sl, "tbl_df")) | !inherits(td, "trackdata")){
    stop(paste0("emuRtrackdata could not be created: sl is not of ",
                "class 'emuRsegs' or td arguments is not of class 'trackdata'"))
  }
  
  # check same number of items
  if(dim(td$index)[1] != nrow(sl)){
    stop(paste0("emuRtrackdata could not be created: td and sl objects don't ",
                "have the same number of elements (dim(td$index)[1] != nrow(sl))"))
  }
  
  
  nframes = 1 + apply(td$index, 1, diff)
  inds = rep(1:nrow(td), nframes)
  # expand seglist
  expSl = sl[inds,]
  
  times = tracktimes(td)
  start.time = rep(start(td), nframes)
  n.time = times - start.time
  rownames(td$data) = NULL
  resTmp = data.frame(sl_rowIdx = inds, expSl, times_orig = times, times_rel = n.time)
  # calculate normalized time (between 0-1)
  resTmp = resTmp %>% 
    dplyr::group_by(.data$sl_rowIdx) %>% 
    dplyr::mutate(times_norm = .data$times_rel / max(.data$times_rel))
  # remove class spectral to avoid usage of [] overide which affects indexing
  class(td$data) = "matrix"
  # add data
  res = data.frame(resTmp, td$data)
  
  class(res) <- c("emuRtrackdata", class(res))
  # set negative values in times_rel and times_norm to 0 
  # these can be caused by this sort of stuff (tracktimes() uses rownames() which are strings): 
  # number = 140.0811234234234123412341234
  # as.numeric(as.character(number)) == number # -> FALSE!
  res$times_rel[res$times_rel < 0] = 0
  res$times_norm[res$times_norm < 0] = 0
  return(res)
}

"check_emuRtrackdataColumns" <- function(td){
  
  # convert factors into characters
  td = td %>% dplyr::mutate_if(is.factor, as.character)
  
  # check if all columns of emuRsegs object are present
  emuRsegsNames = c("sl_rowIdx", "labels", "start", "end", 
                    "db_uuid", "session", 
                    "bundle", "start_item_id", "end_item_id", "level", "start_item_seq_idx",
                    "end_item_seq_idx", "type", "sample_start", "sample_end", "sample_rate")
  
  if(!all(emuRsegsNames %in% names(td))){
    stop(paste0("Not all emuRsegs columns are present in emuRtrackdata object, ",
                "hence it is not an emuRtrackdata object!"))
  }
  
  # check if all time columns are present
  timeColNames = c("times_orig", "times_rel", "times_norm")
  
  if(!all(emuRsegsNames %in% names(td))){
    stop(paste0("Not all time columns are present (times_orig, times_rel, times_norm) ",
                "in emuRtrackdata object, hence it is not an emuRtrackdata oject!"))
  }
  
  # check if every other column is of class numeric
  allColNames = c(emuRsegsNames, timeColNames)
  
  additional_cols = setdiff(names(td), allColNames)
  
  numericDataClasses = c("complex", "single", "double", "integer", "numeric")
  
  for(dc in additional_cols){
    if(!dc %in% c("attribute", "utts")){ # ignore waring for columns that are part of tibble/emuRtrackdata (no common to both)
      if(!class(td[[dc]]) %in% numericDataClasses){
        warning(paste0('Found additional column that is not of a number class ',
                       '("complex", "single", "double", "integer", "numeric"). Column name is: "', 
                       dc, '". The first entry of each segment is reduplicated to match the ',
                       'length of each normalized segment.'))
      }
    }
  }
  
}


##' Normalize length of segments contained in a \code{data.frame} like object returned by \code{\link{get_trackdata}}
##'
##' @param x data.frame like object that was generated by \code{\link{get_trackdata}} with 
##' the resultType set to either \code{emuRtrackdata} or \code{tibble}
##' @param N specify length of normalized segments (each segment in resulting
##' object will consist of \code{N} rows).
##' @param colNames character vector containing names of columns to normalize. If not set all 
##' data columns are normalized (T1-TN as well as other numeric columns).
##' @return data.frame like object containing the length normalized segments
##' @seealso \code{\link{emuRtrackdata} \link{emuRsegs}}
##' @export
"normalize_length" <-function(x, colNames = NULL, N = 21){
  
  nonDataColNames = c("sl_rowIdx", "labels", "start", "end", "utts", "db_uuid", "session", 
                      "bundle", "start_item_id", "end_item_id", "level", "start_item_seq_idx",
                      "end_item_seq_idx", "type", "sample_start", "sample_end", "sample_rate",
                      "times_orig", "times_rel", "times_norm")
  
  # check if object that was passed in has all the needed columns
  check_emuRtrackdataColumns(x)
  
  # extract data cols
  if(is.null(colNames)){
    additional_cols = setdiff(names(x), nonDataColNames)
  }else{
    if(all(colNames %in% names(x))){
      additional_cols = colNames
    }else{
      stop("Passed in column names don't exist in x")
    }
    
  }
  
  # look 4 repeats:
  if(any(duplicated(rle(x$sl_rowIdx)$values))){
    stop(paste0("found repeating sl_rowIdx sequences (e.g. c(1,1,1,2,2,2,1,1,1)\n ",
                "where 1 is repeated). This is not permitted! Please fix this in\n ", 
                "the passed in trackdata (== parameter 'x')\n"))
  }
  
  urowIdx = unique(x$sl_rowIdx)
  
  resLen = length(urowIdx) * N
  
  # preallocate resulting tibble
  res_tbl = tibble::tibble(sl_rowIdx = integer(resLen), 
                           labels = character(resLen), 
                           start = double(resLen), 
                           end = double(resLen), 
                           utts = character(resLen), 
                           db_uuid = character(resLen), 
                           session = character(resLen), 
                           bundle = character(resLen), 
                           start_item_id = integer(resLen), 
                           end_item_id = integer(resLen), 
                           level = integer(resLen), 
                           start_item_seq_idx = integer(resLen),
                           end_item_seq_idx = integer(resLen), 
                           type = integer(resLen), 
                           sample_start = integer(resLen), 
                           sample_end = integer(resLen), 
                           sample_rate = integer(resLen), 
                           times_orig = double(resLen), 
                           times_rel = double(resLen), 
                           times_norm = double(resLen))
  
  # add other columns that are not emuRsegsColNames, hence added columns
  for(colName in additional_cols){
    res_tbl[,colName] = NA # add empty column
    class(res_tbl[[colName]]) = class(x[[colName]]) # set col column class
  }
  
  res_list = list()
  
  for (i in unique(x$sl_rowIdx)){
    # get current segment and remove unwanted columns
    eRtd = x[x$sl_rowIdx == i, names(x) %in% c(nonDataColNames, additional_cols)]
    
    xynew = approx(eRtd$times_norm, eRtd$T1, n = N)
    # create data.frame of correct length (all relevant entries are replaced)
    # and fill with values of first row (only rel. for redundant columns such as sl_rowIdx, labels)
    eRtd.normtemp = tibble::tibble(eRtd[1,], .rows = N)
    eRtd.normtemp$times_norm = seq(0, 1, length.out = N) #xynew$x - use seq instead of xynew$x to avoid approx rounding issues
    # interpolate data columns
    for (name in additional_cols){
      # y = dplyr::pull(eRtd, name)
      y = eRtd[[name]]
      if(!inherits(y, "character")){
        eRtd.normtemp[,name] = approx(eRtd$times_norm, y, n = N)$y
      }else{
        eRtd.normtemp[,name]  = y[1] # use first element to fill up vector (R's recycling)
      }
    }
    # recalculate times_orig & rimes_rel
    eRtd.normtemp$times_orig = seq(unique(eRtd.normtemp$start), 
                                   unique(eRtd.normtemp$end),
                                   length.out = N)
    eRtd.normtemp$times_rel = seq(0,
                                  unique(eRtd.normtemp$end) - unique(eRtd.normtemp$start), 
                                  length.out = N)
    
    res_list[[i]] = eRtd.normtemp
    
  }
  
  res_tbl = do.call(rbind, res_list)

  return(res_tbl)
}


##' Print emuRtrackdata object
##' @param x object to print
##' @param ... additional params
##' @export
"print.emuRtrackdata" <-  function(x, ...) 
{
  trackNames = names(x)[stringr::str_detect(names(x), 'T.*')]
  printX = '[.data.frame'(x, c('sl_rowIdx', 
                               'labels', 
                               'start', 
                               'end', 
                               'session', 
                               'bundle', 
                               'level', 
                               'type', 
                               'times_orig', 
                               'times_rel', 
                               'times_norm', 
                               trackNames))
  print.data.frame(printX, ...)
}

##' convert tracks of a tibble trackdata object to the long form
##' 
##' Converts a trackdata tibble object of the form (==wide):
##' \tabular{lllllll}{
##' sl_rowIdx \tab ... \tab T1 \tab T2 \tab T3 \tab ... \tab TN\cr
##' 1 \tab ... \tab T1_value \tab T2_value \tab T3_value \tab ... \tab TN_value
##' }
##' to its long form equivalent:
##' \tabular{llll}{
##' sl_rowIdx \tab ... \tab track_name \tab track_value \cr
##' 1 \tab ... \tab T1 \tab T1_value \cr
##' 1 \tab ... \tab T2 \tab T2_value \cr
##' 1 \tab ... \tab T3 \tab T3_value \cr
##' ... \tab ... \tab ... \tab ... \cr
##' 1 \tab ... \tab TN \tab TN_value \cr
##' }
##' 
##' @param td wide form trackdata tibble object
##' @param calcFreqs calculate an additional column containing 
##' frequency values from 0-nyquist frequency that match T1-TN (can be quite useful for spectral data)
##' @return long form trackdata tibble object
##' @export
convert_wideToLong <- function(td, calcFreqs = FALSE){
  
  # get col idx values of tracks (T1-TN)
  tracks_colIdx = grep(pattern = "^T[0-9]+$", names(td))
  
  tracks_long = dplyr::ungroup(td) %>% 
    tidyr::gather(key = "track_name", 
                  value = "track_value", 
                  min(tracks_colIdx):max(tracks_colIdx), 
                  convert = TRUE) %>% 
    dplyr::mutate(freq = as.numeric(substring(.data$track_name, 2))) %>% 
    dplyr::group_by(.data$sl_rowIdx)  %>%
    dplyr::arrange(.data$freq, .by_group = TRUE)
  
  # calc freq if calcFreqs = FALSE otherwise drop column
  if(calcFreqs) {
    tracks_long = tracks_long %>% 
      dplyr::mutate(freq = rep(seq(0, 
                                   (unique(.data$sample_rate) / 2), 
                                   length.out = length(tracks_colIdx)), 
                               each = dplyr::n() / length(tracks_colIdx)))
  } else{
    tracks_long = tracks_long %>%
      dplyr::select(-"freq")
  } 
  
  return(dplyr::ungroup(tracks_long))
}

#######################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_emuRtrackdata.R')
# test_file('tests/testthat/test_zzz_cleanUp.R')
