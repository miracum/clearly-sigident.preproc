load_eset <- function(name,
                      datadir,
                      targetcolname,
                      targetcol,
                      targetname,
                      controlname,
                      targetlevelname,
                      controllevelname,
                      use_rawdata = FALSE,
                      setid = 1) {
  # original GEO data
  eset <- GEOquery::getGEO(name,
                           destdir = datadir)[[setid]]

  # rename targetcol
  colnames(Biobase::pData(eset))[which(
    colnames(Biobase::pData(eset)) == targetcolname
  )] <- targetcol

  # rename levels of targetcol
  if (!is.null(targetlevelname)) {
    levelnames <- c(targetname, controlname)
    names(levelnames) <- c(targetlevelname,
                           controllevelname)
    eset[[targetcol]] <-
      plyr::revalue(eset[[targetcol]], levelnames)
  }

  if (isTRUE(use_rawdata)) {

    eset <- use_raw_data(
      eset = eset,
      name = name,
      datadir = datadir
    )
  }
  return(eset)
}
