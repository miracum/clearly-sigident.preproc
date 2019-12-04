#' @title geo_load_eset
#'
#' @description Helper function to load expression sets from GEO.
#'
#' @param name A character string. The name of the GEO study.
#' @param datadir A character string. The path to the directory to store
#'   the downloaded GEO datasets.
#' @param targetcolname A character string. The name of the column in the
#'   GEO expression set's pheno data, holding the variable of interest
#'   (mapping information).
#' @param targetlevelname A character string. The name of the the targets in
#'   `targetcolname` (mapping information).
#' @param controllevelname A character string. The name of the the controls in
#'   `targetcolname` (mapping information).
#' @param targetcol A character string. The columname of `sample_metadata`
#'   holding the targets (default: "target"; caution: this should not
#'   be changed!).
#' @param targetname A character string. Name of the the targets,
#'   specified in the 'target' column of `sample_metadata` (default: "Target";
#'   caution: this should not be changed!).
#' @param controlname A character string. Name of the the controls,
#'   specified in the 'target' column of `sample_metadata` (default: "Control";
#'   caution: this should not be changed!).
#' @param use_rawdata A logical. Indicates, if raw the data is downloaded
#'   in CEL file format, uncompressed and subsequently normalized with a GCRMA
#'   normalization, or if the already normalized datasets from GEO are
#'   downloaded (default: FALSE).
#' @param setid A integer. Indicates, which set should be used by the software
#'   for subsequent analyses (default: 1).
#'
#' @export

geo_load_eset <- function(name,
                          datadir,
                          targetcolname,
                          targetlevelname,
                          controllevelname,
                          targetcol,
                          targetname,
                          controlname,
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

    eset <- geo_use_raw_data(
      eset = eset,
      name = name,
      datadir = datadir
    )
  }
  return(eset)
}
