#' @title Load datasets from the GEO database
#'
#' @description Load datasets that are stored on the GEO databse
#'
#' @param studiesinfo A list that contains specifications
#'   on the studie's / studies' meta data.
#' @param datadir A character string. Path to the data-folder.
#'
#' @export

load_geo_data <- function(studiesinfo,
                          datadir) {

  stopifnot(
    is.list(studiesinfo),
    is.character(datadir)
  )

  targetcol <- "target"
  controlname <- "Control"
  targetname <- "Cancer"

  out_mergeset <- list()

  for (st in names(studiesinfo)) {
    stopifnot(
      is.character(studiesinfo[[st]]$targetcolname),
      is.character(studiesinfo[[st]]$targetlevelname),
      is.character(studiesinfo[[st]]$controllevelname),
      is.logical(studiesinfo[[st]]$use_rawdata) ||
        is.null(studiesinfo[[st]]$use_rawdata),
      is.numeric(studiesinfo[[st]]$setid)
    )

    use_raw <- ifelse(
      is.null(studiesinfo[[st]]$use_rawdata),
      FALSE,
      TRUE
    )

    eset <- load_eset(
      name = st,
      datadir = datadir,
      targetcolname = studiesinfo[[st]]$targetcolname,
      targetcol = targetcol,
      targetname = targetname,
      controlname = controlname,
      targetlevelname = studiesinfo[[st]]$targetlevelname,
      controllevelname = studiesinfo[[st]]$controllevelname,
      use_rawdata = use_raw,
      setid = studiesinfo[[st]]$setid
    )


    global_env_hack(
      key = st,
      val = eset,
      pos = 1L
    )

    vec <- colnames(
      Biobase::pData(eset)
    )[which(colnames(
      Biobase::pData(eset)
    ) %in% c("title", "geo_accession", targetcol))]
    p_new <- Biobase::pData(eset)[, vec]

    eset_append <- eset
    Biobase::pData(eset_append) = p_new
    out_mergeset <- c(out_mergeset, eset_append)
    rm(eset_append)
  }

  # extract and assign sample metadata
  sample_metadata <- extract_sample_metadata(
    studiesinfo = studiesinfo,
    targetcol = targetcol
  )

  global_env_hack(
    key = "sample_metadata",
    val = sample_metadata,
    pos = 1L
  )

  # merge sets
  mergedset <- merge(out_mergeset)

  global_env_hack(
    key = "mergedset",
    val = mergedset,
    pos = 1L
  )
  invisible(gc)
}
