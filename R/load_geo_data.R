#' @title Load datasets from the GEO database
#'
#' @description Load datasets that are stored on the GEO databse
#'
#' @param studiesinfo A list that contains specifications
#'   on the studie's / studies' meta data.
#' @param datadir A character string. Path to the data-folder.
#' @param plotdir A character string. Path to the folder to store resulting
#'   plots. Default: "./plots/".
#' @param idtype A character string. The type of ID used to name the
#'   genes. One of 'entrez' or 'affy' intended to use either entrez IDs or
#'   affy IDs. Caution: when using entrez IDs, missing and duplicated IDs
#'   are being removed!
#' @param visualize_batches A logical. Indicates, if batch effects and batch
#'   effect removals should be plotted (default: TRUE).
#'
#' @details The functions writes objects to the global environment, including
#'   the expression sets of the studies specified in `studiesinfo` - the
#'   here used study names (list keys) are used for the naming in the global
#'   environment. Furthermore, `mergedset`, a large expression set containing
#'   all studies, is also written to the global environment. Please note,
#'   that this set contains the raw expression data with batch effects.
#'   Bbatch effects are detected, removed and provided with the object
#'   `mergeset`, a matrix containing batch corrected expression data with
#'   genes in the rows and samples in the columns. `sample_metadata` is a
#'   data.frame, which holds information on the samples which are included
#'   in the studies, including if they are a "Target" or a "Control".
#'   `diagnosis` (a binary coding of the target variable), `design` () and
#'   `batch` () are also provided to the global environment to be used with
#'   other functions of the `sigident` R package.
#'
#' @seealso sigident
#'
#' @import data.table
#'
#' @references \url{https://gitlab.miracum.org/clearly/sigident}
#'
#' @export

load_geo_data <- function(studiesinfo,
                          datadir,
                          plotdir = "./plots/",
                          idtype,
                          visualize_batches = TRUE) {

  stopifnot(
    is.list(studiesinfo),
    is.character(datadir),
    idtype %in% c("affy", "entrez"),
    is.logical(visualize_batches)
  )

  targetcol <- "target"
  controlname <- "Control"
  targetname <- "Target"

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

    # setd use_raw, if not provided with function arguments
    use_raw <- ifelse(
      is.null(studiesinfo[[st]]$use_rawdata),
      FALSE,
      TRUE
    )

    # load eset
    eset <- geo_load_eset(
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

    if (visualize_batches) {
      # plot batch effects of single eset
      filename <- paste0(plotdir, "/", st, "_batch_effect_boxplot.jpg")
      batch_effect_boxplot(eset = geo_create_expressionset(eset, idtype),
                           plot_title = paste0(st, " before batch correction"),
                           filename = filename)
    }

    # set vector of pheno data variables of interest for our merging
    vec <- colnames(
      Biobase::pData(eset)
    )[which(colnames(
      Biobase::pData(eset)
    ) %in% c("title", "geo_accession", targetcol))]

    # reduce phenodata to variables of interest
    p_new <- Biobase::pData(eset)[, vec]

    # temporarily store eset
    eset_append <- eset

    # overwrite pheno data
    Biobase::pData(eset_append) = p_new

    # append eset to out_mergeset
    out_mergeset <- c(out_mergeset, eset_append)
    rm(eset_append)
  }

  # extract and assign sample metadata
  sample_metadata <- geo_extract_sample_metadata(
    studiesinfo = studiesinfo,
    targetcol = targetcol
  )
  # assign sample_metadata to the global env
  global_env_hack(
    key = "sample_metadata",
    val = sample_metadata,
    pos = 1L
  )

  # merge sets
  mergedset <- geo_merge(out_mergeset)
  # assign mergedset to the global env
  global_env_hack(
    key = "mergedset",
    val = mergedset,
    pos = 1L
  )

  if (visualize_batches) {
    # plot batch effects of mergedset (before batch correction)
    filename <- paste0(plotdir, "/Merged_before_batch_effect_boxplot.jpg")
    batch_effect_boxplot(eset = mergedset@assayData$exprs,
                         plot_title = "Merged data before batch correction",
                         filename = filename)
  }

  # perform batch correction
  # conducting gPCA for batch effect detection
  DF <- mergedset@assayData$exprs

  # define batches with number of samples
  batch <- geo_create_batch(sample_metadata = sample_metadata)

  if (visualize_batches) {
    gPCA_before <- geo_batch_detection(mergeset = DF,
                                       batch = batch)
    filename <- paste0(plotdir, "PCplot_before.png")
    plot_batchplot(correction_obj = gPCA_before,
                   filename = filename,
                   time = "before")
  }
  rm(DF)

  # generate list dd with diagnosis and design
  dd <- geo_create_diagnosisdesignbatch(
    sample_metadata = sample_metadata
  )

  diagnosis <- dd$diagnosis
  # assign diagnosis to the global env
  global_env_hack(
    key = "diagnosis",
    val = diagnosis,
    pos = 1L
  )

  design <- dd$design
  # assign design to the global env
  global_env_hack(
    key = "design",
    val = design,
    pos = 1L
  )

  batch <- dd$batch
  # assign batch to the global env
  global_env_hack(
    key = "batch",
    val = batch,
    pos = 1L
  )

  mergeset <- geo_batch_correction(mergedset = mergedset,
                                   batch = batch,
                                   design = design,
                                   idtype = idtype)
  # assign mergeset to the global env
  global_env_hack(
    key = "mergeset",
    val = mergeset,
    pos = 1L
  )

  if (visualize_batches) {
    # plot batch effects of mergeset (after batch correction)
    filename <- paste0(plotdir, "/Merged_after_batch_effect_boxplot.jpg")
    batch_effect_boxplot(eset = mergeset,
                         plot_title = "Merged data after batch correction",
                         filename = filename)

    gPCA_after <- geo_batch_detection(mergeset = mergeset,
                                      batch = batch)
    filename <- paste0(plotdir, "PCplot_after.png")
    plot_batchplot(correction_obj = gPCA_after,
                   filename = filename,
                   time = "after")
    rm(gPCA_after, gPCA_before, filename)
  }

  # perform garbage collection
  invisible(gc())
}
