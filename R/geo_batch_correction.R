#' @title geo_batch_correction
#'
#' @description Helper function correcting for batch effects and mapping
#'   affy probes to Entrez IDs
#'
#' @param mergedset A large Expression Set. The output of the function
#'   `geo_merge`. Please note, that mergedset holds data, which are not
#'   yet batch corrected.
#' @param diagnosis A vector of integers, holding the binary outcome variable
#'   (0 = "Control", 1 = "Target").
#'
#' @details This function takes a Bioconductor's ExpressionSet class (the
#'   output of the function `geo_merge`) and outputs a batch corrected
#'   matrix containing expression data. In order to correct for occurring
#'   batch effects and other unwanted variation in high-throughput
#'   experiments the `ComBat` function from the sva package is conducted.
#'   The affy probes are mapped to their Entrez IDs. Thereby, empty and
#'   replicated character strings are removed.
#'
#' @inheritParams geo_batch_detection
#' @inheritParams load_geo_data
#'
#' @references W.E. Johnson, C. Li, and A. Rabinovic. Adjusting batch effects
#'   in microarray data using empirical bayes methods. Biostatistics,
#'   8(1):118–127, 2007. Jeffrey T. Leek, W. Evan Johnson, Hilary S. Parker,
#'   Elana J. Fertig, Andrew E. Jaffe, John D. Storey, Yuqing Zhang and
#'   Leonardo Collado Torres (2019). sva: Surrogate Variable Analysis.
#'   R package version 3.30.1.
#'
geo_batch_correction <- function(mergedset,
                                 batch,
                                 diagnosis,
                                 idtype) {
  # generate data frame with expression values and model matrix
  # regardarding diagnosis
  #

  # reduce assay Data to hold only cases of remaining cases
  df <- mergedset@assayData$exprs[, colnames(mergedset@assayData$exprs) %in%
                                    mergedset@phenoData$geo_accession]
  edata <- tryCatch(
    expr = {
      message("\nTrying batch effect correction with covariates\n")
      edata <- sva::ComBat(
        df,
        batch = batch,
        mod = stats::model.matrix(~ diagnosis),
        par.prior = T
      )
      edata
    }, error = function(e) {
      message(e)
      message(paste0("\nERROR: performing now batch effect correction ",
                     "without covariates\n"))
      edata <- sva::ComBat(
        df,
        batch = batch,
        par.prior = T
      )
      edata
    }, finally = function(f) {
      return(edata)
    }
  )
  edata <- geo_id_type(expr = edata,
                       eset = mergedset,
                       idtype = idtype)
  return(edata)
}
