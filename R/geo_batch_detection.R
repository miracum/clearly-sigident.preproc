#' @title geo_batch_detection
#'
#' @description Helper function to detect batches
#'
#' @param mergeset A matrix of merged expression sets (rows = genes,
#'   columns = samples).
#' @param batch Takes the results from \code{create_batch()} as input.
#'
geo_batch_detection <- function(mergeset,
                                batch) {
  outdat <-
    gPCA::gPCA.batchdetect(x = t(mergeset),
                           batch = batch,
                           center = FALSE)
  return(outdat)
}
