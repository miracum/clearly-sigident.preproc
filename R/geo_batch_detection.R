geo_batch_detection <- function(mergeset,
                                batch) {
  outdat <-
    gPCA::gPCA.batchdetect(x = t(mergeset),
                           batch = batch,
                           center = FALSE)
  return(outdat)
}
