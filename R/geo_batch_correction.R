geo_batch_correction <- function(mergedset,
                                 batch,
                                 design,
                                 idtype) {
  # generate data frame with expression values and model matrix
  # regardarding diagnosis
  #

  df <- mergedset@assayData$exprs
  edata <- sva::ComBat(
    df,
    batch = batch,
    mod = design,
    par.prior = T
  )
  edata <- geo_id_type(expr = edata,
                       eset = mergedset,
                       idtype = idtype)
  return(edata)
}
