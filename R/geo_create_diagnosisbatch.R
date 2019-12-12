#' @title geo_create_diagnosisbatch
#'
#' @description Helper function to create diagnosis and batch
#'
#' @param sample_metadata A data frame. The data frame holding the
#'   sample metadata.
#'
#' @export

geo_create_diagnosisbatch <- function(sample_metadata) {


  stopifnot(
    is.data.frame(sample_metadata)
  )

  targetcol <- "target"
  controlname <- "Control"
  targetname <- "Target"

  discoverydata <- sample_metadata[[targetcol]]

  # create diagnosis
  diagnosis <- geo_create_diagnosis(vector = discoverydata,
                                    controlname = controlname,
                                    targetname = targetname)

  # create batch
  batch <- geo_create_batch(sample_metadata = sample_metadata)

  return(list(
    diagnosis = diagnosis,
    batch = batch
  ))
}
