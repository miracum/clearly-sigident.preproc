#' @title geo_create_diagnosisdesignbatch
#'
#' @description Helper function to create diagnosis, design and batch
#'
#' @param sample_metadata A data frame. The data frame holding the
#'   sample metadata.
#'
#' @export

geo_create_diagnosisdesignbatch <- function(sample_metadata) {


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
  # create design
  design <- stats::model.matrix(~ diagnosis)

  # create batch
  batch <- geo_create_batch(sample_metadata = sample_metadata)

  return(list(
    diagnosis = diagnosis,
    design = design,
    batch = batch
  ))
}

