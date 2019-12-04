geo_create_diagnosisdesignbatch <- function(sample_metadata,
                                            controlname,
                                            targetname,
                                            targetcol) {
  stopifnot(
    is.data.frame(sample_metadata),
    is.character(controlname),
    is.character(targetname),
    is.character(targetcol)
  )

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

