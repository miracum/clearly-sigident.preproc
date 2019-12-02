extract_sample_metadata <- function(studiesinfo, targetcol) {

  sample_metadata <- data.table::data.table()

  for (st in names(studiesinfo)) {
    sample_metadata <- rbind(
      sample_metadata,
      cbind(
        study = st,
        sample = eval(parse(text = st),
                      envir = 1L)@phenoData@data[, "geo_accession"],
        target = as.character(
          eval(parse(text = st),
               envir = 1L)@phenoData@data[, targetcol]
        )
      )
    )
  }
  return(sample_metadata)
}
