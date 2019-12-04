geo_create_diagnosis <- function(vector, controlname, targetname) {
  diag <- as.vector(vector)

  diagnosis <- gsub(controlname, "0", diag)
  diagnosis <- gsub(targetname, "1", diagnosis)

  outdat <- as.integer(diagnosis)
  return(outdat)
}
