#' @title geo_create_diagnosis
#'
#' @description Helper function to transform the target variable into a
#'   binary numeric diagnosis variable.
#'
#' @param vector A vector of characters. The target variable, consisting of
#'   two levels, named `controlname` and `targetname`.
#' @param controlname A character string. Name of the the controls,
#'   specified in the 'target' column of `sample_metadata` (default: Control).
#' @param targetname A character string. Name of the the targets, specified
#'   in the 'target' column of `sample_metadata` (default: Target).
#'
#' @export
#'
geo_create_diagnosis <- function(vector, controlname, targetname) {
  diag <- as.vector(vector)

  levelnames <- c(0, 1)
  names(levelnames) <- c(controlname, targetname)

  diagnosis <- plyr::revalue(diag, levelnames)
  outdat <- as.numeric(diagnosis)

  return(outdat)
}
