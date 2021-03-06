#' @title geo_create_batch
#'
#' @description Helper function to create batch
#'
#' @inheritParams geo_create_diagnosisbatch
#'
geo_create_batch <- function(sample_metadata) {
  studylist <- list()

  for (d in unique(sample_metadata$study)) {
    studylist[[d]] <- sum(sample_metadata$study == d)
  }

  x <- seq_len(length(studylist))

  times <- sapply(names(studylist), function(n) {
    studylist[[n]]
  }, USE.NAMES = F)
  outdat <- rep(x = x, times = times)
  return(outdat)
}
