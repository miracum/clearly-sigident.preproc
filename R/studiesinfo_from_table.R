#' @title studiesinfo_from_table
#'
#' @description Helper function to import studiesinfo from table
#'
#' @param tab A table containing the studiesinfo.
#'
#' @details The function takes a table as input and converts it to a
#'   studiesinfo list.
#'
#' @export
#'
studiesinfo_from_table <- function(tab) {

  stopifnot(
    ncol(tab) >= 4,
    nrow(tab) >= 2,
    c("geo_id",
      "targetcolname",
      "targetlevelname",
      "controllevelname") %in% colnames(tab)
  )

  # init outlist
  outlist <- list()

  for (studyname in tab[, get("geo_id")]) {
    outlist[[studyname]] <- list(
      setid = tab[get("geo_id") ==
                    studyname, get("setid")],
      targetcolname = tab[get("geo_id") ==
                            studyname, get("targetcolname")],
      targetlevelname = tab[get("geo_id") ==
                              studyname, get("targetlevelname")],
      controllevelname = tab[get("geo_id") ==
                               studyname, get("controllevelname")]
    )
  }

  return(outlist)
}
