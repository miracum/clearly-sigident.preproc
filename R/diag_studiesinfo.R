#' @title diag_studiesinfo
#'
#' @description Helper function to import studiesinfos for diagnostic
#'   studies from table
#'
#' @param tab A table containing the studiesinfo.
#' @param type A character string. Either "discovery" or "validation".
#'
#' @details The function takes a table as input and converts it to a
#'   studiesinfo list.
#'
#' @export
#'
diag_studiesinfo <- function(tab, type) {

  stopifnot(
    ncol(tab) >= 8,
    nrow(tab) >= 2,
    c("geo_id", "setid", "use_rawdata", "targetcolname", "targetlevelname",
      "controllevelname", "diagnostic_validation", "diagnostic_discovery") %in%
      colnames(tab),
    sum(tab[, get("use_rawdata")] %in% c(0, 1)) == nrow(tab),
    sum(tab[, get("diagnostic_validation")] %in% c(0, 1)) ==
      nrow(tab),
    sum(tab[, get("diagnostic_discovery")] %in% c(0, 1)) ==
      nrow(tab),
    type %in% c("discovery", "validation")
  )

  if (type == "discovery") {
    tab <- tab[get("diagnostic_discovery") == 1, ]
  } else if (type == "validation") {
    tab <- tab[get("diagnostic_validation") == 1, ]
  } else {
    stop("Wrong type")
  }

  stopifnot(nrow(tab) > 0)

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
                               studyname, get("controllevelname")],
      use_rawdata = tab[get("geo_id") ==
                          studyname, as.logical(as.numeric(
                            get("use_rawdata"))
                            )]
    )

    if (outlist[[studyname]]$targetlevelname == "NULL") {
      outlist[[studyname]]$targetlevelname <- NULL
    }
    if (outlist[[studyname]]$controllevelname == "NULL") {
      outlist[[studyname]]$controllevelname <- NULL
    }
  }

  return(outlist)
}
