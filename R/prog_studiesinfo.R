#' @title prog_studiesinfo
#'
#' @description Helper function to import studiesinfos for prognostic
#'   studies from table
#'
#' @details The function takes a table as input and converts it to a
#'   studiesinfo list.
#'
#' @inheritParams diag_studiesinfo
#'
#' @export
#'
prog_studiesinfo <- function(tab, type) {

  stopifnot(
    ncol(tab) >= 7,
    nrow(tab) >= 2,
    c("geo_id", "setid", "use_rawdata", "targetcolname", "targetlevelname",
      "controllevelname", "diag_validation_study", "prog_validation_study",
      "timecolname", "statuscolname", "statuslevel_alive",
      "statuslevel_deceased", "status_na") %in% colnames(tab),
    sum(tab[, get("use_rawdata")] %in% c(0, 1)) == nrow(tab),
    sum(tab[, get("prog_validation_study")] %in% c(-1, 0, 1, "")) ==
      nrow(tab),
    type %in% c("discovery", "validation")
  )

  if (type == "discovery") {
    tab <- tab[get("prog_validation_study") == 0, ]
  } else if (type == "validation") {
    tab <- tab[get("prog_validation_study") == 1, ]
  } else {
    stop("Wrong type")
  }

  # init outlist
  outlist <- list()

  for (studyname in tab[, get("geo_id")]) {
    stat_na <- tab[get("geo_id") ==
                     studyname, get("status_na")]

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
                            )],
      timecol = tab[get("geo_id") ==
                      studyname, get("timecolname")],
      status = list(
        statuscol = tab[get("geo_id") ==
                          studyname, get("statuscolname")],
        levels = list(
          alive = tab[get("geo_id") ==
                        studyname, get("statuslevel_alive")],
          deceased = tab[get("geo_id") ==
                           studyname, get("statuslevel_deceased")],
          na = ifelse(stat_na == "NA" || stat_na == "", NA, stat_na)
        )
      )
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
