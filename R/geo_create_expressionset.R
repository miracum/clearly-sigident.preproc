#' @title geo_create_expressionset
#'
#' @description Helper function to generate a Expression Set.
#'
#' @param eset The raw expression set, downloaded with the `GEOquery::getGEO`
#'   funciton.
#'
#' @inheritParams load_geo_data
#'
#' @export
#'
geo_create_expressionset <- function(eset, idtype) {

  stopifnot(
    idtype %in% c("affy", "entrez")
  )
  expr <- Biobase::exprs(eset)
  expr <- geo_id_type(expr = expr,
                      eset = eset,
                      idtype = idtype)
  return(expr)
}
