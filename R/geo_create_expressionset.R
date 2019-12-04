geo_create_expressionset <- function(eset, idtype) {
  expr <- Biobase::exprs(eset)
  expr <- geo_id_type(expr = expr,
                      eset = eset,
                      idtype = idtype)
  return(expr)
}
