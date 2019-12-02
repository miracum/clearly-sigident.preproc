create_expressionset <- function(eset, idtype) {
  expr <- Biobase::exprs(eset)
  expr <- id_type(expr = expr,
                  eset = eset,
                  idtype = idtype)
  return(expr)
}
