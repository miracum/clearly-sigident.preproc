geo_id_type <- function(expr, eset, idtype) {
  stopifnot(idtype %in% c("entrez", "affy"))
  if (idtype == "entrez") {
    rownames(expr) <- as.character(eset@featureData@data$ENTREZ_GENE_ID)
    # remove empty characters and replicates in EntrezIDs
    expr <- expr[rownames(expr) != "", ]
    expr <- expr[!duplicated(rownames(expr)), ]
  } else if (idtype == "affy") {
    rownames(expr) <- as.character(eset@featureData@data$ID)
  }
  return(expr)
}
