merge <- function(esetlist) {
  # set1 = first eset in list, will be overwritten by combined esets
  set1 <- esetlist[[1]]

  for (i in 2:length(esetlist)) {
    # loop combines esets one by one

    set2 <- esetlist[[i]] # (selecting the next eset)
    # Expression-Matrices
    e1 <- Biobase::exprs(set1)
    e2 <- Biobase::exprs(set2)
    # get list of all overlapping genes
    # overlap is used to selectonly overlapping genes -> genes that are only
    # present in one eset will be discarded
    overlap <- sort(intersect(rownames(e1), rownames(e2)))
    # --> allows for combination of different chips (with differing feature
    # numbers), as long as the IDs are mapped
    # Merging ExpressionSets
    #
    # Generate fData
    f_data_new <- Biobase::fData(set1)[overlap, ]

    # Generate pData
    p1 <- Biobase::pData(set1)
    p2 <- Biobase::pData(set2)

    # pData is bound rowwise, therefore sample-order is unaffected
    p_data_new <- rbind(p1, p2)
    # Annotation
    # Merge expression data

    # expression matrices are bound columnwise, therefor
    # sample-order is unaffected
    e_new <- cbind(e1[overlap, ], e2[overlap, ])

    # Rebuild eset
    # new eset has to build from scratch; cant just be appended
    # (like insilicomerging tried to do)
    set1 <- methods::new("ExpressionSet", exprs = e_new)
    Biobase::pData(set1) <- p_data_new
    Biobase::fData(set1) <- f_data_new
  }

  # overwriting set1 with newly combined eset, then jumping back in the loop
  eset_new <- set1
  # loop is finished after combining all esets; result is returned here
  return(eset_new)
}
