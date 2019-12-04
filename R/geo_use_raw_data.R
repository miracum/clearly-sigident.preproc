geo_use_raw_data <- function(eset, name, datadir) {

  rawdir <- paste0(datadir, "rawdata/")
  dir.create(rawdir)
  exdir <- paste0(rawdir, "celfiles/")
  dir.create(exdir)

  GEOquery::getGEOSuppFiles(name, baseDir = rawdir)

  tar_file <- list.files(paste0(rawdir, name), pattern = ".tar$")

  stopifnot(length(tar_file) == 1)

  utils::untar(paste0(rawdir, name, "/", tar_file), exdir = exdir)

  cels <- list.files(exdir, pattern = "[gz]")

  tryCatch({
    sapply(paste(exdir, cels, sep = "/"), GEOquery::gunzip)
    cmd <-
      paste0("bash -c 'ls ",
             exdir,
             "/*.CEL > ",
             exdir,
             "/phenodata.txt'")
    system(cmd)
  }, error = function(e) {
    print(e)
  })

  cels <- list.files(exdir, pattern = "CEL$")

  celfiles <- paste0(exdir, cels)
  eset_c <- gcrma::justGCRMA(filenames = celfiles, fast = TRUE)
  gc()

  p_data <- Biobase::pData(eset)
  f_data <- Biobase::fData(eset)
  Biobase::pData(eset_c) <- p_data
  Biobase::fData(eset_c) <- f_data
  colnames(Biobase::exprs(eset_c)) <-
    colnames(Biobase::exprs(eset))
  Biobase::annotation(eset_c) <- Biobase::annotation(eset)

  return(eset_c)
}
