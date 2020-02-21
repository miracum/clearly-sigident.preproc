geo_use_raw_data <- function(eset, name, datadir) {

  rawdir <- paste0(datadir, "/rawdata/")
  dir.create(rawdir)
  exdir <- paste0(rawdir, "/celfiles/")
  # remove preexisting exdir
  cf <- list.files(exdir)
  if (!identical(cf, character(0))) {
    file.remove(paste0(exdir, "/", cf))
  }
  unlink(exdir, recursive = T, force = T)
  # create exdir
  dir.create(exdir)

  if (!file.exists(paste0(rawdir, name, "/", name, "_RAW.tar"))) {
    GEOquery::getGEOSuppFiles(
      GEO = name,
      makeDirectory = TRUE,
      baseDir = rawdir,
      filter_regex = "[.tar]$"
    )
  }

  tar_file <- list.files(paste0(rawdir, name), pattern = "[.tar]$")

  stopifnot(length(tar_file) == 1)

  utils::untar(paste0(rawdir, name, "/", tar_file), exdir = exdir)

  cels <- list.files(exdir, pattern = "[gz]$")

  tryCatch({
    sapply(paste(exdir, cels, sep = "/"), GEOquery::gunzip,
           overwrite = T,
           remove = F)
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

  celfiles <- list.files(exdir, pattern = "CEL$")
  # path must be relative for justGCRMA to work!
  eset_c <- gcrma::justGCRMA(
    filenames = celfiles,
    fast = TRUE,
    celfile.path = exdir
  )
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
