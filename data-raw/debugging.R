# initialize filePath:
filePath <- tempdir()

# define datadir
maindir <- "./geodata/"
datadir <- paste0(maindir, "data/")
dir.create(maindir)
dir.create(datadir)

# define plotdir
plotdir <- "./plots/"
dir.create(plotdir)

# define idtype
idtype <- "affy"

visualize_batches = FALSE

# test studiesinfo
studiesinfo <- list(
  "GSE18842" = list(
    setid = 1,
    targetcolname = "source_name_ch1",
    targetlevelname = NULL,
    controllevelname = "Human Lung Control|||Human Lung Tumor"
  ),

  "GSE19804" = list(
    setid = 1,
    targetcolname = "source_name_ch1",
    targetlevelname = NULL,
    controllevelname = "frozen tissue of adjacent normal|||frozen tissue of primary tumor"
  ),

  "GSE19188" = list(
    setid = 1,
    targetcolname = "characteristics_ch1",
    targetlevelname = "tissue type: healthy|||tissue type: tumor",
    controllevelname = NULL,
    use_rawdata = TRUE
  )
)

load_geo_data(studiesinfo = studiesinfo,
              datadir = datadir,
              plotdir = plotdir,
              idtype = idtype,
              visualize_batches = FALSE)
