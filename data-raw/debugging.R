
# define plotdir
plotdir <- "./temp_files_sigident/plots/"
datadir <- "./temp_files_sigident/datadir/"

# # define datadir
# maindir <- "./geodata/"
# datadir <- paste0(maindir, "data/")
# dir.create(maindir)
dir.create(datadir)
#
# # define plotdir
# plotdir <- "./plots/"
dir.create(plotdir)

# define idtype
idtype <- "affy"

viz_batch_boxp = F
viz_batch_gpca = F

# test studiesinfo
# studiesinfo <- list(
#   "GSE18842" = list(
#     setid = 1,
#     targetcolname = "source_name_ch1",
#     targetlevelname = NULL,
#     controllevelname = "Human Lung Control|||Human Lung Tumor"
#   ),
#
#   "GSE19804" = list(
#     setid = 1,
#     targetcolname = "source_name_ch1",
#     targetlevelname = NULL,
#     controllevelname = "frozen tissue of adjacent normal|||frozen tissue of primary tumor"
#   ),
#
#   "GSE19188" = list(
#     setid = 1,
#     targetcolname = "characteristics_ch1",
#     targetlevelname = "tissue type: healthy|||tissue type: tumor",
#     controllevelname = NULL,
#     use_rawdata = TRUE
#   )
# )

studiesinfo <- list(
  "GSE18842" = list(
    setid = 1,
    targetcolname = "source_name_ch1",
    targetlevelname = "Human Lung Tumor",
    controllevelname = "Human Lung Control"
  ),

  "GSE19804" = list(
    setid = 1,
    targetcolname = "source_name_ch1",
    targetlevelname = "frozen tissue of primary tumor",
    controllevelname = "frozen tissue of adjacent normal"
  ),

  "GSE19188" = list(
    setid = 1,
    targetcolname = "characteristics_ch1",
    controllevelname = "tissue type: healthy",
    targetlevelname = "tissue type: tumor",
    use_rawdata = TRUE
  )
)

studiesinfo$GSE19188$use_rawdata <- F

load_geo_data(studiesinfo = studiesinfo,
              datadir = datadir,
              plotdir = plotdir,
              idtype = idtype,
              viz_batch_boxp = F,
              viz_batch_gpca = F)
