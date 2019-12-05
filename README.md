# sigident.preproc (!!! currently under development !!!)

<!-- badges: start -->
[![pipeline status](https://gitlab.miracum.org/clearly/sigident.preproc/badges/master/pipeline.svg)](https://gitlab.miracum.org/clearly/sigident.preproc/commits/master)
[![coverage report](https://gitlab.miracum.org/clearly/sigident.preproc/badges/master/coverage.svg)](https://gitlab.miracum.org/clearly/sigident.preproc/commits/master)
<!-- badges: end -->

This is the repository of the R package `sigident.preproc`. It provides data preprocessing functionalities in order to preprare datasets for further usage with the `sigident` R package: [https://gitlab.miracum.org/clearly/sigident](https://gitlab.miracum.org/clearly/sigident)

# Overview 

The preprocessing includes the following steps:  
- GEO  
  + downloading of the specified datasets from the [GEO database](https://www.ncbi.nlm.nih.gov/geo/)  
  + optional: downloading the raw CEL files and subsequent GCRMA normalization  
  + batch effect detection and batch effect correction  
  + visualization of batch effects  

All created objects will be 

Currently supported input file formats are:

- GEO data
  + Platform GPL570 [HG-U133_Plus_2] Affymetrix Human Genome U133 Plus 2.0 Array

# Installation

You can install *sigident.preproc* with the following commands in R:

``` r
options('repos' = 'https://ftp.fau.de/cran/')
install.packages("devtools")
devtools::install_git("https://gitlab.miracum.org/clearly/sigident.preproc.git")
```

# Example: download datasets from GEO

First, one has to define some needed variables and create some directories.

```r
library(sigident.preproc)

# define datadir
datadir <- "./geodata/data/"
dir.create(datadir)

# define plotdir
plotdir <- "./plots/"
dir.create(plotdir)

# define idtype
idtype <- "affy"
```

The, a list needs to be defined, that contains a representation of the studies metadata. In order to get the information needed to fill this list, the dataset has probably to be downloaded and the mapping information has to be extracted manually.

```r
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
```

Then, the function `load_geo_data` can be executed in order to load and preprocess the specified studies.

```r
load_geo_data(studiesinfo = studiesinfo,
              datadir = datadir,
              plotdir = plotdir,
              idtype = idtype) 
```

All downloaded datasets and resulting objects are assigned to the global environment and are suitable to be used in the subsequent analyses implemented in the R package `sigident`.

Please view the package's vignette to see a detailled description how to prepare datasets in order to be suitable for usage with the `sigident` package.

Since the building the package vignette takes rather long (~ 20 min.), we provide the already built vignettes in [this repository](https://gitlab.miracum.org/clearly/sigident_vignettes). 

# Caution 

The *sigident.preproc* package is under active development and not on CRAN yet - this means, that from time to time, the API can break, due to extending and modifying its functionality. It can also happen, that previoulsy included functions and/or function arguments are no longer supported. 
However, a detailed package vignette will be provided alongside with every major change in order to describe the currently supported workflow.
