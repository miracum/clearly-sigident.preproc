context("lints")

if (dir.exists("../../00_pkg_src")) {
  prefix <- "../../00_pkg_src/sigident.preproc/"
} else if (dir.exists("../../R")) {
  prefix <- "../../"
} else if (dir.exists("./R")) {
  prefix <- "./"
}

test_that(
  desc = "test lints",
  code = {
    lintlist <- list(
      "R" = list(
        "geo_batch_correction.R" = NULL,
        "geo_batch_detection.R" = NULL,
        "geo_create_batch.R" = NULL,
        "geo_create_diagnosis.R" = NULL,
        "geo_create_diagnosisbatch.R" = NULL,
        "geo_create_expressionset.R" = NULL,
        "geo_extract_sample_metadata.R" = NULL,
        "geo_id_type.R" = NULL,
        "geo_load_esets.R" = list(
          list(message = "curly", line_number = 57),
          list(message = "curly", line_number = 69)
        ),
        "geo_merge.R" = NULL,
        "geo_use_raw_data.R" = NULL,
        "global_env_hack.R" = NULL,
        "load_geo_data.R" = NULL,
        "plotting.R" = NULL,
        "diag_studiesinfo.R" = NULL,
        "prog_studiesinfo.R" = NULL,
        "utils.R" = NULL
      ),
      "tests/testthat" = list(
        "test-lints.R" = NULL
      )
    )

    for (directory in names(lintlist)) {
      print(directory)
      for (fname in names(lintlist[[directory]])) {
        print(fname)
        #% print(list.files(prefix))

        # skip on covr
        skip_on_covr()

        lintr::expect_lint(
          file = paste0(
            prefix,
            directory,
            "/",
            fname
          ),
          checks = lintlist[[directory]][[fname]]
        )
      }
    }
  }
)
