batch_effect_boxplot <- function(eset,
                                 plot_title,
                                 filename = NULL) {
  if (is.null(filename)) {
    filename <- "./plots/batch_effect_boxplot.png"
    if (!dir.exists("./plots/")) {
      dir.create("./plots/")
    }
  }

  grDevices::png(
    filename = filename,
    res = 150,
    height = 1000,
    width = 2000
  )
  print({
    graphics::boxplot(
      eset,
      main = plot_title,
      xlab = "Samples",
      ylab = "Expression value"
    )
  })
  grDevices::dev.off()
}

plot_batchplot <- function(correction_obj,
                           filename = NULL,
                           time) {

  if (is.null(filename)) {
    filename <- paste0("./plots/PCplot_", time, ".png")
    if (!dir.exists("./plots/")) {
      dir.create("./plots/")
    }
  }

  grDevices::png(
    filename = filename,
    res = 150,
    height = 1500,
    width = 1500
  )
  print({
    # time == "before" or "after"
    gPCA::PCplot(
      correction_obj,
      ug = "guided",
      type = "1v2",
      main = paste("gPCA",
                   time,
                   "batch correction")
    )
  })
  grDevices::dev.off()
}
