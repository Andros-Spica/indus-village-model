export_figure <- function(
    plot,
    filename,
    width = 8,
    height = 5,
    dpi = 300,
    export_png = TRUE,
    export_eps = TRUE
) {

  dir.create("plots", recursive = TRUE, showWarnings = FALSE)

  if (export_png) {

    ggsave(
      filename = paste0("plots/", filename, ".png"),
      plot = plot,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  if (export_eps) {

    ggsave(
      filename = paste0("plots/", filename, ".eps"),
      plot = plot,
      width = width,
      height = height,
      device = cairo_ps
    )
  }
}