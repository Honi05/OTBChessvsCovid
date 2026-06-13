# Generates the "screenshot placeholder" images used in presentation_pptx.qmd.
# Each placeholder marks a spot where a real ggplot2 plot from analysis.R
# should be pasted in (after running the corresponding section in RStudio).

library(ragg)

draw_placeholder <- function(file, width_in, height_in, plot_title, instructions) {
  agg_png(file, width = width_in, height = height_in, units = "in", res = 200)
  on.exit(dev.off())

  par(mar = c(0, 0, 0, 0))
  plot.new()

  # Card background + dashed border
  rect(0, 0, 1, 1, col = "#EAF7F6", border = NA)
  rect(0.01, 0.02, 0.99, 0.98, col = NA, border = "#3FA9A2", lwd = 3, lty = 2)

  # "Camera" icon: simple rectangle with a circle lens
  icon_y <- 0.78
  rect(0.43, icon_y - 0.05, 0.57, icon_y + 0.05, col = "#3FA9A2", border = NA)
  symbols(0.5, icon_y, circles = 0.025, inches = FALSE, add = TRUE,
          bg = "#EAF7F6", fg = "#EAF7F6")
  symbols(0.5, icon_y, circles = 0.018, inches = FALSE, add = TRUE,
          bg = NA, fg = "#3FA9A2", lwd = 2)

  text(0.5, 0.62, "SCREENSHOT PLACEHOLDER",
       cex = 2.0, font = 2, col = "#21726D", family = "Cambria")

  text(0.5, 0.48, plot_title,
       cex = 1.35, font = 2, col = "#33312E", family = "sans")

  text(0.5, 0.22, instructions,
       cex = 1.05, col = "#555248", family = "sans")

  invisible(NULL)
}

dir.create("presentation_assets/placeholders", showWarnings = FALSE, recursive = TRUE)

draw_placeholder(
  "presentation_assets/placeholders/active-players.png",
  width_in = 9, height_in = 5,
  plot_title = "Active FIDE-rated players over time (p_active)",
  instructions = paste(
    "Run analysis.R section 5.1 in RStudio, then evaluate p_active.",
    "Right-click the plot in the Plots pane -> Copy, then paste it here (Ctrl+V),",
    "replacing this box.",
    sep = "\n"
  )
)

draw_placeholder(
  "presentation_assets/placeholders/participation.png",
  width_in = 9, height_in = 5,
  plot_title = "Share of active players who played >=1 rated game (p_participation)",
  instructions = paste(
    "Run analysis.R section 5.1 in RStudio, then evaluate p_participation.",
    "Right-click the plot in the Plots pane -> Copy, then paste it here (Ctrl+V),",
    "replacing this box.",
    sep = "\n"
  )
)

draw_placeholder(
  "presentation_assets/placeholders/inflation.png",
  width_in = 8, height_in = 4.5,
  plot_title = "FIDE Standard rating distribution: Feb 2015 vs Jun 2026",
  instructions = paste(
    "Run analysis.R section 5.5 in RStudio, then evaluate the final ggplot",
    "(the rating histogram). Right-click it in the Plots pane -> Copy, then",
    "paste it here (Ctrl+V), replacing this box.",
    sep = "\n"
  )
)

cat("Wrote 3 placeholder images to presentation_assets/placeholders/\n")
