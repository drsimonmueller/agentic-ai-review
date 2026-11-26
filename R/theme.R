# ggplot themes and vector pdf / 300 dpi ghostscript png export

options(bitmapType = "cairo")

# Exact vector-PDF point-to-ggplot2/grid line width conversions
pt_to_lwd    <- function(pt) pt / (ggplot2::.pt * 0.75)
LW_025       <- pt_to_lwd(0.25)   # exact 0.25 pt hairline
LW_050       <- pt_to_lwd(0.50)   # exact 0.50 pt emphasis / median
GRID_LWD_025 <- 0.25 / 0.75       # exact 0.25 pt in grid::gpar(lwd = ...)

theme_paper <- function(base_size = 6, base_family = "Helvetica") {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text              = ggplot2::element_text(color = "black", face = "plain", size = base_size),
      plot.title        = ggplot2::element_text(color = "black", face = "plain", size = base_size),
      plot.subtitle     = ggplot2::element_text(color = "black", face = "plain", size = base_size),
      axis.text         = ggplot2::element_text(color = "black", face = "plain", size = base_size),
      axis.title        = ggplot2::element_text(color = "black", face = "plain", size = base_size),
      legend.text       = ggplot2::element_text(color = "black", face = "plain", size = base_size),
      legend.title      = ggplot2::element_text(color = "black", face = "plain", size = base_size),
      strip.text        = ggplot2::element_text(color = "black", face = "plain", size = base_size),
      strip.background  = ggplot2::element_blank(),
      axis.line         = ggplot2::element_line(color = "black", linewidth = LW_025),
      axis.ticks        = ggplot2::element_line(color = "black", linewidth = LW_025),
      axis.ticks.length = grid::unit(1.8, "pt"),
      plot.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.background  = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

save_panel_pdf <- function(plot_obj, path, width, height) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename    = path,
    plot        = plot_obj,
    device      = grDevices::pdf,
    family      = "Helvetica",
    encoding    = "WinAnsi.enc",
    useDingbats = FALSE,
    width       = width,
    height      = height,
    units       = "in"
  )
  invisible(path)
}

save_panel_png <- function(pdf_path, png_path = sub("\\.pdf$", ".png", pdf_path), dpi = 300L) {
  if (!file.exists(pdf_path)) {
    stop("cannot render png; pdf not found: ", pdf_path, call. = FALSE)
  }
  gs_bin <- Sys.which("gs")
  if (!nzchar(gs_bin)) {
    stop("ghostscript ('gs') not found on PATH", call. = FALSE)
  }
  dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)
  args <- c(
    "-dSAFER",
    "-dBATCH",
    "-dNOPAUSE",
    "-sDEVICE=png16m",
    paste0("-r", as.integer(dpi)),
    "-dTextAlphaBits=4",
    "-dGraphicsAlphaBits=4",
    paste0("-sOutputFile=", png_path),
    pdf_path
  )
  status <- system2(gs_bin, args = args, stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) {
    stop("ghostscript failed converting ", pdf_path, " to ", png_path, " (exit ", status, ")", call. = FALSE)
  }
  invisible(png_path)
}

save_panel_pair <- function(plot_obj, pdf_path, width, height, dpi = 300L) {
  save_panel_pdf(plot_obj, pdf_path, width = width, height = height)
  png_path <- sub("\\.pdf$", ".png", pdf_path)
  save_panel_png(pdf_path, png_path, dpi = dpi)
  invisible(list(pdf = pdf_path, png = png_path))
}
