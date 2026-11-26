# shared plotting utilities

# add bold lowercase panel tag (9 pt); Nature style omits panel titles (titles belong in legends)
apply_panel_header <- function(plot_obj, tag, title = NULL, tag_size = 9.0, title_size = 6.0) {
  plot_obj +
    ggplot2::labs(tag = tag, title = NULL) +
    ggplot2::theme(
      plot.tag = ggplot2::element_text(
        family = "Helvetica", face = "bold", size = tag_size, color = "black"
      ),
      plot.tag.position = c(0.008, 0.99),
      plot.title = ggplot2::element_blank()
    )
}

# format count and percentage label
format_count_pct <- function(k, n, digits = 1L) {
  pct <- 100 * k / n
  sprintf(paste0("%d/%d (%.", digits, "f%%)"), k, n, pct)
}

# shared discrete tile matrix helper (e.g., run completion, node recovery)
build_tile_matrix <- function(df, x_col, y_col, fill_col, fill_palette,
                              tile_color = "white", tile_lwd = 0.6,
                              base_size = 9) {
  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[fill_col]])
  ) +
    ggplot2::geom_tile(color = tile_color, linewidth = tile_lwd) +
    ggplot2::scale_fill_manual(values = fill_palette, drop = FALSE) +
    theme_paper(base_size = base_size) +
    ggplot2::theme(
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
}
