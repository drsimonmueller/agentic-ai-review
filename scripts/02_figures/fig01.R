#!/usr/bin/env Rscript
# Figure 1: verify and copy workflow architecture schematic to outputs/figures/Figure_1/

repo_root_init <- Sys.getenv("AGENTIC_REPO_ROOT", unset = ".")
source(file.path(repo_root_init, "config", "paths.R"))

fig1_dir <- file.path(path_figures, "Figure_1")
ensure_dir(fig1_dir)

src_pdf <- require_file(file.path(path_schematics, "Figure1_workflow_architecture.pdf"))
src_png <- require_file(file.path(path_schematics, "Figure1_workflow_architecture.png"))

dst_pdf <- file.path(fig1_dir, "fig1_workflow_architecture.pdf")
dst_png <- file.path(fig1_dir, "fig1_workflow_architecture.png")

ok_pdf <- file.copy(from = src_pdf, to = dst_pdf, overwrite = TRUE)
ok_png <- file.copy(from = src_png, to = dst_png, overwrite = TRUE)

if (!isTRUE(ok_pdf) || !file.exists(dst_pdf) || file.info(dst_pdf)$size <= 0) {
  stop("failed to copy Figure 1 PDF to: ", dst_pdf, call. = FALSE)
}
if (!isTRUE(ok_png) || !file.exists(dst_png) || file.info(dst_png)$size <= 0) {
  stop("failed to copy Figure 1 PNG to: ", dst_png, call. = FALSE)
}

cat(
  sprintf(
    "Figure 1 copied cleanly:\n  PDF: %s (%d bytes)\n  PNG: %s (%d bytes)\n",
    dst_pdf, as.integer(file.info(dst_pdf)$size),
    dst_png, as.integer(file.info(dst_png)$size)
  )
)
