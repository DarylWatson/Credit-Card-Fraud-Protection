# 03_save_figures.R
# Saves Figures 1 to 3 as PNGs in ../figures, 6.5 inches wide at 300 dpi
# (they go into Word at 6.5 inches without resizing).
# Run from this folder with source("03_save_figures.R"). It runs
# 02_eda_figures.R, which runs 01_load_clean.R if needed.

source("02_eda_figures.R")

dir.create("../figures", showWarnings = FALSE)

ggsave("../figures/fig1_risk_flags.png", fig_flags,
       width = 6.5, height = 2.5, dpi = 300, bg = "white")
ggsave("../figures/fig2_fraud_rate_by_group.png", fig_groups,
       width = 6.5, height = 11 / 3, dpi = 300, bg = "white")

# Figure 3 is base R graphics, so ggsave() can't save it; use png() instead.
png("../figures/fig3_distributions.png",
    width = 1950, height = 1200, res = 300, bg = "white")
draw_distributions()
dev.off()
