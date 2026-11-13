# =============================================================================
# 03_save_figures.R
# CSC 3220 Final Project, Team10: Predicting credit card fraud
#
# What this script does
#   Saves the three exploratory figures as PNG image files in a "figures"
#   folder, at the exact size they have in the Word report (6.5 inches wide,
#   the text width of a Letter page with 1-inch margins).
#
# How to run it
#   source("03_save_figures.R")
#   It runs 02_eda_figures.R first, which builds the figures (and runs
#   01_load_clean.R if the cleaned data is not loaded yet).
#   To update a figure in the Word report: right-click the picture in Word,
#   choose Change Picture > From a File, and pick the new PNG.
#
# Package used
#   ggplot2   ggsave() writes a ggplot to an image file.
#             width and height are in inches. dpi = 300 means 300 pixels per
#             inch, which is sharp enough for print.
#             bg = "white" gives the image a white background instead of a
#             transparent one.
# =============================================================================

source("02_eda_figures.R")

dir.create("figures", showWarnings = FALSE)

ggsave("figures/fig1_risk_flags.png", fig_flags,
       width = 6.5, height = 2.5, dpi = 300, bg = "white")
ggsave("figures/fig2_fraud_rate_by_group.png", fig_groups,
       width = 6.5, height = 3.0, dpi = 300, bg = "white")
ggsave("figures/fig3_distributions.png", fig_density,
       width = 6.5, height = 3.3, dpi = 300, bg = "white")
