# Predicting Credit Card Fraud

CSC 3220 Final Project · Team 10: Daryl Watson, J.T. Brooks, Grayson Heyboer, Abdullah Javed

This repository holds the R code for our CSC 3220 final project, which asks how well we can estimate the probability that a card transaction is fraudulent, given only what is known at the moment it is made. The data comes from Kaggle's Predicting Credit Card Fraud competition (Playground Series, Season 6, Episode 11), a synthetic data set of about 700,000 labeled transactions in which only 1.67% are fraud. Everything here is written in base R and the tidyverse, with the moments package for skewness, so the code sticks to methods covered in the course.

**What's in the repository.** The `Deliverable 1` folder holds the exploratory work behind the first part of our report. Its `Scripts` folder contains six scripts. The three exploration scripts come first: `00a_first_look.R` checks the raw files for size, column types, missing values, duplicates, and skew; `00b_fraud_patterns.R` shows how the fraud rate changes across every predictor; and `00c_consistency_checks.R` looks for columns that should agree with each other but do not, which is how we found the anomalies described in the report. `01_load_clean.R` then loads and cleans the training data, `02_eda_figures.R` builds the summary tables and draws Figures 1 to 3, and `03_save_figures.R` saves those figures as PNG images in the `figures` folder.

**What you need.** You will need R and RStudio, plus two packages, which only have to be installed once:

```r
install.packages(c("tidyverse", "moments"))
```

You will also need the data, which is not stored here; the training file alone is about 98 MB. Download `train.csv` and `test.csv` from the competition's [data page](https://www.kaggle.com/competitions/playground-series-s6e11/data). Kaggle asks you to sign in and accept the competition rules first, and the data is released under a CC BY 4.0 license.

**Running the code.** Put `train.csv` and `test.csv` in the `Scripts` folder, next to the scripts. Open a script in RStudio and choose Session > Set Working Directory > To Source File Location, so R can find the files. Then run the scripts in order: 00a, 00b, 00c, 01, 02, and finally 03. Every script can also be run on its own. The three exploration scripts each load the data themselves, `02_eda_figures.R` runs `01_load_clean.R` first if the cleaned data is not loaded yet, and `03_save_figures.R` runs `02_eda_figures.R`. The `.gitignore` file keeps the CSV files out of git, so having them in the `Scripts` folder will not commit them by accident. Loading the training file takes several seconds, so a short pause at the start of each script is normal, not a crash.

Run each script either line by line with Ctrl+Enter or all at once with Ctrl+Shift+Enter, which is RStudio's Source with Echo (on a Mac, use Cmd instead of Ctrl). The scripts show their results by leaving them on a line of their own rather than calling `print()`, so sourcing a script the usual way runs everything and shows nothing, which is accurate but not very helpful. Each script starts with a short note on what it does, and comments mark the steps that are not obvious from the code.

**Saving the figures.** Run `source("03_save_figures.R")` to remake all three PNG files in the `figures` folder in one step. You can also save a figure by hand: `02_eda_figures.R` draws each one in RStudio's Plots pane, where Export > Save as Image at 780 × 300 pixels (Figure 1), 780 × 440 (Figure 2), or 780 × 480 (Figure 3) gives a similar image at a lower resolution. In Word, insert the image at 6.5 inches wide, so that its text comes out at about the same size as the report's.

**Reproducibility.** We tested the scripts with R 4.3.3 and tidyverse 2.0.0. Nothing in Deliverable 1 involves randomness, so there is no random seed yet; we will set one when we split the data and start modeling in Deliverable 2.