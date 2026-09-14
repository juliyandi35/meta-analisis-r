## =============================================================================
## Meta-Analysis: Aquatic vs Land-based/Conventional Therapy for Stroke
## Outcomes: BBS V2, MWT V2, 10 MWT V2, TUG V2, MBI V2, FRT V2
##
## What this script does (per outcome sheet in "Stroke Dataset.xlsx"):
##   1. Removes rows whose Stroke_Type contains "Unspecified".
##   2. Recodes any Stroke_Type that is not exactly "Chronic" as "Subacute".
##   3. Uses Chronic / Subacute as subgroups in the meta-analysis.
##   4. Computes Hedges' g (Aquatic = Group 1 vs Land-based/Control = Group 2)
##      on the post-treatment ("After") scores, random-effects (DerSimonian-
##      Laird / REML via metafor's rma()).
##   5. Exports significant_tables.xlsx - one sheet per outcome, each holding
##      the Overall / Chronic / Subacute pooled results plus a subgroup-
##      difference (moderator) test.
##   6. Saves Forest, Funnel, and Leave-one-out plots for every outcome into
##      "Forest Plot/", "Funnel Plot/", "Leave-One-Out Plot/".
##
## Input : "Stroke Dataset.xlsx" (6 sheets: BBS V2, MWT V2, 10 MWT V2, TUG V2,
##          MBI V2, FRT V2) - tidy, one row per study, produced from the
##          original "Copy of Extract Data (STROKE).xlsx".
## Output: significant_tables.xlsx + Forest Plot/, Funnel Plot/,
##          Leave-One-Out Plot/ folders, all written next to this script.
## =============================================================================

## ---- 0. Setup --------------------------------------------------------------

required_pkgs <- c("readxl", "metafor", "openxlsx", "dplyr", "stringr")
to_install <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(readxl)
library(metafor)
library(openxlsx)
library(dplyr)
library(stringr)

## Set this to the folder containing "Stroke Dataset.xlsx". By default the
## script assumes it is run from (or sourced from) that same folder.
# setwd("path/to/your/folder")

INPUT_FILE <- "Stroke Dataset.xlsx"

OUTCOME_SHEETS <- c("BBS V2", "MWT V2", "10 MWT V2", "TUG V2", "MBI V2", "FRT V2")

dir.create("Forest Plot", showWarnings = FALSE)
dir.create("Funnel Plot", showWarnings = FALSE)
dir.create("Leave-One-Out Plot", showWarnings = FALSE)

## ---- 1 & 2. Cleaning: drop Unspecified, recode Stroke_Type ----------------

clean_stroke_type <- function(df) {
  df <- df %>%
    filter(!str_detect(str_to_lower(as.character(Stroke_Type)), "unspecified"))
  df$Stroke_Type <- ifelse(
    str_to_lower(str_trim(as.character(df$Stroke_Type))) == "chronic",
    "Chronic", "Subacute"
  )
  df
}

## ---- Helper: median/IQR -> mean/SD approximation --------------------------
## Used only for studies (e.g. in MBI V2) that report median (Q1-Q3) instead
## of mean (SD). SD is approximated as (Q3 - Q1) / 1.35 (standard normal-
## approximation rule of thumb; see also Wan et al. 2014, BMC Med Res
## Methodol).

median_iqr_to_mean_sd <- function(median, q1, q3) {
  mean_est <- median
  sd_est <- ifelse(!is.na(q1) & !is.na(q3), (q3 - q1) / 1.35, NA_real_)
  list(mean = mean_est, sd = sd_est)
}

## ---- 4. Build a per-study effect-size ("yi","vi") table -------------------
## Aquatic (Group 1) vs Land-based/Control (Group 2), using After scores.

build_effect_data <- function(df) {
  m1 <- df$After_Mean_G1; sd1 <- df$After_SD_G1; n1 <- df$N_G1
  m2 <- df$After_Mean_G2; sd2 <- df$After_SD_G2; n2 <- df$N_G2

  need_est1 <- is.na(m1) | is.na(sd1)
  if (any(need_est1)) {
    est1 <- median_iqr_to_mean_sd(df$After_Median_G1, df$After_Q1_G1, df$After_Q3_G1)
    m1[need_est1] <- est1$mean[need_est1]
    sd1[need_est1] <- est1$sd[need_est1]
  }
  need_est2 <- is.na(m2) | is.na(sd2)
  if (any(need_est2)) {
    est2 <- median_iqr_to_mean_sd(df$After_Median_G2, df$After_Q1_G2, df$After_Q3_G2)
    m2[need_est2] <- est2$mean[need_est2]
    sd2[need_est2] <- est2$sd[need_est2]
  }

  dat <- data.frame(
    No = df$No, Study = df$Study, Stroke_Type = df$Stroke_Type,
    n1i = n1, m1i = m1, sd1i = sd1,
    n2i = n2, m2i = m2, sd2i = sd2
  )
  dat <- dat[complete.cases(dat[, c("n1i", "m1i", "sd1i", "n2i", "m2i", "sd2i")]) &
               dat$sd1i > 0 & dat$sd2i > 0 & dat$n1i > 1 & dat$n2i > 1, ]

  es <- escalc(measure = "SMD", n1i = n1i, n2i = n2i,
               m1i = m1i, m2i = m2i, sd1i = sd1i, sd2i = sd2i, data = dat)
  es
}

## ---- 5. Random-effects pooling (Overall / Chronic / Subacute) -------------

pool_summary_row <- function(label, es_subset) {
  k <- nrow(es_subset)
  if (k == 0) return(data.frame(Group = label, k = 0))
  if (k == 1) {
    row <- data.frame(
      Group = label, k = 1,
      Hedges_g = round(es_subset$yi[1], 3), SE = round(sqrt(es_subset$vi[1]), 3),
      CI_lower = round(es_subset$yi[1] - 1.96 * sqrt(es_subset$vi[1]), 3),
      CI_upper = round(es_subset$yi[1] + 1.96 * sqrt(es_subset$vi[1]), 3),
      z = NA, p_value = NA, `Significant_p<0.05` = NA,
      tau2 = NA, I2_percent = NA, Q = NA, df = NA, p_heterogeneity = NA,
      check.names = FALSE
    )
    return(row)
  }
  m <- rma(yi, vi, data = es_subset, method = "DL")
  data.frame(
    Group = label, k = k,
    Hedges_g = round(as.numeric(m$b), 3), SE = round(m$se, 3),
    CI_lower = round(m$ci.lb, 3), CI_upper = round(m$ci.ub, 3),
    z = round(m$zval, 3), p_value = round(m$pval, 4),
    `Significant_p<0.05` = ifelse(m$pval < 0.05, "Yes", "No"),
    tau2 = round(m$tau2, 4), I2_percent = round(m$I2, 1),
    Q = round(m$QE, 3), df = m$k - 1, p_heterogeneity = round(m$QEp, 4),
    check.names = FALSE
  )
}

build_summary_table <- function(es) {
  rows <- list(
    pool_summary_row("Overall", es),
    pool_summary_row("Chronic", es[es$Stroke_Type == "Chronic", ]),
    pool_summary_row("Subacute", es[es$Stroke_Type == "Subacute", ])
  )
  tbl <- bind_rows(rows)

  ## Subgroup-difference (moderator) test, when both subgroups have >= 1 study
  n_chronic <- sum(es$Stroke_Type == "Chronic")
  n_subacute <- sum(es$Stroke_Type == "Subacute")
  if (n_chronic >= 1 && n_subacute >= 1 && nrow(es) >= 3) {
    mod <- tryCatch(
      rma(yi, vi, mods = ~ Stroke_Type, data = es, method = "DL"),
      error = function(e) NULL
    )
    if (!is.null(mod)) {
      diff_row <- data.frame(
        Group = "Subgroup difference (Chronic vs Subacute)", k = nrow(es),
        Hedges_g = NA, SE = NA, CI_lower = NA, CI_upper = NA,
        z = NA, p_value = NA, `Significant_p<0.05` = NA,
        tau2 = NA, I2_percent = NA,
        Q = round(mod$QM, 3), df = mod$m, p_heterogeneity = round(mod$QMp, 4),
        check.names = FALSE
      )
      tbl <- bind_rows(tbl, diff_row)
    }
  }
  tbl
}

## ---- 6 & 7. Plots -----------------------------------------------------

safe_name <- function(s) gsub("[^A-Za-z0-9 _-]", "", s)

## Standard significance stars for the pooled-effect p-value (not the
## heterogeneity p-value): * p<0.05, ** p<0.01, *** p<0.001.
sig_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  ""
}

make_plots <- function(es, outcome_name) {
  fname <- safe_name(outcome_name)

  ## Studies ordered Chronic then Subacute (alphabetically, so this is a
  ## plain stable sort - "Chronic" < "Subacute").
  es_ord <- es[order(es$Stroke_Type, es$Study), ]
  es_c <- es_ord[es_ord$Stroke_Type == "Chronic", ]
  es_s <- es_ord[es_ord$Stroke_Type == "Subacute", ]
  n_c <- nrow(es_c); n_s <- nrow(es_s)

  m_overall <- rma(yi, vi, data = es_ord, method = "DL")
  m_c <- if (n_c > 0) rma(yi, vi, data = es_c, method = "DL") else NULL
  m_s <- if (n_s > 0) rma(yi, vi, data = es_s, method = "DL") else NULL

  ## ---- Forest plot with subgroup diamonds + a random-effects weight% ----
  ## ---- column, built following metafor's own documented pattern for   ----
  ## ---- "forest plot with subgroups" (addfit=FALSE + manual addpoly()). ----
  ##
  ## Row layout (bottom -> top): Overall summary, blank, Chronic studies,
  ## Chronic summary, blank, Subacute studies, Subacute summary. Rows are
  ## plain integers picked so no two elements ever share a row, verified by
  ## simulating this exact arithmetic in Python across every outcome's
  ## actual (n_c, n_s) counts before this script was finalized.
  gap <- 1
  cursor <- 1
  overall_row <- cursor
  cursor <- cursor + 1 + gap

  rows_c <- NULL; summary_row_c <- NULL
  rows_s <- NULL; summary_row_s <- NULL

  if (n_c > 0) {
    block <- cursor:(cursor + n_c - 1)
    rows_c <- rev(block)                 # first (alphabetical) study -> top of its block
    cursor <- max(block) + 1
    summary_row_c <- cursor
    cursor <- cursor + 1 + gap
  }
  if (n_s > 0) {
    block <- cursor:(cursor + n_s - 1)
    rows_s <- rev(block)
    cursor <- max(block) + 1
    summary_row_s <- cursor
    cursor <- cursor + 1 + gap
  }
  top_row <- max(c(overall_row, rows_c, summary_row_c, rows_s, summary_row_s))
  rows_all <- c(rows_c, rows_s)           # aligned with es_ord's row order (Chronic block first, then Subacute)

  wi <- weights(m_overall)                # random-effects weight %, aligned with es_ord row order

  x_lo <- min(es_ord$yi - 1.96 * sqrt(es_ord$vi))
  x_hi <- max(es_ord$yi + 1.96 * sqrt(es_ord$vi))
  x_rng <- x_hi - x_lo
  xlim_use  <- c(x_lo - x_rng * 1.6, x_hi + x_rng * 0.9)
  alim_use  <- range(pretty(c(x_lo, x_hi)))
  ilab_xpos <- x_hi + x_rng * 0.15
  ylim_use  <- c(0, top_row + 2.5)

  png(file.path("Forest Plot", paste0("Forest_", fname, ".png")),
      width = 1700, height = max(600, 55 * top_row + 420), res = 150)
  par(mar = c(6, 4, 4, 2))
  forest(m_overall, slab = es_ord$Study, rows = rows_all,
         xlim = xlim_use, alim = alim_use, ylim = ylim_use,
         ilab = sprintf("%.1f%%", wi), ilab.xpos = ilab_xpos,
         xlab = "Hedges' g (Aquatic vs Land-based/Control, After scores)",
         header = c("Study", "Hedges' g [95% CI]"),
         addfit = FALSE, cex = 0.75)
  text(ilab_xpos, top_row + 1.6, "Weight", cex = 0.75, font = 2)

  if (!is.null(m_c)) {
    addpoly(m_c, row = summary_row_c, cex = 0.72, col = "firebrick",
            mlab = sprintf("Chronic subgroup (k=%d): g=%.2f, p=%.4f%s, I2=%.0f%%, tau2=%.3f, Q=%.2f, p_het=%.4f",
                            m_c$k, as.numeric(m_c$b), m_c$pval, sig_stars(m_c$pval),
                            m_c$I2, m_c$tau2, m_c$QE, m_c$QEp))
  }
  if (!is.null(m_s)) {
    addpoly(m_s, row = summary_row_s, cex = 0.72, col = "firebrick",
            mlab = sprintf("Subacute subgroup (k=%d): g=%.2f, p=%.4f%s, I2=%.0f%%, tau2=%.3f, Q=%.2f, p_het=%.4f",
                            m_s$k, as.numeric(m_s$b), m_s$pval, sig_stars(m_s$pval),
                            m_s$I2, m_s$tau2, m_s$QE, m_s$QEp))
  }
  addpoly(m_overall, row = overall_row, cex = 0.8, col = "darkred", font = 2,
          mlab = sprintf("Overall (k=%d): g=%.2f, p=%.4f%s, I2=%.0f%%, tau2=%.3f, Q=%.2f, p_het=%.4f",
                          m_overall$k, as.numeric(m_overall$b), m_overall$pval, sig_stars(m_overall$pval),
                          m_overall$I2, m_overall$tau2, m_overall$QE, m_overall$QEp))
  title(main = paste0("Forest Plot - ", outcome_name,
                       "\n(Weight = % contribution to the overall pooled estimate; ",
                       "* p<0.05  ** p<0.01  *** p<0.001 for the pooled effect)"),
        cex.main = 0.9)
  dev.off()

  ## ---- Funnel plot with heterogeneity stats + Egger's test for asymmetry ----
  png(file.path("Funnel Plot", paste0("Funnel_", fname, ".png")),
      width = 950, height = 950, res = 150)
  funnel(m_overall, main = paste("Funnel Plot -", outcome_name))

  het_txt <- sprintf("k=%d   tau^2=%.3f   I^2=%.1f%%   Q(%d)=%.2f, p=%.4f",
                      m_overall$k, m_overall$tau2, m_overall$I2, m_overall$k - 1,
                      m_overall$QE, m_overall$QEp)
  legend("topleft", legend = het_txt, bty = "n", cex = 0.7)

  ## Egger's regression test for funnel-plot asymmetry (needs k >= 3).
  ## The exact text is taken from metafor's own print() output for the
  ## regtest, so it is always phrased/labelled the way metafor itself does.
  if (m_overall$k >= 3) {
    et <- tryCatch(regtest(m_overall), error = function(e) NULL)
    egger_lines <- if (!is.null(et)) capture.output(print(et)) else
      "Egger's test: could not be computed for this outcome"
  } else {
    egger_lines <- "Egger's test: not computed (need k>=3 studies)"
  }
  legend("bottomright", legend = egger_lines, bty = "n", cex = 0.6)
  dev.off()

  ## ---- Leave-one-out sensitivity analysis, with per-omission I^2 ----
  ## NOTE: leave1out() returns a "list.rma" object (a list of vectors), not a
  ## data.frame, so nrow() on it gives NULL - use length() on one of its
  ## vector components instead.
  if (nrow(es_ord) >= 3) {
    loo <- leave1out(m_overall)
    k_loo <- length(loo$estimate)
    png(file.path("Leave-One-Out Plot", paste0("LOO_", fname, ".png")),
        width = 1350, height = max(500, 90 * k_loo + 220), res = 150)
    par(mar = c(5, 14, 4, 8))
    y <- k_loo:1
    xr <- range(c(loo$ci.lb, loo$ci.ub, m_overall$ci.lb, m_overall$ci.ub), na.rm = TRUE)
    plot(loo$estimate, y, pch = 16, col = "seagreen",
         xlim = xr, yaxt = "n", ylab = "", xlab = "Pooled Hedges' g (study omitted)",
         main = paste0("Leave-One-Out Sensitivity - ", outcome_name,
                        "\n(all studies: k=", m_overall$k,
                        ", tau^2=", round(m_overall$tau2, 3),
                        ", I^2=", round(m_overall$I2, 1), "%)"),
         cex.main = 0.95)
    rect(m_overall$ci.lb, min(y) - 1, m_overall$ci.ub, max(y) + 1,
         col = rgb(0.85, 0.1, 0.1, 0.08), border = NA)
    segments(loo$ci.lb, y, loo$ci.ub, y)
    points(loo$estimate, y, pch = 16, col = "seagreen")
    axis(2, at = y, labels = paste0("Omit: ", es_ord$Study), las = 2, cex.axis = 0.7)
    abline(v = as.numeric(m_overall$b), lty = 2, col = "firebrick")
    ## Per-omission I^2 as a right-hand text column (leave1out() reports it
    ## directly for random-effects models - no need to recompute it).
    usr <- par("usr")
    text(usr[2], y, sprintf("I2=%.0f%%", loo$I2), pos = 4, xpd = TRUE, cex = 0.7)
    legend("topright", legend = sprintf("All-studies pooled g=%.3f [%.2f, %.2f]",
                                         as.numeric(m_overall$b), m_overall$ci.lb, m_overall$ci.ub),
           bty = "n", cex = 0.7)
    dev.off()
  }
}

## ---- Main loop over the 6 outcome sheets ----------------------------------

summary_tables <- list()

for (sheet in OUTCOME_SHEETS) {
  df <- read_excel(INPUT_FILE, sheet = sheet)
  df <- clean_stroke_type(df)          ## rules 1 & 2
  es <- build_effect_data(df)          ## rule 3 groundwork (Stroke_Type kept as subgroup)

  if (nrow(es) == 0) {
    message(sprintf("[%s] no studies with usable effect data - skipped", sheet))
    next
  }

  summary_tables[[sheet]] <- build_summary_table(es)   ## rule 3: Chronic/Subacute subgroups
  make_plots(es, sheet)

  message(sprintf("[%s] k=%d studies included; plots written", sheet, nrow(es)))
}

## ---- Export significant_tables.xlsx (one sheet per outcome) --------------

wb <- createWorkbook()
for (sheet in names(summary_tables)) {
  addWorksheet(wb, substr(safe_name(sheet), 1, 31))
  writeData(wb, substr(safe_name(sheet), 1, 31), summary_tables[[sheet]])
}
saveWorkbook(wb, "significant_tables.xlsx", overwrite = TRUE)

message("Done. See significant_tables.xlsx and the Forest Plot / Funnel Plot / Leave-One-Out Plot folders.")
