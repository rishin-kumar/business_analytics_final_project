# =============================================================================
# 03_tests.R  --  Prague Cafe Project, Batch 1 (cappuccino price and location)
#
# Runs the ten analyses A1-A10 on analysis/cafe_clean.csv. Each analysis is one
# commented block: hypotheses (direction fixed in advance), methods, results.
# For every analysis it exports one table (report/tables/A#_table.tex) and one
# figure (report/figures/A#_figure.pdf, plus a PNG copy for the slides in
# presentation/figures). report/tables/numbers.tex holds every number quoted in
# the text of the report as a LaTeX macro, so report and script cannot drift.
#
# RUN FROM THE REPOSITORY ROOT. Order: 01_cleaning.R -> 02_descriptives.R -> 03_tests.R
# =============================================================================

## 0. SETUP ------------------------------------------------------------------
if (!file.exists("analysis/cafe_clean.csv")) stop("Run 01_cleaning.R first (from the repository root).")
# Stops with a message if the cleaned dataset is missing (script 01 must be run first).
for (f in c("report/tables", "report/figures", "presentation/figures")) dir.create(f, recursive = TRUE, showWarnings = FALSE)
# Creates the output folders for tables, report figures and slide figures if they do not exist.

load_packages <- function(pkgs) {
# Defines a helper that takes a vector of package names.
  for (p in pkgs) {
  # Loops over each package name.
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
    # Installs the package from CRAN if it is not installed yet.
    library(p, character.only = TRUE)
    # Attaches the package so its functions can be used.
  }
  # Ends the loop.
}
# Ends the helper function.
load_packages(c("ggplot2", "xtable", "sandwich", "lmtest", "car", "boot"))
# Loads plotting (ggplot2), LaTeX tables (xtable), robust standard errors (sandwich, lmtest), VIF (car) and bootstrap (boot).

SEED <- 2026       # seed for every bootstrap in this script
# Stores the random seed used for every bootstrap so results are reproducible.
B    <- 5000       # bootstrap resamples (at least 5,000)
# Stores the number of bootstrap resamples (5,000).
set.seed(SEED)
# Sets the random seed.
ALPHA <- .05       # significance level, stated once in the report
# Stores the significance level alpha = .05.

# ---- A1 needs a benchmark: YOUR OWN guess of the average Prague cappuccino price,
# ---- written down BEFORE you open the data. >>> REPLACE THIS VALUE WITH YOUR OWN. <<<
BENCHMARK <- 90
# Your prior guess of the average cappuccino price (CZK), used as the benchmark in A1.

COP <- "#512bb8"; ESP <- "#2B1D16"; GREY <- "#8A7F76"; PALE <- "#E7E2DC"
# Defines the colours used in all figures (purple, dark brown, grey, pale beige).
theme_set(theme_minimal(base_size = 11) +
# Sets the default plot theme to a clean minimal one with 11 pt text,
          theme(panel.grid.minor = element_blank(),
          # removing minor grid lines,
                plot.title = element_text(face = "bold"),
                # making titles bold,
                plot.subtitle = element_text(colour = GREY),
                # colouring subtitles grey,
                strip.text = element_text(face = "bold")))
                # and making panel labels bold.

## 1. HELPERS ------------------------------------------------------------------
fmt  <- function(x, k = 2) { x[!is.na(x) & round(x, k) == 0] <- 0; s <- formatC(x, format = "f", digits = k); gsub("-", "$-$", s) }   # LaTeX minus
# Formats a number with k decimals as text, turns a rounded zero into 0 (no '-0.00') and replaces '-' by a LaTeX minus sign.
fmtp <- function(p) if (p < .001) "$<$.001" else sub("^0", "", formatC(p, format = "f", digits = 3))
# Formats a p-value for a table: '<.001' if tiny, otherwise three decimals without the leading zero.
ptex <- function(p) if (p < .001) "$p$ < .001" else paste0("$p$ = ", sub("^0", "", formatC(p, format = "f", digits = 3)))
# Formats a p-value as LaTeX text for the summary table ('p < .001' or 'p = .xxx').
ptxt <- function(p) if (p < .001) "\\plt{.001}" else paste0("\\pval{", sub("^0", "", formatC(p, format = "f", digits = 3)), "}")
# Formats a p-value as a LaTeX macro (\plt or \pval) used in the report text.
ci_str <- function(lo, hi, k = 2) paste0("[", fmt(lo, k), ", ", fmt(hi, k), "]")
# Formats a confidence interval as '[low, high]'.
ntests <- 0; tick <- function(k = 1) ntests <<- ntests + k       # counts the tests in the report
# Keeps a running count of the number of tests performed (tick() adds to it).

save_fig <- function(p, name, w = 6.5, h = 4) {
# Defines a helper that saves a plot with a given name, width and height.
  ggsave(file.path("report/figures", paste0(name, ".pdf")), p, width = w, height = h)
  # Saves the plot as a PDF for the report.
  ggsave(file.path("presentation/figures", paste0(name, ".png")), p, width = w, height = h, dpi = 200)
  # Saves the same plot as a 200-dpi PNG for the presentation slides.
}
# Ends the helper function.
write_tab <- function(df, name, align = NULL) {
# Defines a helper that saves a data frame as a LaTeX table.
  print(xtable(df, align = align, digits = 0),
  # Converts the data frame to an xtable with no decimals (numbers are already formatted text),
        file = file.path("report/tables", paste0(name, ".tex")),
        # and writes it to report/tables/<name>.tex,
        floating = FALSE, booktabs = TRUE, include.rownames = FALSE,
        # without a table wrapper, with booktabs rules and no row numbers,
        sanitize.text.function = function(x) x)
        # leaving the text unescaped so LaTeX symbols in cells work.
}
# Ends the helper function.
# macros for the numbers quoted in the report text
macros <- character(0)
# Creates an empty named vector that will collect all numbers quoted in the report text.
mac <- function(name, value) macros[name] <<- gsub("$-$", "\\ensuremath{-}", as.character(value), fixed = TRUE)
# Defines a helper that stores one number under a macro name, converting the '$-$' minus into \ensuremath{-}.

# effect-size intervals (normal approximation, Hedges & Olkin)
se_d_one <- function(d, n)       sqrt(1 / n + d^2 / (2 * n))
# Standard error of Cohen's d for one sample or paired data (Hedges & Olkin approximation).
se_d_two <- function(d, n1, n2)  sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
# Standard error of Cohen's d for two independent groups (Hedges & Olkin approximation).
# rank-biserial correlations
rb_two    <- function(x, y) { U <- sum(outer(x, y, ">")) + .5 * sum(outer(x, y, "==")); 2 * U / (length(x) * length(y)) - 1 }
# Rank-biserial correlation for two groups: probability that x beats y (ties count half), rescaled to -1..1.
rb_signed <- function(dd) { dd <- dd[dd != 0]; r <- rank(abs(dd)); (sum(r[dd > 0]) - sum(r[dd < 0])) / sum(r) }
# Rank-biserial correlation for paired/one-sample data from the ranks of the non-zero differences.
pct <- function(x) quantile(x, c(.025, .975), names = FALSE)
# Returns the 2.5th and 97.5th percentiles, i.e. a bootstrap percentile 95% interval.
# confidence interval for eta^2 from the non-central F distribution
eta2_ci <- function(Fv, df1, df2, N, level = .95) {
# Computes a 95% CI for eta squared by inverting the non-central F distribution.
  a <- (1 - level) / 2
  # Splits the 5% error rate into two equal tails.
  lo <- if (pf(Fv, df1, df2, ncp = 0) < 1 - a) 0 else uniroot(function(l) pf(Fv, df1, df2, ncp = l) - (1 - a), c(0, 1e4))$root
  # Finds the lower limit of the non-centrality parameter (0 if the F value is too small to exclude 0).
  hi <- if (pf(Fv, df1, df2, ncp = 0) < a) 0 else uniroot(function(l) pf(Fv, df1, df2, ncp = l) - a, c(0, 1e5))$root
  # Finds the upper limit of the non-centrality parameter in the same way.
  c(lo, hi) / (c(lo, hi) + N)
  # Converts the non-centrality limits into eta-squared limits.
}
# Ends the function.
# prediction with a heteroscedasticity-robust (HC3) confidence band
robust_band <- function(m, nd) {
# Predicts fitted values with a robust (HC3) 95% confidence band for the new data nd.
  X <- model.matrix(delete.response(terms(m)), nd, xlev = m$xlevels)
  # Builds the design matrix for the new data from the model's terms.
  V <- vcovHC(m, type = "HC3"); fit <- drop(X %*% coef(m))
  # Gets the HC3 robust covariance matrix of the coefficients and the fitted values.
  se <- sqrt(rowSums((X %*% V) * X)); tq <- qt(.975, df.residual(m))
  # Computes the robust standard error of each fitted value and the t critical value.
  data.frame(nd, fit = fit, lo = fit - tq * se, hi = fit + tq * se)
  # Returns the new data with the fit and its lower and upper band limits.
}
# Ends the function.
# coefficient table with robust standard errors and 95% CIs (HC3)
robust_coef <- function(m) {
# Returns a coefficient table with HC3 robust standard errors, CIs, t and p-values.
  V <- vcovHC(m, type = "HC3"); b <- coef(m); se <- sqrt(diag(V)); tq <- qt(.975, df.residual(m))
  # Gets the robust covariance matrix, the coefficients, their robust standard errors and the t critical value.
  data.frame(term = names(b), b = b, se = se, lo = b - tq * se, hi = b + tq * se,
  # Builds a table of coefficient, standard error and 95% confidence limits,
             t = b / se, p = 2 * pt(-abs(b / se), df.residual(m)), row.names = NULL)
             # plus the t statistic and two-sided p-value for each coefficient.
}
# Ends the function.
reg_table <- function(models, model_names, term_labels, name, outcome_note) {
# Builds and saves a regression table showing several models side by side.
  rows <- list()
  # Starts an empty list of table rows.
  for (tl in names(term_labels)) {
  # Loops over each coefficient to be shown.
    r1 <- c(term_labels[[tl]]); r2 <- c("")
    # Starts two rows for the coefficient: one for estimate (SE), one for the CI; the first cell is its label.
    for (m in models) {
    # Loops over the models.
      rc <- robust_coef(m); i <- match(tl, rc$term)
      # Gets the robust coefficients of this model and finds the row of the current term (if present).
      if (is.na(i)) { r1 <- c(r1, ""); r2 <- c(r2, "") } else {
      # If the model does not contain the term, leaves its cells blank, otherwise:
        r1 <- c(r1, paste0(fmt(rc$b[i], 2), " (", fmt(rc$se[i], 2), ")"))
        # writes the estimate with its robust standard error in brackets,
        r2 <- c(r2, paste0("[", fmt(rc$lo[i], 2), ", ", fmt(rc$hi[i], 2), "]"))
        # and writes the 95% confidence interval on the line below.
      }
      # Ends the else branch.
    }
    # Ends the loop over models.
    rows[[length(rows) + 1]] <- r1; rows[[length(rows) + 1]] <- r2
    # Adds both rows to the list of rows.
  }
  # Ends the loop over coefficients.
  stat_row <- function(label, f) c(label, sapply(models, f))
  # Defines a helper that builds a row of model-level statistics.
  rows[[length(rows) + 1]] <- stat_row("Observations", function(m) as.character(nobs(m)))
  # Adds a row with the number of observations.
  rows[[length(rows) + 1]] <- stat_row("$R^2$", function(m) fmt(summary(m)$r.squared, 3))
  # Adds a row with R squared.
  rows[[length(rows) + 1]] <- stat_row("Adjusted $R^2$", function(m) fmt(summary(m)$adj.r.squared, 3))
  # Adds a row with adjusted R squared.
  rows[[length(rows) + 1]] <- stat_row("Residual SE", function(m) fmt(summary(m)$sigma, 3))
  # Adds a row with the residual standard error.
  tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  # Stacks all rows into a data frame.
  names(tab) <- c(" ", model_names)
  # Names the columns (an empty first one, then the model names).
  write_tab(tab, name, align = c("l", "l", rep("r", length(models))))
  # Saves the table as LaTeX.
  tab
  # Returns the table.
}
# Ends the function.
# diagnostic plots of a regression: residuals vs fitted, normal Q-Q, residuals vs leverage
diag_plots <- function(m, name) {
# Draws three regression diagnostic plots for a model and saves them as one PDF.
  pdf(file.path("report/figures", paste0(name, ".pdf")), width = 9, height = 3.2)
  # Opens a PDF file 9 x 3.2 inches in report/figures.
  par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
  # Arranges three plots in one row with small margins.
  plot(m, which = c(1, 2, 5), pch = 16, col = adjustcolor(COP, .7), cex = .7, caption = "", sub.caption = "")
  # Plots residuals vs fitted, normal Q-Q, and residuals vs leverage (with Cook's distance) in the house colour.
  dev.off()
  # Closes the PDF file.
}
# Ends the function.

d <- read.csv("analysis/cafe_clean.csv", stringsAsFactors = FALSE)
# Reads the cleaned dataset.
d$type     <- factor(d$type, levels = c("Independent", "Local chain", "International chain"))
# Makes cafe type a factor with Independent as the reference.
d$chain    <- factor(d$chain, levels = c("Independent", "Chain"))
# Makes the chain indicator a factor with Independent as the reference.
d$prague1  <- factor(d$prague1, levels = c("Rest of Prague", "Prague 1"))
# Makes the location a factor with 'Rest of Prague' as the reference.
d$district <- factor(d$district)
# Makes the district a factor.
d$district_main <- factor(ifelse(d$district %in% paste("Prague", c(1, 2, 3, 5, 7, 8)), as.character(d$district), NA),
# Keeps only the six districts with at least 12 cafes (others become NA),
                          levels = paste("Prague", c(1, 2, 3, 5, 7, 8)))
                          # and fixes their order.
N_ALL <- nrow(d)
# Stores the total number of cafes.
mac("Nall", N_ALL)
# Saves it as the macro \Nall for the report.
res <- list()      # one row per analysis for the summary table
# Creates an empty list that will hold one summary row per analysis.

##############################################################################
## A1  ONE SAMPLE  --  is the average cappuccino price different from my own prior guess?
##     H0: mean = benchmark.  H1: mean differs from the benchmark (two-tailed).
##     Methods: one-sample t-test, Wilcoxon signed-rank test, bootstrap CI.
##############################################################################
x <- d$cappuccino; n <- length(x)
# Takes the cappuccino prices and counts them.
t1 <- t.test(x, mu = BENCHMARK); tick()
# Runs a one-sample t-test against the benchmark (two-tailed) and counts it as a test.
d1 <- (mean(x) - BENCHMARK) / sd(x); d1ci <- d1 + c(-1, 1) * 1.96 * se_d_one(d1, n)
# Computes Cohen's d against the benchmark and its normal-approximation 95% CI.
w1 <- wilcox.test(x, mu = BENCHMARK, conf.int = TRUE, exact = FALSE); tick()
# Runs the Wilcoxon signed-rank test against the benchmark with a pseudo-median and CI.
rb1 <- rb_signed(x - BENCHMARK)
# Computes the rank-biserial correlation as the effect size for the Wilcoxon test.
rb1ci <- pct(replicate(B, rb_signed(sample(x, replace = TRUE) - BENCHMARK)))
# Bootstraps a 95% percentile CI for the rank-biserial correlation.
bt1 <- boot(x, function(v, i) mean(v[i]), R = B)
# Bootstraps the sample mean B times.
bca1 <- boot.ci(bt1, type = "bca")$bca[4:5]
# Extracts the BCa bootstrap 95% CI for the mean.
A1 <- data.frame(
# Builds the A1 results table:
  Method = c("One-sample $t$-test", "Wilcoxon signed-rank", "Bootstrap (BCa)"),
  # one row per method,
  `Estimate (CZK)` = c(fmt(mean(x), 1), fmt(w1$estimate, 1), fmt(mean(x), 1)),
  # with the estimate in CZK,
  `95\\% CI` = c(ci_str(t1$conf.int[1], t1$conf.int[2], 1), ci_str(w1$conf.int[1], w1$conf.int[2], 1), ci_str(bca1[1], bca1[2], 1)),
  # its 95% confidence interval,
  `Effect size [95\\% CI]` = c(paste0("$d$ = ", fmt(d1), " ", ci_str(d1ci[1], d1ci[2])),
  # the effect size (Cohen's d with CI, rank-biserial r with CI),
                              paste0("$r_{rb}$ = ", fmt(rb1), " ", ci_str(rb1ci[1], rb1ci[2])), "--"),
                              # (second line of the effect-size column)
  Statistic = c(paste0("$t$(", t1$parameter, ") = ", fmt(t1$statistic)), paste0("$V$ = ", formatC(w1$statistic, format = "d", big.mark = "")), "--"),
  # the test statistic (t, and V for Wilcoxon),
  `$p$` = c(fmtp(t1$p.value), fmtp(w1$p.value), "--"), check.names = FALSE)
  # and the p-value.
write_tab(A1, "A1_table", align = c("l", "l", "r", "r", "r", "r", "r"))
# Saves the table as report/tables/A1_table.tex.
p <- ggplot(data.frame(x = x), aes(x)) +
# Starts the A1 figure: a histogram of prices,
  geom_histogram(binwidth = 5, fill = COP, colour = "white", alpha = .85) +
  # using bins of 5 CZK,
  annotate("rect", xmin = t1$conf.int[1], xmax = t1$conf.int[2], ymin = -Inf, ymax = Inf, fill = ESP, alpha = .15) +
  # shading the 95% CI of the mean,
  geom_vline(xintercept = mean(x), colour = ESP, linewidth = .9) +
  # drawing a solid line at the sample mean,
  geom_vline(xintercept = BENCHMARK, colour = GREY, linetype = "dashed", linewidth = .9) +
  # drawing a dashed line at the benchmark,
  annotate("text", x = mean(x) - 1, y = Inf, vjust = 1.6, hjust = 1, label = paste0("Mean ", sprintf("%.1f", mean(x)), " CZK\n(shaded: 95% CI)"), size = 3.4, colour = ESP) +
  # labelling the mean line,
  annotate("text", x = BENCHMARK + 1, y = Inf, vjust = 1.6, hjust = 0, label = paste0("Benchmark ", BENCHMARK, " CZK"), size = 3.4, colour = GREY) +
  # labelling the benchmark line,
  labs(x = "Cappuccino price (CZK)", y = "Number of cafes",
  # and adding axis labels,
       title = "Cappuccino prices against the benchmark", subtitle = paste0("n = ", n, " cafes"))
       # a title and a subtitle with the sample size.
save_fig(p, "A1_figure")
# Saves the figure (PDF for the report, PNG for the slides).
mac("AoneBench", BENCHMARK); mac("AoneN", n)
# Stores the benchmark and sample size as report macros.
mac("AoneMean", fmt(mean(x), 1)); mac("AoneLo", fmt(t1$conf.int[1], 1)); mac("AoneHi", fmt(t1$conf.int[2], 1))
# Stores the mean and its CI.
mac("AoneT", fmt(t1$statistic)); mac("AoneDf", t1$parameter); mac("AoneP", ptxt(t1$p.value))
# Stores the t statistic, its degrees of freedom and the p-value.
mac("AoneD", fmt(d1)); mac("AoneDLo", fmt(d1ci[1])); mac("AoneDHi", fmt(d1ci[2]))
# Stores Cohen's d and its CI.
mac("AoneWmed", fmt(w1$estimate, 1)); mac("AoneWP", ptxt(w1$p.value))
# Stores the Wilcoxon pseudo-median and p-value.
mac("AoneBLo", fmt(bca1[1], 1)); mac("AoneBHi", fmt(bca1[2], 1))
# Stores the bootstrap BCa CI limits.
res$A1 <- c("A1", "Average price vs.\\ own benchmark", "One-sample $t$-test",
# Adds the A1 row (question, method, estimate, effect size, test) to the summary list:
            paste0(fmt(mean(x), 1), " CZK ", ci_str(t1$conf.int[1], t1$conf.int[2], 1)),
            # the estimate and its CI,
            paste0("$d$ = ", fmt(d1), " ", ci_str(d1ci[1], d1ci[2])),
            # the effect size,
            paste0("$t$(", t1$parameter, ") = ", fmt(t1$statistic), ", ", ptex(t1$p.value)))
            # and the test result.

##############################################################################
## A2  TWO GROUPS  --  is a cappuccino dearer in Prague 1 than in the rest of Prague?
##     H0: mean prices equal.  H1: Prague 1 is higher (ONE-tailed, fixed in advance).
##     Methods: Welch t-test, Wilcoxon rank-sum test, Cohen's d.
##############################################################################
g1 <- d$cappuccino[d$prague1 == "Prague 1"]; g0 <- d$cappuccino[d$prague1 == "Rest of Prague"]
# Splits the cappuccino prices into the Prague 1 group (g1) and the rest of Prague (g0).
n1 <- length(g1); n0 <- length(g0)
# Counts the cafes in each group.
t2 <- t.test(g1, g0, alternative = "greater"); tick()            # one-tailed p
# Runs a one-tailed Welch t-test (Prague 1 higher) and counts it.
t2b <- t.test(g1, g0)                                              # two-sided CI for the difference
# Runs the two-sided version to get the confidence interval of the difference.
diff2 <- mean(g1) - mean(g0)
# Computes the difference in means (Prague 1 minus rest).
sp <- sqrt(((n1 - 1) * var(g1) + (n0 - 1) * var(g0)) / (n1 + n0 - 2))
# Computes the pooled standard deviation of the two groups.
d2 <- diff2 / sp; d2ci <- d2 + c(-1, 1) * 1.96 * se_d_two(d2, n1, n0)
# Computes Cohen's d and its normal-approximation 95% CI.
# (both groups have at least 20 observations, so Cohen's d is used, not Hedges' g)
w2 <- wilcox.test(g1, g0, alternative = "greater", exact = FALSE); tick()
# Runs a one-tailed Wilcoxon rank-sum test (Prague 1 higher) and counts it.
w2b <- wilcox.test(g1, g0, conf.int = TRUE, exact = FALSE)        # Hodges-Lehmann shift, two-sided CI
# Runs the two-sided version to get the Hodges-Lehmann shift and its CI.
rb2 <- rb_two(g1, g0)
# Computes the rank-biserial correlation as the effect size for the rank test.
rb2ci <- pct(replicate(B, rb_two(sample(g1, replace = TRUE), sample(g0, replace = TRUE))))
# Bootstraps a 95% percentile CI for the rank-biserial correlation, resampling each group separately.
A2 <- data.frame(
# Builds the A2 results table:
  Method = c("Welch $t$-test", "Wilcoxon rank-sum"),
  # one row per method,
  `Estimate (CZK)` = c(fmt(diff2, 1), fmt(w2b$estimate, 1)),
  # with the estimate (difference in means / HL shift),
  `95\\% CI` = c(ci_str(t2b$conf.int[1], t2b$conf.int[2], 1), ci_str(w2b$conf.int[1], w2b$conf.int[2], 1)),
  # its 95% CI,
  `Effect size [95\\% CI]` = c(paste0("$d$ = ", fmt(d2), " ", ci_str(d2ci[1], d2ci[2])),
  # the effect size (Cohen's d, rank-biserial r) with CIs,
                              paste0("$r_{rb}$ = ", fmt(rb2), " ", ci_str(rb2ci[1], rb2ci[2]))),
                              # (second line of the effect-size column)
  Statistic = c(paste0("$t$(", fmt(t2$parameter, 1), ") = ", fmt(t2$statistic)), paste0("$W$ = ", formatC(w2$statistic, format = "d", big.mark = ""))),
  # the test statistic (t and W),
  `$p$ (one-tailed)` = c(fmtp(t2$p.value), fmtp(w2$p.value)), check.names = FALSE)
  # and the one-tailed p-value.
write_tab(A2, "A2_table", align = c("l", "l", "r", "r", "r", "r", "r"))
# Saves the table as LaTeX.
g_df <- data.frame(group = factor(rep(c("Rest of Prague", "Prague 1"), c(n0, n1)), levels = c("Rest of Prague", "Prague 1")), y = c(g0, g1))
# Puts both groups into one long data frame for plotting.
ci_g <- do.call(rbind, lapply(split(g_df$y, g_df$group), function(v) { h <- qt(.975, length(v) - 1) * sd(v) / sqrt(length(v)); c(m = mean(v), lo = mean(v) - h, hi = mean(v) + h) }))
# Computes the mean and 95% CI for each group.
ci_g <- data.frame(group = factor(rownames(ci_g), levels = levels(g_df$group)), ci_g)
# Turns the result into a data frame with the group as a factor.
p <- ggplot(g_df, aes(group, y)) +
# Starts the A2 figure,
  geom_jitter(width = .15, height = 0, size = 1.2, alpha = .45, colour = COP) +
  # showing each cafe as a jittered point,
  geom_errorbar(data = ci_g, aes(y = m, ymin = lo, ymax = hi), width = .12, colour = ESP, linewidth = .9) +
  # adding error bars for the group CIs,
  geom_point(data = ci_g, aes(y = m), size = 3.2, colour = ESP) +
  # adding a large point for each group mean,
  labs(x = NULL, y = "Cappuccino price (CZK)", title = "Cappuccino price: Prague 1 vs. the rest of Prague",
  # and adding labels and a title,
       subtitle = paste0("Dark points: group means with 95% CI.  Prague 1 minus rest = ", sprintf("%.1f", diff2), " CZK"))
       # and a subtitle with the difference in means.
save_fig(p, "A2_figure")
# Saves the figure.
mac("AtwoNa", n1); mac("AtwoNb", n0)
# Stores the group sizes as report macros.
mac("AtwoMa", fmt(mean(g1), 1)); mac("AtwoMb", fmt(mean(g0), 1))
# Stores the group means.
mac("AtwoDiff", fmt(diff2, 1)); mac("AtwoLo", fmt(t2b$conf.int[1], 1)); mac("AtwoHi", fmt(t2b$conf.int[2], 1))
# Stores the difference and its CI.
mac("AtwoD", fmt(d2)); mac("AtwoDLo", fmt(d2ci[1])); mac("AtwoDHi", fmt(d2ci[2]))
# Stores Cohen's d and its CI.
mac("AtwoT", fmt(t2$statistic)); mac("AtwoDf", fmt(t2$parameter, 1)); mac("AtwoP", ptxt(t2$p.value))
# Stores the Welch t, degrees of freedom and one-tailed p-value.
mac("AtwoWP", ptxt(w2$p.value)); mac("AtwoHL", fmt(w2b$estimate, 1)); mac("AtwoHLLo", fmt(w2b$conf.int[1], 1)); mac("AtwoHLHi", fmt(w2b$conf.int[2], 1))
# Stores the Wilcoxon p-value and the Hodges-Lehmann shift with its CI.
mac("AtwoRB", fmt(rb2)); mac("AtwoRBLo", fmt(rb2ci[1])); mac("AtwoRBHi", fmt(rb2ci[2]))
# Stores the rank-biserial correlation and its CI.
res$A2 <- c("A2", "Prague 1 minus rest (one-tailed)", "Welch $t$-test",
# Adds the A2 row to the summary list:
            paste0(fmt(diff2, 1), " CZK ", ci_str(t2b$conf.int[1], t2b$conf.int[2], 1)),
            # the estimate and its CI,
            paste0("$d$ = ", fmt(d2), " ", ci_str(d2ci[1], d2ci[2])),
            # the effect size,
            paste0("$t$(", fmt(t2$parameter, 1), ") = ", fmt(t2$statistic), ", ", ptex(t2$p.value)))
            # and the test result.

##############################################################################
## A3  PAIRED  --  how much more does a cappuccino cost than an espresso in the same cafe?
##     H0: mean within-cafe difference = 0.  H1: cappuccino dearer (ONE-tailed).
##     Methods: paired t-test, Wilcoxon signed-rank test, d_z.
##############################################################################
ok <- !is.na(d$cappuccino) & !is.na(d$espresso)
# Flags the cafes that have both a cappuccino and an espresso price.
dd <- d$cappuccino[ok] - d$espresso[ok]; n3 <- length(dd)
# Computes the within-cafe difference (cappuccino minus espresso) and counts the pairs.
t3 <- t.test(dd, alternative = "greater"); tick()
# Runs a one-tailed one-sample (paired) t-test on the differences and counts it.
t3b <- t.test(dd)
# Runs the two-sided version to get the CI.
dz <- mean(dd) / sd(dd); dzci <- dz + c(-1, 1) * 1.96 * se_d_one(dz, n3)
# Computes the paired effect size d_z and its 95% CI.
w3 <- wilcox.test(dd, alternative = "greater", exact = FALSE); tick()
# Runs a one-tailed Wilcoxon signed-rank test on the differences and counts it.
w3b <- wilcox.test(dd, conf.int = TRUE, exact = FALSE)
# Runs the two-sided version for the Hodges-Lehmann shift and its CI.
rb3 <- rb_signed(dd); rb3ci <- pct(replicate(B, rb_signed(sample(dd, replace = TRUE))))
# Computes the rank-biserial correlation of the differences and its bootstrap CI.
A3 <- data.frame(
# Builds the A3 results table:
  Method = c("Paired $t$-test", "Wilcoxon signed-rank"),
  # one row per method,
  `Estimate (CZK)` = c(fmt(mean(dd), 1), fmt(w3b$estimate, 1)),
  # with the estimate (mean difference / HL shift),
  `95\\% CI` = c(ci_str(t3b$conf.int[1], t3b$conf.int[2], 1), ci_str(w3b$conf.int[1], w3b$conf.int[2], 1)),
  # its 95% CI,
  `Effect size [95\\% CI]` = c(paste0("$d_z$ = ", fmt(dz), " ", ci_str(dzci[1], dzci[2])),
  # the effect size (d_z, rank-biserial r) with CIs,
                              paste0("$r_{rb}$ = ", fmt(rb3), " ", ci_str(rb3ci[1], rb3ci[2]))),
                              # (second line of the effect-size column)
  Statistic = c(paste0("$t$(", t3$parameter, ") = ", fmt(t3$statistic)), paste0("$V$ = ", formatC(w3$statistic, format = "d", big.mark = ""))),
  # the test statistic (t and V),
  `$p$ (one-tailed)` = c(fmtp(t3$p.value), fmtp(w3$p.value)), check.names = FALSE)
  # and the one-tailed p-value.
write_tab(A3, "A3_table", align = c("l", "l", "r", "r", "r", "r", "r"))
# Saves the table as LaTeX.
p <- ggplot(data.frame(dd), aes(dd)) +
# Starts the A3 figure: a histogram of the differences,
  geom_histogram(binwidth = 3, fill = COP, colour = "white", alpha = .85) +
  # using bins of 3 CZK,
  annotate("rect", xmin = t3b$conf.int[1], xmax = t3b$conf.int[2], ymin = -Inf, ymax = Inf, fill = ESP, alpha = .2) +
  # shading the 95% CI of the mean difference,
  geom_vline(xintercept = mean(dd), colour = ESP, linewidth = .9) + geom_vline(xintercept = 0, colour = GREY, linetype = "dashed") +
  # drawing a solid line at the mean difference and a dashed line at zero,
  labs(x = "Cappuccino minus espresso price in the same cafe (CZK)", y = "Number of cafes",
  # and adding axis labels,
       title = "Within-cafe price difference", subtitle = paste0("Line: mean difference ", sprintf("%.1f", mean(dd)), " CZK; shaded: 95% CI; dashed: zero.  n = ", n3, " cafes"))
       # a title and a subtitle explaining the lines.
save_fig(p, "A3_figure")
# Saves the figure.
mac("AthreeN", n3); mac("AthreeM", fmt(mean(dd), 1)); mac("AthreeLo", fmt(t3b$conf.int[1], 1)); mac("AthreeHi", fmt(t3b$conf.int[2], 1))
# Stores the pair count, mean difference and its CI as report macros.
mac("AthreeDz", fmt(dz)); mac("AthreeDzLo", fmt(dzci[1])); mac("AthreeDzHi", fmt(dzci[2]))
# Stores d_z and its CI.
mac("AthreeT", fmt(t3$statistic)); mac("AthreeDf", t3$parameter); mac("AthreeP", ptxt(t3$p.value)); mac("AthreeWP", ptxt(w3$p.value))
# Stores the t statistic, degrees of freedom and the two p-values.
mac("AthreeHL", fmt(w3b$estimate, 1)); mac("AthreeHLLo", fmt(w3b$conf.int[1], 1)); mac("AthreeHLHi", fmt(w3b$conf.int[2], 1))
# Stores the Hodges-Lehmann shift and its CI.
mac("AthreeSD", fmt(sd(dd), 1))
# Stores the standard deviation of the differences.
res$A3 <- c("A3", "Cappuccino minus espresso (one-tailed)", "Paired $t$-test",
# Adds the A3 row to the summary list:
            paste0(fmt(mean(dd), 1), " CZK ", ci_str(t3b$conf.int[1], t3b$conf.int[2], 1)),
            # the estimate and its CI,
            paste0("$d_z$ = ", fmt(dz), " ", ci_str(dzci[1], dzci[2])),
            # the effect size,
            paste0("$t$(", t3$parameter, ") = ", fmt(t3$statistic), ", ", ptex(t3$p.value)))
            # and the test result.

##############################################################################
## A4  SEVERAL GROUPS  --  do cappuccino prices differ across districts 1, 2, 3, 5, 7, 8?
##     H0: all district means equal.  H1: at least one differs.
##     Methods: Welch ANOVA, Kruskal-Wallis, Games-Howell / Dunn post hoc tests.
##############################################################################
d4 <- d[!is.na(d$district_main), ]; d4$district_main <- droplevels(d4$district_main)
# Keeps cafes in the six main districts and drops unused factor levels.
n4 <- nrow(d4); k4 <- nlevels(d4$district_main)
# Counts the cafes and the districts.
wa <- oneway.test(cappuccino ~ district_main, data = d4, var.equal = FALSE); tick()
# Runs Welch's ANOVA (unequal variances) of price by district and counts it.
aov4 <- summary(aov(cappuccino ~ district_main, data = d4))[[1]]
# Fits a classical ANOVA to get the sums of squares.
SSB <- aov4[1, "Sum Sq"]; SSW <- aov4[2, "Sum Sq"]; MSW <- aov4[2, "Mean Sq"]; F4 <- aov4[1, "F value"]
# Extracts the between-group, within-group sums of squares, the within mean square and F.
eta2 <- SSB / (SSB + SSW); eta2ci <- eta2_ci(F4, k4 - 1, n4 - k4, n4)
# Computes eta squared and its non-central-F confidence interval.
omega2 <- (SSB - (k4 - 1) * MSW) / (SSB + SSW + MSW)
# Computes omega squared, a less biased effect size.
kw <- kruskal.test(cappuccino ~ district_main, data = d4); tick()
# Runs the Kruskal-Wallis rank test and counts it.
eps2 <- unname(kw$statistic) / (n4 - 1)
# Computes epsilon squared, the rank-based effect size.
eps2ci <- pct(replicate(B, { i <- sample(n4, replace = TRUE); unname(kruskal.test(d4$cappuccino[i], d4$district_main[i])$statistic) / (n4 - 1) }))
# Bootstraps a 95% percentile CI for epsilon squared.
A4 <- data.frame(
# Builds the A4 results table:
  Test = c("Welch ANOVA", "Kruskal--Wallis"),
  # one row per test,
  Statistic = c(paste0("$F$(", k4 - 1, ", ", fmt(wa$parameter[2], 1), ") = ", fmt(wa$statistic)), paste0("$H$(", kw$parameter, ") = ", fmt(kw$statistic))),
  # with the test statistic (F or H),
  `$p$` = c(fmtp(wa$p.value), fmtp(kw$p.value)),
  # the p-value,
  `Effect size [95\\% CI]` = c(paste0("$\\eta^2$ = ", fmt(eta2, 3), " ", ci_str(eta2ci[1], eta2ci[2], 3), "; $\\omega^2$ = ", fmt(omega2, 3)),
  # the effect size (eta squared with CI and omega squared; epsilon squared with CI),
                              paste0("$\\varepsilon^2$ = ", fmt(eps2, 3), " ", ci_str(eps2ci[1], eps2ci[2], 3))),
                              # (second line of the effect-size column)
  check.names = FALSE)
  # keeping the column names as typed.
write_tab(A4, "A4_table", align = c("l", "l", "r", "r", "p{6.4cm}"))
# Saves the table as LaTeX.
# post hoc: Games-Howell (primary, unequal variances) and Dunn with Holm adjustment
y <- d4$cappuccino; g <- d4$district_main
# Shortcuts for the prices (y) and the districts (g).
m_i <- tapply(y, g, mean); v_i <- tapply(y, g, var); n_i <- tapply(y, g, length)
# Computes the mean, variance and size of each district.
pairs <- t(combn(levels(g), 2)); gh <- NULL; dn <- NULL
# Lists all 15 pairs of districts and prepares empty result tables for Games-Howell (gh) and Dunn (dn).
rk <- rank(y); R_i <- tapply(rk, g, mean); tt <- table(y); tie_corr <- sum(tt^3 - tt) / (12 * (n4 - 1))
# Ranks all prices and computes the ties correction used in Dunn's test.
for (r in seq_len(nrow(pairs))) {
# Loops over every pair of districts.
  a <- pairs[r, 1]; b <- pairs[r, 2]
  # Takes the names of the two districts in this pair.
  dif <- m_i[a] - m_i[b]; se <- sqrt(v_i[a] / n_i[a] + v_i[b] / n_i[b])
  # Computes the difference in means and its standard error (unequal variances).
  dfw <- se^4 / ((v_i[a] / n_i[a])^2 / (n_i[a] - 1) + (v_i[b] / n_i[b])^2 / (n_i[b] - 1))
  # Computes the Welch degrees of freedom for the pair.
  q <- abs(dif / se) * sqrt(2); pg <- ptukey(q, k4, dfw, lower.tail = FALSE); crit <- qtukey(.95, k4, dfw) * se / sqrt(2)
  # Computes the studentised range statistic, its Tukey p-value and the CI half-width.
  gh <- rbind(gh, data.frame(Comparison = paste(a, "minus", b), Diff = dif, Lo = dif - crit, Hi = dif + crit, p = pg))
  # Adds the Games-Howell result (difference, CI, adjusted p) for this pair.
  zz <- (R_i[a] - R_i[b]) / sqrt((n4 * (n4 + 1) / 12 - tie_corr * 1) * (1 / n_i[a] + 1 / n_i[b]))
  # Computes Dunn's z for this pair from the mean ranks.
  dn <- rbind(dn, data.frame(Comparison = paste(a, "minus", b), z = zz, p = 2 * pnorm(-abs(zz))))
  # Adds the Dunn result (z and unadjusted p) for this pair.
}
# Ends the loop over pairs.
tick(2 * nrow(pairs))
# Counts the 30 post hoc tests (15 per method).
dn$p_holm <- p.adjust(dn$p, "holm")
# Adjusts the Dunn p-values with Holm's method.
gh_tab <- data.frame(Comparison = gh$Comparison, `Difference (CZK)` = fmt(gh$Diff, 1), `95\\% CI` = mapply(ci_str, gh$Lo, gh$Hi, MoreArgs = list(k = 1)), `Adjusted $p$` = sapply(gh$p, fmtp), check.names = FALSE)
# Formats the Games-Howell results as a table.
dn_tab <- data.frame(Comparison = dn$Comparison, `$z$` = fmt(dn$z), `$p$ (unadjusted)` = sapply(dn$p, fmtp), `$p$ (Holm)` = sapply(dn$p_holm, fmtp), check.names = FALSE)
# Formats the Dunn results as a table.
write_tab(gh_tab, "A4_posthoc_gameshowell", align = c("l", "l", "r", "r", "r"))
# Saves the Games-Howell table.
write_tab(dn_tab, "A4_posthoc_dunn", align = c("l", "l", "r", "r", "r"))
# Saves the Dunn table.
ci_d <- do.call(rbind, lapply(split(y, g), function(v) { h <- qt(.975, length(v) - 1) * sd(v) / sqrt(length(v)); c(m = mean(v), lo = mean(v) - h, hi = mean(v) + h, n = length(v)) }))
# Computes the mean, 95% CI and size of each district for the figure.
ci_d <- data.frame(group = factor(rownames(ci_d), levels = levels(g)), ci_d)
# Turns the result into a data frame with the district as a factor.
p <- ggplot(d4, aes(district_main, cappuccino)) +
# Starts the A4 figure with the cafes of each district,
  geom_jitter(width = .15, height = 0, size = 1.1, alpha = .4, colour = COP) +
  # drawn as jittered points,
  geom_hline(yintercept = mean(y), colour = GREY, linetype = "dashed") +
  # adding a dashed line at the overall mean,
  geom_errorbar(data = ci_d, aes(x = group, y = m, ymin = lo, ymax = hi), width = .15, colour = ESP, linewidth = .9) +
  # adding error bars for the district CIs,
  geom_point(data = ci_d, aes(x = group, y = m), size = 3, colour = ESP) +
  # adding a large point for each district mean,
  geom_text(data = ci_d, aes(x = group, y = 38, label = paste0("n = ", n)), size = 3, colour = GREY) +
  # printing the group size at the bottom of each column,
  labs(x = NULL, y = "Cappuccino price (CZK)", title = "Cappuccino price by district",
  # and adding labels and a title,
       subtitle = "Dark points: district means with 95% CI; dashed line: overall mean of the six districts")
       # and a subtitle explaining the symbols.
save_fig(p, "A4_figure", 7, 4)
# Saves the figure (wider, 7 x 4 inches).
best <- gh[which.min(gh$p), ]
# Finds the pair of districts with the smallest adjusted p-value (the largest gap).
mac("AfourN", n4); mac("AfourF", fmt(wa$statistic)); mac("AfourDfa", k4 - 1); mac("AfourDfb", fmt(wa$parameter[2], 1)); mac("AfourP", ptxt(wa$p.value))
# Stores the sample size, Welch F, degrees of freedom and p-value as report macros.
mac("AfourEta", fmt(eta2, 3)); mac("AfourEtaLo", fmt(eta2ci[1], 3)); mac("AfourEtaHi", fmt(eta2ci[2], 3)); mac("AfourOmega", fmt(omega2, 3))
# Stores eta squared with its CI and omega squared.
mac("AfourH", fmt(kw$statistic)); mac("AfourKWdf", kw$parameter); mac("AfourKWP", ptxt(kw$p.value)); mac("AfourEps", fmt(eps2, 3))
# Stores the Kruskal-Wallis statistic, degrees of freedom, p-value and epsilon squared.
mac("AfourBestPair", best$Comparison); mac("AfourBestDiff", fmt(best$Diff, 1)); mac("AfourBestLo", fmt(best$Lo, 1)); mac("AfourBestHi", fmt(best$Hi, 1)); mac("AfourBestP", ptxt(best$p))
# Stores the largest pairwise gap, its CI and adjusted p-value.
mac("AfourNsigGH", sum(gh$p < ALPHA)); mac("AfourNsigDunn", sum(dn$p_holm < ALPHA))
# Stores how many pairs are significant by Games-Howell and by Dunn-Holm.
mac("AfourMax", names(which.max(m_i))); mac("AfourMin", names(which.min(m_i))); mac("AfourMaxM", fmt(max(m_i), 1)); mac("AfourMinM", fmt(min(m_i), 1))
# Stores the highest and lowest district means and their names.
res$A4 <- c("A4", "Six districts", "Welch ANOVA",
# Adds the A4 row to the summary list:
            paste0("means ", fmt(min(m_i), 1), "--", fmt(max(m_i), 1), " CZK"),
            # the range of district means,
            paste0("$\\eta^2$ = ", fmt(eta2, 3), " ", ci_str(eta2ci[1], eta2ci[2], 3)),
            # the effect size,
            paste0("$F$(", k4 - 1, ", ", fmt(wa$parameter[2], 1), ") = ", fmt(wa$statistic), ", ", ptex(wa$p.value)))
            # and the test result.

##############################################################################
## A5  TWO CATEGORICAL VARIABLES  --  is cafe type related to being in Prague 1?
##     H0: type and location independent.  H1: associated.
##     Methods: chi-square or Fisher's exact test (check expected counts), Cramer's V,
##     standardised residuals.
##############################################################################
d5 <- d[!is.na(d$type), ]; n5 <- nrow(d5)
# Keeps cafes with a known type and counts them.
tab5 <- table(d5$type, d5$prague1)
# Builds the cross-table of cafe type by location.
cs <- suppressWarnings(chisq.test(tab5, correct = FALSE)); tick()
# Runs the Pearson chi-square test (no continuity correction) and counts it.
min_exp <- min(cs$expected)
# Finds the smallest expected cell count (to decide whether chi-square is trustworthy).
fi <- fisher.test(tab5); tick()
# Runs Fisher's exact test and counts it.
V <- function(tb) { e <- outer(rowSums(tb), colSums(tb)) / sum(tb); sqrt(sum((tb - e)^2 / e) / (sum(tb) * (min(dim(tb)) - 1))) }
# Defines Cramer's V from a cross-table.
Vhat <- V(tab5)
# Computes Cramer's V for the data.
Vci <- pct(replicate(B, { i <- sample(n5, replace = TRUE); tb <- table(d5$type[i], d5$prague1[i]); if (any(dim(tb) < 2)) NA else V(tb) }) |> (\(z) z[!is.na(z)])())
# Bootstraps a 95% percentile CI for V, skipping resamples that lose a category.
use_fisher <- min_exp < 5
# Chooses Fisher's test as primary when the smallest expected count is below 5.
A5 <- data.frame(
# Builds the A5 results table:
  Test = c("Pearson chi-square", "Fisher's exact test"),
  # one row per test,
  Statistic = c(paste0("$\\chi^2$(", cs$parameter, ", $N$ = ", n5, ") = ", fmt(cs$statistic)), "--"),
  # with the test statistic (chi-square),
  `$p$` = c(fmtp(cs$p.value), fmtp(fi$p.value)),
  # the p-value,
  `Smallest expected count` = c(fmt(min_exp, 2), "--"),
  # the smallest expected count,
  `Cram\\'er's $V$ [95\\% CI]` = c(paste0(fmt(Vhat), " ", ci_str(Vci[1], Vci[2])), "--"), check.names = FALSE)
  # and Cramer's V with its CI.
write_tab(A5, "A5_table", align = c("l", "l", "r", "r", "r", "r"))
# Saves the table as LaTeX.
pr <- prop.table(tab5, 1) * 100
# Computes the row percentages of the cross-table.
cnt <- data.frame(`Cafe type` = rownames(tab5),
# Builds the counts table with one row per cafe type:
                  `Prague 1` = paste0(tab5[, "Prague 1"], " (", fmt(pr[, "Prague 1"], 1), "\\%)"),
                  # count and percentage in Prague 1,
                  `Rest of Prague` = paste0(tab5[, "Rest of Prague"], " (", fmt(pr[, "Rest of Prague"], 1), "\\%)"),
                  # count and percentage in the rest of Prague,
                  `Total` = rowSums(tab5),
                  # the row total,
                  `Std. residual (Prague 1)` = fmt(cs$stdres[, "Prague 1"]), check.names = FALSE)
                  # and the standardised residual for Prague 1.
write_tab(cnt, "A5_counts", align = c("l", "l", "r", "r", "r", "r"))
# Saves the table as LaTeX.
pp <- do.call(rbind, lapply(rownames(tab5), function(l) { k <- tab5[l, "Prague 1"]; nn <- sum(tab5[l, ]); ci <- prop.test(k, nn, correct = FALSE)$conf.int
# For each type, computes the share in Prague 1 and its Wilson 95% CI.
  data.frame(type = l, share = k / nn, lo = ci[1], hi = ci[2], n = nn) }))
  # Collects the shares, CI limits and group sizes in a data frame.
pp$type <- factor(pp$type, levels = levels(d5$type))
# Fixes the order of the types.
p <- ggplot(pp, aes(type, share)) +
# Starts the A5 figure: the share in Prague 1 by type,
  geom_col(fill = COP, alpha = .8, width = .6) +
  # drawn as bars,
  geom_errorbar(aes(ymin = lo, ymax = hi), width = .12, colour = ESP, linewidth = .9) +
  # with error bars for the CIs,
  geom_text(aes(y = 0.02, label = paste0("n = ", n)), colour = "white", size = 3.4, vjust = 0) +
  # and the group size printed at the base of each bar,
  scale_y_continuous(labels = function(v) paste0(round(v * 100), "%"), limits = c(0, 1)) +
  # with the y axis shown as percentages from 0 to 100%,
  labs(x = NULL, y = "Share of cafes located in Prague 1", title = "Cafe type and location",
  # and adding labels and a title,
       subtitle = "Bars: share in Prague 1; error bars: 95% Wilson CI")
       # and a subtitle explaining the symbols.
save_fig(p, "A5_figure")
# Saves the figure.
mac("AfiveN", n5); mac("AfiveChi", fmt(cs$statistic)); mac("AfiveDf", cs$parameter); mac("AfiveP", ptxt(cs$p.value)); mac("AfiveFisherP", ptxt(fi$p.value))
# Stores the sample size, chi-square, degrees of freedom and both p-values as report macros.
mac("AfiveMinExp", fmt(min_exp, 2)); mac("AfiveV", fmt(Vhat)); mac("AfiveVLo", fmt(Vci[1])); mac("AfiveVHi", fmt(Vci[2]))
# Stores the smallest expected count and Cramer's V with its CI.
mac("AfiveShareInd", fmt(100 * pp$share[1], 1)); mac("AfiveShareLoc", fmt(100 * pp$share[2], 1)); mac("AfiveShareInt", fmt(100 * pp$share[3], 1))
# Stores the share in Prague 1 for each type.
mac("AfiveNInt", pp$n[3]); mac("AfiveKInt", tab5["International chain", "Prague 1"])
# Stores the number of international chains and how many are in Prague 1.
mac("AfiveResInt", fmt(cs$stdres["International chain", "Prague 1"])); mac("AfiveResInd", fmt(cs$stdres["Independent", "Prague 1"])); mac("AfiveResLoc", fmt(cs$stdres["Local chain", "Prague 1"]))
# Stores the standardised residuals for Prague 1 for each type.
res$A5 <- c("A5", "Cafe type $\\times$ Prague 1", if (use_fisher) "Fisher's exact test" else "Pearson chi-square",
# Adds the A5 row to the summary list (naming the primary test):
            paste0("share in Prague 1: ", fmt(100 * pp$share[1], 0), "--", fmt(100 * max(pp$share), 0), "\\%"),
            # the range of shares in Prague 1,
            paste0("$V$ = ", fmt(Vhat), " ", ci_str(Vci[1], Vci[2])),
            # the effect size,
            if (use_fisher) paste0("$N$ = ", n5, ", ", ptex(fi$p.value)) else paste0("$\\chi^2$(", cs$parameter, ", $N$ = ", n5, ") = ", fmt(cs$statistic), ", ", ptex(cs$p.value)))
            # and the test result.

##############################################################################
## A6  CORRELATION  --  does the price fall with the distance from the Orloj?
##     H0: rho = 0.  H1: rho < 0 (ONE-tailed, because the centre is dearer).
##     Methods: Pearson, Spearman, Kendall; bootstrap confidence intervals.
##############################################################################
xs <- d$distance; ys <- d$cappuccino; n6 <- length(xs)
# Takes distance (xs) and price (ys) and counts the cafes.
pe <- cor.test(xs, ys, alternative = "less"); tick(); pe2 <- cor.test(xs, ys)   # one-tailed p; two-sided CI
# Runs a one-tailed Pearson correlation test (negative), counts it, and a two-sided one for the CI.
sr <- suppressWarnings(cor.test(xs, ys, method = "spearman", alternative = "less", exact = FALSE)); tick()
# Runs a one-tailed Spearman rank correlation test and counts it.
ke <- suppressWarnings(cor.test(xs, ys, method = "kendall", alternative = "less", exact = FALSE)); tick()
# Runs a one-tailed Kendall rank correlation test and counts it.
bs_sp <- replicate(B, { i <- sample(n6, replace = TRUE); cor(xs[i], ys[i], method = "spearman") })
# Bootstraps Spearman's rho B times.
bs_ke <- replicate(B, { i <- sample(n6, replace = TRUE); cor(xs[i], ys[i], method = "kendall") })
# Bootstraps Kendall's tau B times.
bs_pe <- replicate(B, { i <- sample(n6, replace = TRUE); cor(xs[i], ys[i]) })
# Bootstraps Pearson's r B times.
sp_ci <- pct(bs_sp); ke_ci <- pct(bs_ke); pe_bci <- pct(bs_pe)
# Takes the 95% percentile intervals of the three bootstraps.
A6 <- data.frame(
# Builds the A6 results table:
  Coefficient = c("Pearson $r$", "Spearman $\\rho$", "Kendall $\\tau$"),
  # one row per coefficient,
  Estimate = c(fmt(pe$estimate), fmt(sr$estimate), fmt(ke$estimate)),
  # with the estimate,
  `95\\% CI` = c(ci_str(pe2$conf.int[1], pe2$conf.int[2]), paste0(ci_str(sp_ci[1], sp_ci[2]), "$^{b}$"), paste0(ci_str(ke_ci[1], ke_ci[2]), "$^{b}$")),
  # its 95% CI (marked b for bootstrap),
  `Shared variance` = c(paste0(fmt(100 * pe$estimate^2, 1), "\\%"), paste0(fmt(100 * sr$estimate^2, 1), "\\%"), "--"),
  # the shared variance (r squared in percent),
  Statistic = c(paste0("$t$(", pe$parameter, ") = ", fmt(pe$statistic)), paste0("$S$ = ", formatC(sr$statistic, format = "f", digits = 0, big.mark = "")), paste0("$z$ = ", fmt(ke$statistic))),
  # the test statistic,
  `$p$ (one-tailed)` = c(fmtp(pe$p.value), fmtp(sr$p.value), fmtp(ke$p.value)), check.names = FALSE)
  # and the one-tailed p-value.
write_tab(A6, "A6_table", align = c("l", "l", "r", "r", "r", "r", "r"))
# Saves the table as LaTeX.
p <- ggplot(data.frame(xs, ys), aes(xs, ys)) +
# Starts the A6 figure: a scatter plot of price against distance,
  geom_point(colour = COP, alpha = .55, size = 1.8) +
  # with each cafe as a point,
  geom_smooth(method = "lm", formula = y ~ x, colour = ESP, fill = ESP, alpha = .15, linewidth = .9) +
  # a linear fit with its confidence band,
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = GREY, linetype = "dashed", linewidth = .8) +
  # and a dashed loess smoother to check for curvature,
  labs(x = "Distance from the Orloj (km)", y = "Cappuccino price (CZK)", title = "Cappuccino price and distance from the Orloj",
  # and adding labels and a title,
       subtitle = paste0("Solid: linear fit with 95% CI; dashed: loess smoother.  Pearson r = ", sprintf("%.2f", pe$estimate), ", n = ", n6))
       # and a subtitle with Pearson's r and n.
save_fig(p, "A6_figure")
# Saves the figure.
mac("AsixN", n6); mac("AsixR", fmt(pe$estimate)); mac("AsixRLo", fmt(pe2$conf.int[1])); mac("AsixRHi", fmt(pe2$conf.int[2]))
# Stores n, Pearson's r and its CI as report macros.
mac("AsixRsq", fmt(100 * pe$estimate^2, 1)); mac("AsixT", fmt(pe$statistic)); mac("AsixDf", pe$parameter); mac("AsixP", ptxt(pe$p.value))
# Stores r squared (percent), the t statistic, degrees of freedom and p-value.
mac("AsixRho", fmt(sr$estimate)); mac("AsixRhoLo", fmt(sp_ci[1])); mac("AsixRhoHi", fmt(sp_ci[2])); mac("AsixRhoP", ptxt(sr$p.value))
# Stores Spearman's rho, its CI and p-value.
mac("AsixTau", fmt(ke$estimate)); mac("AsixTauLo", fmt(ke_ci[1])); mac("AsixTauHi", fmt(ke_ci[2])); mac("AsixTauP", ptxt(ke$p.value))
# Stores Kendall's tau, its CI and p-value.
res$A6 <- c("A6", "Price vs.\\ distance (one-tailed)", "Pearson $r$",
# Adds the A6 row to the summary list:
            paste0("$r$ = ", fmt(pe$estimate), " ", ci_str(pe2$conf.int[1], pe2$conf.int[2])),
            # the estimate and its CI,
            paste0("$r^2$ = ", fmt(pe$estimate^2, 3)),
            # the effect size (r squared),
            paste0("$t$(", pe$parameter, ") = ", fmt(pe$statistic), ", ", ptex(pe$p.value)))
            # and the test result.

##############################################################################
## A7  SIMPLE REGRESSION  --  by what percentage does the price change per kilometre?
##     H0: slope = 0.  H1: slope != 0.
##     Methods: log(price) ~ distance (log-linear) compared with price ~ distance (CZK/km),
##     residual plots, robust (HC3) standard errors.
##############################################################################
m7lin <- lm(cappuccino ~ distance, data = d)
# Fits the linear regression of price (CZK) on distance.
m7log <- lm(log(cappuccino) ~ distance, data = d)
# Fits the log-linear regression of log(price) on distance.
rc_lin <- robust_coef(m7lin)[2, ]; rc_log <- robust_coef(m7log)[2, ]; tick(2)
# Gets the robust slope rows of both models and counts the two tests.
pc <- 100 * (exp(rc_log[c("b", "lo", "hi")]) - 1)      # percentage change per km
# Converts the log slope and its CI into a percentage change in price per km.
reg_table(list(m7lin, m7log), c("Price (CZK)", "log(price)"),
# Builds and saves the A7 regression table with both models,
          list("(Intercept)" = "Intercept", "distance" = "Distance (km)"), "A7_table", "")
          # showing the intercept and the distance slope.
diag_plots(m7lin, "A7_diagnostics_lin"); diag_plots(m7log, "A7_diagnostics_log")
# Saves the diagnostic plots of both models.
grid7 <- data.frame(distance = seq(min(d$distance), max(d$distance), length.out = 100))
# Creates a grid of 100 distances covering the data range.
bl <- robust_band(m7lin, grid7); bg <- robust_band(m7log, grid7)
# Computes the fitted values with robust bands for the linear and log models.
p <- ggplot(d, aes(distance, cappuccino)) +
# Starts the A7 figure: price against distance,
  geom_point(colour = COP, alpha = .5, size = 1.8) +
  # with each cafe as a point,
  geom_ribbon(data = bl, aes(y = fit, ymin = lo, ymax = hi), fill = ESP, alpha = .15) +
  # the robust confidence band of the linear fit,
  geom_line(data = bl, aes(y = fit), colour = ESP, linewidth = .9) +
  # the linear fit as a solid line,
  geom_line(data = bg, aes(y = exp(fit)), colour = GREY, linetype = "dashed", linewidth = .9) +
  # the log-linear fit (back-transformed to CZK) as a dashed line,
  labs(x = "Distance from the Orloj (km)", y = "Cappuccino price (CZK)", title = "Simple regression of price on distance",
  # and adding labels and a title,
       subtitle = paste0("Solid: ", sprintf("%.2f", rc_lin$b), " CZK per km (robust 95% CI band); dashed: log-linear fit (", sprintf("%.1f", pc$b), "% per km)"))
       # and a subtitle with the slope in CZK and the percentage change per km.
save_fig(p, "A7_figure")
# Saves the figure.
cd7 <- cooks.distance(m7lin); infl7 <- which(cd7 > 4 / nrow(d))
# Finds the influential cafes with Cook's distance above 4/n.
m7lin_x <- lm(cappuccino ~ distance, data = d[-infl7, ])
# Refits the linear model without those cafes.
mac("AsevenN", nobs(m7lin)); mac("AsevenB", fmt(rc_lin$b)); mac("AsevenLo", fmt(rc_lin$lo)); mac("AsevenHi", fmt(rc_lin$hi))
# Stores n, the CZK slope with its CI as report macros.
mac("AsevenP", ptxt(rc_lin$p)); mac("AsevenT", fmt(rc_lin$t)); mac("AsevenDf", df.residual(m7lin)); mac("AsevenRsq", fmt(summary(m7lin)$r.squared, 3))
# Stores the p-value, t statistic, degrees of freedom and R squared of the linear model.
mac("AsevenLogB", fmt(rc_log$b, 3)); mac("AsevenPct", fmt(pc$b, 1)); mac("AsevenPctLo", fmt(pc$lo, 1)); mac("AsevenPctHi", fmt(pc$hi, 1))
# Stores the log slope and the percentage change with its CI.
mac("AsevenLogP", ptxt(rc_log$p)); mac("AsevenLogT", fmt(rc_log$t)); mac("AsevenLogRsq", fmt(summary(m7log)$r.squared, 3))
# Stores the p-value, t statistic and R squared of the log model.
mac("AsevenNinfl", length(infl7)); mac("AsevenBinfl", fmt(coef(m7lin_x)[2])); mac("AsevenMaxCook", fmt(max(cd7), 2))
# Stores the number of influential cafes, the slope without them and the largest Cook's distance.
mac("AsevenMaxDist", fmt(max(d$distance), 1))
# Stores the largest distance in the data.
res$A7 <- c("A7", "log(price) on distance", "Log-linear regression, HC3",
# Adds the A7 row to the summary list:
            paste0(fmt(pc$b, 1), "\\% per km ", ci_str(pc$lo, pc$hi, 1)),
            # the estimate (percent per km) and its CI,
            paste0("$R^2$ = ", fmt(summary(m7log)$r.squared, 3)),
            # the effect size (R squared),
            paste0("$t$(", df.residual(m7log), ") = ", fmt(rc_log$t), ", ", ptex(rc_log$p)))
            # and the test result.

##############################################################################
## A8  MULTIPLE REGRESSION  --  does the distance slope survive once cafe type is in the model?
##     H0: cafe type adds nothing.  H1: cafe type improves the model.
##     Methods: price ~ distance + type, nested F-test, variance inflation factors.
##############################################################################
d8 <- d[!is.na(d$type), ]                       # the one cafe without a type is lost in both models
# Keeps cafes with a known type (the same cafes are used in both models).
m8a <- lm(cappuccino ~ distance, data = d8)
# Fits Model 1: price on distance.
m8b <- lm(cappuccino ~ distance + type, data = d8)
# Fits Model 2: price on distance and cafe type.
nf8 <- anova(m8a, m8b); tick()
# Runs the nested F-test comparing the two models and counts it.
vif8 <- vif(m8b)
# Computes the variance inflation factors of Model 2.
rc8a <- robust_coef(m8a); rc8b <- robust_coef(m8b)
# Gets the robust coefficient tables of both models.
reg_table(list(m8a, m8b), c("Model 1", "Model 2"),
# Builds and saves the A8 regression table,
          list("(Intercept)" = "Intercept", "distance" = "Distance (km)",
          # listing the intercept, distance
               "typeLocal chain" = "Local chain (vs. independent)", "typeInternational chain" = "International chain (vs. independent)"),
               # and the two type effects with readable labels,
          "A8_table", "")
          # under the file name A8_table.
diag_plots(m8b, "A8_diagnostics")
# Saves the diagnostic plots of Model 2.
b_a <- coef(m8a)["distance"]; b_b <- coef(m8b)["distance"]
# Stores the distance slope without and with type.
grid8 <- expand.grid(distance = seq(min(d8$distance), max(d8$distance), length.out = 60), type = levels(d8$type))
# Creates a grid of distances for each cafe type.
grid8$type <- factor(grid8$type, levels = levels(d8$type)); b8 <- robust_band(m8b, grid8)
# Makes the type a factor and computes the fitted values with robust bands.
p <- ggplot(d8, aes(distance, cappuccino, colour = type)) +
# Starts the A8 figure: price against distance coloured by type,
  geom_point(alpha = .5, size = 1.7) + geom_line(data = b8, aes(y = fit), linewidth = 1) +
  # with each cafe as a point and the fitted line of Model 2 per type,
  scale_colour_manual(values = c("Independent" = ESP, "Local chain" = COP, "International chain" = "#4F6F5B"), name = NULL) +
  # using a fixed colour for each type,
  labs(x = "Distance from the Orloj (km)", y = "Cappuccino price (CZK)", title = "Price, distance and cafe type",
  # and adding labels and a title,
       subtitle = paste0("Lines: Model 2 fits. Distance slope ", sprintf("%.2f", rc8b$b[2]), " CZK per km, robust 95% CI [", sprintf("%.2f", rc8b$lo[2]), ", ", sprintf("%.2f", rc8b$hi[2]), "]")) +
       # and a subtitle with the distance slope and its robust CI,
  theme(legend.position = "bottom")
  # placing the legend below the plot.
save_fig(p, "A8_figure")
# Saves the figure.
mac("AeightN", nobs(m8b)); mac("AeightBa", fmt(b_a)); mac("AeightBb", fmt(b_b)); mac("AeightBbLo", fmt(rc8b$lo[2])); mac("AeightBbHi", fmt(rc8b$hi[2])); mac("AeightBaLo", fmt(rc8a$lo[2])); mac("AeightBaHi", fmt(rc8a$hi[2]))
# Stores n, both distance slopes and their robust CIs as report macros.
mac("AeightChange", fmt(b_b - b_a)); mac("AeightRelChange", fmt(100 * (b_b - b_a) / abs(b_a), 1))
# Stores how much the slope changes when type is added.
mac("AeightF", fmt(nf8$F[2])); mac("AeightDfa", nf8$Df[2]); mac("AeightDfb", nf8$Res.Df[2]); mac("AeightP", ptxt(nf8$`Pr(>F)`[2]))
# Stores the F test of the nested comparison (F, degrees of freedom, p-value).
mac("AeightDeltaR", fmt(summary(m8b)$r.squared - summary(m8a)$r.squared, 3)); mac("AeightRsqA", fmt(summary(m8a)$r.squared, 3)); mac("AeightRsqB", fmt(summary(m8b)$r.squared, 3))
# Stores the change in R squared and both R squared values.
mac("AeightVifDist", fmt(vif8["distance", 1])); mac("AeightVifType", fmt(vif8["type", 1])); mac("AeightGVIFadj", fmt(vif8["type", 3]))
# Stores the VIFs for distance and type.
mac("AeightLocal", fmt(rc8b$b[3], 1)); mac("AeightLocalLo", fmt(rc8b$lo[3], 1)); mac("AeightLocalHi", fmt(rc8b$hi[3], 1))
# Stores the local-chain effect and its robust CI.
mac("AeightInt", fmt(rc8b$b[4], 1)); mac("AeightIntLo", fmt(rc8b$lo[4], 1)); mac("AeightIntHi", fmt(rc8b$hi[4], 1))
# Stores the international-chain effect and its robust CI.
cd8 <- cooks.distance(m8b); infl8 <- which(cd8 > 4 / nrow(d8)); m8x <- lm(cappuccino ~ distance + type, data = d8[-infl8, ])
# Finds the influential cafes (Cook's distance > 4/n) and refits Model 2 without them.
mac("AeightNinfl", length(infl8)); mac("AeightBinfl", fmt(coef(m8x)["distance"]))
# Stores the number of influential cafes and the distance slope without them.
res$A8 <- c("A8", "Type adds to distance model", "Nested $F$-test, HC3 slopes",
# Adds the A8 row to the summary list:
            paste0("slope ", fmt(rc8b$b[2]), " CZK/km ", ci_str(rc8b$lo[2], rc8b$hi[2])),
            # the distance slope and its CI,
            paste0("$\\Delta R^2$ = ", fmt(summary(m8b)$r.squared - summary(m8a)$r.squared, 3)),
            # the effect size (change in R squared),
            paste0("$F$(", nf8$Df[2], ", ", nf8$Res.Df[2], ") = ", fmt(nf8$F[2]), ", ", ptex(nf8$`Pr(>F)`[2])))
            # and the nested F-test result.

##############################################################################
## A9  INTERACTION  --  does the price fall with distance in the same way for
##     independent cafes and chains (local and international together)?
##     H0: interaction coefficient = 0.  H1: interaction coefficient != 0.
##     Methods: price ~ distance * chain, nested F-test against the model without the
##     interaction, slope in each group with its CI, plot of the fitted lines.
##############################################################################
d9 <- d[!is.na(d$chain), ]
# Keeps cafes with a known chain status.
m9a <- lm(cappuccino ~ distance + chain, data = d9)
# Fits the model without interaction (same slope in both groups).
m9b <- lm(cappuccino ~ distance * chain, data = d9)
# Fits the model with interaction (different slopes allowed).
nf9 <- anova(m9a, m9b); tick()
# Runs the nested F-test for the interaction and counts it.
rc9 <- robust_coef(m9b); V9 <- vcovHC(m9b, type = "HC3"); tq9 <- qt(.975, df.residual(m9b))
# Gets the robust coefficients, the robust covariance matrix and the t critical value.
b9 <- coef(m9b)
# Stores the coefficients of the interaction model.
slope_ind <- b9["distance"]; se_ind <- sqrt(V9["distance", "distance"])
# Computes the distance slope for independents and its robust standard error.
slope_ch  <- b9["distance"] + b9["distance:chainChain"]
# Computes the distance slope for chains (base slope plus interaction).
se_ch <- sqrt(V9["distance", "distance"] + V9["distance:chainChain", "distance:chainChain"] + 2 * V9["distance", "distance:chainChain"])
# Computes the robust standard error of the chain slope from the variances and covariance.
reg_table(list(m9a, m9b), c("No interaction", "Interaction"),
# Builds and saves the A9 regression table with both models,
          list("(Intercept)" = "Intercept", "distance" = "Distance (km)", "chainChain" = "Chain (vs. independent)", "distance:chainChain" = "Distance $\\times$ chain"),
          # listing the intercept, distance, chain and the interaction,
          "A9_table", "")
          # under the file name A9_table.
diag_plots(m9b, "A9_diagnostics")
# Saves the diagnostic plots of the interaction model.
grid9 <- expand.grid(distance = seq(min(d9$distance), max(d9$distance), length.out = 60), chain = levels(d9$chain))
# Creates a grid of distances for each group.
grid9$chain <- factor(grid9$chain, levels = levels(d9$chain)); b9b <- robust_band(m9b, grid9)
# Makes the chain indicator a factor and computes the fitted values with robust bands.
i9 <- which(rc9$term == "distance:chainChain")
# Finds the row of the interaction coefficient.
p <- ggplot(d9, aes(distance, cappuccino, colour = chain, fill = chain)) +
# Starts the A9 figure: price against distance coloured by group,
  geom_point(alpha = .5, size = 1.7) +
  # with each cafe as a point,
  geom_ribbon(data = b9b, aes(y = fit, ymin = lo, ymax = hi), alpha = .15, colour = NA) +
  # a robust confidence band for each group,
  geom_line(data = b9b, aes(y = fit), linewidth = 1) +
  # and the fitted line for each group,
  scale_colour_manual(values = c("Independent" = ESP, "Chain" = COP), name = NULL) +
  # using fixed line colours,
  scale_fill_manual(values = c("Independent" = ESP, "Chain" = COP), name = NULL) +
  # and fixed fill colours,
  labs(x = "Distance from the Orloj (km)", y = "Cappuccino price (CZK)", title = "Price against distance: independent cafes and chains",
  # and adding labels and a title,
       subtitle = paste0("Slopes: independent ", sprintf("%.2f", slope_ind), ", chains ", sprintf("%.2f", slope_ch), " CZK per km (robust 95% CI bands)")) +
       # and a subtitle with the two slopes,
  theme(legend.position = "bottom")
  # placing the legend below the plot.
save_fig(p, "A9_figure")
# Saves the figure.
mac("AnineN", nobs(m9b)); mac("AnineNind", sum(d9$chain == "Independent")); mac("AnineNch", sum(d9$chain == "Chain"))
# Stores the sample size and group sizes as report macros.
mac("AnineInt", fmt(rc9$b[i9])); mac("AnineIntLo", fmt(rc9$lo[i9])); mac("AnineIntHi", fmt(rc9$hi[i9]))
# Stores the interaction coefficient and its robust CI.
mac("AnineF", fmt(nf9$F[2])); mac("AnineDfa", nf9$Df[2]); mac("AnineDfb", nf9$Res.Df[2]); mac("AnineP", ptxt(nf9$`Pr(>F)`[2]))
# Stores the nested F-test result (F, degrees of freedom, p-value).
mac("AnineDeltaR", fmt(summary(m9b)$r.squared - summary(m9a)$r.squared, 3))
# Stores the change in R squared caused by the interaction.
mac("AnineSlopeInd", fmt(slope_ind)); mac("AnineSlopeIndLo", fmt(slope_ind - tq9 * se_ind)); mac("AnineSlopeIndHi", fmt(slope_ind + tq9 * se_ind))
# Stores the independent slope and its CI.
mac("AnineSlopeCh", fmt(slope_ch)); mac("AnineSlopeChLo", fmt(slope_ch - tq9 * se_ch)); mac("AnineSlopeChHi", fmt(slope_ch + tq9 * se_ch))
# Stores the chain slope and its CI.
mac("AnineMaxDiff", fmt(max(abs(rc9$lo[i9]), abs(rc9$hi[i9]))))
# Stores the largest difference in slopes compatible with the CI.
res$A9 <- c("A9", "Distance slope: independent vs. chain", "Nested $F$-test, HC3",
# Adds the A9 row to the summary list:
            paste0("interaction ", fmt(rc9$b[i9]), " CZK/km ", ci_str(rc9$lo[i9], rc9$hi[i9])),
            # the interaction and its CI,
            paste0("$\\Delta R^2$ = ", fmt(summary(m9b)$r.squared - summary(m9a)$r.squared, 3)),
            # the effect size (change in R squared),
            paste0("$F$(", nf9$Df[2], ", ", nf9$Res.Df[2], ") = ", fmt(nf9$F[2]), ", ", ptex(nf9$`Pr(>F)`[2])))
            # and the nested F-test result.

##############################################################################
## A10  LOGISTIC REGRESSION  --  by how many percentage points does the probability that a
##      cappuccino costs more than the median price (83 CZK, from the brief) change per km?
##      H0: distance coefficient = 0.  H1: distance coefficient != 0.
##      Methods: logit of the indicator on distance and cafe type, average marginal effect
##      (AME) of distance in percentage points, predicted-probability plot.
##############################################################################
d10 <- d[!is.na(d$type), ]; d10$high <- as.integer(d10$cappuccino > 83)
# Keeps cafes with a known type and creates the 0/1 outcome 'price above 83 CZK'.
m10 <- glm(high ~ distance + type, data = d10, family = binomial()); tick()
# Fits a logistic regression of that outcome on distance and type and counts it.
X <- model.matrix(m10); pr10 <- fitted(m10); bd <- coef(m10)["distance"]; V10 <- vcov(m10)
# Gets the design matrix, fitted probabilities, the distance coefficient and the covariance matrix.
ame <- mean(bd * pr10 * (1 - pr10))
# Computes the average marginal effect (AME) of distance on the probability.
# delta method: gradient of the AME with respect to all coefficients
grad <- colMeans(X * (bd * pr10 * (1 - pr10) * (1 - 2 * pr10))) + c(0, mean(pr10 * (1 - pr10)), 0, 0)
# Computes the gradient of the AME with respect to the coefficients.
se_ame <- sqrt(drop(t(grad) %*% V10 %*% grad))
# Computes the AME's standard error by the delta method.
ame_ci <- ame + c(-1, 1) * 1.96 * se_ame
# Computes the 95% CI of the AME.
or10 <- exp(bd); or10ci <- exp(confint.default(m10)["distance", ])
# Computes the odds ratio per km and its CI.
ll1 <- as.numeric(logLik(m10)); ll0 <- as.numeric(logLik(glm(high ~ 1, data = d10, family = binomial())))
# Computes the log-likelihoods of the model and of the intercept-only model.
mcf <- 1 - ll1 / ll0; zz10 <- summary(m10)$coefficients["distance", "z value"]; pz10 <- summary(m10)$coefficients["distance", "Pr(>|z|)"]
# Computes McFadden's pseudo R squared and the Wald z and p-value of distance.
A10 <- data.frame(
# Builds the A10 results table:
  Quantity = c("Average marginal effect of distance (percentage points per km)", "Odds ratio for distance (per km)", "Logit coefficient for distance", "Observations / events (price above 83 CZK)", "McFadden pseudo-$R^2$"),
  # one row per quantity,
  Estimate = c(fmt(100 * ame, 2), fmt(or10, 2), fmt(bd, 2), paste0(nrow(d10), " / ", sum(d10$high)), fmt(mcf, 3)),
  # with the estimate,
  `95\\% CI` = c(ci_str(100 * ame_ci[1], 100 * ame_ci[2]), ci_str(or10ci[1], or10ci[2]), ci_str(confint.default(m10)["distance", 1], confint.default(m10)["distance", 2]), "--", "--"),
  # its 95% CI,
  Statistic = c("--", "--", paste0("$z$ = ", fmt(zz10)), "--", "--"),
  # the test statistic,
  `$p$` = c("--", "--", fmtp(pz10), "--", "--"), check.names = FALSE)
  # and the p-value.
write_tab(A10, "A10_table", align = c("l", "p{6cm}", "r", "r", "r", "r"))
# Saves the table as LaTeX.
diag_plots_logit <- NULL
# Placeholder (not used).
grid10 <- data.frame(distance = seq(min(d10$distance), max(d10$distance), length.out = 100), type = factor("Independent", levels = levels(d10$type)))
# Creates a grid of distances for an independent cafe.
pl <- predict(m10, grid10, type = "link", se.fit = TRUE)
# Predicts the log-odds with standard errors.
grid10$fit <- plogis(pl$fit); grid10$lo <- plogis(pl$fit - 1.96 * pl$se.fit); grid10$hi <- plogis(pl$fit + 1.96 * pl$se.fit)
# Converts the predictions and CI limits to probabilities.
p <- ggplot(d10, aes(distance)) +
# Starts the A10 figure with distance on the x axis,
  geom_point(aes(y = high), position = position_jitter(height = .03, width = 0), colour = COP, alpha = .35, size = 1.4) +
  # showing the observed 0/1 outcomes as jittered points,
  geom_ribbon(data = grid10, aes(ymin = lo, ymax = hi), fill = ESP, alpha = .15) +
  # a confidence band for the predicted probability,
  geom_line(data = grid10, aes(y = fit), colour = ESP, linewidth = 1) +
  # the predicted probability curve,
  scale_y_continuous(labels = function(v) paste0(round(v * 100), "%"), limits = c(-.05, 1.05), breaks = seq(0, 1, .25)) +
  # with the y axis as percentages,
  labs(x = "Distance from the Orloj (km)", y = "Probability of a price above 83 CZK", title = "Probability of an above-median cappuccino price",
  # and adding labels and a title,
       subtitle = paste0("Curve: independent cafe, 95% CI band. Average marginal effect: ", sprintf("%.1f", 100 * ame), " pp per km"))
       # and a subtitle with the AME.
save_fig(p, "A10_figure")
# Saves the figure.
mac("AtenN", nrow(d10)); mac("AtenEvents", sum(d10$high)); mac("AtenAME", fmt(100 * ame, 1)); mac("AtenAMELo", fmt(100 * ame_ci[1], 1)); mac("AtenAMEHi", fmt(100 * ame_ci[2], 1))
# Stores n, the number of events, the AME and its CI as report macros.
mac("AtenOR", fmt(or10)); mac("AtenORLo", fmt(or10ci[1])); mac("AtenORHi", fmt(or10ci[2])); mac("AtenZ", fmt(zz10)); mac("AtenP", ptxt(pz10)); mac("AtenMcF", fmt(mcf, 3))
# Stores the odds ratio and its CI, the Wald z, p-value and McFadden R squared.
mac("AtenSampleMedian", fmt(median(d$cappuccino), 1)); mac("AtenShareHigh", fmt(100 * mean(d10$high), 1))
# Stores the sample median price and the share of cafes above 83 CZK.
res$A10 <- c("A10", "Probability price $>$ 83 CZK", "Logit, average marginal effect",
# Adds the A10 row to the summary list:
             paste0(fmt(100 * ame, 1), " pp per km ", ci_str(100 * ame_ci[1], 100 * ame_ci[2], 1)),
             # the AME and its CI,
             paste0("OR = ", fmt(or10), " ", ci_str(or10ci[1], or10ci[2])),
             # the odds ratio and its CI,
             paste0("$z$ = ", fmt(zz10), ", ", ptex(pz10)))
             # and the Wald test result.

##############################################################################
## SUMMARY TABLE OF ALL TEN ANALYSES
##############################################################################
summ <- as.data.frame(do.call(rbind, res), stringsAsFactors = FALSE)
# Stacks the ten summary rows into one data frame.
names(summ) <- c("", "Question", "Primary method", "Estimate [95\\% CI]", "Effect size [95\\% CI]", "Test")
# Names its columns.
write_tab(summ, "summary", align = c("l", "l", "p{2.6cm}", "p{2.6cm}", "p{3.0cm}", "p{2.6cm}", "p{3.0cm}"))
# Saves the summary table as LaTeX.

##############################################################################
## ROBUSTNESS OF THE DISTANCE RESULT (appendix): does it survive reasonable alternatives?
##############################################################################
rob_row <- function(label, dat) {
# Defines a helper that reruns the distance analysis on a given subset of the data.
  r <- cor.test(dat$distance, dat$cappuccino); s <- suppressWarnings(cor.test(dat$distance, dat$cappuccino, method = "spearman", exact = FALSE))
  # Computes Pearson's r and Spearman's rho for the subset.
  m <- lm(cappuccino ~ distance, data = dat); rc <- robust_coef(m)[2, ]
  # Fits the linear regression and gets its robust slope.
  ml <- lm(log(cappuccino) ~ distance, data = dat); rl <- robust_coef(ml)[2, ]
  # Fits the log-linear regression and gets its robust slope.
  c(label, nrow(dat), fmt(r$estimate), fmt(s$estimate), paste0(fmt(rc$b), " ", ci_str(rc$lo, rc$hi)), paste0(fmt(100 * (exp(rl$b) - 1), 1), "\\%"))
  # Returns one row: label, n, r, rho, slope with CI, and percent change per km.
}
# Ends the function.
tick(2 * 4)
# Counts the 8 tests in this robustness table.
rob <- as.data.frame(do.call(rbind, list(
# Builds the robustness table from five variants of the sample:
  rob_row("All cafes (main analysis)", d),
  # all cafes,
  rob_row("Without the 12 cells with several prices", d[!d$multi_cappuccino, ]),
  # without cappuccino cells that listed several prices,
  rob_row("Without the cafe that sells oat milk only (Srnky)", d[!grepl("oat milk", tolower(d$note)) , ]),
  # without the oat-milk-only cafe,
  rob_row("Without the most distant cafes (> 5 km)", d[d$distance <= 5, ]),
  # without cafes farther than 5 km,
  rob_row("Without influential cafes (Cook's $D$ > 4/$n$)", d[-infl7, ])
  # and without the influential cafes.
)), stringsAsFactors = FALSE)
# Converts the rows to a data frame.
names(rob) <- c("Sample", "$n$", "Pearson $r$", "Spearman $\\rho$", "Slope, CZK per km [robust 95\\% CI]", "Change per km (log model)")
# Names the columns.
write_tab(rob, "robustness", align = c("l", "p{5.2cm}", "r", "r", "r", "p{3.4cm}", "r"))
# Saves the table as LaTeX.
mac("RobMinR", fmt(min(as.numeric(gsub("\\$-\\$", "-", rob[[3]]))))); mac("RobMaxR", fmt(max(as.numeric(gsub("\\$-\\$", "-", rob[[3]])))))
# Stores the smallest and largest Pearson r across the variants as report macros.

## EXTRA NUMBERS QUOTED IN THE TEXT OF THE REPORT -------------------------------------
skew <- function(v) { v <- v[!is.na(v)]; m <- mean(v); mean((v - m)^3) / mean((v - m)^2)^1.5 }
# Defines the skewness function (third central moment over variance to the power 1.5).
cl <- read.csv("analysis/cleaning_log.csv", stringsAsFactors = FALSE)
# Reads the cleaning log written by 01_cleaning.R.
getn <- function(s) cl$cafes_affected[cl$step == s]
# Defines a helper that looks up the number of cafes affected by a cleaning step.
mac("ClnEmpty", getn("Empty rows")); mac("ClnDistrict", getn("District labels")); mac("ClnType", getn("Cafe type"))
# Stores the numbers of empty rows, district labels and missing types changed.
mac("ClnCap", getn("Cappuccino price")); mac("ClnEsp", getn("Espresso price")); mac("ClnSep", getn("Number of ratings")); mac("ClnDup", getn("Duplicate cafes"))
# Stores the numbers of multi-price cells, thousands separators and duplicates.
mac("Nraw", nrow(read.csv("analysis/cafe_raw.csv", colClasses = "character")))
# Stores the number of rows in the raw file.
mac("Nfilled", N_ALL + getn("Duplicate cafes"))
# Stores the number of filled rows (cafes before removing duplicates).
mac("Ntype", sum(!is.na(d$type)))
# Stores the number of cafes with a known type.
mac("PriceMean", fmt(mean(d$cappuccino), 1)); mac("PriceSD", fmt(sd(d$cappuccino), 1)); mac("PriceMedian", fmt(median(d$cappuccino), 1))
# Stores the mean, SD and median of the cappuccino price.
mac("PriceIQR", fmt(IQR(d$cappuccino), 1)); mac("PriceSkew", fmt(skew(d$cappuccino))); mac("PriceMin", fmt(min(d$cappuccino), 0)); mac("PriceMax", fmt(max(d$cappuccino), 0))
# Stores the IQR, skewness, minimum and maximum of the price.
mac("DistSkew", fmt(skew(d$distance))); mac("DistMedian", fmt(median(d$distance))); mac("DistMax", fmt(max(d$distance), 1))
# Stores the skewness, median and maximum of the distance.
mac("DiffSkew", fmt(skew(dd)))
# Stores the skewness of the cappuccino-minus-espresso differences.
mac("AsevenSkewLin", fmt(skew(resid(m7lin)))); mac("AsevenSkewLog", fmt(skew(resid(m7log))))
# Stores the skewness of the residuals of the two A7 models.
mac("AoneDiffAbs", fmt(abs(mean(d$cappuccino) - BENCHMARK), 1)); mac("AoneDir", if (mean(d$cappuccino) < BENCHMARK) "below" else "above")
# Stores the absolute gap to the benchmark and whether the mean is below or above it.
mac("AonePctRel", fmt(100 * abs(mean(d$cappuccino) - BENCHMARK) / BENCHMARK, 1))
# Stores that gap as a percentage of the benchmark.
mac("NstudentsCafes", 20)
# Stores the number of cafes each student visited (20).
mac("AthreePctEsp", fmt(100 * mean(dd) / mean(d$espresso), 0)); mac("EspMean", fmt(mean(d$espresso), 1))
# Stores the cappuccino markup over the espresso in percent and the mean espresso price.

## NUMBER OF TESTS -------------------------------------------------------------------
mac("Ntests", ntests)
# Stores the total number of tests counted.
cat("\nTests counted in this report:", ntests, "\n")
# Prints that number on screen.

## WRITE THE MACROS ------------------------------------------------------------------
writeLines(c("% numbers.tex -- written by analysis/03_tests.R; do not edit by hand",
# Writes numbers.tex: a header line followed by one \newcommand per stored number,
             paste0("\\newcommand{\\", names(macros), "}{", macros, "}")), "report/tables/numbers.tex")
             # using the macro names and values, to report/tables/numbers.tex.

## PRINT THE KEY RESULTS ------------------------------------------------------------
print(summ[, c(1, 4:6)], right = FALSE)
# Prints the summary table (without the question column) on screen.
cat("\nPlanned checks:\n"); print(A6); print(rob)
# Prints the headline correlation and robustness tables as a check.
sessionInfo()
# Prints the R and package versions used, for reproducibility.
