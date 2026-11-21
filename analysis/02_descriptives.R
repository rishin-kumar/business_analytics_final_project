# =============================================================================
# 02_descriptives.R  --  Prague Cafe Project, Batch 1
#
# Reads analysis/cafe_clean.csv (made by 01_cleaning.R) and produces the
# description of the data and the checks of distributions and normality that
# are needed to choose the primary method for each question in 03_tests.R.
# All tables go to report/tables, all figures to report/figures.
#
# RUN FROM THE REPOSITORY ROOT. Order: 01_cleaning.R -> 02_descriptives.R -> 03_tests.R
# =============================================================================

## 0. SETUP ------------------------------------------------------------------
if (!file.exists("analysis/cafe_clean.csv")) stop("Run 01_cleaning.R first (from the repository root).")
# Stops with a message if the cleaned dataset does not exist yet (script 01 must run first).
dir.create("report/tables",  recursive = TRUE, showWarnings = FALSE)
# Makes sure the folder for LaTeX tables exists.
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)
# Makes sure the folder for figures exists.

load_packages <- function(pkgs) {
  # Defines a helper that takes a vector of package names.
  for (p in pkgs) {
    # Loops over each package name.
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
    # Installs the package from CRAN only if it is missing.
    library(p, character.only = TRUE)
    # Attaches the package.
  }
  # Ends the loop.
}
# Ends the helper function.
load_packages(c("ggplot2", "xtable"))
# Loads ggplot2 (figures) and xtable (LaTeX tables).

COP <- "#512bb8"; ESP <- "#2B1D16"; GREY <- "#8A7F76"       # colours used in all figures
# Defines the three colours (purple, dark brown, grey) so every figure looks the same.
theme_set(theme_minimal(base_size = 11) +
          # Sets the default plot theme to a clean minimal one with 11 pt text,
          theme(panel.grid.minor = element_blank(),
                # removing the minor grid lines,
                plot.title = element_text(face = "bold"),
                # making titles bold,
                strip.text = element_text(face = "bold")))
                # and making facet (panel) labels bold.

save_fig <- function(p, name, w = 6.5, h = 4) {
  # Defines a helper that saves a plot p under a given name with width w and height h (inches).
  ggsave(file.path("report/figures", paste0(name, ".pdf")), p, width = w, height = h)
  # Writes the plot as a PDF into report/figures.
}
# Ends the helper function.
save_tab <- function(df, name, align = NULL, digits = 2) {
  # Defines a helper that saves a data frame as a LaTeX table.
  print(xtable(df, align = align, digits = digits),
        # Converts the data frame to an xtable with the given column alignment and decimals,
        file = file.path("report/tables", paste0(name, ".tex")),
        # and writes it to report/tables/<name>.tex,
        floating = FALSE, booktabs = TRUE, include.rownames = FALSE,
        # without a table wrapper, with booktabs rules and without row numbers,
        sanitize.text.function = function(x) x)
        # leaving the text unescaped so LaTeX symbols in cells work.
}
# Ends the helper function.

d <- read.csv("analysis/cafe_clean.csv", stringsAsFactors = FALSE)
# Reads the cleaned dataset into d.
d$type     <- factor(d$type, levels = c("Independent", "Local chain", "International chain"))
# Turns the cafe type back into a factor with a fixed order (Independent is the reference).
d$prague1  <- factor(d$prague1, levels = c("Rest of Prague", "Prague 1"))
# Turns the location into a factor with "Rest of Prague" as the reference.
d$district <- factor(d$district)
# Turns the district into a factor.
# districts with at least 12 cafes (the brief): Prague 1, 2, 3, 5, 7, 8
d$district_main <- factor(ifelse(d$district %in% paste("Prague", c(1, 2, 3, 5, 7, 8)),
                                 # Creates a district variable that keeps only the six districts listed,
                                 as.character(d$district), NA),
                                 # giving NA to cafes in all other (small) districts,
                          levels = paste("Prague", c(1, 2, 3, 5, 7, 8)))
                          # and fixing the order of the six levels.
d$diff_cap_esp <- d$cappuccino - d$espresso   # for the paired question
# Computes the within-cafe difference between cappuccino and espresso price (used in A3).

## 1. HELPERS ------------------------------------------------------------------
skew <- function(x) { x <- x[!is.na(x)]; m <- mean(x); mean((x - m)^3) / mean((x - m)^2)^1.5 }
# Defines the sample skewness: third central moment divided by the variance to the power 1.5 (missing values ignored).
fmt <- function(x, k = 2) formatC(x, format = "f", digits = k)
# Defines a formatter that prints numbers with k decimals (default 2) as text.
mean_ci <- function(x) {
  # Defines a function returning the 95% confidence interval for a mean.
  x <- x[!is.na(x)]; n <- length(x)
  # Drops missing values and counts the remaining observations.
  half <- qt(.975, n - 1) * sd(x) / sqrt(n)
  # Computes the half-width: t critical value times the standard error.
  c(mean(x) - half, mean(x) + half)
  # Returns the lower and upper limits.
}
# Ends the function.

## 2. NUMERIC VARIABLES --------------------------------------------------------
num_vars <- list("Cappuccino price (CZK)" = d$cappuccino,
                 # Collects the numeric variables to describe in a named list:
                 "Espresso price (CZK)"   = d$espresso,
                 # espresso price,
                 "Distance from the Orloj (km)" = d$distance,
                 # distance from the Orloj,
                 "Cappuccino minus espresso (CZK)" = d$diff_cap_esp)
                 # and the cappuccino-minus-espresso difference.
desc_num <- do.call(rbind, lapply(names(num_vars), function(nm) {
  # Builds one table row per variable and stacks the rows together.
  x <- num_vars[[nm]]; ci <- mean_ci(x)
  # Takes the variable's values and its 95% CI for the mean.
  data.frame(Variable = nm, n = sum(!is.na(x)), Missing = sum(is.na(x)),
             # One row with the name, number of valid values and number missing,
             Mean = fmt(mean(x, na.rm = TRUE)),
             # the mean,
             `95\\% CI` = paste0("[", fmt(ci[1]), ", ", fmt(ci[2]), "]"),
             # its confidence interval written as [low, high],
             SD = fmt(sd(x, na.rm = TRUE)), Median = fmt(median(x, na.rm = TRUE)),
             # the standard deviation and median,
             IQR = fmt(IQR(x, na.rm = TRUE)),
             # the interquartile range,
             Min = fmt(min(x, na.rm = TRUE)), Max = fmt(max(x, na.rm = TRUE)),
             # the minimum and maximum,
             Skew = fmt(skew(x)), check.names = FALSE)
             # and the skewness (column names are kept as typed).
}))
# Ends the row-building function and the stacking.
save_tab(desc_num, "descriptives", align = c("l", "l", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r"), digits = 0)
# Saves the numeric descriptives as report/tables/descriptives.tex.
print(desc_num)
# Shows the same table on screen.

## 3. CATEGORICAL VARIABLES ----------------------------------------------------
cat_row <- function(var, levels_tab) {
  # Defines a helper that turns a frequency table into rows of counts and percentages.
  data.frame(Variable = var, Level = names(levels_tab),
             # One row per category with the variable name and the category label,
             n = as.integer(levels_tab),
             # the count,
             Percent = fmt(100 * as.integer(levels_tab) / nrow(d), 1),
             # the percentage of all cafes (one decimal),
             stringsAsFactors = FALSE)
             # keeping text as text.
}
# Ends the helper function.
t_district <- table(d$district)
# Counts cafes per district.
t_type     <- table(d$type, useNA = "ifany"); names(t_type)[is.na(names(t_type))] <- "Missing"
# Counts cafes per type (including missing) and labels the missing group "Missing".
t_p1       <- table(d$prague1)
# Counts cafes in Prague 1 versus the rest.
desc_cat <- rbind(cat_row("District", t_district), cat_row("Location", t_p1), cat_row("Cafe type", t_type))
# Stacks the three frequency tables into one.
desc_cat$Variable[duplicated(desc_cat$Variable)] <- ""
# Blanks repeated variable names so each name shows only on its first row.
save_tab(desc_cat, "descriptives_cat", align = c("l", "l", "l", "r", "r"), digits = 0)
# Saves the categorical descriptives as a LaTeX table.
print(desc_cat)
# Shows the table on screen.

## 4. CAPPUCCINO PRICE BY GROUP --------------------------------------------------
grp_row <- function(label, x) {
  # Defines a helper that summarises the price of one group.
  ci <- if (length(x) > 2) mean_ci(x) else c(NA, NA)
  # Computes the CI only when the group has more than two cafes; otherwise leaves it empty.
  data.frame(Group = label, n = length(x), Mean = fmt(mean(x)),
             # One row with the group name, size and mean,
             `95\\% CI` = paste0("[", fmt(ci[1]), ", ", fmt(ci[2]), "]"),
             # the confidence interval,
             SD = fmt(sd(x)), Median = fmt(median(x)), IQR = fmt(IQR(x)),
             # the SD, median and IQR,
             Skew = fmt(skew(x)), check.names = FALSE)
             # and the skewness.
}
# Ends the helper function.
g_list <- c(list("Prague 1" = d$cappuccino[d$prague1 == "Prague 1"],
                 # Builds a list of price vectors by group, starting with Prague 1
                 "Rest of Prague" = d$cappuccino[d$prague1 == "Rest of Prague"]),
                 # and the rest of Prague,
            setNames(lapply(levels(d$district_main), function(l) d$cappuccino[which(d$district_main == l)]),
                     # then one vector for each of the six main districts,
                     levels(d$district_main)),
                     # named by the district,
            setNames(lapply(levels(d$type), function(l) d$cappuccino[which(d$type == l)]), levels(d$type)),
            # then one vector for each cafe type,
            list("Independent" = d$cappuccino[which(d$chain == "Independent")],
                 # then independents again (as the chain-indicator reference)
                 "Chain (local + international)" = d$cappuccino[which(d$chain == "Chain")]))
                 # and local plus international chains together.
# (Independent appears twice: once as a type, once as the reference of the chain indicator; keep one)
g_list <- g_list[!duplicated(names(g_list))]
# Removes the duplicated "Independent" entry so each group appears once.
desc_grp <- do.call(rbind, lapply(names(g_list), function(nm) grp_row(nm, g_list[[nm]])))
# Builds the summary row for every group and stacks them into one table.
save_tab(desc_grp, "descriptives_groups", align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"), digits = 0)
# Saves the group table as a LaTeX table.
print(desc_grp)
# Shows the table on screen.

## 5. NORMALITY CHECKS ---------------------------------------------------------
# The plots come first (histograms, box plots, Q-Q plots); the skewness and
# the Shapiro-Wilk test only supplement them. The test has little power in
# small samples and rejects trivial deviations in large ones.
sw <- function(x) if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x)$p.value else NA
# Defines a helper returning the Shapiro-Wilk p-value, or NA if the sample size is outside the test's allowed 3-5000.
norm_tab <- data.frame(
  # Starts the normality table for the groups:
  Sample = names(g_list),
  # the group name,
  n = sapply(g_list, length),
  # the group size,
  Skewness = fmt(sapply(g_list, skew)),
  # the group skewness,
  `Shapiro--Wilk p` = sapply(sw_p <- sapply(g_list, sw), function(p) if (is.na(p)) "--" else if (p < .001) "$<$.001" else sub("^0", "", fmt(p, 3))),
  # the Shapiro-Wilk p-value formatted as text ("--" if not computed, "<.001" if tiny, otherwise without the leading zero),
  check.names = FALSE, row.names = NULL)
  # keeping the column names as typed and dropping row names.
extra <- data.frame(Sample = c("All cafes: cappuccino", "All cafes: cappuccino minus espresso", "All cafes: distance"),
                    # Builds extra rows for the whole sample: cappuccino, the paired difference and distance,
                    n = c(sum(!is.na(d$cappuccino)), sum(!is.na(d$diff_cap_esp)), sum(!is.na(d$distance))),
                    # with the number of valid values,
                    Skewness = fmt(c(skew(d$cappuccino), skew(d$diff_cap_esp), skew(d$distance))),
                    # their skewness,
                    `Shapiro--Wilk p` = sapply(list(d$cappuccino, d$diff_cap_esp[!is.na(d$diff_cap_esp)], d$distance), function(x) { p <- sw(x); if (p < .001) "$<$.001" else sub("^0", "", fmt(p, 3)) }),
                    # and their Shapiro-Wilk p-values formatted the same way,
                    check.names = FALSE)
                    # keeping the column names as typed.
norm_tab <- rbind(extra, norm_tab)
# Puts the whole-sample rows above the group rows.
save_tab(norm_tab, "normality", align = c("l", "l", "r", "r", "r"), digits = 0)
# Saves the normality table as a LaTeX table.
print(norm_tab)
# Shows the table on screen.

## 6. FIGURES --------------------------------------------------------------------
long <- rbind(
  # Stacks the four numeric variables into one long table (one row per value) for faceted plots:
  data.frame(variable = "Cappuccino price (CZK)", value = d$cappuccino),
  # cappuccino prices,
  data.frame(variable = "Espresso price (CZK)",   value = d$espresso),
  # espresso prices,
  data.frame(variable = "Distance from the Orloj (km)", value = d$distance),
  # distances,
  data.frame(variable = "Cappuccino minus espresso (CZK)", value = d$diff_cap_esp))
  # and the within-cafe differences.
long <- long[!is.na(long$value), ]
# Drops missing values.

# 6a. histogram + density
p_hist <- ggplot(long, aes(value)) +
  # Starts a plot of the values,
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = COP, colour = "white", alpha = .85) +
  # drawing a histogram with 25 bins scaled to density,
  geom_density(colour = ESP, linewidth = .8) +
  # overlaying a smooth density curve,
  facet_wrap(~variable, scales = "free", ncol = 2) +
  # making one panel per variable with its own axes in two columns,
  labs(x = NULL, y = "Density")
  # and labelling the y axis "Density".
save_fig(p_hist, "distributions", 7, 5)
# Saves the histograms as report/figures/distributions.pdf.

# 6b. box plots
p_box <- ggplot(long, aes(x = value, y = variable)) +
  # Starts a plot of the values by variable,
  geom_boxplot(fill = COP, alpha = .5, colour = ESP, outlier.colour = ESP) +
  # drawing a horizontal box plot for each,
  facet_wrap(~variable, scales = "free", ncol = 2) +
  # splitting into one panel per variable with free axes,
  labs(x = NULL, y = NULL) + theme(axis.text.y = element_blank())
  # and removing axis titles and the redundant y labels.
save_fig(p_box, "boxplots", 7, 4)
# Saves the box plots as report/figures/boxplots.pdf.

# 6c. groups side by side (cappuccino price)
grp_long <- rbind(
  # Builds a long table of cappuccino prices labelled by grouping scheme:
  data.frame(panel = "Location", group = as.character(d$prague1), y = d$cappuccino),
  # prices by Prague 1 versus the rest,
  data.frame(panel = "District", group = as.character(d$district_main), y = d$cappuccino),
  # prices by main district,
  data.frame(panel = "Cafe type", group = as.character(d$type), y = d$cappuccino))
  # and prices by cafe type.
grp_long <- grp_long[!is.na(grp_long$group) & !is.na(grp_long$y), ]
# Drops rows with a missing group or price.
grp_long$group <- factor(grp_long$group, levels = c("Rest of Prague", "Prague 1",
  # Turns the group into a factor with a chosen display order:
  paste("Prague", c(1, 2, 3, 5, 7, 8))[-1], "Independent", "Local chain", "International chain"))
  # rest of Prague, Prague 1, the other districts, then the three cafe types.
grp_long$group <- factor(grp_long$group, levels = unique(c(levels(grp_long$group), "Prague 1")))
# Re-creates the factor making sure "Prague 1" is a valid level for the district panel too.
p_grp <- ggplot(grp_long, aes(group, y)) +
  # Starts a plot of price by group,
  geom_boxplot(fill = COP, alpha = .35, colour = ESP, outlier.shape = NA) +
  # drawing box plots without outlier dots (the points are shown below),
  geom_jitter(width = .15, height = 0, size = .9, alpha = .55, colour = COP) +
  # adding each cafe as a slightly horizontally jittered point,
  facet_wrap(~panel, scales = "free_x", nrow = 1) +
  # putting the three grouping schemes in three panels in one row,
  labs(x = NULL, y = "Cappuccino price (CZK)") +
  # labelling the y axis,
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
  # and tilting the group labels so they do not overlap.
save_fig(p_grp, "groups_side_by_side", 8, 4)
# Saves the figure as report/figures/groups_side_by_side.pdf.

# 6d. normal Q-Q plots (overall, by group, and the paired differences)
qq_long <- rbind(
  # Builds a long table of values for the Q-Q plots, one panel per sample:
  data.frame(panel = "All cafes", y = d$cappuccino),
  # all cafes,
  data.frame(panel = "Prague 1", y = d$cappuccino[d$prague1 == "Prague 1"]),
  # Prague 1,
  data.frame(panel = "Rest of Prague", y = d$cappuccino[d$prague1 == "Rest of Prague"]),
  # the rest of Prague,
  data.frame(panel = "Cappuccino minus espresso", y = d$diff_cap_esp[!is.na(d$diff_cap_esp)]),
  # the paired difference,
  do.call(rbind, lapply(levels(d$district_main), function(l)
    # and one panel for each of the six districts.
    data.frame(panel = l, y = d$cappuccino[which(d$district_main == l)]))))
    # (each district's cappuccino prices labelled with its name)
qq_long$panel <- factor(qq_long$panel, levels = unique(qq_long$panel))
# Fixes the panel order as first listed.
p_qq <- ggplot(qq_long, aes(sample = y)) +
  # Starts a Q-Q plot of the values,
  stat_qq(colour = COP, size = 1) + stat_qq_line(colour = ESP) +
  # drawing the sample-versus-normal quantile points and the reference line,
  facet_wrap(~panel, scales = "free", ncol = 3) +
  # one panel per sample in three columns,
  labs(x = "Theoretical quantiles", y = "Sample quantiles")
  # and labelling the axes.
save_fig(p_qq, "qq_plots", 7.5, 7)
# Saves the Q-Q plots as report/figures/qq_plots.pdf.

cat("\nDescriptive tables and figures written to report/tables and report/figures.\n")
# Prints a confirmation message that everything has been written.
