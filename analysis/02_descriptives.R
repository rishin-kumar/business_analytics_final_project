if (!file.exists("analysis/cafe_clean.csv")) stop("Run 01_cleaning.R first (from the repository root).")
dir.create("report/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("report/figures", recursive = TRUE, showWarnings = FALSE)

load_packages <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
    library(p, character.only = TRUE)
  }
}
load_packages(c("ggplot2", "xtable"))

COP <- "#512bb8"; ESP <- "#2B1D16"; GREY <- "#8A7F76"
theme_set(theme_minimal(base_size = 11) +
          theme(panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold"),
                strip.text = element_text(face = "bold")))

save_fig <- function(p, name, w = 6.5, h = 4) {
  ggsave(file.path("report/figures", paste0(name, ".pdf")), p, width = w, height = h)
}
save_tab <- function(df, name, align = NULL, digits = 2) {
  print(xtable(df, align = align, digits = digits),
        file = file.path("report/tables", paste0(name, ".tex")),
        floating = FALSE, booktabs = TRUE, include.rownames = FALSE,
        sanitize.text.function = function(x) x)
}

d <- read.csv("analysis/cafe_clean.csv", stringsAsFactors = FALSE)
d$type     <- factor(d$type, levels = c("Independent", "Local chain", "International chain"))
d$prague1  <- factor(d$prague1, levels = c("Rest of Prague", "Prague 1"))
d$district <- factor(d$district)
d$district_main <- factor(ifelse(d$district %in% paste("Prague", c(1, 2, 3, 5, 7, 8)),
                                 as.character(d$district), NA),
                          levels = paste("Prague", c(1, 2, 3, 5, 7, 8)))
d$diff_cap_esp <- d$cappuccino - d$espresso

skew <- function(x) { x <- x[!is.na(x)]; m <- mean(x); mean((x - m)^3) / mean((x - m)^2)^1.5 }
fmt <- function(x, k = 2) formatC(x, format = "f", digits = k)
mean_ci <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  half <- qt(.975, n - 1) * sd(x) / sqrt(n)
  c(mean(x) - half, mean(x) + half)
}

num_vars <- list("Cappuccino price (CZK)" = d$cappuccino,
                 "Espresso price (CZK)"   = d$espresso,
                 "Distance from the Orloj (km)" = d$distance,
                 "Cappuccino minus espresso (CZK)" = d$diff_cap_esp)
desc_num <- do.call(rbind, lapply(names(num_vars), function(nm) {
  x <- num_vars[[nm]]; ci <- mean_ci(x)
  data.frame(Variable = nm, n = sum(!is.na(x)), Missing = sum(is.na(x)),
             Mean = fmt(mean(x, na.rm = TRUE)),
             `95\\% CI` = paste0("[", fmt(ci[1]), ", ", fmt(ci[2]), "]"),
             SD = fmt(sd(x, na.rm = TRUE)), Median = fmt(median(x, na.rm = TRUE)),
             IQR = fmt(IQR(x, na.rm = TRUE)),
             Min = fmt(min(x, na.rm = TRUE)), Max = fmt(max(x, na.rm = TRUE)),
             Skew = fmt(skew(x)), check.names = FALSE)
}))
save_tab(desc_num, "descriptives", align = c("l", "l", "r", "r", "r", "r", "r", "r", "r", "r", "r", "r"), digits = 0)
print(desc_num)

cat_row <- function(var, levels_tab) {
  data.frame(Variable = var, Level = names(levels_tab),
             n = as.integer(levels_tab),
             Percent = fmt(100 * as.integer(levels_tab) / nrow(d), 1),
             stringsAsFactors = FALSE)
}
t_district <- table(d$district)
t_type     <- table(d$type, useNA = "ifany"); names(t_type)[is.na(names(t_type))] <- "Missing"
t_p1       <- table(d$prague1)
desc_cat <- rbind(cat_row("District", t_district), cat_row("Location", t_p1), cat_row("Cafe type", t_type))
desc_cat$Variable[duplicated(desc_cat$Variable)] <- ""
save_tab(desc_cat, "descriptives_cat", align = c("l", "l", "l", "r", "r"), digits = 0)
print(desc_cat)

grp_row <- function(label, x) {
  ci <- if (length(x) > 2) mean_ci(x) else c(NA, NA)
  data.frame(Group = label, n = length(x), Mean = fmt(mean(x)),
             `95\\% CI` = paste0("[", fmt(ci[1]), ", ", fmt(ci[2]), "]"),
             SD = fmt(sd(x)), Median = fmt(median(x)), IQR = fmt(IQR(x)),
             Skew = fmt(skew(x)), check.names = FALSE)
}
g_list <- c(list("Prague 1" = d$cappuccino[d$prague1 == "Prague 1"],
                 "Rest of Prague" = d$cappuccino[d$prague1 == "Rest of Prague"]),
            setNames(lapply(levels(d$district_main), function(l) d$cappuccino[which(d$district_main == l)]),
                     levels(d$district_main)),
            setNames(lapply(levels(d$type), function(l) d$cappuccino[which(d$type == l)]), levels(d$type)),
            list("Independent" = d$cappuccino[which(d$chain == "Independent")],
                 "Chain (local + international)" = d$cappuccino[which(d$chain == "Chain")]))
g_list <- g_list[!duplicated(names(g_list))]
desc_grp <- do.call(rbind, lapply(names(g_list), function(nm) grp_row(nm, g_list[[nm]])))
save_tab(desc_grp, "descriptives_groups", align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"), digits = 0)
print(desc_grp)

sw <- function(x) if (length(x) >= 3 && length(x) <= 5000) shapiro.test(x)$p.value else NA
norm_tab <- data.frame(
  Sample = names(g_list),
  n = sapply(g_list, length),
  Skewness = fmt(sapply(g_list, skew)),
  `Shapiro--Wilk p` = sapply(sw_p <- sapply(g_list, sw), function(p) if (is.na(p)) "--" else if (p < .001) "$<$.001" else sub("^0", "", fmt(p, 3))),
  check.names = FALSE, row.names = NULL)
extra <- data.frame(Sample = c("All cafes: cappuccino", "All cafes: cappuccino minus espresso", "All cafes: distance"),
                    n = c(sum(!is.na(d$cappuccino)), sum(!is.na(d$diff_cap_esp)), sum(!is.na(d$distance))),
                    Skewness = fmt(c(skew(d$cappuccino), skew(d$diff_cap_esp), skew(d$distance))),
                    `Shapiro--Wilk p` = sapply(list(d$cappuccino, d$diff_cap_esp[!is.na(d$diff_cap_esp)], d$distance), function(x) { p <- sw(x); if (p < .001) "$<$.001" else sub("^0", "", fmt(p, 3)) }),
                    check.names = FALSE)
norm_tab <- rbind(extra, norm_tab)
save_tab(norm_tab, "normality", align = c("l", "l", "r", "r", "r"), digits = 0)
print(norm_tab)

long <- rbind(
  data.frame(variable = "Cappuccino price (CZK)", value = d$cappuccino),
  data.frame(variable = "Espresso price (CZK)",   value = d$espresso),
  data.frame(variable = "Distance from the Orloj (km)", value = d$distance),
  data.frame(variable = "Cappuccino minus espresso (CZK)", value = d$diff_cap_esp))
long <- long[!is.na(long$value), ]

p_hist <- ggplot(long, aes(value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = COP, colour = "white", alpha = .85) +
  geom_density(colour = ESP, linewidth = .8) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = NULL, y = "Density")
save_fig(p_hist, "distributions", 7, 5)

p_box <- ggplot(long, aes(x = value, y = variable)) +
  geom_boxplot(fill = COP, alpha = .5, colour = ESP, outlier.colour = ESP) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = NULL, y = NULL) + theme(axis.text.y = element_blank())
save_fig(p_box, "boxplots", 7, 4)

grp_long <- rbind(
  data.frame(panel = "Location", group = as.character(d$prague1), y = d$cappuccino),
  data.frame(panel = "District", group = as.character(d$district_main), y = d$cappuccino),
  data.frame(panel = "Cafe type", group = as.character(d$type), y = d$cappuccino))
grp_long <- grp_long[!is.na(grp_long$group) & !is.na(grp_long$y), ]
grp_long$group <- factor(grp_long$group, levels = c("Rest of Prague", "Prague 1",
  paste("Prague", c(1, 2, 3, 5, 7, 8))[-1], "Independent", "Local chain", "International chain"))
grp_long$group <- factor(grp_long$group, levels = unique(c(levels(grp_long$group), "Prague 1")))
p_grp <- ggplot(grp_long, aes(group, y)) +
  geom_boxplot(fill = COP, alpha = .35, colour = ESP, outlier.shape = NA) +
  geom_jitter(width = .15, height = 0, size = .9, alpha = .55, colour = COP) +
  facet_wrap(~panel, scales = "free_x", nrow = 1) +
  labs(x = NULL, y = "Cappuccino price (CZK)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig(p_grp, "groups_side_by_side", 8, 4)

qq_long <- rbind(
  data.frame(panel = "All cafes", y = d$cappuccino),
  data.frame(panel = "Prague 1", y = d$cappuccino[d$prague1 == "Prague 1"]),
  data.frame(panel = "Rest of Prague", y = d$cappuccino[d$prague1 == "Rest of Prague"]),
  data.frame(panel = "Cappuccino minus espresso", y = d$diff_cap_esp[!is.na(d$diff_cap_esp)]),
  do.call(rbind, lapply(levels(d$district_main), function(l)
    data.frame(panel = l, y = d$cappuccino[which(d$district_main == l)]))))
qq_long$panel <- factor(qq_long$panel, levels = unique(qq_long$panel))
p_qq <- ggplot(qq_long, aes(sample = y)) +
  stat_qq(colour = COP, size = 1) + stat_qq_line(colour = ESP) +
  facet_wrap(~panel, scales = "free", ncol = 3) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles")
save_fig(p_qq, "qq_plots", 7.5, 7)

cat("\nDescriptive tables and figures written to report/tables and report/figures.\n")
