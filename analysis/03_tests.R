if (!file.exists("analysis/cafe_clean.csv")) stop("Run 01_cleaning.R first (from the repository root).")
for (f in c("report/tables", "report/figures", "presentation/figures")) dir.create(f, recursive = TRUE, showWarnings = FALSE)

load_packages <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
    library(p, character.only = TRUE)
  }
}
load_packages(c("ggplot2", "xtable", "sandwich", "lmtest", "car", "boot"))

SEED <- 2026
B    <- 5000
set.seed(SEED)
ALPHA <- .05

BENCHMARK <- 90

COP <- "#512bb8"; ESP <- "#2B1D16"; GREY <- "#8A7F76"; PALE <- "#E7E2DC"
theme_set(theme_minimal(base_size = 11) +
          theme(panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold"),
                plot.subtitle = element_text(colour = GREY),
                strip.text = element_text(face = "bold")))

fmt  <- function(x, k = 2) { x[!is.na(x) & round(x, k) == 0] <- 0; s <- formatC(x, format = "f", digits = k); gsub("-", "$-$", s) }
fmtp <- function(p) if (p < .001) "$<$.001" else sub("^0", "", formatC(p, format = "f", digits = 3))
ptex <- function(p) if (p < .001) "$p$ < .001" else paste0("$p$ = ", sub("^0", "", formatC(p, format = "f", digits = 3)))
ptxt <- function(p) if (p < .001) "\\plt{.001}" else paste0("\\pval{", sub("^0", "", formatC(p, format = "f", digits = 3)), "}")
ci_str <- function(lo, hi, k = 2) paste0("[", fmt(lo, k), ", ", fmt(hi, k), "]")
ntests <- 0; tick <- function(k = 1) ntests <<- ntests + k

save_fig <- function(p, name, w = 6.5, h = 4) {
  ggsave(file.path("report/figures", paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path("presentation/figures", paste0(name, ".png")), p, width = w, height = h, dpi = 200)
}
write_tab <- function(df, name, align = NULL) {
  print(xtable(df, align = align, digits = 0),
        file = file.path("report/tables", paste0(name, ".tex")),
        floating = FALSE, booktabs = TRUE, include.rownames = FALSE,
        sanitize.text.function = function(x) x)
}
macros <- character(0)
mac <- function(name, value) macros[name] <<- gsub("$-$", "\\ensuremath{-}", as.character(value), fixed = TRUE)

se_d_one <- function(d, n)       sqrt(1 / n + d^2 / (2 * n))
se_d_two <- function(d, n1, n2)  sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
rb_two    <- function(x, y) { U <- sum(outer(x, y, ">")) + .5 * sum(outer(x, y, "==")); 2 * U / (length(x) * length(y)) - 1 }
rb_signed <- function(dd) { dd <- dd[dd != 0]; r <- rank(abs(dd)); (sum(r[dd > 0]) - sum(r[dd < 0])) / sum(r) }
pct <- function(x) quantile(x, c(.025, .975), names = FALSE)
eta2_ci <- function(Fv, df1, df2, N, level = .95) {
  a <- (1 - level) / 2
  lo <- if (pf(Fv, df1, df2, ncp = 0) < 1 - a) 0 else uniroot(function(l) pf(Fv, df1, df2, ncp = l) - (1 - a), c(0, 1e4))$root
  hi <- if (pf(Fv, df1, df2, ncp = 0) < a) 0 else uniroot(function(l) pf(Fv, df1, df2, ncp = l) - a, c(0, 1e5))$root
  c(lo, hi) / (c(lo, hi) + N)
}
robust_band <- function(m, nd) {
  X <- model.matrix(delete.response(terms(m)), nd, xlev = m$xlevels)
  V <- vcovHC(m, type = "HC3"); fit <- drop(X %*% coef(m))
  se <- sqrt(rowSums((X %*% V) * X)); tq <- qt(.975, df.residual(m))
  data.frame(nd, fit = fit, lo = fit - tq * se, hi = fit + tq * se)
}
robust_coef <- function(m) {
  V <- vcovHC(m, type = "HC3"); b <- coef(m); se <- sqrt(diag(V)); tq <- qt(.975, df.residual(m))
  data.frame(term = names(b), b = b, se = se, lo = b - tq * se, hi = b + tq * se,
             t = b / se, p = 2 * pt(-abs(b / se), df.residual(m)), row.names = NULL)
}
reg_table <- function(models, model_names, term_labels, name, outcome_note) {
  rows <- list()
  for (tl in names(term_labels)) {
    r1 <- c(term_labels[[tl]]); r2 <- c("")
    for (m in models) {
      rc <- robust_coef(m); i <- match(tl, rc$term)
      if (is.na(i)) { r1 <- c(r1, ""); r2 <- c(r2, "") } else {
        r1 <- c(r1, paste0(fmt(rc$b[i], 2), " (", fmt(rc$se[i], 2), ")"))
        r2 <- c(r2, paste0("[", fmt(rc$lo[i], 2), ", ", fmt(rc$hi[i], 2), "]"))
      }
    }
    rows[[length(rows) + 1]] <- r1; rows[[length(rows) + 1]] <- r2
  }
  stat_row <- function(label, f) c(label, sapply(models, f))
  rows[[length(rows) + 1]] <- stat_row("Observations", function(m) as.character(nobs(m)))
  rows[[length(rows) + 1]] <- stat_row("$R^2$", function(m) fmt(summary(m)$r.squared, 3))
  rows[[length(rows) + 1]] <- stat_row("Adjusted $R^2$", function(m) fmt(summary(m)$adj.r.squared, 3))
  rows[[length(rows) + 1]] <- stat_row("Residual SE", function(m) fmt(summary(m)$sigma, 3))
  tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(tab) <- c(" ", model_names)
  write_tab(tab, name, align = c("l", "l", rep("r", length(models))))
  tab
}
diag_plots <- function(m, name) {
  pdf(file.path("report/figures", paste0(name, ".pdf")), width = 9, height = 3.2)
  par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
  plot(m, which = c(1, 2, 5), pch = 16, col = adjustcolor(COP, .7), cex = .7, caption = "", sub.caption = "")
  dev.off()
}

d <- read.csv("analysis/cafe_clean.csv", stringsAsFactors = FALSE)
d$type     <- factor(d$type, levels = c("Independent", "Local chain", "International chain"))
d$chain    <- factor(d$chain, levels = c("Independent", "Chain"))
d$prague1  <- factor(d$prague1, levels = c("Rest of Prague", "Prague 1"))
d$district <- factor(d$district)
d$district_main <- factor(ifelse(d$district %in% paste("Prague", c(1, 2, 3, 5, 7, 8)), as.character(d$district), NA),
                          levels = paste("Prague", c(1, 2, 3, 5, 7, 8)))
N_ALL <- nrow(d)
mac("Nall", N_ALL)
res <- list()

x <- d$cappuccino; n <- length(x)
t1 <- t.test(x, mu = BENCHMARK); tick()
d1 <- (mean(x) - BENCHMARK) / sd(x); d1ci <- d1 + c(-1, 1) * 1.96 * se_d_one(d1, n)
w1 <- wilcox.test(x, mu = BENCHMARK, conf.int = TRUE, exact = FALSE); tick()
rb1 <- rb_signed(x - BENCHMARK)
rb1ci <- pct(replicate(B, rb_signed(sample(x, replace = TRUE) - BENCHMARK)))
bt1 <- boot(x, function(v, i) mean(v[i]), R = B)
bca1 <- boot.ci(bt1, type = "bca")$bca[4:5]
A1 <- data.frame(
  Method = c("One-sample $t$-test", "Wilcoxon signed-rank", "Bootstrap (BCa)"),
  `Estimate (CZK)` = c(fmt(mean(x), 1), fmt(w1$estimate, 1), fmt(mean(x), 1)),
  `95\\% CI` = c(ci_str(t1$conf.int[1], t1$conf.int[2], 1), ci_str(w1$conf.int[1], w1$conf.int[2], 1), ci_str(bca1[1], bca1[2], 1)),
  `Effect size [95\\% CI]` = c(paste0("$d$ = ", fmt(d1), " ", ci_str(d1ci[1], d1ci[2])),
                              paste0("$r_{rb}$ = ", fmt(rb1), " ", ci_str(rb1ci[1], rb1ci[2])), "--"),
  Statistic = c(paste0("$t$(", t1$parameter, ") = ", fmt(t1$statistic)), paste0("$V$ = ", formatC(w1$statistic, format = "d", big.mark = "")), "--"),
  `$p$` = c(fmtp(t1$p.value), fmtp(w1$p.value), "--"), check.names = FALSE)
write_tab(A1, "A1_table", align = c("l", "l", "r", "r", "r", "r", "r"))
p <- ggplot(data.frame(x = x), aes(x)) +
  geom_histogram(binwidth = 5, fill = COP, colour = "white", alpha = .85) +
  annotate("rect", xmin = t1$conf.int[1], xmax = t1$conf.int[2], ymin = -Inf, ymax = Inf, fill = ESP, alpha = .15) +
  geom_vline(xintercept = mean(x), colour = ESP, linewidth = .9) +
  geom_vline(xintercept = BENCHMARK, colour = GREY, linetype = "dashed", linewidth = .9) +
  annotate("text", x = mean(x) - 1, y = Inf, vjust = 1.6, hjust = 1, label = paste0("Mean ", sprintf("%.1f", mean(x)), " CZK\n(shaded: 95% CI)"), size = 3.4, colour = ESP) +
  annotate("text", x = BENCHMARK + 1, y = Inf, vjust = 1.6, hjust = 0, label = paste0("Benchmark ", BENCHMARK, " CZK"), size = 3.4, colour = GREY) +
  labs(x = "Cappuccino price (CZK)", y = "Number of cafes",
       title = "Cappuccino prices against the benchmark", subtitle = paste0("n = ", n, " cafes"))
save_fig(p, "A1_figure")
mac("AoneBench", BENCHMARK); mac("AoneN", n)
mac("AoneMean", fmt(mean(x), 1)); mac("AoneLo", fmt(t1$conf.int[1], 1)); mac("AoneHi", fmt(t1$conf.int[2], 1))
mac("AoneT", fmt(t1$statistic)); mac("AoneDf", t1$parameter); mac("AoneP", ptxt(t1$p.value))
mac("AoneD", fmt(d1)); mac("AoneDLo", fmt(d1ci[1])); mac("AoneDHi", fmt(d1ci[2]))
mac("AoneWmed", fmt(w1$estimate, 1)); mac("AoneWP", ptxt(w1$p.value))
mac("AoneBLo", fmt(bca1[1], 1)); mac("AoneBHi", fmt(bca1[2], 1))
res$A1 <- c("A1", "Average price vs.\\ own benchmark", "One-sample $t$-test",
            paste0(fmt(mean(x), 1), " CZK ", ci_str(t1$conf.int[1], t1$conf.int[2], 1)),
            paste0("$d$ = ", fmt(d1), " ", ci_str(d1ci[1], d1ci[2])),
            paste0("$t$(", t1$parameter, ") = ", fmt(t1$statistic), ", ", ptex(t1$p.value)))

g1 <- d$cappuccino[d$prague1 == "Prague 1"]; g0 <- d$cappuccino[d$prague1 == "Rest of Prague"]
n1 <- length(g1); n0 <- length(g0)
t2 <- t.test(g1, g0, alternative = "greater"); tick()
t2b <- t.test(g1, g0)
diff2 <- mean(g1) - mean(g0)
sp <- sqrt(((n1 - 1) * var(g1) + (n0 - 1) * var(g0)) / (n1 + n0 - 2))
d2 <- diff2 / sp; d2ci <- d2 + c(-1, 1) * 1.96 * se_d_two(d2, n1, n0)
w2 <- wilcox.test(g1, g0, alternative = "greater", exact = FALSE); tick()
w2b <- wilcox.test(g1, g0, conf.int = TRUE, exact = FALSE)
rb2 <- rb_two(g1, g0)
rb2ci <- pct(replicate(B, rb_two(sample(g1, replace = TRUE), sample(g0, replace = TRUE))))
A2 <- data.frame(
  Method = c("Welch $t$-test", "Wilcoxon rank-sum"),
  `Estimate (CZK)` = c(fmt(diff2, 1), fmt(w2b$estimate, 1)),
  `95\\% CI` = c(ci_str(t2b$conf.int[1], t2b$conf.int[2], 1), ci_str(w2b$conf.int[1], w2b$conf.int[2], 1)),
  `Effect size [95\\% CI]` = c(paste0("$d$ = ", fmt(d2), " ", ci_str(d2ci[1], d2ci[2])),
                              paste0("$r_{rb}$ = ", fmt(rb2), " ", ci_str(rb2ci[1], rb2ci[2]))),
  Statistic = c(paste0("$t$(", fmt(t2$parameter, 1), ") = ", fmt(t2$statistic)), paste0("$W$ = ", formatC(w2$statistic, format = "d", big.mark = ""))),
  `$p$ (one-tailed)` = c(fmtp(t2$p.value), fmtp(w2$p.value)), check.names = FALSE)
write_tab(A2, "A2_table", align = c("l", "l", "r", "r", "r", "r", "r"))
g_df <- data.frame(group = factor(rep(c("Rest of Prague", "Prague 1"), c(n0, n1)), levels = c("Rest of Prague", "Prague 1")), y = c(g0, g1))
ci_g <- do.call(rbind, lapply(split(g_df$y, g_df$group), function(v) { h <- qt(.975, length(v) - 1) * sd(v) / sqrt(length(v)); c(m = mean(v), lo = mean(v) - h, hi = mean(v) + h) }))
ci_g <- data.frame(group = factor(rownames(ci_g), levels = levels(g_df$group)), ci_g)
p <- ggplot(g_df, aes(group, y)) +
  geom_jitter(width = .15, height = 0, size = 1.2, alpha = .45, colour = COP) +
  geom_errorbar(data = ci_g, aes(y = m, ymin = lo, ymax = hi), width = .12, colour = ESP, linewidth = .9) +
  geom_point(data = ci_g, aes(y = m), size = 3.2, colour = ESP) +
  labs(x = NULL, y = "Cappuccino price (CZK)", title = "Cappuccino price: Prague 1 vs. the rest of Prague",
       subtitle = paste0("Dark points: group means with 95% CI.  Prague 1 minus rest = ", sprintf("%.1f", diff2), " CZK"))
save_fig(p, "A2_figure")
mac("AtwoNa", n1); mac("AtwoNb", n0)
mac("AtwoMa", fmt(mean(g1), 1)); mac("AtwoMb", fmt(mean(g0), 1))
mac("AtwoDiff", fmt(diff2, 1)); mac("AtwoLo", fmt(t2b$conf.int[1], 1)); mac("AtwoHi", fmt(t2b$conf.int[2], 1))
mac("AtwoD", fmt(d2)); mac("AtwoDLo", fmt(d2ci[1])); mac("AtwoDHi", fmt(d2ci[2]))
mac("AtwoT", fmt(t2$statistic)); mac("AtwoDf", fmt(t2$parameter, 1)); mac("AtwoP", ptxt(t2$p.value))
mac("AtwoWP", ptxt(w2$p.value)); mac("AtwoHL", fmt(w2b$estimate, 1)); mac("AtwoHLLo", fmt(w2b$conf.int[1], 1)); mac("AtwoHLHi", fmt(w2b$conf.int[2], 1))
mac("AtwoRB", fmt(rb2)); mac("AtwoRBLo", fmt(rb2ci[1])); mac("AtwoRBHi", fmt(rb2ci[2]))
res$A2 <- c("A2", "Prague 1 minus rest (one-tailed)", "Welch $t$-test",
            paste0(fmt(diff2, 1), " CZK ", ci_str(t2b$conf.int[1], t2b$conf.int[2], 1)),
            paste0("$d$ = ", fmt(d2), " ", ci_str(d2ci[1], d2ci[2])),
            paste0("$t$(", fmt(t2$parameter, 1), ") = ", fmt(t2$statistic), ", ", ptex(t2$p.value)))

ok <- !is.na(d$cappuccino) & !is.na(d$espresso)
dd <- d$cappuccino[ok] - d$espresso[ok]; n3 <- length(dd)
t3 <- t.test(dd, alternative = "greater"); tick()
t3b <- t.test(dd)
dz <- mean(dd) / sd(dd); dzci <- dz + c(-1, 1) * 1.96 * se_d_one(dz, n3)
w3 <- wilcox.test(dd, alternative = "greater", exact = FALSE); tick()
w3b <- wilcox.test(dd, conf.int = TRUE, exact = FALSE)
rb3 <- rb_signed(dd); rb3ci <- pct(replicate(B, rb_signed(sample(dd, replace = TRUE))))
A3 <- data.frame(
  Method = c("Paired $t$-test", "Wilcoxon signed-rank"),
  `Estimate (CZK)` = c(fmt(mean(dd), 1), fmt(w3b$estimate, 1)),
  `95\\% CI` = c(ci_str(t3b$conf.int[1], t3b$conf.int[2], 1), ci_str(w3b$conf.int[1], w3b$conf.int[2], 1)),
  `Effect size [95\\% CI]` = c(paste0("$d_z$ = ", fmt(dz), " ", ci_str(dzci[1], dzci[2])),
                              paste0("$r_{rb}$ = ", fmt(rb3), " ", ci_str(rb3ci[1], rb3ci[2]))),
  Statistic = c(paste0("$t$(", t3$parameter, ") = ", fmt(t3$statistic)), paste0("$V$ = ", formatC(w3$statistic, format = "d", big.mark = ""))),
  `$p$ (one-tailed)` = c(fmtp(t3$p.value), fmtp(w3$p.value)), check.names = FALSE)
write_tab(A3, "A3_table", align = c("l", "l", "r", "r", "r", "r", "r"))
p <- ggplot(data.frame(dd), aes(dd)) +
  geom_histogram(binwidth = 3, fill = COP, colour = "white", alpha = .85) +
  annotate("rect", xmin = t3b$conf.int[1], xmax = t3b$conf.int[2], ymin = -Inf, ymax = Inf, fill = ESP, alpha = .2) +
  geom_vline(xintercept = mean(dd), colour = ESP, linewidth = .9) + geom_vline(xintercept = 0, colour = GREY, linetype = "dashed") +
  labs(x = "Cappuccino minus espresso price in the same cafe (CZK)", y = "Number of cafes",
       title = "Within-cafe price difference", subtitle = paste0("Line: mean difference ", sprintf("%.1f", mean(dd)), " CZK; shaded: 95% CI; dashed: zero.  n = ", n3, " cafes"))
save_fig(p, "A3_figure")
mac("AthreeN", n3); mac("AthreeM", fmt(mean(dd), 1)); mac("AthreeLo", fmt(t3b$conf.int[1], 1)); mac("AthreeHi", fmt(t3b$conf.int[2], 1))
mac("AthreeDz", fmt(dz)); mac("AthreeDzLo", fmt(dzci[1])); mac("AthreeDzHi", fmt(dzci[2]))
mac("AthreeT", fmt(t3$statistic)); mac("AthreeDf", t3$parameter); mac("AthreeP", ptxt(t3$p.value)); mac("AthreeWP", ptxt(w3$p.value))
mac("AthreeHL", fmt(w3b$estimate, 1)); mac("AthreeHLLo", fmt(w3b$conf.int[1], 1)); mac("AthreeHLHi", fmt(w3b$conf.int[2], 1))
mac("AthreeSD", fmt(sd(dd), 1))
res$A3 <- c("A3", "Cappuccino minus espresso (one-tailed)", "Paired $t$-test",
            paste0(fmt(mean(dd), 1), " CZK ", ci_str(t3b$conf.int[1], t3b$conf.int[2], 1)),
            paste0("$d_z$ = ", fmt(dz), " ", ci_str(dzci[1], dzci[2])),
            paste0("$t$(", t3$parameter, ") = ", fmt(t3$statistic), ", ", ptex(t3$p.value)))

d4 <- d[!is.na(d$district_main), ]; d4$district_main <- droplevels(d4$district_main)
n4 <- nrow(d4); k4 <- nlevels(d4$district_main)
wa <- oneway.test(cappuccino ~ district_main, data = d4, var.equal = FALSE); tick()
aov4 <- summary(aov(cappuccino ~ district_main, data = d4))[[1]]
SSB <- aov4[1, "Sum Sq"]; SSW <- aov4[2, "Sum Sq"]; MSW <- aov4[2, "Mean Sq"]; F4 <- aov4[1, "F value"]
eta2 <- SSB / (SSB + SSW); eta2ci <- eta2_ci(F4, k4 - 1, n4 - k4, n4)
omega2 <- (SSB - (k4 - 1) * MSW) / (SSB + SSW + MSW)
kw <- kruskal.test(cappuccino ~ district_main, data = d4); tick()
eps2 <- unname(kw$statistic) / (n4 - 1)
eps2ci <- pct(replicate(B, { i <- sample(n4, replace = TRUE); unname(kruskal.test(d4$cappuccino[i], d4$district_main[i])$statistic) / (n4 - 1) }))
A4 <- data.frame(
  Test = c("Welch ANOVA", "Kruskal--Wallis"),
  Statistic = c(paste0("$F$(", k4 - 1, ", ", fmt(wa$parameter[2], 1), ") = ", fmt(wa$statistic)), paste0("$H$(", kw$parameter, ") = ", fmt(kw$statistic))),
  `$p$` = c(fmtp(wa$p.value), fmtp(kw$p.value)),
  `Effect size [95\\% CI]` = c(paste0("$\\eta^2$ = ", fmt(eta2, 3), " ", ci_str(eta2ci[1], eta2ci[2], 3), "; $\\omega^2$ = ", fmt(omega2, 3)),
                              paste0("$\\varepsilon^2$ = ", fmt(eps2, 3), " ", ci_str(eps2ci[1], eps2ci[2], 3))),
  check.names = FALSE)
write_tab(A4, "A4_table", align = c("l", "l", "r", "r", "p{6.4cm}"))
y <- d4$cappuccino; g <- d4$district_main
m_i <- tapply(y, g, mean); v_i <- tapply(y, g, var); n_i <- tapply(y, g, length)
pairs <- t(combn(levels(g), 2)); gh <- NULL; dn <- NULL
rk <- rank(y); R_i <- tapply(rk, g, mean); tt <- table(y); tie_corr <- sum(tt^3 - tt) / (12 * (n4 - 1))
for (r in seq_len(nrow(pairs))) {
  a <- pairs[r, 1]; b <- pairs[r, 2]
  dif <- m_i[a] - m_i[b]; se <- sqrt(v_i[a] / n_i[a] + v_i[b] / n_i[b])
  dfw <- se^4 / ((v_i[a] / n_i[a])^2 / (n_i[a] - 1) + (v_i[b] / n_i[b])^2 / (n_i[b] - 1))
  q <- abs(dif / se) * sqrt(2); pg <- ptukey(q, k4, dfw, lower.tail = FALSE); crit <- qtukey(.95, k4, dfw) * se / sqrt(2)
  gh <- rbind(gh, data.frame(Comparison = paste(a, "minus", b), Diff = dif, Lo = dif - crit, Hi = dif + crit, p = pg))
  zz <- (R_i[a] - R_i[b]) / sqrt((n4 * (n4 + 1) / 12 - tie_corr * 1) * (1 / n_i[a] + 1 / n_i[b]))
  dn <- rbind(dn, data.frame(Comparison = paste(a, "minus", b), z = zz, p = 2 * pnorm(-abs(zz))))
}
tick(2 * nrow(pairs))
dn$p_holm <- p.adjust(dn$p, "holm")
gh_tab <- data.frame(Comparison = gh$Comparison, `Difference (CZK)` = fmt(gh$Diff, 1), `95\\% CI` = mapply(ci_str, gh$Lo, gh$Hi, MoreArgs = list(k = 1)), `Adjusted $p$` = sapply(gh$p, fmtp), check.names = FALSE)
dn_tab <- data.frame(Comparison = dn$Comparison, `$z$` = fmt(dn$z), `$p$ (unadjusted)` = sapply(dn$p, fmtp), `$p$ (Holm)` = sapply(dn$p_holm, fmtp), check.names = FALSE)
write_tab(gh_tab, "A4_posthoc_gameshowell", align = c("l", "l", "r", "r", "r"))
write_tab(dn_tab, "A4_posthoc_dunn", align = c("l", "l", "r", "r", "r"))
ci_d <- do.call(rbind, lapply(split(y, g), function(v) { h <- qt(.975, length(v) - 1) * sd(v) / sqrt(length(v)); c(m = mean(v), lo = mean(v) - h, hi = mean(v) + h, n = length(v)) }))
ci_d <- data.frame(group = factor(rownames(ci_d), levels = levels(g)), ci_d)
p <- ggplot(d4, aes(district_main, cappuccino)) +
  geom_jitter(width = .15, height = 0, size = 1.1, alpha = .4, colour = COP) +
  geom_hline(yintercept = mean(y), colour = GREY, linetype = "dashed") +
  geom_errorbar(data = ci_d, aes(x = group, y = m, ymin = lo, ymax = hi), width = .15, colour = ESP, linewidth = .9) +
  geom_point(data = ci_d, aes(x = group, y = m), size = 3, colour = ESP) +
  geom_text(data = ci_d, aes(x = group, y = 38, label = paste0("n = ", n)), size = 3, colour = GREY) +
  labs(x = NULL, y = "Cappuccino price (CZK)", title = "Cappuccino price by district",
       subtitle = "Dark points: district means with 95% CI; dashed line: overall mean of the six districts")
save_fig(p, "A4_figure", 7, 4)
best <- gh[which.min(gh$p), ]
mac("AfourN", n4); mac("AfourF", fmt(wa$statistic)); mac("AfourDfa", k4 - 1); mac("AfourDfb", fmt(wa$parameter[2], 1)); mac("AfourP", ptxt(wa$p.value))
mac("AfourEta", fmt(eta2, 3)); mac("AfourEtaLo", fmt(eta2ci[1], 3)); mac("AfourEtaHi", fmt(eta2ci[2], 3)); mac("AfourOmega", fmt(omega2, 3))
mac("AfourH", fmt(kw$statistic)); mac("AfourKWdf", kw$parameter); mac("AfourKWP", ptxt(kw$p.value)); mac("AfourEps", fmt(eps2, 3))
mac("AfourBestPair", best$Comparison); mac("AfourBestDiff", fmt(best$Diff, 1)); mac("AfourBestLo", fmt(best$Lo, 1)); mac("AfourBestHi", fmt(best$Hi, 1)); mac("AfourBestP", ptxt(best$p))
mac("AfourNsigGH", sum(gh$p < ALPHA)); mac("AfourNsigDunn", sum(dn$p_holm < ALPHA))
mac("AfourMax", names(which.max(m_i))); mac("AfourMin", names(which.min(m_i))); mac("AfourMaxM", fmt(max(m_i), 1)); mac("AfourMinM", fmt(min(m_i), 1))
res$A4 <- c("A4", "Six districts", "Welch ANOVA",
            paste0("means ", fmt(min(m_i), 1), "--", fmt(max(m_i), 1), " CZK"),
            paste0("$\\eta^2$ = ", fmt(eta2, 3), " ", ci_str(eta2ci[1], eta2ci[2], 3)),
            paste0("$F$(", k4 - 1, ", ", fmt(wa$parameter[2], 1), ") = ", fmt(wa$statistic), ", ", ptex(wa$p.value)))

d5 <- d[!is.na(d$type), ]; n5 <- nrow(d5)
tab5 <- table(d5$type, d5$prague1)
cs <- suppressWarnings(chisq.test(tab5, correct = FALSE)); tick()
min_exp <- min(cs$expected)
fi <- fisher.test(tab5); tick()
V <- function(tb) { e <- outer(rowSums(tb), colSums(tb)) / sum(tb); sqrt(sum((tb - e)^2 / e) / (sum(tb) * (min(dim(tb)) - 1))) }
Vhat <- V(tab5)
Vci <- pct(replicate(B, { i <- sample(n5, replace = TRUE); tb <- table(d5$type[i], d5$prague1[i]); if (any(dim(tb) < 2)) NA else V(tb) }) |> (\(z) z[!is.na(z)])())
use_fisher <- min_exp < 5
A5 <- data.frame(
  Test = c("Pearson chi-square", "Fisher's exact test"),
  Statistic = c(paste0("$\\chi^2$(", cs$parameter, ", $N$ = ", n5, ") = ", fmt(cs$statistic)), "--"),
  `$p$` = c(fmtp(cs$p.value), fmtp(fi$p.value)),
  `Smallest expected count` = c(fmt(min_exp, 2), "--"),
  `Cram\\'er's $V$ [95\\% CI]` = c(paste0(fmt(Vhat), " ", ci_str(Vci[1], Vci[2])), "--"), check.names = FALSE)
write_tab(A5, "A5_table", align = c("l", "l", "r", "r", "r", "r"))
pr <- prop.table(tab5, 1) * 100
cnt <- data.frame(`Cafe type` = rownames(tab5),
                  `Prague 1` = paste0(tab5[, "Prague 1"], " (", fmt(pr[, "Prague 1"], 1), "\\%)"),
                  `Rest of Prague` = paste0(tab5[, "Rest of Prague"], " (", fmt(pr[, "Rest of Prague"], 1), "\\%)"),
                  `Total` = rowSums(tab5),
                  `Std. residual (Prague 1)` = fmt(cs$stdres[, "Prague 1"]), check.names = FALSE)
write_tab(cnt, "A5_counts", align = c("l", "l", "r", "r", "r", "r"))
pp <- do.call(rbind, lapply(rownames(tab5), function(l) { k <- tab5[l, "Prague 1"]; nn <- sum(tab5[l, ]); ci <- prop.test(k, nn, correct = FALSE)$conf.int
  data.frame(type = l, share = k / nn, lo = ci[1], hi = ci[2], n = nn) }))
pp$type <- factor(pp$type, levels = levels(d5$type))
p <- ggplot(pp, aes(type, share)) +
  geom_col(fill = COP, alpha = .8, width = .6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = .12, colour = ESP, linewidth = .9) +
  geom_text(aes(y = 0.02, label = paste0("n = ", n)), colour = "white", size = 3.4, vjust = 0) +
  scale_y_continuous(labels = function(v) paste0(round(v * 100), "%"), limits = c(0, 1)) +
  labs(x = NULL, y = "Share of cafes located in Prague 1", title = "Cafe type and location",
       subtitle = "Bars: share in Prague 1; error bars: 95% Wilson CI")
save_fig(p, "A5_figure")
mac("AfiveN", n5); mac("AfiveChi", fmt(cs$statistic)); mac("AfiveDf", cs$parameter); mac("AfiveP", ptxt(cs$p.value)); mac("AfiveFisherP", ptxt(fi$p.value))
mac("AfiveMinExp", fmt(min_exp, 2)); mac("AfiveV", fmt(Vhat)); mac("AfiveVLo", fmt(Vci[1])); mac("AfiveVHi", fmt(Vci[2]))
mac("AfiveShareInd", fmt(100 * pp$share[1], 1)); mac("AfiveShareLoc", fmt(100 * pp$share[2], 1)); mac("AfiveShareInt", fmt(100 * pp$share[3], 1))
mac("AfiveNInt", pp$n[3]); mac("AfiveKInt", tab5["International chain", "Prague 1"])
mac("AfiveResInt", fmt(cs$stdres["International chain", "Prague 1"])); mac("AfiveResInd", fmt(cs$stdres["Independent", "Prague 1"])); mac("AfiveResLoc", fmt(cs$stdres["Local chain", "Prague 1"]))
res$A5 <- c("A5", "Cafe type $\\times$ Prague 1", if (use_fisher) "Fisher's exact test" else "Pearson chi-square",
            paste0("share in Prague 1: ", fmt(100 * pp$share[1], 0), "--", fmt(100 * max(pp$share), 0), "\\%"),
            paste0("$V$ = ", fmt(Vhat), " ", ci_str(Vci[1], Vci[2])),
            if (use_fisher) paste0("$N$ = ", n5, ", ", ptex(fi$p.value)) else paste0("$\\chi^2$(", cs$parameter, ", $N$ = ", n5, ") = ", fmt(cs$statistic), ", ", ptex(cs$p.value)))

xs <- d$distance; ys <- d$cappuccino; n6 <- length(xs)
pe <- cor.test(xs, ys, alternative = "less"); tick(); pe2 <- cor.test(xs, ys)
sr <- suppressWarnings(cor.test(xs, ys, method = "spearman", alternative = "less", exact = FALSE)); tick()
ke <- suppressWarnings(cor.test(xs, ys, method = "kendall", alternative = "less", exact = FALSE)); tick()
bs_sp <- replicate(B, { i <- sample(n6, replace = TRUE); cor(xs[i], ys[i], method = "spearman") })
bs_ke <- replicate(B, { i <- sample(n6, replace = TRUE); cor(xs[i], ys[i], method = "kendall") })
bs_pe <- replicate(B, { i <- sample(n6, replace = TRUE); cor(xs[i], ys[i]) })
sp_ci <- pct(bs_sp); ke_ci <- pct(bs_ke); pe_bci <- pct(bs_pe)
A6 <- data.frame(
  Coefficient = c("Pearson $r$", "Spearman $\\rho$", "Kendall $\\tau$"),
  Estimate = c(fmt(pe$estimate), fmt(sr$estimate), fmt(ke$estimate)),
  `95\\% CI` = c(ci_str(pe2$conf.int[1], pe2$conf.int[2]), paste0(ci_str(sp_ci[1], sp_ci[2]), "$^{b}$"), paste0(ci_str(ke_ci[1], ke_ci[2]), "$^{b}$")),
  `Shared variance` = c(paste0(fmt(100 * pe$estimate^2, 1), "\\%"), paste0(fmt(100 * sr$estimate^2, 1), "\\%"), "--"),
  Statistic = c(paste0("$t$(", pe$parameter, ") = ", fmt(pe$statistic)), paste0("$S$ = ", formatC(sr$statistic, format = "f", digits = 0, big.mark = "")), paste0("$z$ = ", fmt(ke$statistic))),
  `$p$ (one-tailed)` = c(fmtp(pe$p.value), fmtp(sr$p.value), fmtp(ke$p.value)), check.names = FALSE)
write_tab(A6, "A6_table", align = c("l", "l", "r", "r", "r", "r", "r"))
p <- ggplot(data.frame(xs, ys), aes(xs, ys)) +
  geom_point(colour = COP, alpha = .55, size = 1.8) +
  geom_smooth(method = "lm", formula = y ~ x, colour = ESP, fill = ESP, alpha = .15, linewidth = .9) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = GREY, linetype = "dashed", linewidth = .8) +
  labs(x = "Distance from the Orloj (km)", y = "Cappuccino price (CZK)", title = "Cappuccino price and distance from the Orloj",
       subtitle = paste0("Solid: linear fit with 95% CI; dashed: loess smoother.  Pearson r = ", sprintf("%.2f", pe$estimate), ", n = ", n6))
save_fig(p, "A6_figure")
mac("AsixN", n6); mac("AsixR", fmt(pe$estimate)); mac("AsixRLo", fmt(pe2$conf.int[1])); mac("AsixRHi", fmt(pe2$conf.int[2]))
mac("AsixRsq", fmt(100 * pe$estimate^2, 1)); mac("AsixT", fmt(pe$statistic)); mac("AsixDf", pe$parameter); mac("AsixP", ptxt(pe$p.value))
mac("AsixRho", fmt(sr$estimate)); mac("AsixRhoLo", fmt(sp_ci[1])); mac("AsixRhoHi", fmt(sp_ci[2])); mac("AsixRhoP", ptxt(sr$p.value))
mac("AsixTau", fmt(ke$estimate)); mac("AsixTauLo", fmt(ke_ci[1])); mac("AsixTauHi", fmt(ke_ci[2])); mac("AsixTauP", ptxt(ke$p.value))
res$A6 <- c("A6", "Price vs.\\ distance (one-tailed)", "Pearson $r$",
            paste0("$r$ = ", fmt(pe$estimate), " ", ci_str(pe2$conf.int[1], pe2$conf.int[2])),
            paste0("$r^2$ = ", fmt(pe$estimate^2, 3)),
            paste0("$t$(", pe$parameter, ") = ", fmt(pe$statistic), ", ", ptex(pe$p.value)))

m7lin <- lm(cappuccino ~ distance, data = d)
m7log <- lm(log(cappuccino) ~ distance, data = d)
rc_lin <- robust_coef(m7lin)[2, ]; rc_log <- robust_coef(m7log)[2, ]; tick(2)
pc <- 100 * (exp(rc_log[c("b", "lo", "hi")]) - 1)
reg_table(list(m7lin, m7log), c("Price (CZK)", "log(price)"),
          list("(Intercept)" = "Intercept", "distance" = "Distance (km)"), "A7_table", "")
diag_plots(m7lin, "A7_diagnostics_lin"); diag_plots(m7log, "A7_diagnostics_log")
grid7 <- data.frame(distance = seq(min(d$distance), max(d$distance), length.out = 100))
bl <- robust_band(m7lin, grid7); bg <- robust_band(m7log, grid7)
p <- ggplot(d, aes(distance, cappuccino)) +
  geom_point(colour = COP, alpha = .5, size = 1.8) +
  geom_ribbon(data = bl, aes(y = fit, ymin = lo, ymax = hi), fill = ESP, alpha = .15) +
  geom_line(data = bl, aes(y = fit), colour = ESP, linewidth = .9) +
  geom_line(data = bg, aes(y = exp(fit)), colour = GREY, linetype = "dashed", linewidth = .9) +
  labs(x = "Distance from the Orloj (km)", y = "Cappuccino price (CZK)", title = "Simple regression of price on distance",
       subtitle = paste0("Solid: ", sprintf("%.2f", rc_lin$b), " CZK per km (robust 95% CI band); dashed: log-linear fit (", sprintf("%.1f", pc$b), "% per km)"))
save_fig(p, "A7_figure")
cd7 <- cooks.distance(m7lin); infl7 <- which(cd7 > 4 / nrow(d))
m7lin_x <- lm(cappuccino ~ distance, data = d[-infl7, ])
mac("AsevenN", nobs(m7lin)); mac("AsevenB", fmt(rc_lin$b)); mac("AsevenLo", fmt(rc_lin$lo)); mac("AsevenHi", fmt(rc_lin$hi))
mac("AsevenP", ptxt(rc_lin$p)); mac("AsevenT", fmt(rc_lin$t)); mac("AsevenDf", df.residual(m7lin)); mac("AsevenRsq", fmt(summary(m7lin)$r.squared, 3))
mac("AsevenLogB", fmt(rc_log$b, 3)); mac("AsevenPct", fmt(pc$b, 1)); mac("AsevenPctLo", fmt(pc$lo, 1)); mac("AsevenPctHi", fmt(pc$hi, 1))
mac("AsevenLogP", ptxt(rc_log$p)); mac("AsevenLogT", fmt(rc_log$t)); mac("AsevenLogRsq", fmt(summary(m7log)$r.squared, 3))
mac("AsevenNinfl", length(infl7)); mac("AsevenBinfl", fmt(coef(m7lin_x)[2])); mac("AsevenMaxCook", fmt(max(cd7), 2))
mac("AsevenMaxDist", fmt(max(d$distance), 1))
res$A7 <- c("A7", "log(price) on distance", "Log-linear regression, HC3",
            paste0(fmt(pc$b, 1), "\\% per km ", ci_str(pc$lo, pc$hi, 1)),
            paste0("$R^2$ = ", fmt(summary(m7log)$r.squared, 3)),
            paste0("$t$(", df.residual(m7log), ") = ", fmt(rc_log$t), ", ", ptex(rc_log$p)))

d8 <- d[!is.na(d$type), ]
m8a <- lm(cappuccino ~ distance, data = d8)
m8b <- lm(cappuccino ~ distance + type, data = d8)
nf8 <- anova(m8a, m8b); tick()
vif8 <- vif(m8b)
rc8a <- robust_coef(m8a); rc8b <- robust_coef(m8b)
reg_table(list(m8a, m8b), c("Model 1", "Model 2"),
          list("(Intercept)" = "Intercept", "distance" = "Distance (km)",
               "typeLocal chain" = "Local chain (vs. independent)", "typeInternational chain" = "International chain (vs. independent)"),
          "A8_table", "")
diag_plots(m8b, "A8_diagnostics")
b_a <- coef(m8a)["distance"]; b_b <- coef(m8b)["distance"]
grid8 <- expand.grid(distance = seq(min(d8$distance), max(d8$distance), length.out = 60), type = levels(d8$type))
grid8$type <- factor(grid8$type, levels = levels(d8$type)); b8 <- robust_band(m8b, grid8)
p <- ggplot(d8, aes(distance, cappuccino, colour = type)) +
  geom_point(alpha = .5, size = 1.7) + geom_line(data = b8, aes(y = fit), linewidth = 1) +
  scale_colour_manual(values = c("Independent" = ESP, "Local chain" = COP, "International chain" = "#4F6F5B"), name = NULL) +
  labs(x = "Distance from the Orloj (km)", y = "Cappuccino price (CZK)", title = "Price, distance and cafe type",
       subtitle = paste0("Lines: Model 2 fits. Distance slope ", sprintf("%.2f", rc8b$b[2]), " CZK per km, robust 95% CI [", sprintf("%.2f", rc8b$lo[2]), ", ", sprintf("%.2f", rc8b$hi[2]), "]")) +
  theme(legend.position = "bottom")
save_fig(p, "A8_figure")
mac("AeightN", nobs(m8b)); mac("AeightBa", fmt(b_a)); mac("AeightBb", fmt(b_b)); mac("AeightBbLo", fmt(rc8b$lo[2])); mac("AeightBbHi", fmt(rc8b$hi[2])); mac("AeightBaLo", fmt(rc8a$lo[2])); mac("AeightBaHi", fmt(rc8a$hi[2]))
mac("AeightChange", fmt(b_b - b_a)); mac("AeightRelChange", fmt(100 * (b_b - b_a) / abs(b_a), 1))
mac("AeightF", fmt(nf8$F[2])); mac("AeightDfa", nf8$Df[2]); mac("AeightDfb", nf8$Res.Df[2]); mac("AeightP", ptxt(nf8$`Pr(>F)`[2]))
mac("AeightDeltaR", fmt(summary(m8b)$r.squared - summary(m8a)$r.squared, 3)); mac("AeightRsqA", fmt(summary(m8a)$r.squared, 3)); mac("AeightRsqB", fmt(summary(m8b)$r.squared, 3))
mac("AeightVifDist", fmt(vif8["distance", 1])); mac("AeightVifType", fmt(vif8["type", 1])); mac("AeightGVIFadj", fmt(vif8["type", 3]))
mac("AeightLocal", fmt(rc8b$b[3], 1)); mac("AeightLocalLo", fmt(rc8b$lo[3], 1)); mac("AeightLocalHi", fmt(rc8b$hi[3], 1))
mac("AeightInt", fmt(rc8b$b[4], 1)); mac("AeightIntLo", fmt(rc8b$lo[4], 1)); mac("AeightIntHi", fmt(rc8b$hi[4], 1))
cd8 <- cooks.distance(m8b); infl8 <- which(cd8 > 4 / nrow(d8)); m8x <- lm(cappuccino ~ distance + type, data = d8[-infl8, ])
mac("AeightNinfl", length(infl8)); mac("AeightBinfl", fmt(coef(m8x)["distance"]))
res$A8 <- c("A8", "Type adds to distance model", "Nested $F$-test, HC3 slopes",
            paste0("slope ", fmt(rc8b$b[2]), " CZK/km ", ci_str(rc8b$lo[2], rc8b$hi[2])),
            paste0("$\\Delta R^2$ = ", fmt(summary(m8b)$r.squared - summary(m8a)$r.squared, 3)),
            paste0("$F$(", nf8$Df[2], ", ", nf8$Res.Df[2], ") = ", fmt(nf8$F[2]), ", ", ptex(nf8$`Pr(>F)`[2])))

d9 <- d[!is.na(d$chain), ]
m9a <- lm(cappuccino ~ distance + chain, data = d9)
m9b <- lm(cappuccino ~ distance * chain, data = d9)
nf9 <- anova(m9a, m9b); tick()
rc9 <- robust_coef(m9b); V9 <- vcovHC(m9b, type = "HC3"); tq9 <- qt(.975, df.residual(m9b))
b9 <- coef(m9b)
slope_ind <- b9["distance"]; se_ind <- sqrt(V9["distance", "distance"])
slope_ch  <- b9["distance"] + b9["distance:chainChain"]
se_ch <- sqrt(V9["distance", "distance"] + V9["distance:chainChain", "distance:chainChain"] + 2 * V9["distance", "distance:chainChain"])
reg_table(list(m9a, m9b), c("No interaction", "Interaction"),
          list("(Intercept)" = "Intercept", "distance" = "Distance (km)", "chainChain" = "Chain (vs. independent)", "distance:chainChain" = "Distance $\\times$ chain"),
          "A9_table", "")
diag_plots(m9b, "A9_diagnostics")
grid9 <- expand.grid(distance = seq(min(d9$distance), max(d9$distance), length.out = 60), chain = levels(d9$chain))
grid9$chain <- factor(grid9$chain, levels = levels(d9$chain)); b9b <- robust_band(m9b, grid9)
i9 <- which(rc9$term == "distance:chainChain")
p <- ggplot(d9, aes(distance, cappuccino, colour = chain, fill = chain)) +
  geom_point(alpha = .5, size = 1.7) +
  geom_ribbon(data = b9b, aes(y = fit, ymin = lo, ymax = hi), alpha = .15, colour = NA) +
  geom_line(data = b9b, aes(y = fit), linewidth = 1) +
  scale_colour_manual(values = c("Independent" = ESP, "Chain" = COP), name = NULL) +
  scale_fill_manual(values = c("Independent" = ESP, "Chain" = COP), name = NULL) +
  labs(x = "Distance from the Orloj (km)", y = "Cappuccino price (CZK)", title = "Price against distance: independent cafes and chains",
       subtitle = paste0("Slopes: independent ", sprintf("%.2f", slope_ind), ", chains ", sprintf("%.2f", slope_ch), " CZK per km (robust 95% CI bands)")) +
  theme(legend.position = "bottom")
save_fig(p, "A9_figure")
mac("AnineN", nobs(m9b)); mac("AnineNind", sum(d9$chain == "Independent")); mac("AnineNch", sum(d9$chain == "Chain"))
mac("AnineInt", fmt(rc9$b[i9])); mac("AnineIntLo", fmt(rc9$lo[i9])); mac("AnineIntHi", fmt(rc9$hi[i9]))
mac("AnineF", fmt(nf9$F[2])); mac("AnineDfa", nf9$Df[2]); mac("AnineDfb", nf9$Res.Df[2]); mac("AnineP", ptxt(nf9$`Pr(>F)`[2]))
mac("AnineDeltaR", fmt(summary(m9b)$r.squared - summary(m9a)$r.squared, 3))
mac("AnineSlopeInd", fmt(slope_ind)); mac("AnineSlopeIndLo", fmt(slope_ind - tq9 * se_ind)); mac("AnineSlopeIndHi", fmt(slope_ind + tq9 * se_ind))
mac("AnineSlopeCh", fmt(slope_ch)); mac("AnineSlopeChLo", fmt(slope_ch - tq9 * se_ch)); mac("AnineSlopeChHi", fmt(slope_ch + tq9 * se_ch))
mac("AnineMaxDiff", fmt(max(abs(rc9$lo[i9]), abs(rc9$hi[i9]))))
res$A9 <- c("A9", "Distance slope: independent vs. chain", "Nested $F$-test, HC3",
            paste0("interaction ", fmt(rc9$b[i9]), " CZK/km ", ci_str(rc9$lo[i9], rc9$hi[i9])),
            paste0("$\\Delta R^2$ = ", fmt(summary(m9b)$r.squared - summary(m9a)$r.squared, 3)),
            paste0("$F$(", nf9$Df[2], ", ", nf9$Res.Df[2], ") = ", fmt(nf9$F[2]), ", ", ptex(nf9$`Pr(>F)`[2])))

d10 <- d[!is.na(d$type), ]; d10$high <- as.integer(d10$cappuccino > 83)
m10 <- glm(high ~ distance + type, data = d10, family = binomial()); tick()
X <- model.matrix(m10); pr10 <- fitted(m10); bd <- coef(m10)["distance"]; V10 <- vcov(m10)
ame <- mean(bd * pr10 * (1 - pr10))
grad <- colMeans(X * (bd * pr10 * (1 - pr10) * (1 - 2 * pr10))) + c(0, mean(pr10 * (1 - pr10)), 0, 0)
se_ame <- sqrt(drop(t(grad) %*% V10 %*% grad))
ame_ci <- ame + c(-1, 1) * 1.96 * se_ame
or10 <- exp(bd); or10ci <- exp(confint.default(m10)["distance", ])
ll1 <- as.numeric(logLik(m10)); ll0 <- as.numeric(logLik(glm(high ~ 1, data = d10, family = binomial())))
mcf <- 1 - ll1 / ll0; zz10 <- summary(m10)$coefficients["distance", "z value"]; pz10 <- summary(m10)$coefficients["distance", "Pr(>|z|)"]
A10 <- data.frame(
  Quantity = c("Average marginal effect of distance (percentage points per km)", "Odds ratio for distance (per km)", "Logit coefficient for distance", "Observations / events (price above 83 CZK)", "McFadden pseudo-$R^2$"),
  Estimate = c(fmt(100 * ame, 2), fmt(or10, 2), fmt(bd, 2), paste0(nrow(d10), " / ", sum(d10$high)), fmt(mcf, 3)),
  `95\\% CI` = c(ci_str(100 * ame_ci[1], 100 * ame_ci[2]), ci_str(or10ci[1], or10ci[2]), ci_str(confint.default(m10)["distance", 1], confint.default(m10)["distance", 2]), "--", "--"),
  Statistic = c("--", "--", paste0("$z$ = ", fmt(zz10)), "--", "--"),
  `$p$` = c("--", "--", fmtp(pz10), "--", "--"), check.names = FALSE)
write_tab(A10, "A10_table", align = c("l", "p{6cm}", "r", "r", "r", "r"))
diag_plots_logit <- NULL
grid10 <- data.frame(distance = seq(min(d10$distance), max(d10$distance), length.out = 100), type = factor("Independent", levels = levels(d10$type)))
pl <- predict(m10, grid10, type = "link", se.fit = TRUE)
grid10$fit <- plogis(pl$fit); grid10$lo <- plogis(pl$fit - 1.96 * pl$se.fit); grid10$hi <- plogis(pl$fit + 1.96 * pl$se.fit)
p <- ggplot(d10, aes(distance)) +
  geom_point(aes(y = high), position = position_jitter(height = .03, width = 0), colour = COP, alpha = .35, size = 1.4) +
  geom_ribbon(data = grid10, aes(ymin = lo, ymax = hi), fill = ESP, alpha = .15) +
  geom_line(data = grid10, aes(y = fit), colour = ESP, linewidth = 1) +
  scale_y_continuous(labels = function(v) paste0(round(v * 100), "%"), limits = c(-.05, 1.05), breaks = seq(0, 1, .25)) +
  labs(x = "Distance from the Orloj (km)", y = "Probability of a price above 83 CZK", title = "Probability of an above-median cappuccino price",
       subtitle = paste0("Curve: independent cafe, 95% CI band. Average marginal effect: ", sprintf("%.1f", 100 * ame), " pp per km"))
save_fig(p, "A10_figure")
mac("AtenN", nrow(d10)); mac("AtenEvents", sum(d10$high)); mac("AtenAME", fmt(100 * ame, 1)); mac("AtenAMELo", fmt(100 * ame_ci[1], 1)); mac("AtenAMEHi", fmt(100 * ame_ci[2], 1))
mac("AtenOR", fmt(or10)); mac("AtenORLo", fmt(or10ci[1])); mac("AtenORHi", fmt(or10ci[2])); mac("AtenZ", fmt(zz10)); mac("AtenP", ptxt(pz10)); mac("AtenMcF", fmt(mcf, 3))
mac("AtenSampleMedian", fmt(median(d$cappuccino), 1)); mac("AtenShareHigh", fmt(100 * mean(d10$high), 1))
res$A10 <- c("A10", "Probability price $>$ 83 CZK", "Logit, average marginal effect",
             paste0(fmt(100 * ame, 1), " pp per km ", ci_str(100 * ame_ci[1], 100 * ame_ci[2], 1)),
             paste0("OR = ", fmt(or10), " ", ci_str(or10ci[1], or10ci[2])),
             paste0("$z$ = ", fmt(zz10), ", ", ptex(pz10)))

summ <- as.data.frame(do.call(rbind, res), stringsAsFactors = FALSE)
names(summ) <- c("", "Question", "Primary method", "Estimate [95\\% CI]", "Effect size [95\\% CI]", "Test")
write_tab(summ, "summary", align = c("l", "l", "p{2.6cm}", "p{2.6cm}", "p{3.0cm}", "p{2.6cm}", "p{3.0cm}"))

rob_row <- function(label, dat) {
  r <- cor.test(dat$distance, dat$cappuccino); s <- suppressWarnings(cor.test(dat$distance, dat$cappuccino, method = "spearman", exact = FALSE))
  m <- lm(cappuccino ~ distance, data = dat); rc <- robust_coef(m)[2, ]
  ml <- lm(log(cappuccino) ~ distance, data = dat); rl <- robust_coef(ml)[2, ]
  c(label, nrow(dat), fmt(r$estimate), fmt(s$estimate), paste0(fmt(rc$b), " ", ci_str(rc$lo, rc$hi)), paste0(fmt(100 * (exp(rl$b) - 1), 1), "\\%"))
}
tick(2 * 4)
rob <- as.data.frame(do.call(rbind, list(
  rob_row("All cafes (main analysis)", d),
  rob_row("Without the 12 cells with several prices", d[!d$multi_cappuccino, ]),
  rob_row("Without the cafe that sells oat milk only (Srnky)", d[!grepl("oat milk", tolower(d$note)) , ]),
  rob_row("Without the most distant cafes (> 5 km)", d[d$distance <= 5, ]),
  rob_row("Without influential cafes (Cook's $D$ > 4/$n$)", d[-infl7, ])
)), stringsAsFactors = FALSE)
names(rob) <- c("Sample", "$n$", "Pearson $r$", "Spearman $\\rho$", "Slope, CZK per km [robust 95\\% CI]", "Change per km (log model)")
write_tab(rob, "robustness", align = c("l", "p{5.2cm}", "r", "r", "r", "p{3.4cm}", "r"))
mac("RobMinR", fmt(min(as.numeric(gsub("\\$-\\$", "-", rob[[3]]))))); mac("RobMaxR", fmt(max(as.numeric(gsub("\\$-\\$", "-", rob[[3]])))))

skew <- function(v) { v <- v[!is.na(v)]; m <- mean(v); mean((v - m)^3) / mean((v - m)^2)^1.5 }
cl <- read.csv("analysis/cleaning_log.csv", stringsAsFactors = FALSE)
getn <- function(s) cl$cafes_affected[cl$step == s]
mac("ClnEmpty", getn("Empty rows")); mac("ClnDistrict", getn("District labels")); mac("ClnType", getn("Cafe type"))
mac("ClnCap", getn("Cappuccino price")); mac("ClnEsp", getn("Espresso price")); mac("ClnSep", getn("Number of ratings")); mac("ClnDup", getn("Duplicate cafes"))
mac("Nraw", nrow(read.csv("analysis/cafe_raw.csv", colClasses = "character")))
mac("Nfilled", N_ALL + getn("Duplicate cafes"))
mac("Ntype", sum(!is.na(d$type)))
mac("PriceMean", fmt(mean(d$cappuccino), 1)); mac("PriceSD", fmt(sd(d$cappuccino), 1)); mac("PriceMedian", fmt(median(d$cappuccino), 1))
mac("PriceIQR", fmt(IQR(d$cappuccino), 1)); mac("PriceSkew", fmt(skew(d$cappuccino))); mac("PriceMin", fmt(min(d$cappuccino), 0)); mac("PriceMax", fmt(max(d$cappuccino), 0))
mac("DistSkew", fmt(skew(d$distance))); mac("DistMedian", fmt(median(d$distance))); mac("DistMax", fmt(max(d$distance), 1))
mac("DiffSkew", fmt(skew(dd)))
mac("AsevenSkewLin", fmt(skew(resid(m7lin)))); mac("AsevenSkewLog", fmt(skew(resid(m7log))))
mac("AoneDiffAbs", fmt(abs(mean(d$cappuccino) - BENCHMARK), 1)); mac("AoneDir", if (mean(d$cappuccino) < BENCHMARK) "below" else "above")
mac("AonePctRel", fmt(100 * abs(mean(d$cappuccino) - BENCHMARK) / BENCHMARK, 1))
mac("NstudentsCafes", 20)
mac("AthreePctEsp", fmt(100 * mean(dd) / mean(d$espresso), 0)); mac("EspMean", fmt(mean(d$espresso), 1))

mac("Ntests", ntests)
cat("\nTests counted in this report:", ntests, "\n")

writeLines(c("% numbers.tex -- written by analysis/03_tests.R; do not edit by hand",
             paste0("\\newcommand{\\", names(macros), "}{", macros, "}")), "report/tables/numbers.tex")

print(summ[, c(1, 4:6)], right = FALSE)
cat("\nPlanned checks:\n"); print(A6); print(rob)
sessionInfo()
