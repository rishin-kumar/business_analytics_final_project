if (!dir.exists("analysis")) {
  stop("Run this script from the repository root (the folder containing analysis/).")
}
dir.create("report/tables", recursive = TRUE, showWarnings = FALSE)

load_packages <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
    library(p, character.only = TRUE)
  }
}
load_packages(c("dplyr", "xtable"))

cleaning_log <- data.frame(step = character(), decision = character(),
                           cafes_affected = integer(), stringsAsFactors = FALSE)
log_step <- function(step, decision, n) {
  cleaning_log[nrow(cleaning_log) + 1, ] <<- list(step, decision, as.integer(n))
  cat(sprintf("%-28s %4d  %s\n", step, n, decision))
}

raw <- read.csv("analysis/cafe_raw.csv", stringsAsFactors = FALSE,
                colClasses = "character", na.strings = character(0),
                check.names = FALSE, encoding = "UTF-8")
n_raw_rows <- nrow(raw)

names(raw)[1:21] <- c("student", "visit", "district_raw", "district_name",
                      "cafe", "address", "coords", "gmaps", "espresso_raw",
                      "cappuccino_raw", "flatwhite_raw", "filter_raw",
                      "payment", "seats_in_raw", "seats_out_raw", "rating_raw",
                      "n_ratings_raw", "type_raw", "roastery_raw", "photo", "note")
names(raw)[23:25] <- c("distance_raw", "lat_raw", "lon_raw")
raw <- raw[, c(1:21, 23:25)]

na_if_blank <- function(x) {
  x <- trimws(x)
  x[x == "" | toupper(x) == "NA"] <- NA
  x
}
raw[] <- lapply(raw, na_if_blank)

is_empty <- is.na(raw$student) & is.na(raw$cafe)
df <- raw[!is_empty, ]
log_step("Empty rows", "Rows with no student and no cafe name dropped", sum(is_empty))

dist_num <- as.integer(sub("^\\D*(\\d+)\\D*$", "\\1", df$district_raw))
n_changed <- sum(df$district_raw != paste0("Prague ", dist_num), na.rm = TRUE)
df$district <- factor(paste0("Prague ", dist_num))
log_step("District labels", "Harmonised to 'Prague N' (case, spaces, typos)", n_changed)

type_clean <- tolower(df$type_raw)
df$type <- factor(type_clean,
                  levels = c("independent", "local chain", "international chain"),
                  labels = c("Independent", "Local chain", "International chain"))
log_step("Cafe type", "Blank type set to missing (case harmonised)", sum(is.na(df$type)))

first_price <- function(x) {
  as.numeric(sub("^\\s*([0-9]+(\\.[0-9]+)?).*$", "\\1", x))
}
is_multi <- function(x) !is.na(x) & !grepl("^[0-9]+(\\.[0-9]+)?$", x)

df$espresso   <- first_price(df$espresso_raw)
df$cappuccino <- first_price(df$cappuccino_raw)
df$flatwhite  <- first_price(df$flatwhite_raw)
df$filter     <- first_price(df$filter_raw)
log_step("Espresso price",   "Several prices in a cell: first listed price kept", sum(is_multi(df$espresso_raw)))
log_step("Cappuccino price", "Several prices in a cell: first listed price kept", sum(is_multi(df$cappuccino_raw)))
log_step("Flat white price", "Several prices in a cell: first listed price kept", sum(is_multi(df$flatwhite_raw)))
log_step("Filter price",     "Several prices in a cell: first listed price kept", sum(is_multi(df$filter_raw)))
df$multi_cappuccino <- is_multi(df$cappuccino_raw)

n_sep <- sum(grepl(",", df$n_ratings_raw), na.rm = TRUE)
df$n_ratings <- as.numeric(gsub(",", "", df$n_ratings_raw))
log_step("Number of ratings", "Thousands separator removed", n_sep)

df$seats_in  <- as.numeric(df$seats_in_raw)
df$seats_out <- as.numeric(df$seats_out_raw)
df$rating    <- as.numeric(df$rating_raw)
df$roastery  <- as.numeric(df$roastery_raw)

df$distance <- as.numeric(df$distance_raw)
df$lat <- as.numeric(df$lat_raw)
df$lon <- as.numeric(df$lon_raw)
log_step("Distance", "Given distance (km) used; no missing values expected", sum(is.na(df$distance)))

dup <- duplicated(tolower(paste(trimws(df$cafe), trimws(df$address))))
df <- df[!dup, ]
log_step("Duplicate cafes", "Same cafe + address recorded twice: first record kept", sum(dup))

df$prague1 <- factor(ifelse(df$district == "Prague 1", "Prague 1", "Rest of Prague"),
                     levels = c("Rest of Prague", "Prague 1"))
df$chain <- factor(ifelse(df$type == "Independent", "Independent", "Chain"),
                   levels = c("Independent", "Chain"))
df$high_price <- as.integer(df$cappuccino > 83)
df$log_price <- log(df$cappuccino)

keep <- c("student", "cafe", "address", "district", "district_name", "type",
          "chain", "prague1", "distance", "lat", "lon", "espresso", "cappuccino",
          "flatwhite", "filter", "seats_in", "seats_out", "rating", "n_ratings",
          "roastery", "multi_cappuccino", "high_price", "log_price", "note")
df <- df[, keep]
rownames(df) <- NULL
write.csv(df, "analysis/cafe_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cleaning_log, "analysis/cleaning_log.csv", row.names = FALSE)

tab <- cleaning_log
names(tab) <- c("Step", "Decision", "Cafes affected")
print(xtable(tab, digits = 0, align = c("l", "l", "p{8.2cm}", "r")),
      file = "report/tables/cleaning_log.tex", floating = FALSE,
      booktabs = TRUE, include.rownames = FALSE,
      sanitize.text.function = function(x) x)

cat("\nRows in the raw file:", n_raw_rows, "\n")
cat("Cafes in the cleaned dataset:", nrow(df), "\n")
cat("Missing values per variable used in Batch 1:\n")
print(colSums(is.na(df[, c("cappuccino", "espresso", "distance", "district", "type")])))
cat("\nDistricts:\n"); print(table(df$district))
cat("\nCafe type:\n"); print(table(df$type, useNA = "ifany"))
cat("\nCappuccino price summary (CZK):\n"); print(summary(df$cappuccino))
cat("Median cappuccino price:", median(df$cappuccino, na.rm = TRUE), "\n")
