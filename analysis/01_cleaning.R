# =============================================================================
# 01_cleaning.R  --  Prague Cafe Project, Batch 1 (cappuccino price and location)
#
# Turns the raw data file (analysis/cafe_raw.csv, never edited by hand) into the
# dataset used by 02_descriptives.R and 03_tests.R (analysis/cafe_clean.csv).
# Every cleaning step is done here in code and logged, decision by decision,
# with the number of cafes affected (analysis/cleaning_log.csv and
# report/tables/cleaning_log.tex).
#
# RUN FROM THE REPOSITORY ROOT (the folder that contains analysis/, report/,
# presentation/), e.g. in RStudio: setwd("path/to/repository")
# Order of scripts: 01_cleaning.R -> 02_descriptives.R -> 03_tests.R
# =============================================================================

## 0. SETUP ------------------------------------------------------------------
if (!dir.exists("analysis")) {
  # Checks that the working directory is the repository root (it must contain analysis/).
  stop("Run this script from the repository root (the folder containing analysis/).")
  # Stops with a clear message if the script was started from the wrong folder.
}
# Closes the working-directory check.
dir.create("report/tables", recursive = TRUE, showWarnings = FALSE)
# Makes sure the output folder for LaTeX tables exists (silently does nothing if it already does).

load_packages <- function(pkgs) {
  # Defines a helper that takes a vector of package names.
  for (p in pkgs) {
    # Loops over each package name.
    if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
    # Installs the package from CRAN only if it is not already installed.
    library(p, character.only = TRUE)
    # Attaches the package so its functions can be used.
  }
  # Ends the loop.
}
# Ends the helper function.
load_packages(c("dplyr", "xtable"))
# Loads dplyr (data handling) and xtable (writes LaTeX tables).

cleaning_log <- data.frame(step = character(), decision = character(),
                           # Creates an empty table with a column for the step name and one for the decision,
                           cafes_affected = integer(), stringsAsFactors = FALSE)
                           # plus a column counting affected cafes; text stays as text.
log_step <- function(step, decision, n) {
  # Defines a helper that records one cleaning decision.
  cleaning_log[nrow(cleaning_log) + 1, ] <<- list(step, decision, as.integer(n))
  # Appends a new row to the global log (<<- writes to the outer variable).
  cat(sprintf("%-28s %4d  %s\n", step, n, decision))
  # Prints the same entry on screen in aligned columns so you can follow the run.
}
# Ends the helper function.

## 1. READ THE RAW FILE (as text, so nothing is silently converted) -----------
raw <- read.csv("analysis/cafe_raw.csv", stringsAsFactors = FALSE,
                # Reads the raw CSV file, keeping text as text,
                colClasses = "character", na.strings = character(0),
                # reading every column as character and treating no string as NA yet,
                check.names = FALSE, encoding = "UTF-8")
                # keeping the original column names and reading as UTF-8 (for Czech letters).
n_raw_rows <- nrow(raw)
# Remembers how many rows the raw file has, for the final check.

# short names; the order of the columns is the order in the raw file
names(raw)[1:21] <- c("student", "visit", "district_raw", "district_name",
                      # Renames the first 21 columns to short, code-friendly names:
                      "cafe", "address", "coords", "gmaps", "espresso_raw",
                      # cafe name, address, coordinates, map link, raw espresso price,
                      "cappuccino_raw", "flatwhite_raw", "filter_raw",
                      # raw cappuccino, flat white and filter prices,
                      "payment", "seats_in_raw", "seats_out_raw", "rating_raw",
                      # payment type, seats inside/outside and rating (all still raw text),
                      "n_ratings_raw", "type_raw", "roastery_raw", "photo", "note")
                      # number of ratings, cafe type, roastery, photo and free-text note.
names(raw)[23:25] <- c("distance_raw", "lat_raw", "lon_raw")
# Renames columns 23-25 to distance, latitude and longitude (column 22 is skipped).
raw <- raw[, c(1:21, 23:25)]
# Keeps columns 1-21 and 23-25, dropping column 22 which is not needed.

# helper: blank strings and the text "NA" are missing values
na_if_blank <- function(x) {
  # Defines a helper that turns blank-looking text into proper missing values.
  x <- trimws(x)
  # Removes leading and trailing spaces.
  x[x == "" | toupper(x) == "NA"] <- NA
  # Replaces empty strings and the text "NA" (any case) with a real NA.
  x
  # Returns the cleaned vector.
}
# Ends the helper function.
raw[] <- lapply(raw, na_if_blank)
# Applies the helper to every column while keeping the table's structure.

## 2. DROP EMPTY ROWS ---------------------------------------------------------
# The sheet has 300 rows reserved for data but only some are filled in; the
# empty ones carry only an error value (#VALUE!) from the distance formula.
is_empty <- is.na(raw$student) & is.na(raw$cafe)
# Flags rows that have neither a student nor a cafe name, i.e. unused rows.
df <- raw[!is_empty, ]
# Keeps only the filled-in rows in a new data frame called df.
log_step("Empty rows", "Rows with no student and no cafe name dropped", sum(is_empty))
# Logs how many empty rows were dropped.

## 3. HARMONISE DISTRICT LABELS -----------------------------------------------
# "prague 1", "Prague 1 ", "prauge 1", "Prage 7" ... -> "Prague 1" etc.
# The district number is taken from the digits in the label.
dist_num <- as.integer(sub("^\\D*(\\d+)\\D*$", "\\1", df$district_raw))
# Extracts just the digits from each label with a regular expression and turns them into a number.
n_changed <- sum(df$district_raw != paste0("Prague ", dist_num), na.rm = TRUE)
# Counts the labels that differ from the standard "Prague N" form (i.e. had typos/case/spaces).
df$district <- factor(paste0("Prague ", dist_num))
# Builds the clean district variable as a factor with the standard label.
log_step("District labels", "Harmonised to 'Prague N' (case, spaces, typos)", n_changed)
# Logs how many labels were changed.

## 4. CAFE TYPE ---------------------------------------------------------------
type_clean <- tolower(df$type_raw)
# Lower-cases the type text so spelling variants in case match.
df$type <- factor(type_clean,
                  # Converts the type to a factor,
                  levels = c("independent", "local chain", "international chain"),
                  # accepting only these three values (anything else becomes NA),
                  labels = c("Independent", "Local chain", "International chain"))
                  # and giving them nice display labels.
log_step("Cafe type", "Blank type set to missing (case harmonised)", sum(is.na(df$type)))
# Logs how many cafes have a missing type.

## 5. PRICES: ONE VALUE PER CELL ----------------------------------------------
# Some cells list several prices, e.g. "85, 95, 105" (three cup sizes) or
# "65/40". RULE (brief default): take the FIRST listed price, which is the
# dine-in or smallest size. Thousands separators are not removed from prices
# first: "115,125,135" is the three prices 115, 125 and 135 (the sheet displays
# it with a thousands format), not one number.
first_price <- function(x) {
  # Defines a helper that extracts the first number from a price cell.
  as.numeric(sub("^\\s*([0-9]+(\\.[0-9]+)?).*$", "\\1", x))
  # A regular expression keeps the leading number (with optional decimals), drops the rest, and converts to numeric.
}
# Ends the helper function.
is_multi <- function(x) !is.na(x) & !grepl("^[0-9]+(\\.[0-9]+)?$", x)
# Flags cells that are not a single plain number (so they list several prices or contain extra text).

df$espresso   <- first_price(df$espresso_raw)
# Creates the numeric espresso price from the raw text.
df$cappuccino <- first_price(df$cappuccino_raw)
# Creates the numeric cappuccino price (the main outcome variable).
df$flatwhite  <- first_price(df$flatwhite_raw)
# Creates the numeric flat white price.
df$filter     <- first_price(df$filter_raw)
# Creates the numeric filter coffee price.
log_step("Espresso price",   "Several prices in a cell: first listed price kept", sum(is_multi(df$espresso_raw)))
# Logs how many espresso cells listed several prices.
log_step("Cappuccino price", "Several prices in a cell: first listed price kept", sum(is_multi(df$cappuccino_raw)))
# Logs how many cappuccino cells listed several prices.
log_step("Flat white price", "Several prices in a cell: first listed price kept", sum(is_multi(df$flatwhite_raw)))
# Logs how many flat white cells listed several prices.
log_step("Filter price",     "Several prices in a cell: first listed price kept", sum(is_multi(df$filter_raw)))
# Logs how many filter coffee cells listed several prices.
df$multi_cappuccino <- is_multi(df$cappuccino_raw)   # kept for a robustness check
# Stores a TRUE/FALSE flag for multi-price cappuccino cells so the robustness check can drop them later.

## 6. COUNTS AND RATING -------------------------------------------------------
# number of ratings: remove the thousands separator ("1,738" -> 1738)
n_sep <- sum(grepl(",", df$n_ratings_raw), na.rm = TRUE)
# Counts the cells that contain a comma (a thousands separator).
df$n_ratings <- as.numeric(gsub(",", "", df$n_ratings_raw))
# Removes the commas and converts the number of ratings to numeric.
log_step("Number of ratings", "Thousands separator removed", n_sep)
# Logs how many cells had a separator removed.

df$seats_in  <- as.numeric(df$seats_in_raw)
# Converts the indoor seat count to numeric.
df$seats_out <- as.numeric(df$seats_out_raw)
# Converts the outdoor seat count to numeric.
df$rating    <- as.numeric(df$rating_raw)
# Converts the rating to numeric.
df$roastery  <- as.numeric(df$roastery_raw)
# Converts the roastery column to numeric.

## 7. DISTANCE AND COORDINATES ------------------------------------------------
# The distance from the Orloj (km) is given in the file (calculated there from
# the coordinates); it is used as it is.
df$distance <- as.numeric(df$distance_raw)
# Converts the distance from the Orloj to numeric.
df$lat <- as.numeric(df$lat_raw)
# Converts the latitude to numeric.
df$lon <- as.numeric(df$lon_raw)
# Converts the longitude to numeric.
log_step("Distance", "Given distance (km) used; no missing values expected", sum(is.na(df$distance)))
# Logs the number of missing distances (expected to be zero).

## 8. DUPLICATE CAFES ---------------------------------------------------------
# The instructions to the students say duplicates do not count: the same cafe
# recorded twice is one cafe. The first record is kept.
dup <- duplicated(tolower(paste(trimws(df$cafe), trimws(df$address))))
# Flags every repeat of the same cafe name + address (ignoring case and stray spaces); the first occurrence is not flagged.
df <- df[!dup, ]
# Removes the repeated records.
log_step("Duplicate cafes", "Same cafe + address recorded twice: first record kept", sum(dup))
# Logs how many duplicates were removed.

## 9. DERIVED VARIABLES USED IN BATCH 1 ----------------------------------------
df$prague1 <- factor(ifelse(df$district == "Prague 1", "Prague 1", "Rest of Prague"),
                     # Creates a two-group variable: Prague 1 versus everything else,
                     levels = c("Rest of Prague", "Prague 1"))
                     # with "Rest of Prague" as the reference group.
df$chain <- factor(ifelse(df$type == "Independent", "Independent", "Chain"),
                   # Creates a two-group variable: independent versus chain,
                   levels = c("Independent", "Chain"))   # local + international together
                   # with "Independent" as the reference group.
df$high_price <- as.integer(df$cappuccino > 83)           # 83 CZK = median given in the brief
# Creates a 0/1 indicator that is 1 when the cappuccino costs more than 83 CZK (used in the logistic regression).
df$log_price <- log(df$cappuccino)
# Creates the natural log of the cappuccino price (used in the log-linear regression).

## 10. SAVE ---------------------------------------------------------------------
keep <- c("student", "cafe", "address", "district", "district_name", "type",
          # Lists the columns to keep in the final dataset:
          "chain", "prague1", "distance", "lat", "lon", "espresso", "cappuccino",
          # identifiers and grouping variables, location, and prices,
          "flatwhite", "filter", "seats_in", "seats_out", "rating", "n_ratings",
          # more prices, seats, rating and number of ratings,
          "roastery", "multi_cappuccino", "high_price", "log_price", "note")
          # and the roastery flag, the derived variables and the note.
df <- df[, keep]
# Drops all the other (raw) columns.
rownames(df) <- NULL
# Resets the row numbers to 1, 2, 3, ... after the rows were removed.
write.csv(df, "analysis/cafe_clean.csv", row.names = FALSE, fileEncoding = "UTF-8")
# Saves the cleaned dataset as a CSV file.
write.csv(cleaning_log, "analysis/cleaning_log.csv", row.names = FALSE)
# Saves the cleaning log as a CSV file.

# the log as a LaTeX table for the report
tab <- cleaning_log
# Copies the log so its column names can be changed for display.
names(tab) <- c("Step", "Decision", "Cafes affected")
# Gives the columns reader-friendly headings.
print(xtable(tab, digits = 0, align = c("l", "l", "p{8.2cm}", "r")),
      # Builds the LaTeX table: no decimals, a left-aligned step column, a wrapped decision column and a right-aligned count,
      file = "report/tables/cleaning_log.tex", floating = FALSE,
      # and writes it to the report folder without a surrounding table environment (the report adds it),
      booktabs = TRUE, include.rownames = FALSE,
      # using professional booktabs rules and no row numbers,
      sanitize.text.function = function(x) x)
      # and leaving the text as it is (no LaTeX escaping).

## 11. CHECKS -------------------------------------------------------------------
cat("\nRows in the raw file:", n_raw_rows, "\n")
# Prints the number of rows in the raw file.
cat("Cafes in the cleaned dataset:", nrow(df), "\n")
# Prints the number of cafes that remain after cleaning.
cat("Missing values per variable used in Batch 1:\n")
# Prints a heading for the missing-value check.
print(colSums(is.na(df[, c("cappuccino", "espresso", "distance", "district", "type")])))
# Prints the count of missing values in each key variable.
cat("\nDistricts:\n"); print(table(df$district))
# Prints a heading and then the number of cafes in each district.
cat("\nCafe type:\n"); print(table(df$type, useNA = "ifany"))
# Prints a heading and then the number of cafes of each type, including missing.
cat("\nCappuccino price summary (CZK):\n"); print(summary(df$cappuccino))
# Prints a heading and then min, quartiles, mean and max of the cappuccino price.
cat("Median cappuccino price:", median(df$cappuccino, na.rm = TRUE), "\n")
# Prints the median cappuccino price, which should be 83 CZK.
