# packages
{
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
}

# Paths + logging funs
{
  main_path <- "~/Documents/Education/UCI - PhD/-Research (DNM)/Projects with Gillen/c3_efficienttrials/data/A4_cognition_clean.csv"
  cdr_path  <- "~/Documents/Education/UCI - PhD/-Research (DNM)/Projects with Gillen/c3_efficienttrials/data/cdr.csv"
  out_path  <- "~/Documents/Education/UCI - PhD/-Research (DNM)/Projects with Gillen/c3_efficienttrials/data/A4_CogstateRealigned_withCDR.csv"
  fig_dir   <- "~/Documents/Education/UCI - PhD/-Research (DNM)/Projects with Gillen/c3_efficienttrials/figures/"
  
  dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Known raw visit schedules (used only for diagnostic figures)
  c3_raw_visits  <- c(1, 3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63)
  cdr_raw_visits <- c(1, 18, 33, 48, 57, 66)
  
  ## Logging helper functions
  
  ts          <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_section <- function(title) {
    cat("\n", strrep("=", 80), "\n", sep = "")
    cat("[", ts(), "] ", title, "\n", sep = "")
    cat(strrep("=", 80), "\n", sep = "")
  }
  log_kv <- function(k, v) cat(sprintf("[%s] %-38s %s\n", ts(), paste0(k, ":"), v))
  log_n  <- function(k, v) log_kv(k, format(v, big.mark = ","))
  peek_vals <- function(x, n = 8) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return("<<none>>")
    paste(head(unique(x), n), collapse = " | ")
  }
  
  log_dups <- function(df, keys, name, top_n = 20) {
    d <- df %>%
      count(across(all_of(keys)), name = "n") %>%
      filter(n > 1) %>%
      arrange(desc(n))
    log_section(paste0("DUPLICATE CHECK: ", name))
    log_n("Rows",                  nrow(df))
    log_n("Duplicate key combos",  nrow(d))
    if (nrow(d) > 0) { cat("\nTop duplicates:\n"); print(head(d, top_n)) }
    else cat("No duplicates.\n")
    invisible(d)
  }
}
# Load

log_section("LOAD DATA")
main <- read_csv(main_path, show_col_types = FALSE)
cdr  <- read_csv(cdr_path,  show_col_types = FALSE)

log_n("Main rows", nrow(main))
log_n("CDR rows",  nrow(cdr))

# Verify + coerce
{
log_section("VERIFY COLUMNS & COERCE TYPES")

if (!("AVISIT" %in% names(cdr))) {
  if ("VISIT" %in% names(cdr)) {
    cdr <- cdr %>% mutate(AVISIT = VISIT)
    log_kv("AVISIT", "Created from VISIT column")
  } else stop("CDR file missing AVISIT and no VISIT fallback.")
}
if (!("CDGLOBAL" %in% names(cdr))) stop("CDR file missing CDGLOBAL.")

main <- main %>%
  mutate(
    BID   = str_trim(as.character(BID)),
    VISIT = suppressWarnings(as.numeric(VISIT))
  )

cdr <- cdr %>%
  mutate(
    BID    = str_trim(as.character(BID)),
    AVISIT = suppressWarnings(as.numeric(AVISIT))
  )

log_n("Main VISIT  NAs after coerce", sum(is.na(main$VISIT)))
log_n("CDR  AVISIT NAs after coerce", sum(is.na(cdr$AVISIT)))
}
# DIAGNOSTIC FIGURES

log_section("DIAGNOSTIC FIGURES — RAW VISIT DISTRIBUTIONS")

# count tables
c3_counts <- main %>%
  filter(!is.na(VISIT)) %>%
  count(VISIT, name = "n") %>%
  arrange(VISIT) %>%
  mutate(source = "C3 / Cogstate")

cdr_counts <- cdr %>%
  filter(!is.na(AVISIT)) %>%
  count(AVISIT, name = "n") %>%
  rename(VISIT = AVISIT) %>%
  arrange(VISIT) %>%
  mutate(source = "CDR")

cat("\n--- C3 / Cogstate visit counts ---\n")
print(c3_counts, n = Inf)
cat("\n--- CDR visit counts ---\n")
print(cdr_counts, n = Inf)

# Figure 1: visit distributions side-by-side 
all_visits <- sort(union(c3_counts$VISIT, cdr_counts$VISIT))
combined   <- bind_rows(c3_counts, cdr_counts) %>%
  mutate(VISIT = factor(VISIT, levels = all_visits))

fig1 <- ggplot(combined, aes(x = VISIT, y = n, fill = source)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_vline(
    xintercept = which(all_visits %in% cdr_raw_visits),
    linetype = "dashed", color = "#FF5722", linewidth = 0.6, alpha = 0.8
  ) +
  scale_fill_manual(values = c("C3 / Cogstate" = "#2196F3", "CDR" = "#FF5722")) +
  labs(
    title    = "Figure 1: Visit Count Distributions — C3/Cogstate vs CDR",
    subtitle = "No remapping applied  |  Dashed orange lines = CDR anchor visits (1, 18, 33, 48, 57, 66)",
    x        = "(raw) Visit Number",
    y        = "Number of Records",
    fill     = "Dataset"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "grey40"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

ggsave(file.path(fig_dir, "fig1_raw_visit_distributions.png"),
       fig1, width = 14, height = 6, dpi = 150)
cat("Saved: fig1_raw_visit_distributions.png\n")

## not sure if needed

{
# SECTION B: DEDUPLICATE CDR

log_section("DEDUPLICATE CDR — ONE ROW PER (BID, AVISIT)")

cdr_value_cols <- setdiff(
  names(cdr),
  c("SUBSTUDY","BID","VISCODE","DONE","NDREASON","CDSPVERS","CDPTSRCE",
    "CDSPSRCE","BPID","AVISIT","EPOCH","CDEVENT","CDOLEEVENT")
)

date_candidates <- intersect(
  names(cdr),
  c("EXAMDATE","CDRDATE","VISITDATE","DATE",
    "CDADTC_DAYS_CONSENT","CDADTC_DAYS_T0","CDDY")
)
log_kv("Date columns found", if (length(date_candidates) == 0) "NONE"
       else paste(date_candidates, collapse = ", "))

cdr_dedup <- cdr %>%
  mutate(
    .nonmiss = rowSums(!is.na(across(any_of(cdr_value_cols)))),
    .row_id  = row_number()
  )

numeric_days <- intersect(date_candidates,
                          c("CDADTC_DAYS_CONSENT","CDADTC_DAYS_T0","CDDY"))

if (length(numeric_days) > 0) {
  order_col <- numeric_days[1]
  log_kv("Dedup order key", paste0(order_col, " (largest = latest)"))
  cdr_dedup <- cdr_dedup %>%
    group_by(BID, AVISIT) %>%
    arrange(desc(.data[[order_col]]), desc(.nonmiss), desc(.row_id),
            .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
} else if (length(date_candidates) > 0) {
  order_col <- date_candidates[1]
  log_kv("Dedup order key", paste0(order_col, " (latest date)"))
  cdr_dedup <- cdr_dedup %>%
    mutate(.parsed_date = suppressWarnings(lubridate::ymd(.data[[order_col]]))) %>%
    group_by(BID, AVISIT) %>%
    arrange(desc(.parsed_date), desc(.nonmiss), desc(.row_id), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(-.parsed_date)
} else {
  log_kv("Dedup order key", "most non-missing fields, then last row")
  cdr_dedup <- cdr_dedup %>%
    group_by(BID, AVISIT) %>%
    arrange(desc(.nonmiss), desc(.row_id), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
}

cdr_dedup <- cdr_dedup %>% select(-.nonmiss, -.row_id)

log_n("CDR rows before dedup", nrow(cdr))
log_n("CDR rows after dedup",  nrow(cdr_dedup))
log_dups(cdr_dedup %>% filter(!is.na(AVISIT)),
         c("BID", "AVISIT"), "CDR_DEDUP (BID, AVISIT)")

# ==================================================================
# SECTION C: JOIN — main(VISIT) to cdr(AVISIT) using raw values
# ==================================================================
log_section("LEFT JOIN: main(BID, VISIT) -> cdr(BID, AVISIT)  [raw, no remap]")

merged <- main %>%
  left_join(
    cdr_dedup,
    by     = c("BID" = "BID", "VISIT" = "AVISIT"),
    suffix = c("", "_CDR")
  )

log_n("Main rows",   nrow(main))
log_n("Merged rows", nrow(merged))
if (nrow(merged) != nrow(main))
  log_kv("WARNING", "Row count changed — check for CDR duplicates.")

match_col <- intersect(names(merged), c("CDRSB", "CDSOB", "CDGLOBAL"))[1]
if (!is.na(match_col)) {
  log_n(paste0("Matched rows (non-missing ", match_col, ")"),
        sum(!is.na(merged[[match_col]])))
  log_n(paste0("Unmatched rows (missing ",   match_col, ")"),
        sum(is.na(merged[[match_col]])))
}
}

# output
#log_section("WRITE OUTPUT")
#write_csv(merged, out_path)
#log_kv("Saved to", out_path)
#log_kv("Done", "✅")