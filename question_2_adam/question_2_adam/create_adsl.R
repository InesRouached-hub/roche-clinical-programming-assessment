
# Question 2 - ADaM ADSL Dataset Creation

library(dplyr)
library(readr)
library(admiral)

# Define output files
output_dir <- "/cloud/project/question_2_adam"
output_csv <- file.path(output_dir, "adsl.csv")
output_log <- file.path(output_dir, "adsl_log.txt")

# Load the SDTM source datasets
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

# Helper function to identify complete ISO dates
is_complete_date <- function(x) {
  !is.na(x) &
    grepl("^\\d{4}-\\d{2}-\\d{2}", x) &
    !is.na(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
}

# Helper function to extract date parts
get_datepart <- function(x) {
  as.Date(substr(x, 1, 10), format = "%Y-%m-%d")
}

# Start ADSL from DM
adsl <- dm %>%
  mutate(
    AGEGR9 = case_when(
      !is.na(AGE) & AGE < 18 ~ "<18",
      !is.na(AGE) & AGE >= 18 & AGE <= 50 ~ "18 - 50",
      !is.na(AGE) & AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGEGR9 == "<18" ~ 1,
      AGEGR9 == "18 - 50" ~ 2,
      AGEGR9 == ">50" ~ 3,
      TRUE ~ NA_real_
    ),
    ITTFL = if_else(
      !is.na(ARM) & trimws(ARM) != "",
      "Y",
      "N"
    )
  )

# Filter valid exposure records
ex_valid <- ex %>%
  filter(
    !is.na(EXDOSE),
    EXDOSE > 0 |
      (EXDOSE == 0 &
         !is.na(EXTRT) &
         grepl("PLACEBO", EXTRT, ignore.case = TRUE))
  )

# Derive treatment start date-time and imputation flag
ex_start_dtm <- ex_valid %>%
  filter(is_complete_date(EXSTDTC)) %>%
  admiral::derive_vars_dtm(
    new_vars_prefix = "TRTS",
    dtc = EXSTDTC,
    highest_imputation = "h",
    ignore_seconds_flag = TRUE
  ) %>%
  filter(!is.na(TRTSDTM)) %>%
  arrange(USUBJID, TRTSDTM, EXSEQ) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  select(USUBJID, TRTSDTM, TRTSTMF)

# Derive treatment end date-time
ex_end_dtm <- ex_valid %>%
  filter(is_complete_date(EXENDTC)) %>%
  admiral::derive_vars_dtm(
    new_vars_prefix = "TRTE",
    dtc = EXENDTC,
    highest_imputation = "h",
    ignore_seconds_flag = TRUE
  ) %>%
  filter(!is.na(TRTEDTM)) %>%
  arrange(USUBJID, desc(TRTEDTM), desc(EXSEQ)) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  select(USUBJID, TRTEDTM)

# Latest valid vital signs date
vs_last <- vs %>%
  mutate(VSDATE = get_datepart(VSDTC)) %>%
  filter(
    !(is.na(VSSTRESN) & is.na(VSSTRESC)),
    !is.na(VSDATE)
  ) %>%
  group_by(USUBJID) %>%
  summarise(LSTVS = max(VSDATE), .groups = "drop")

# Latest adverse event onset date
ae_last <- ae %>%
  mutate(AEDATE = get_datepart(AESTDTC)) %>%
  filter(!is.na(AEDATE)) %>%
  group_by(USUBJID) %>%
  summarise(LSTAE = max(AEDATE), .groups = "drop")

# Latest disposition date
ds_last <- ds %>%
  mutate(DSDATE = get_datepart(DSSTDTC)) %>%
  filter(!is.na(DSDATE)) %>%
  group_by(USUBJID) %>%
  summarise(LSTDS = max(DSDATE), .groups = "drop")

# Merge derived variables into ADSL
adsl <- adsl %>%
  left_join(ex_start_dtm, by = "USUBJID") %>%
  left_join(ex_end_dtm, by = "USUBJID") %>%
  left_join(vs_last, by = "USUBJID") %>%
  left_join(ae_last, by = "USUBJID") %>%
  left_join(ds_last, by = "USUBJID")

# Derive last known alive date
adsl <- adsl %>%
  rowwise() %>%
  mutate(
    LSTAVLDT = {
      candidate_dates <- c(
        as.numeric(LSTVS),
        as.numeric(LSTAE),
        as.numeric(LSTDS),
        as.numeric(as.Date(TRTEDTM))
      )
      
      if (all(is.na(candidate_dates))) {
        as.Date(NA)
      } else {
        as.Date(
          max(candidate_dates, na.rm = TRUE),
          origin = "1970-01-01"
        )
      }
    }
  ) %>%
  ungroup() %>%
  select(-any_of(c("LSTVS", "LSTAE", "LSTDS")))

# Validate the ADSL dataset
stopifnot(nrow(adsl) == nrow(dm))
stopifnot(!anyDuplicated(adsl$USUBJID))
stopifnot(all(adsl$ITTFL %in% c("Y", "N")))
stopifnot(all(is.na(adsl$AGEGR9N) | adsl$AGEGR9N %in% c(1, 2, 3)))

# Export the dataset
readr::write_csv(adsl, output_csv, na = "")

# Create the execution log
log_lines <- c(
  "ADSL creation completed successfully.",
  paste("Finished:", Sys.time()),
  paste("Number of subjects:", nrow(adsl)),
  paste("Number of variables:", ncol(adsl)),
  paste("Missing TRTSDTM:", sum(is.na(adsl$TRTSDTM))),
  paste("Missing TRTSTMF:", sum(is.na(adsl$TRTSTMF))),
  paste("Missing LSTAVLDT:", sum(is.na(adsl$LSTAVLDT))),
  paste("Duplicate USUBJID values:", anyDuplicated(adsl$USUBJID)),
  "Validation checks passed."
)

writeLines(
  c(paste("Started:", Sys.time()), log_lines),
  output_log
)

# Display a summary
print(
  adsl %>%
    select(
      USUBJID, AGE, AGEGR9, AGEGR9N,
      TRTSDTM, TRTSTMF, TRTEDTM, ITTFL, LSTAVLDT
    ) %>%
    head(10)
)

cat("\nADSL creation completed successfully.\n")
cat("Dataset:", output_csv, "\n")
cat("Log:", output_log, "\n")
