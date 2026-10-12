# Question 1 - Create the SDTM DS domain
# Purpose: Transform raw disposition data into an SDTM-like DS dataset.

library(dplyr)
library(readr)
library(pharmaverseraw)
library(sdtm.oak)

# Load raw datasets
ds_raw <- pharmaverseraw::ds_raw
ec_raw <- pharmaverseraw::ec_raw

# Derive a reference date from the first recorded exposure per subject.
# Note: This is a proxy because the official DM reference date was not provided.
reference_dates <- ec_raw %>%
  mutate(ECSTDTC = as.Date(IT.ECSTDAT, format = "%d-%b-%Y")) %>%
  filter(!is.na(ECSTDTC)) %>%
  group_by(STUDY, PATNUM) %>%
  summarise(RFSTDTC = min(ECSTDTC), .groups = "drop") %>%
  mutate(USUBJID = paste(STUDY, PATNUM, sep = "-")) %>%
  select(USUBJID, RFSTDTC)

# Define a provisional visit order.
# This order should be confirmed against the study metadata if available.
visit_order <- c(
  "Screening 1", "Baseline", "Week 2", "Week 4", "Week 6",
  "Week 8", "Week 12", "Week 16", "Week 20", "Week 24",
  "Week 26", "Retrieval", "Ambul Ecg Removal",
  "Unscheduled 1.1", "Unscheduled 4.1", "Unscheduled 5.1",
  "Unscheduled 6.1", "Unscheduled 8.2", "Unscheduled 13.1"
)

# Map raw disposition data to SDTM variables.
ds_prepared <- ds_raw %>%
  mutate(
    STUDYID = STUDY,
    DOMAIN = "DS",
    USUBJID = paste(STUDY, PATNUM, sep = "-"),
    VISIT = INSTANCE,
    VISITNUM = match(INSTANCE, visit_order),
    DSTERM = if_else(
      !is.na(OTHERSP) & OTHERSP != "",
      OTHERSP,
      IT.DSTERM
    ),
    DSDECOD = if_else(
      !is.na(OTHERSP) & OTHERSP != "",
      OTHERSP,
      IT.DSDECOD
    ),
    DSCAT = case_when(
      !is.na(OTHERSP) & OTHERSP != "" ~ "OTHER EVENT",
      IT.DSDECOD == "Randomized" ~ "PROTOCOL MILESTONE",
      TRUE ~ "DISPOSITION EVENT"
    ),
    DSSTDTC = format(
      as.Date(IT.DSSTDAT, format = "%m-%d-%Y"),
      "%Y-%m-%d"
    ),
    COLLECTION_DATE = format(
      as.Date(DSDTCOL, format = "%m-%d-%Y"),
      "%Y-%m-%d"
    ),
    DSDTC = if_else(
      is.na(DSTMCOL) | DSTMCOL == "",
      COLLECTION_DATE,
      paste0(COLLECTION_DATE, "T", DSTMCOL)
    )
  ) %>%
  left_join(reference_dates, by = "USUBJID") %>%
  select(-RFSTDTC) %>%
  
  mutate(
    DSSTDTC = as.Date(DSSTDTC),
    DSDTC = as.POSIXct(
      if_else(
        is.na(DSTMCOL) | DSTMCOL == "",
        paste0(COLLECTION_DATE, "T00:00"),
        paste0(COLLECTION_DATE, "T", DSTMCOL)
      ),
      format = "%Y-%m-%dT%H:%M",
      tz = "UTC"
    )
  )

# Derive DSSEQ using sdtm.oak.
ds_domain <- sdtm.oak::derive_seq(
  tgt_dat = ds_prepared,
  tgt_var = "DSSEQ",
  rec_vars = c("USUBJID", "DSSTDTC", "DSDTC"),
  sbj_vars = c("STUDYID", "USUBJID")
)

# Derive DSSTDY using sdtm.oak.
ds_domain <- sdtm.oak::derive_study_day(
  sdtm_in = ds_domain,
  dm_domain = reference_dates,
  tgdt = "DSSTDTC",
  refdt = "RFSTDTC",
  study_day_var = "DSSTDY",
  merge_key = "USUBJID"
)

# Keep the required variables in the requested order.
ds_domain <- ds_domain %>%
  mutate(
    DSSTDTC = format(DSSTDTC, "%Y-%m-%d"),
    DSDTC = if_else(
      format(DSDTC, "%H:%M:%S") == "00:00:00",
      format(DSDTC, "%Y-%m-%d"),
      format(DSDTC, "%Y-%m-%dT%H:%M:%S")
    )
  ) %>%
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD,
    DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
  )

# Validate the output structure and key requirements.
expected_variables <- c(
  "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM",
  "DSDECOD", "DSCAT", "VISITNUM", "VISIT", "DSDTC",
  "DSSTDTC", "DSSTDY"
)

stopifnot(identical(names(ds_domain), expected_variables))
stopifnot(nrow(ds_domain) == nrow(ds_raw))
stopifnot(all(ds_domain$DOMAIN == "DS"))
stopifnot(!any(is.na(ds_domain$STUDYID)))
stopifnot(!any(is.na(ds_domain$USUBJID)))
stopifnot(!any(is.na(ds_domain$DSSEQ)))
stopifnot(!any(is.na(ds_domain$VISITNUM)))

# Export the dataset and a simple execution log.
write_csv(ds_domain, "question_1_sdtm/ds_domain.csv")

writeLines(
  c(
    "DS domain creation completed.",
    paste("Number of records:", nrow(ds_domain)),
    paste("Number of subjects:", n_distinct(ds_domain$USUBJID)),
    paste("Missing DSSTDY values:", sum(is.na(ds_domain$DSSTDY))),
    "Validation checks passed."
  ),
  "question_1_sdtm/ds_domain_log.txt"
)

# Display the resulting dataset.
print(ds_domain)
