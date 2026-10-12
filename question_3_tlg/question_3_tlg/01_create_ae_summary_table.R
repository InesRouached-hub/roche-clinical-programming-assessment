
# Question 3 - TEAE Summary Table using gtsummary

library(dplyr)
library(tidyr)
library(gtsummary)

# Define output files
output_dir <- "/cloud/project/question_3_tlg"
output_html <- file.path(output_dir, "ae_summary_table.html")
output_log <- file.path(output_dir, "ae_summary_table_log.txt")

# Load source datasets
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Define the analysis population and treatment denominators
analysis_subjects <- adsl %>%
  filter(!is.na(ACTARM), ACTARM != "Screen Failure") %>%
  distinct(USUBJID, ACTARM)

treatment_denominators <- analysis_subjects %>%
  count(ACTARM, name = "N")

# Keep treatment-emergent adverse events
teae <- adae %>%
  filter(
    TRTEMFL == "Y",
    !is.na(AETERM)
  ) %>%
  semi_join(analysis_subjects, by = "USUBJID")

# Count each event once per participant
event_totals <- teae %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, name = "Total_n")

# Create an event-to-variable mapping, sorted by overall frequency
event_map <- event_totals %>%
  arrange(desc(Total_n), AETERM) %>%
  mutate(
    var_name = make.unique(
      paste0("AE_", make.names(AETERM))
    )
  )

event_vars <- event_map$var_name

# Create participant-level indicators for each event term
ae_presence <- teae %>%
  distinct(USUBJID, AETERM) %>%
  left_join(
    event_map %>% select(AETERM, var_name),
    by = "AETERM"
  ) %>%
  mutate(HAS_AE = "Yes") %>%
  select(USUBJID, var_name, HAS_AE) %>%
  pivot_wider(
    names_from = var_name,
    values_from = HAS_AE,
    values_fill = "No"
  )

# Include all participants in the treatment groups
table_data <- analysis_subjects %>%
  left_join(ae_presence, by = "USUBJID") %>%
  mutate(
    across(
      all_of(event_vars),
      ~ replace_na(.x, "No")
    )
  )

# Create the summary table using gtsummary
ae_table <- table_data %>%
  tbl_summary(
    by = ACTARM,
    include = all_of(event_vars),
    type = all_dichotomous() ~ "dichotomous",
    value = all_dichotomous() ~ "Yes",
    statistic = all_dichotomous() ~ "{n} ({p}%)",
    digits = all_dichotomous() ~ c(0, 1),
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        .sort_order = match(variable, event_map$var_name),
        label = if_else(
          variable %in% event_map$var_name,
          event_map$AETERM[
            match(variable, event_map$var_name)
          ],
          label
        )
      ) %>%
      arrange(.sort_order) %>%
      select(-.sort_order)
  ) %>%
  modify_header(
    label = "**Adverse event**",
    all_stat_cols() ~ "**{level}**  \nN = {n}"
  ) %>%
  modify_caption(
    "**Treatment-Emergent Adverse Events**"
  ) %>%
  bold_labels()

# Export the gtsummary table as HTML
gt_table <- as_gt(ae_table)

gt::gtsave(
  data = gt_table,
  filename = output_html
)

# Validate the outputs
stopifnot(file.exists(output_html))
stopifnot(nrow(table_data) == nrow(analysis_subjects))
stopifnot(length(event_vars) > 0)

# Write execution log
writeLines(
  c(
    "TEAE summary table created successfully using gtsummary.",
    paste("Finished:", Sys.time()),
    paste("Participants:", nrow(analysis_subjects)),
    paste("Unique TEAE terms:", length(event_vars)),
    paste("TEAE source records:", nrow(teae)),
    paste("HTML output:", output_html),
    "Validation checks passed."
  ),
  output_log
)

cat("gtsummary table created successfully.\n")
cat("Participants:", nrow(analysis_subjects), "\n")
cat("Unique TEAE terms:", length(event_vars), "\n")
cat("HTML:", output_html, "\n")
cat("Log:", output_log, "\n")
