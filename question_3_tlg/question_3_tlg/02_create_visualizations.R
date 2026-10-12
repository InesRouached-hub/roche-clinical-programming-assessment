
# Question 3 - Adverse Events Visualizations

library(dplyr)
library(ggplot2)
library(tidyr)

# Define output files
output_dir <- "/cloud/project/question_3_tlg"
plot1_file <- file.path(output_dir, "ae_severity_by_treatment.png")
plot2_file <- file.path(output_dir, "top10_aes_95ci.png")
output_log <- file.path(output_dir, "visualizations_log.txt")

# Load source datasets
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Define the treatment analysis population
analysis_subjects <- adsl %>%
  filter(!is.na(ACTARM), ACTARM != "Screen Failure") %>%
  distinct(USUBJID, ACTARM)

# Keep treatment-emergent adverse events from the analysis population
teae <- adae %>%
  filter(TRTEMFL == "Y", !is.na(AETERM)) %>%
  semi_join(analysis_subjects, by = "USUBJID") %>%
  mutate(
    ACTARM = factor(
      ACTARM,
      levels = c(
        "Placebo",
        "Xanomeline Low Dose",
        "Xanomeline High Dose"
      )
    )
  )

# -----------------------------------------------
# Plot 1: AE severity distribution by treatment
# -----------------------------------------------

severity_data <- teae %>%
  filter(!is.na(AESEV)) %>%
  distinct(USUBJID, ACTARM, AETERM, AESEV) %>%
  count(ACTARM, AESEV, name = "Event count")

plot1 <- ggplot(
  severity_data,
  aes(x = ACTARM, y = `Event count`, fill = AESEV)
) +
  geom_col(position = "stack") +
  labs(
    title = "Treatment-Emergent Adverse Events by Severity",
    x = "Treatment Group",
    y = "Number of Adverse Events",
    fill = "Severity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

ggsave(
  filename = plot1_file,
  plot = plot1,
  width = 9,
  height = 6,
  dpi = 300
)

# -----------------------------------------------
# Plot 2: Top 10 AEs with 95% confidence intervals
# -----------------------------------------------

# Calculate the number of subjects in each treatment group
denominators <- analysis_subjects %>%
  count(ACTARM, name = "N")

# Count participants with each AE term in each treatment group
ae_subject_counts <- teae %>%
  distinct(USUBJID, ACTARM, AETERM) %>%
  count(AETERM, ACTARM, name = "n") %>%
  left_join(denominators, by = "ACTARM") %>%
  mutate(
    incidence = n / N,
    # Wilson score confidence interval for a binomial proportion
    z = qnorm(0.975),
    denominator = 1 + z^2 / N,
    center = (incidence + z^2 / (2 * N)) / denominator,
    half_width = z * sqrt(
      incidence * (1 - incidence) / N +
        z^2 / (4 * N^2)
    ) / denominator,
    lower = pmax(0, center - half_width),
    upper = pmin(1, center + half_width)
  )

# Select the ten most frequent AE terms overall
top10_terms <- ae_subject_counts %>%
  group_by(AETERM) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  arrange(desc(total_n), AETERM) %>%
  slice_head(n = 10) %>%
  pull(AETERM)

top10_data <- ae_subject_counts %>%
  filter(AETERM %in% top10_terms) %>%
  mutate(
    AETERM = factor(
      AETERM,
      levels = rev(top10_terms)
    )
  )

plot2 <- ggplot(
  top10_data,
  aes(
    x = incidence * 100,
    y = AETERM,
    color = ACTARM
  )
) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 2.5
  ) +
  geom_errorbar(
    aes(
      xmin = lower * 100,
      xmax = upper * 100
    ),
    position = position_dodge(width = 0.6),
    width = 0.2,
    orientation = "y"
  ) +
  labs(
    title = "Top 10 Treatment-Emergent Adverse Events",
    subtitle = "Incidence (%) with 95% Wilson confidence intervals",
    x = "Participants with Event (%)",
    y = "Adverse Event",
    color = "Treatment Group"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 9)
  )

ggsave(
  filename = plot2_file,
  plot = plot2,
  width = 11,
  height = 7,
  dpi = 300
)

# Validate output files
stopifnot(file.exists(plot1_file))
stopifnot(file.exists(plot2_file))

# Write execution log
writeLines(
  c(
    "Adverse event visualizations created successfully.",
    paste("Finished:", Sys.time()),
    paste("TEAE records:", nrow(teae)),
    paste("Severity plot:", plot1_file),
    paste("Top 10 AE plot:", plot2_file),
    "Validation checks passed."
  ),
  output_log
)

cat("Visualizations created successfully.\n")
cat("Plot 1:", plot1_file, "\n")
cat("Plot 2:", plot2_file, "\n")
cat("Log:", output_log, "\n")
