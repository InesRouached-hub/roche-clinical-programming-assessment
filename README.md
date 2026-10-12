# Clinical Programming Assessment

This repository contains my solutions to four clinical programming assessment exercises, covering SDTM data preparation, ADaM dataset creation, adverse event reporting and visualization, and a Python-based clinical data agent.

## Project Structure

### Question 1 — SDTM
- Creation of the DS domain dataset.
- Export of the resulting dataset to CSV.
- Execution log for traceability.

### Question 2 — ADaM
- Creation of the ADSL subject-level analysis dataset.
- Validation of key variables and subject-level records.
- Export of the dataset and execution log.

### Question 3 — Tables, Listings and Graphs (TLGs)
- Summary of treatment-emergent adverse events (TEAEs).
- Visualization of adverse event severity by treatment.
- Visualization of the ten most frequent adverse events with 95% confidence intervals.
- HTML report and execution logs.

### Question 4 — Clinical Data Agent
- A Python-based agent using pandas to query clinical adverse event data through natural-language questions.
- A mock query parser that maps questions to dataset filters.
- Test cases demonstrating subject-level query results.

## Technologies Used

- **R** for clinical dataset preparation, validation, reporting and visualization.
- **Python** and **pandas** for clinical data querying.
- **HTML** and **PNG** outputs for reporting and visualization.

## How to Explore the Repository

Each question has its own folder containing the relevant source code, outputs and logs.

- question_1_sdtm/
- question_2_adam/
- question_3_tlg/
- question_4_genai/

The R scripts can be run in an R environment with the required packages installed. The Python agent and its tests require Python and pandas.

## Notes

The Python clinical data agent uses a deterministic mock parser rather than a live large language model API. It demonstrates the query workflow without requiring an API key.
