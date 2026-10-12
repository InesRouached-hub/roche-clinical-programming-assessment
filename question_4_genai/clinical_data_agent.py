
"""
Clinical Trial Data Assistant
Translates natural-language adverse-event questions into structured filters.
"""

import json
import re
from pathlib import Path

import pandas as pd


# Locate the input dataset relative to this script.
BASE_DIR = Path(__file__).resolve().parent
DATA_FILE = BASE_DIR / "adae.csv"

# Dataset schema provided to the language-model prompt.
SCHEMA = {
    "USUBJID": "Unique subject identifier.",
    "AETERM": "Reported adverse event term, e.g. Headache.",
    "AESEV": "Severity or intensity of the adverse event, e.g. MILD, MODERATE, SEVERE.",
    "AESOC": "System Organ Class associated with the adverse event, e.g. Cardiac disorders or Skin and subcutaneous tissue disorders.",
    "AESER": "Serious adverse event indicator.",
    "AEREL": "Relationship of the event to the study treatment.",
    "AEOUT": "Outcome of the adverse event.",
}

ALLOWED_COLUMNS = {"AETERM", "AESEV", "AESOC", "AESER", "AEREL", "AEOUT"}


class ClinicalTrialDataAgent:
    """Interpret clinical questions and return matching subject IDs."""

    def __init__(self, data_path=DATA_FILE):
        self.data = pd.read_csv(data_path, dtype=str).fillna("")

        required = {"USUBJID", *ALLOWED_COLUMNS}
        missing = required.difference(self.data.columns)
        if missing:
            raise ValueError(f"Dataset is missing required columns: {sorted(missing)}")

        # Build a schema description for the prompt.
        self.schema_description = json.dumps(SCHEMA, indent=2)

    def build_prompt(self, question):
        """Build the prompt that would be sent to an LLM."""
        return f"""
You are a clinical data assistant.
Read the dataset schema:
{self.schema_description}

Convert the user's question into JSON with exactly these keys:
- target_column: one of {sorted(ALLOWED_COLUMNS)}
- filter_value: the value to search for, as written or implied in the question.

Return JSON only. Do not provide SQL or Python code.
User question: {question}
"""

    def parse_question(self, question):
        """
        Local mock of an LLM response.

        This deterministic parser keeps the required Prompt -> Parse -> Execute
        flow testable without an API key. Replace this method with an LLM call
        when credentials and network access are available.
        """
        prompt = self.build_prompt(question)
        question_lower = question.lower()

        # Example mock responses for the three main clinical concepts.
        if any(word in question_lower for word in ("severity", "severe", "moderate", "mild", "intensity")):
            column = "AESEV"
            value_map = {
                "mild": "MILD",
                "moderate": "MODERATE",
                "severe": "SEVERE",
            }
            value = next(
                (mapped for word, mapped in value_map.items() if word in question_lower),
                None,
            )

        elif any(word in question_lower for word in ("body system", "organ system", "cardiac", "skin")):
            column = "AESOC"
            value = None
            # Extract a specific system name if present.
            for candidate in self.data["AESOC"].dropna().unique():
                if candidate and candidate.lower() in question_lower:
                    value = candidate
                    break
            if value is None:
                value = self._extract_mentioned_value(question, "AESOC")

        else:
            column = "AETERM"
            value = self._extract_mentioned_value(question, "AETERM")

        if not value:
            raise ValueError(
                "Could not identify a filter value. Try naming a severity, "
                "adverse event term, or system organ class found in the dataset."
            )

        # Mock structured JSON output, as if returned by an LLM.
        mock_llm_response = json.dumps(
            {"target_column": column, "filter_value": value}
        )
        parsed = json.loads(mock_llm_response)

        if parsed["target_column"] not in ALLOWED_COLUMNS:
            raise ValueError("The parsed target column is not allowed.")

        return parsed

    def _extract_mentioned_value(self, question, column):
        """Find a dataset value mentioned in the question, ignoring case."""
        question_lower = question.lower()
        values = sorted(
            {
                str(value).strip()
                for value in self.data[column].unique()
                if str(value).strip()
            },
            key=len,
            reverse=True,
        )

        for value in values:
            if value.lower() in question_lower:
                return value

        # Allow a simple quoted value such as: events matching "Headache".
        quoted = re.search(r"""['"]([^'"]+)['"]""", question)
        if quoted:
            return quoted.group(1).strip()

        return None

    def execute_query(self, structured_query):
        """Filter the dataframe and return unique matching subject IDs."""
        column = structured_query["target_column"]
        value = structured_query["filter_value"]

        if column not in ALLOWED_COLUMNS:
            raise ValueError(f"Filtering by {column!r} is not allowed.")

        matches = self.data[
            self.data[column].str.casefold() == str(value).casefold()
        ]

        subject_ids = sorted(matches["USUBJID"].dropna().unique().tolist())

        return {
            "target_column": column,
            "filter_value": value,
            "unique_subject_count": len(subject_ids),
            "subject_ids": subject_ids,
        }

    def ask(self, question):
        """Run the complete Prompt -> Parse -> Execute flow."""
        prompt = self.build_prompt(question)
        structured_query = self.parse_question(question)
        result = self.execute_query(structured_query)

        return {
            "question": question,
            "prompt": prompt,
            "structured_query": structured_query,
            "result": result,
        }


if __name__ == "__main__":
    agent = ClinicalTrialDataAgent()
    question = "Give me the subjects who had adverse events of Moderate severity."
    response = agent.ask(question)

    print("Question:", response["question"])
    print("Structured JSON:", json.dumps(response["structured_query"], indent=2))
    print("Unique subjects:", response["result"]["unique_subject_count"])
    print("Subject IDs:", response["result"]["subject_ids"])
