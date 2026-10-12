
"""Test script for the Clinical Trial Data Assistant."""

import json
from clinical_data_agent import ClinicalTrialDataAgent


def main():
    agent = ClinicalTrialDataAgent()

    questions = [
        "Give me the subjects who had adverse events of Moderate severity.",
        "Which subjects had a Headache?",
        "Which subjects had Cardiac disorders?",
    ]

    for number, question in enumerate(questions, start=1):
        print(f"\n{'=' * 70}")
        print(f"TEST {number}: {question}")

        try:
            response = agent.ask(question)

            print(
                "Structured JSON:",
                json.dumps(response["structured_query"], indent=2),
            )
            print(
                "Unique subjects:",
                response["result"]["unique_subject_count"],
            )
            print(
                "Matching subject IDs:",
                response["result"]["subject_ids"],
            )

        except ValueError as error:
            print("Could not process this question:", error)


if __name__ == "__main__":
    main()
