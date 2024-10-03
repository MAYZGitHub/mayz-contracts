# Log Parser and Excel Generator for MAYZ Protocol Automatic Tests

This script parses the log output from MAYZ Protocol's automatic tests and generates an Excel spreadsheet with the extracted information. It's designed to assist in creating the initial rules configuration file for the automatic tests.

## Purpose

The tool automates the process of parsing test logs and creating a structured Excel file that can be used as a starting point for defining test rules. It extracts information about test failures and organizes them in a format compatible with the automatic test suite's configuration.

## Prerequisites

1. Python 3.6 or higher
2. Access to MAYZ Protocol's automatic test suite

## Setup

1. Ensure you have Python 3.6+ installed.
2. Navigate to the script directory:
   ```
   cd tests/tools
   ```
3. Run the setup script:
   ```
   python setup.py
   ```
   This will guide you through creating a virtual environment (conda or venv) and installing required packages.

## Usage

1. Run the MAYZ Protocol automatic tests and capture the output:
   ```
   cabal test AutomaticTests --test-show-details=always > test_log.txt
   ```
   This command runs the automatic tests and redirects the output to a file named `test_log.txt`.

2. Place the `test_log.txt` file in the script directory.

3. Activate the virtual environment as instructed by the setup script.

4. Run the parser:
   ```
   python log_parser.py
   ```

5. Find the generated `test_results.xlsx` in the same directory.

## Output

The script generates an Excel file (`test_results.xlsx` by default) with the following columns:
- USE: Set to 'FALSE' for all entries (users should review and set to 'TRUE' as needed)
- TX NAME: Transaction name
- SCRIPT: The script or policy being tested
- REDEEMER: The redeemer used in the test
- TYPE: Test type
- PATH: Detailed test path
- OUTCOME: Set to 'TestFailure' for parsed errors
- MESSAGE: The error message or 'No error found'
- SOURCE: Set to 'SelfRedeemer' by default

## Configuration

Modify `project.ini` to change environment name or input/output file names. The script uses default values if this file is missing.

## File Structure

```
script_directory/
├── setup.py
├── log_parser.py
├── project.ini (optional)
├── requirements.txt
├── test_log.txt (input from automatic tests)
└── test_results.xlsx (output)
```

## Important Notes

1. This tool is intended to create an initial draft of the rules configuration. Manual review and adjustment of the generated Excel file is crucial.

2. The generated Excel file should be carefully reviewed and updated with correct expectations for each test case.

3. After reviewing and updating the Excel file, it should be saved as `tests/config/tests.xlsx` for use with the automatic test suite.

4. Regularly update the `tests/config/tests.xlsx` file as the protocol evolves to ensure accurate test validations.

5. The parser currently focuses on extracting failure cases. Successful test cases may need to be added manually to the Excel file.

## Workflow

1. Run automatic tests and generate log file
2. Use this parser to create initial Excel file
3. Review and update the Excel file
4. Save the final version as `tests/config/tests.xlsx`
5. Use the updated Excel file with the automatic test suite
6. Repeat this process as needed when significant changes are made to the protocol

By using this tool, you can quickly generate a base configuration for the automatic tests, significantly reducing the manual effort required in setting up test rules and expected outcomes.


## Note

This setup file and process can be reused for other small Python projects. Just update the `requirements.txt` and `project.ini` as needed.