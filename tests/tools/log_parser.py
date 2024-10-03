import re
import os
import sys
import configparser
from openpyxl import Workbook

def get_script_dir():
    return os.path.dirname(os.path.abspath(__file__))

def read_config():
    config = configparser.ConfigParser()
    config_path = os.path.join(get_script_dir(), 'project.ini')
    if os.path.exists(config_path):
        config.read(config_path)
    else:
        print(f"Warning: {config_path} not found. Using default settings.")
        config['environment'] = {'venv_name': 'logparser_env'}
        config['files'] = {'input_log': 'test_log.txt', 'output_excel': 'test_results.xlsx'}
    return config

def check_venv(venv_name):
    venv_path = os.path.join(get_script_dir(), venv_name)
    if not sys.prefix.startswith(venv_path):
        print(f"Warning: It seems you're not running this script in the '{venv_name}' virtual environment.")
        print("Please activate the virtual environment. Refer to the README.md for instructions.")
        sys.exit(1)

def parse_log_file(file_path):
    with open(file_path, 'r') as file:
        content = file.read()

def parse_log_file(file_path):
    with open(file_path, 'r') as file:
        content = file.read()

    # Improved pattern to match various Found and ROUTE sections
    pattern = r'Found: (\[.*?\])\s*<\s*SOURCE:.*?\s*ROUTE: ([^\n]+)'
    matches = re.findall(pattern, content, re.DOTALL)

    results = []
    for found, route in matches:
        route_parts = [part.strip() for part in route.split(',')]
        
        # Process errors
        errors = eval(found) if found != '[]' else []
        error_str = '; '.join(errors) if errors else 'No error found'

        results.append([
            'FALSE',  # Assuming all entries with "Found:" are failures
            route_parts[0],  # TX_NAME
            route_parts[1],  # Policy
            route_parts[2],  # REDEEMER
            *route_parts[3:],  # TestType and TestSubType
            'TestFailure',
            error_str,
            'SelfRedeemer'
        ])

    return results

def write_to_excel(data, output_file):
    wb = Workbook()
    ws = wb.active
    ws.append(['USE', 'TX NAME', 'SCRIPT', 'REDEEMER', 'TYPE', 'PATH', 'OUTCOME',  'MESSAGE', 'SOURCE'])
    for row in data:
        ws.append(row)

    wb.save(output_file)

def main():
    config = read_config()
    venv_name = config['environment']['venv_name']
    input_file = config['files']['input_log']
    output_file = config['files']['output_excel']

    check_venv(venv_name)

    script_dir = get_script_dir()
    input_file_path = os.path.join(script_dir, input_file)
    output_file_path = os.path.join(script_dir, output_file)

    if not os.path.exists(input_file_path):
        print(f"Error: Input file '{input_file_path}' not found.")
        return

    data = parse_log_file(input_file_path)
    write_to_excel(data, output_file_path)
    print(f"Results written to '{output_file_path}'")

if __name__ == "__main__":
    main()