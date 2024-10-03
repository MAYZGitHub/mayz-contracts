import os
import subprocess
import sys
import configparser

def get_script_dir():
    return os.path.dirname(os.path.abspath(__file__))

def read_config():
    config = configparser.ConfigParser()
    config_path = os.path.join(get_script_dir(), 'project.ini')
    if os.path.exists(config_path):
        config.read(config_path)
    else:
        print(f"Warning: {config_path} not found. Using default settings.")
        config['environment'] = {'venv_name': 'script_env'}
    return config

def user_input(prompt, default=None):
    if default:
        response = input(f"{prompt} [{default}]: ").strip() or default
    else:
        response = input(f"{prompt}: ").strip()
    return response

def create_venv(venv_name, env_type, location):
    script_dir = get_script_dir()
    
    if location == 'project':
        venv_path = os.path.join(script_dir, venv_name)
    elif location == 'default':
        if env_type == 'conda':
            venv_path = venv_name  # Conda will use its default location
        else:
            venv_path = os.path.join(os.path.expanduser('~'), '.venvs', venv_name)
    else:  # custom path
        venv_path = os.path.join(location, venv_name)
    
    if os.path.exists(venv_path):
        action = user_input(f"Environment {venv_path} already exists. Overwrite (o), Use existing (u), or Cancel (c)?", "u")
        if action.lower() == 'o':
            if env_type == 'conda':
                subprocess.run(['conda', 'env', 'remove', '-p' if os.path.isabs(venv_path) else '-n', venv_path], check=True)
            else:
                import shutil
                shutil.rmtree(venv_path)
        elif action.lower() == 'c':
            print("Setup cancelled.")
            sys.exit(0)
        else:
            print(f"Using existing environment: {venv_path}")
            return venv_path

    if not os.path.exists(venv_path):
        print(f"Creating {env_type} environment: {venv_path}")
        if env_type == 'conda':
            subprocess.run(['conda', 'create', '-y', '-n' if location == 'default' else '-p', venv_path, 'python=3.8'], check=True)
        else:
            subprocess.run([sys.executable, '-m', 'venv', venv_path], check=True)
    
    return venv_path

def install_requirements(venv_path, env_type):
    script_dir = get_script_dir()
    requirements_path = os.path.join(script_dir, 'requirements.txt')
    
    if os.path.exists(requirements_path):
        print("Installing requirements...")
        if env_type == 'conda':
            subprocess.run(['conda', 'run', '-p' if os.path.isabs(venv_path) else '-n', venv_path, 'pip', 'install', '-r', requirements_path], check=True)
        else:
            pip_path = os.path.join(venv_path, 'bin', 'pip') if os.name != 'nt' else os.path.join(venv_path, 'Scripts', 'pip')
            subprocess.run([pip_path, 'install', '-r', requirements_path], check=True)
    else:
        print(f"Warning: {requirements_path} not found. Skipping package installation.")

def activate_environment(venv_path, env_type):
    if env_type == 'conda':
        activate_cmd = f"conda activate {venv_path}"
    else:
        if os.name != 'nt':
            activate_cmd = f"source {os.path.join(venv_path, 'bin', 'activate')}"
        else:
            activate_cmd = f"{os.path.join(venv_path, 'Scripts', 'activate')}"
    
    print(f"\nTo activate the environment, run the following command:")
    print(activate_cmd)
    
    activate = user_input("Do you want to activate the environment now? (y/n)", "y")
    if activate.lower() == 'y':
        if env_type == 'conda':
            # For conda, we need to modify the current process
            os.environ["CONDA_PREFIX"] = venv_path
            os.environ["CONDA_DEFAULT_ENV"] = os.path.basename(venv_path)
            print(f"Conda environment {venv_path} partially activated.")
            print(f"To fully activate, please run: conda activate {venv_path}")
        else:
            # For venv, we can't modify the current process, so we'll start a new shell
            if os.name != 'nt':
                os.execle(os.environ['SHELL'], os.environ['SHELL'], '-c', f'source "{os.path.join(venv_path, "bin", "activate")}" && exec "$SHELL"', os.environ)
            else:
                os.system(f'start cmd /K {os.path.join(venv_path, "Scripts", "activate.bat")}')

def main():
    config = read_config()
    default_venv_name = config['environment'].get('venv_name', 'script_env')

    print("Welcome to the interactive environment setup script!")
    
    # Check if we're already in a virtual environment
    if sys.prefix != sys.base_prefix or 'CONDA_DEFAULT_ENV' in os.environ:
        print("Warning: You are already in a virtual environment.")
        continue_setup = user_input("Do you want to create/switch to a different environment? (y/n)", "n")
        if continue_setup.lower() != 'y':
            print("Setup cancelled. Exiting.")
            return

    # Determine if Conda is available
    conda_available = 'CONDA_DEFAULT_ENV' in os.environ or subprocess.run(['conda', '--version'], capture_output=True, text=True).returncode == 0
    
    if conda_available:
        env_type = user_input("Conda detected. Do you prefer to use Conda or venv? (conda/venv)", "conda")
    else:
        env_type = "venv"
        print("Conda not detected. Using venv for environment creation.")
    
    venv_name = user_input(f"Enter the name for your virtual environment", default_venv_name)
    
    location_options = "project (p), default (d), or custom (c)"
    location_choice = user_input(f"Where would you like to save the environment? {location_options}", "p")
    
    if location_choice.lower() == 'p':
        location = 'project'
    elif location_choice.lower() == 'd':
        location = 'default'
    else:
        location = user_input("Enter the custom path for the environment")
    
    print(f"\nCreating/Using {env_type} environment '{venv_name}' in {location} location...")
    venv_path = create_venv(venv_name, env_type, location)
    
    install_requirements(venv_path, env_type)
    
    activate_environment(venv_path, env_type)

    print("\nSetup complete!")

if __name__ == "__main__":
    main()