library(reticulate)

# Set Python environment for reticulate
use_virtualenv("drive_tech", required = TRUE)

# Check installed Python libraries
py_run_string("import pkg_resources")
py_run_string("import subprocess")

# Update all libraries
py_run_string("
outdated_packages = [dist.project_name for dist in pkg_resources.working_set]
for package in outdated_packages:
    subprocess.run(['/env/bin/python', '-m', 'pip', 'install', '--upgrade', package])
")