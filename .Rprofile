if (reticulate::virtualenv_exists("drive_tech")) {
	reticulate::use_virtualenv("drive_tech")
} else {
	# reticulate::install_python("3.8")
	# reticulate::virtualenv_remove("drive_tech")
	reticulate::virtualenv_create("drive_tech")
	reticulate::use_virtualenv("drive_tech")
	purrr::map(readLines("requirements.txt"), reticulate::py_install)
}

source("~/.Rprofile")