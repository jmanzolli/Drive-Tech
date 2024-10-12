# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
# pkgs <- attachment::att_amend_desc()
pkgs <- attachment::att_from_rscripts()
pkgs <- pkgs[!(pkgs %in% c("tools", "tibble", "purrr", "dplyr", "stringr", "readr", "ggplot2", "forcats", "tidyr", "config"))]
pkgs <- c(pkgs, "plotly", "shinipsum", "leaflet.extras", "gtExtras")
pkgs <- unique(pkgs)
purrr::map(pkgs, ~ usethis::use_package(.x, min_version = TRUE))

# Dockerfile
deps <- desc::desc_get_deps(file = 'DESCRIPTION');
for (i in 1:nrow(deps)) { 
    package <- deps$package[i]
    version <- deps$version[i]
    version <- stringr::str_extract(version, "[0-9.]+")
    cat("RUN R -e \"pak::pak('", package, "@", version, "')\"\n", sep="")
}

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "loading") # Name of the module
golem::add_module(name = "describe") # Name of the module
golem::add_module(name = "optimize") # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
# golem::add_fct("helpers", with_test = TRUE)
# golem::add_utils("helpers", with_test = TRUE)

## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file("script")
# golem::add_js_handler("handlers")
# golem::add_css_file("custom")
# golem::add_sass_file("custom")

# ## Add internal datasets ----
# ## If you have data in your package
# usethis::use_data_raw(name = "my_dataset", open = FALSE)
