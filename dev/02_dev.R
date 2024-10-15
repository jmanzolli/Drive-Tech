attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "drive_loading", utils = c("crud"), open = FALSE) # Name of the module
golem::add_module(name = "drive_describe", open = FALSE) # Name of the module
golem::add_module(name = "drive_optimize", open = FALSE) # Name of the module

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

#!
#!   DOCKERFILE
#!
golem::add_dockerfile_shinyproxy(output = "deploy/Dockerfile", open=FALSE)

# pkgs <- attachment::att_amend_desc()
# pkgs <- attachment::att_from_rscripts()
# pkgs <- pkgs[!(pkgs %in% c("tools", "tibble", "purrr", "dplyr", "stringr", "readr", "ggplot2", "forcats", "tidyr", "config"))]
# pkgs <- c(pkgs, "plotly", "shinipsum", "leaflet.extras", "gtExtras")
# pkgs <- unique(pkgs)
# purrr::map(pkgs, ~ usethis::use_package(.x, min_version = TRUE))
# deps <- desc::desc_get_deps(file = 'DESCRIPTION');
# for (i in 1:nrow(deps)) { 
#     package <- deps$package[i]
#     version <- deps$version[i]
#     version <- stringr::str_extract(version, "[0-9.]+")
#     cat("RUN R -e \"pak::pak('", package, "@", version, "')\"\n", sep="")
# }
