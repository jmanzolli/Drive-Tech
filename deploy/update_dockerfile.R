df <- readLines("deploy/Dockerfile")

new_lines <- c(
    "RUN R -e 'pak::pak(\"rstudio/reticulate@v1.38.0\")'",
    "RUN R -e 'reticulate::install_python(\"3.8\")'",
    "RUN R -e \"reticulate::virtualenv_create('drive_tech', python = '3.8')\"",
    "RUN R -e \"\\",
    "reticulate::use_virtualenv('drive_tech', required = TRUE); \\",
    "reticulate::py_install(c('pandas', 'numpy==1.24.4', 'pyomo', 'openpyxl', 'matplotlib', 'torch', 'xgboost', 'scikit-learn'))\""
)

index <- stringr::str_which(df, "WORKDIR /build_zone")

df_new <- c(
    df[1:index],
    new_lines,
    df[(index+1):length(df)]
)

write(df_new, "deploy/Dockerfile")
