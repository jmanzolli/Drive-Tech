FROM rocker/verse:4.4.1
RUN apt-get update && apt-get install -y  gdal-bin libcurl4-openssl-dev libgdal-dev libgeos-dev libicu-dev libnode-dev libpng-dev libproj-dev libsqlite3-dev libssl-dev libudunits2-dev libxml2-dev make pandoc python3 zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.8.0")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.2.1")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.1.4")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-3")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.5.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.9.1")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.4")'
RUN Rscript -e 'remotes::install_version("reactable",upgrade="never", version = "0.4.4")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.2.5")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.8.7")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.1.0")'
RUN Rscript -e 'remotes::install_version("shinyalert",upgrade="never", version = "3.1.0")'
RUN Rscript -e 'remotes::install_version("shinipsum",upgrade="never", version = "0.1.1")'
RUN Rscript -e 'remotes::install_version("sf",upgrade="never", version = "1.0-17")'
RUN Rscript -e 'remotes::install_version("readxl",upgrade="never", version = "1.4.3")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.5")'
RUN Rscript -e 'remotes::install_version("openxlsx",upgrade="never", version = "4.2.7.1")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.9.3")'
RUN Rscript -e 'remotes::install_version("leaflet",upgrade="never", version = "2.2.2")'
RUN Rscript -e 'remotes::install_version("gt",upgrade="never", version = "0.11.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.5.1")'
RUN Rscript -e 'remotes::install_version("formattable",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("echarts4r",upgrade="never", version = "0.4.5")'
RUN Rscript -e 'remotes::install_version("bsicons",upgrade="never", version = "0.1.2")'
RUN Rscript -e 'remotes::install_github("rstudio/reticulate@de29e4230b3ce62366832b7896b4bf21dd944139")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'pak::pak("rstudio/reticulate@v1.38.0")'
RUN R -e 'reticulate::install_python("3.8")'
RUN R -e "reticulate::virtualenv_create('drive_tech', python = '3.8')"
RUN R -e "\
    reticulate::use_virtualenv('drive_tech', required = TRUE); \
    reticulate::py_install(c('pandas', 'numpy==1.24.4', 'pyomo', 'openpyxl', 'matplotlib', 'torch', 'xgboost', 'scikit-learn'))"
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');library(driveTech);driveTech::run_app()"]
