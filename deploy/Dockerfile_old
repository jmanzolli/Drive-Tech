FROM rocker/shiny-verse:4.4

COPY .Renviron_github_pat root/.Renviron

RUN apt-get -y update && apt-get install -y  gdal-bin imagemagick libcurl4-openssl-dev libgdal-dev libgeos-dev libgeos++-dev libicu-dev libpng-dev libproj-dev libsasl2-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN apt-get -y update && apt-get install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev libprotobuf-dev libmagick++-6.q16-dev protobuf-compiler libjq-dev libsodium-dev libsodium-dev libtesseract-dev libleptonica-dev libpoppler-cpp-dev
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 1)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("pak")'

RUN apt update
RUN apt install python3 -y
RUN apt install python3-pip -y
RUN apt install python3-venv -y

RUN R -e "pak::pak('bsicons@0.1.2')"
RUN R -e "pak::pak('bslib@0.8.0')"
RUN R -e "pak::pak('echarts4r@0.4.5')"
RUN R -e "pak::pak('formattable@0.2.1')"
RUN R -e "pak::pak('golem@0.5.1')"
RUN R -e "pak::pak('gt@0.11.0')"
RUN R -e "pak::pak('gtExtras@0.5.0')"
RUN R -e "pak::pak('leaflet@2.2.2')"
RUN R -e "pak::pak('leaflet.extras@2.0.1')"
RUN R -e "pak::pak('lubridate@1.9.3')"
RUN R -e "pak::pak('openxlsx@4.2.7.1')"
RUN R -e "pak::pak('plotly@4.10.4')"
RUN R -e "pak::pak('RColorBrewer@1.1.3')"
RUN R -e "pak::pak('reactable@0.4.4')"
RUN R -e "pak::pak('readxl@1.4.3')"
RUN R -e "pak::pak('reticulate@1.38.0')"
RUN R -e "pak::pak('scales@1.3.0')"
RUN R -e "pak::pak('sf@1.0.17')"
RUN R -e "pak::pak('shinipsum@0.1.1')"
RUN R -e "pak::pak('shiny@1.9.1')"
RUN R -e "pak::pak('shinyalert@3.1.0')"
RUN R -e "pak::pak('shinycssloaders@1.1.0')"
RUN R -e "pak::pak('shinydashboard@0.7.2')"
RUN R -e "pak::pak('shinyjs@2.1.0')"
RUN R -e "pak::pak('shinyWidgets@0.8.7')"
RUN R -e "pak::pak('waiter@0.2.5')"

RUN apt update
RUN apt install git -y

RUN R -e 'pak::pak("rstudio/reticulate@v1.38.0")'
RUN R -e 'reticulate::install_python("3.8")'
RUN R -e "reticulate::virtualenv_create('drive_tech', python = '3.8')"

RUN R -e "\
    reticulate::use_virtualenv('drive_tech', required = TRUE); \
    reticulate::py_install(c('pandas', 'numpy==1.24.4', 'pyomo', 'openpyxl', 'matplotlib', 'torch', 'xgboost', 'scikit-learn'))"

# # ================================================================
# #                            GUROBI 
# # ================================================================
# RUN curl https://packages.gurobi.com/11.0/gurobi11.0.3_linux64.tar.gz
# RUN mv gurobi11.0.3_linux64.tar.gz /opt
# RUN cd /opt
# RUN tar -xzv gurobi11.0.3_linux64.tar.gz

# RUN export GUROBI_HOME=/opt/1103/linux64
# RUN export PATH=$GUROBI_HOME/bin:$PATH
# RUN export LD_LIBRARY_PATH=$GUROBI_HOME/lib:$LD_LIBRARY_PATH

WORKDIR /
RUN mkdir /build_zone
WORKDIR /build_zone

COPY inst/ /build_zone/inst
COPY man/ /build_zone/man
COPY R/ /build_zone/R
COPY .env /build_zone/.env
COPY DESCRIPTION /build_zone/DESCRIPTION
COPY NAMESPACE /build_zone/NAMESPACE
COPY .Rbuildignore /build_zone/.Rbuildignore
COPY result.rds /build_zone/result.rds

RUN R -e 'devtools::install(dependencies = FALSE)'

EXPOSE 3838
CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0'); driveTech::run_app()"]