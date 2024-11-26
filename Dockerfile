# Start with a base image
FROM ubuntu:latest

# Install R, Python, and other necessary dependencies
RUN apt-get update && apt-get install -y \
    r-base \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    python3 \
    python3-pip \
    python3-venv \
    && apt-get clean

# Install devtools and base dependencies
RUN R -e "install.packages(c('devtools', 'reticulate', 'golem', 'purrr'), repos = 'http://cran.rstudio.com/')"

# Copy the project files
COPY . /app
WORKDIR /app

# Pre-install all dependencies from DESCRIPTION
RUN R -e "devtools::install_deps('/app', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"

# Set the default command to run the app
CMD ["Rscript", "--vanilla", "-e", "pkgload::load_all('/app', export_all = FALSE, helpers = FALSE, attach_testthat = FALSE); options('golem.app.prod' = TRUE); driveTech::run_app()"]
