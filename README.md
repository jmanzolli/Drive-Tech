## App structure

The app was built using golem framework, e.g, a R package. All the app code is inside the `R/` folder, and the machine learning models and optimization models are in `inst/python/`.


## Emissions Estimator

WRITE THE PURPOSE

- **Metrics** there are example data in `inst/data/geoh/`

- **Predictions (FCR) & Predictions (CO2)** there are example data in `inst/data/geoh/test_data/`


## Driving Electrification 

WRITE THE PURPOSE

- **Data input** 
    - **Excel format**:  there are example data in `inst/data/drive_tech/v2/input.xslx`. For the xlsx format its important that the data follows the example standard as the example data. 
    - **Manual format**: its filling in the application and the example data its in `inst/data/drive_tech/v2/manual_input.json`
- **Data Summary**: show the uploaded / inputed data.
- **Optimizer**: shows the results after the optimizer and the option to export them.

Here's a more detailed step-by-step guide to complete the installation process:

## Getting Started

### 1. Install the App

Open a new R session and type:

```{r}
devtools::install()
```

### 2. Setting up the Python Environment

1. Open a new R session and type:

   ```{r}
   reticulate::virtualenv_create("drive_tech", version = "3.8")
   reticulate::use_virtualenv("drive_tech")
   ```

   > **Note**: If you encounter errors at this step (e.g., missing libraries), you may need to install them separately.

2. Next, install the required Python packages listed in `requirements.txt`:

   ```{r}
   purrr::map(readLines("requirements.txt"), reticulate::py_install)
   ```

3. Verify that the environment is set up correctly by typing:

   ```{r}
   reticulate::py_config()
   ```

   This should display the Python environment information, confirming that `drive_tech` is active.

### 3. Install Gurobi

Gurobi is necessary for optimization functionalities. Follow the video links to install Gurobi based on your operating system:

- [Linux Installation](https://www.youtube.com/watch?v=OYuOKXPJ5PI)
- [Windows Installation](https://www.youtube.com/watch?v=z7t0p5J9YcQ)
- [Mac Installation](https://www.youtube.com/watch?v=dcFstZl5Va4)

> **Note**: After installing, ensure Gurobi is accessible in your path or environment variables and activated with a license if required. You may need to configure it within R using `gurobi::gurobi_setup()` or similar commands based on your setup.