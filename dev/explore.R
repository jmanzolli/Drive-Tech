library(tidyverse)

readxl::read_xlsx("output.xlsx")

readxl::read_xlsx("input.xlsx")

readxl::excel_sheets("output.xlsx")

energy <- readxl::read_xlsx("output.xlsx", sheet = 1)[, -1]
soc <- readxl::read_xlsx("output.xlsx", sheet = 2)[, -1]
power <- readxl::read_xlsx("output.xlsx", sheet = 3)[, -1]
ov <- readxl::read_xlsx("output.xlsx", sheet = 4)[, -1]

best_energy <- tail(energy, 1)
best_soc <- tail(soc, 1)
best_power <- tail(power, 1)

ov


r |> 
    dplyr::summarise_all(sum) |> 
    tidyr::gather(variable, value) |> 
    dplyr::mutate(
        variable = str_replace(variable, "bus|\\s+", ""),
        variable = trimws(variable),
        variable = as.integer(variable)
    ) |> 
    ggplot(aes(x = variable, y = value)) + 
    geom_col()

r |> 
    dplyr::summarise_all(sum) |> 
    tidyr::gather(variable, value) |> 
    dplyr::mutate(
        variable = str_replace(variable, "bus|\\s+", ""),
        variable = trimws(variable),
        variable = as.integer(variable)
    ) |> 
    ggplot(aes(y = value)) + 
    geom_boxplot()


tibble::tibble(
    index = 1:nrow(r),
    value = apply(r, 1, sum)
) |> 
ggplot(aes(x = index, y = value)) +
geom_col()

tibble::tibble(
    index = 1:nrow(r),
    value = apply(r, 1, sum)
) |> 
ggplot(aes(x = index, y = value)) +
geom_point() +
geom_line()



#!
#!   EXPLORE MAP_GIS
#!
library(sf)

paragens <- st_read("inst/map_gis/paragens.shx") |> 
    st_transform(crs = 4326) 

trocos <- st_read("inst/map_gis/trocos.shx") |> 
    st_transform(crs = 4326) 


