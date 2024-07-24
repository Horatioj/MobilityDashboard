### This is the City Mobility Equity Metric Dashoboard
#### Developed by R shiny
Click [here](https://horatioj.shinyapps.io/MobDashboard/) to see the website.
- MobDemo
    - data
        - transport flow and O/D matrix
    - Metric_data
        - MEM for transport network
    - www
        - introduction page
    - bs_memresult1k.csv: Boston MEM combinations
    - bs_merged_geoid_comm_data.csv: contains Boston geometry and census data
    - bs_miresults.csv: contains mobility index information in Boston
    - dc_10.csv: contains Washington DC geometry and census data
    - flow_combine.csv: routes in Boston transport network, geometry data, from [osrm](https://project-osrm.org/)
    - memresult1k.csv: DC MEM combinations
    - results64.csv: DC mobility index
    - styles.css: shiny ui.

The required libraries are below

```R
want <- c("ggplot2", "readxl", "shiny", "igraph", "geojsonR", "shinycssloaders",
          "sf", "leaflet", "plotly", "dplyr", "shinyjs", "rintrojs")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)
```

This dashboard is mainly based on work from [Routing in Mixed Transportation Systems for Mobility Equity](https://arxiv.org/pdf/2309.03981.pdf) &
[A Mobility Equity Metric for Multi-Modal Intelligent Transportation Systems](https://arxiv.org/abs/2405.17599).

Some data are from [American Community Survey](https://www.census.gov/programs-surveys/acs)
isochrones are from [isolines API](https://www.geoapify.com/)
