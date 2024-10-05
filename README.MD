### This is the City Mobility Equity Metric Dashboard
Click [here](https://horatioj.shinyapps.io/MobDashboard/) to see the website.

The following describes project structure, CSV files store calculated results (MEM & MI) under `table` folder:
- MobDashboard
    - data
        - transport flow and O/D matrix
    - Metric_data
        - MEM for transport network
    - www
        - introduction page pictures
    - bs_memresult1k.csv: Boston MEM results by different combinations
    - bs_merged_geoid_comm_data.csv: Boston geographic layer at neighborhood level and census data
    - bs_miresults.csv: contains MI in Boston (percentages of different service types refer to the weights toward MI)
    - dc_10.csv: contains Washington DC geometry and census data
    - flow_combine.csv: routes in Boston transport network, geometry data, from [osrm](https://project-osrm.org/)
    - dc_memresult1k.csv: DC MEM combinations
    - dc_miresults.csv: DC mobility index
    - styles.css: shiny ui.

you can find calculated results for Los Angeles and New York City in similar file name. We use Python to calculate them.

MEMcode_sample.zip contains sample codes to calculate MI and MEM, and draw a map in DC. Please unzip "isochrone" folder before run jupyter notebook, as it is large.

The required libraries are below

```R
want <- c("ggplot2", "readxl", "shiny", "igraph", "geojsonR", "shinycssloaders",
          "sf", "leaflet", "plotly", "dplyr", "shinyjs", "rintrojs")
need <- want[!(want %in% installed.packages()[,"Package"])]
if (length(need)) install.packages(need)
lapply(want, function(i) require(i, character.only=TRUE))
rm(want, need)
```

This dashboard is mainly based on work from [Routing in Mixed Transportation Systems for Mobility Equity](https://arxiv.org/pdf/2309.03981.pdf)，
[A Mobility Equity Metric for Multi-Modal Intelligent Transportation Systems](https://arxiv.org/abs/2405.17599) & [On Mobility Equity and the Promise of Emerging Transportation Systems](https://arxiv.org/abs/2408.15401).
