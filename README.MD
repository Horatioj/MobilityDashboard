### This is the City Mobility Equity Metric Dashoboard
Click [here](https://horatioj.shinyapps.io/MobDashboard/) to see the website.

Following describes folder structure, csv files store calculated results (MEM & MI):
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

you can find calculated results for Los Angelos and New York City in similar file name. We use Python to calculate them.

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