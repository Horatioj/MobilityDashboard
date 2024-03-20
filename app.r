library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(plotly)
library(geojsonR)
library(shinyjs)
library(readxl)
library(igraph)

# dc <- read.csv('dc.csv')
# # pay attention the column names
# dc <- dc %>%
#   filter(income > 0 & population > 0 & MEM > 0)
# dc.sf <- st_as_sf(data.frame(geometry = dc$geometry, Mobility = dc$MEM, 
#                              Income = dc$income, Population = dc$population, Name = dc$name), 
#                   wkt = "geometry", crs = 4326)
# percentiles <- quantile(dc.sf$Income, c(0.3, 0.7))
# dc.sf$category <- cut(dc.sf$Income,
#                          breaks = c(-Inf, percentiles[1], percentiles[2], Inf),
#                          labels = c("Low Income", "Medium Income", "High Income"),
#                          include.lowest = TRUE)
# low_inc_df <- dc.sf[dc.sf$category=='Low Income',]
# # Sort the data by income
# sorted_arr <- arrange(low_inc_df, Mobility)
# # med_val <- median(dc$Income)
# lowest_mi_5 <- head(sorted_arr, 5)
# # highest_inc_5 <- tail(sorted_arr, 5)
# # middle_inc_5 <- sorted_arr[(floor(nrow(sorted_arr)/2)-2):(floor(nrow(sorted_arr)/2)+2),]

server <-
  function(input, output){
    reactive_data <- reactiveVal(NULL)
    observe({
      # MEM.val <- c(0.5830256, 0.7031532, 0.8092773, 0.8602140, 0.8516743, 0.8330003)
      # time for retriving the corresponding MEM values
      time.bicycle <- input$slider1
      time.drive <- input$slider2
      time.transit <- input$slider3#
      time.walk <- input$slider4
      # the first map
      dy.df <- read.csv("results64.csv")
      dc <- read.csv("dc_10.csv") # read fir geometry
      time.df <- read.csv("memresult1k.csv") # for corresponding MEM
      time.val <- paste("MEM =", round(time.df[time.df$time.drive == time.drive &
                                                   time.df$time.bicycle == time.bicycle &
                                                   time.df$time.transit == time.transit &
                                                     time.df$time.walk == time.walk, "MEM"], 4))
      # previously, reading .csv from urls
      # base_u <- "https://raw.githubusercontent.com/Horatioj/R_visual/main/dc/"
      # name <- paste0('dc_', time, '.csv')
      # fpath <- file.path(base_u, name)
      # dc <- read.csv(fpath)
      # dc <- dc %>%
      #   filter(income > 0 & population > 0 & MEM > 0)
      # dc.sf <- st_as_sf(data.frame(geometry = dc$geometry, Mobility = dc$MEM, 
      #                              Income = dc$income, Population = dc$population, Name = dc$name), 
      #                   wkt = "geometry", crs = 4326)
      
      unique_communities <- unique(dy.df$Community) # get the community names
      
      # a new df for storing the first map's Mobility Index values
      com.mi.df <- data.frame(Community = unique_communities,
                              MI = numeric(length(unique_communities)))
      
      # Loop over unique communities
      for (i in seq_along(unique_communities)) {
        community <- unique_communities[i]
        community_mi_values <- numeric()
        
        # Loop over modes for calculating the mobility index of each community
        for (mode in c("drive", "bicycle", "walk", "transit")) {
          current_time <- switch(mode,
                                 "drive" = time.drive,
                                 "transit" = time.transit,
                                 "walk" = time.walk,
                                 "bicycle" = time.bicycle)
          
          # Filter the dataframe based on conditions
          result <- dy.df[dy.df$Community == community &
                            dy.df$Mode == mode &
                            dy.df$Time.Range == current_time, "MI"]
          
          # Append the MI values to the vector
          community_mi_values <- c(community_mi_values, result)
        }
        
        # Calculate and store the mean MI value for the current community
        com.mi.df$MI[i] <- mean(community_mi_values)
      }
      # merge with geometry columns, one community is removed because of NAs
      merge.df <- na.omit(merge(com.mi.df, dc, by.x="Community", by.y="name"))
      # switch to sf df format
      dc.sf <- st_as_sf(data.frame(geometry = merge.df$geometry, Mobility = merge.df$MI, 
                                  Income = merge.df$income, Population = merge.df$population, Name = merge.df$Community),
                       wkt = "geometry", crs = 4326)
      # set thresholds to categorize low, medium, and high income communities
      percentiles <- quantile(dc.sf$Income, c(0.3, 0.7))
      dc.sf$category <- cut(dc.sf$Income,
                            breaks = c(-Inf, percentiles[1], percentiles[2], Inf),
                            labels = c("Low Income", "Medium Income", "High Income"),
                            include.lowest = TRUE)
      low_inc_df <- dc.sf[dc.sf$category=='Low Income', ]
      sorted_arr <- arrange(low_inc_df, Mobility)
      lowest_mi_5 <- head(sorted_arr, 5)
    
      # reactive_data for following draws
      reactive_data(list(dc.sf=dc.sf, lowest_mi_5=lowest_mi_5, low_inc_df=low_inc_df, time.val=time.val))
    })
    
    #################
    ## Boston observe
    #################
    react.data.bs <- reactiveVal(NULL)
    observe({
      time.bicycle <- input$bslider1
      time.drive <- input$bslider2
      time.transit <- input$bslider3
      time.walk <- input$bslider4
      # the first map
      dy.df <- read.csv("bs_miresults.csv")
      bs <- read.csv("bs_merged_geoid_comm_data.csv") # read fir geometry
      time.df <- read.csv("bs_memresult1k.csv") # for corresponding MEM
      time.val <- paste("MEM =", round(time.df[time.df$time.drive == time.drive &
                                                 time.df$time.bicycle == time.bicycle &
                                                 time.df$time.transit == time.transit &
                                                 time.df$time.walk == time.walk, "MEM"], 4))
      unique_communities <- unique(dy.df$Community) # get the community names
      
      # a new df for storing the first map's Mobility Index values
      com.mi.df <- data.frame(Community = unique_communities,
                              MI = numeric(length(unique_communities)))

      # Loop over unique communities
      for (i in seq_along(unique_communities)) {
        community <- unique_communities[i]
        community_mi_values <- numeric()
        
        # Loop over modes for calculating the mobility index of each community
        for (mode in c("drive", "bicycle", "walk", "transit")) {
          current_time <- switch(mode,
                                 "drive" = time.drive,
                                 "transit" = time.transit,
                                 "walk" = time.walk,
                                 "bicycle" = time.bicycle)
          
          # Filter the dataframe based on conditions
          result <- dy.df[dy.df$Community == community &
                            dy.df$Mode == mode &
                            dy.df$Time.Range == current_time, "MI"]
          
          # Append the MI values to the vector
          community_mi_values <- c(community_mi_values, result)
        }
        
        # Calculate and store the mean MI value for the current community
        com.mi.df$MI[i] <- mean(community_mi_values)
      }
      # merge with geometry columns, one community is removed because of NAs
      merge.df <- na.omit(merge(com.mi.df, bs, by.x="Community", by.y="name"))
      # switch to sf df format
      bs.sf <- st_as_sf(data.frame(geometry = merge.df$geometry, Mobility = merge.df$MI, 
                                   Income = merge.df$income, Population = merge.df$population, Name = merge.df$Community),
                        wkt = "geometry", crs = 4326)
      # set thresholds to categorize low, medium, and high income communities
      percentiles <- quantile(bs.sf$Income, c(0.3, 0.7))
      bs.sf$category <- cut(bs.sf$Income,
                            breaks = c(-Inf, percentiles[1], percentiles[2], Inf),
                            labels = c("Low Income", "Medium Income", "High Income"),
                            include.lowest = TRUE)
      low_inc_df <- bs.sf[bs.sf$category=='Low Income', ]
      sorted_arr <- arrange(low_inc_df, Mobility)
      lowest_mi_5 <- head(sorted_arr, 5)
      
      # reactive_data for following draws
      react.data.bs(list(bs.sf=bs.sf, lowest_mi_5=lowest_mi_5, low_inc_df=low_inc_df, time.val=time.val))
    })
    
    ####################
    ######## NYC Observe
    ####################
    
    react.data.ny <- reactiveVal(NULL)
    observe({
      time.bicycle <- input$nyslider1
      time.drive <- input$nyslider2
      time.transit <- input$nyslider3
      time.walk <- input$nyslider4
      # the first map
      dy.df <- read.csv("nyc_miresults.csv")
      ny <- read.csv("ny_merged_geoid_comm_data.csv") # read fir geometry
      time.df <- read.csv("ny_memresult1k.csv") # for corresponding MEM
      time.val <- paste("MEM =", round(time.df[time.df$time.drive == time.drive &
                                                 time.df$time.bicycle == time.bicycle &
                                                 time.df$time.transit == time.transit &
                                                 time.df$time.walk == time.walk, "MEM"], 4))
      unique_communities <- unique(dy.df$Community) # get the community names
      
      # a new df for storing the first map's Mobility Index values
      com.mi.df <- data.frame(Community = unique_communities,
                              MI = numeric(length(unique_communities)))
      
      # Loop over unique communities
      for (i in seq_along(unique_communities)) {
        community <- unique_communities[i]
        community_mi_values <- numeric()
        
        # Loop over modes for calculating the mobility index of each community
        for (mode in c("drive", "bicycle", "walk", "transit")) {
          current_time <- switch(mode,
                                 "drive" = time.drive,
                                 "transit" = time.transit,
                                 "walk" = time.walk,
                                 "bicycle" = time.bicycle)
          
          # Filter the dataframe based on conditions
          result <- dy.df[dy.df$Community == community &
                            dy.df$Mode == mode &
                            dy.df$Time.Range == current_time, "MI"]
          
          # Append the MI values to the vector
          community_mi_values <- c(community_mi_values, result)
        }
        
        # Calculate and store the mean MI value for the current community
        com.mi.df$MI[i] <- mean(community_mi_values)
      }
      # merge with geometry columns, one community is removed because of NAs
      merge.df <- na.omit(merge(com.mi.df, ny, by.x="Community", by.y="name"))
      # switch to sf df format
      ny.sf <- st_as_sf(data.frame(geometry = merge.df$geometry, Mobility = merge.df$MI, 
                                   Income = merge.df$income, Population = merge.df$population, Name = merge.df$Community),
                        wkt = "geometry", crs = 4326)
      # set thresholds to categorize low, medium, and high income communities
      percentiles <- quantile(ny.sf$Income, c(0.3, 0.7))
      ny.sf$category <- cut(ny.sf$Income,
                            breaks = c(-Inf, percentiles[1], percentiles[2], Inf),
                            labels = c("Low Income", "Medium Income", "High Income"),
                            include.lowest = TRUE)
      low_inc_df <- ny.sf[ny.sf$category=='Low Income', ]
      sorted_arr <- arrange(low_inc_df, Mobility)
      lowest_mi_5 <- head(sorted_arr, 5)
      
      # reactive_data for following draws
      react.data.ny(list(ny.sf=ny.sf, lowest_mi_5=lowest_mi_5, low_inc_df=low_inc_df, time.val=time.val))
    })
    
    output$map <- renderLeaflet({
      data <- reactive_data()
      dc.sf <- data$dc.sf
      
      # leaflet map
      # set context map styles, you can search for other base map styles
      leaflet(dc.sf) %>%
        addProviderTiles(provider = "Stadia", 
                         options = providerTileOptions(url = "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.{ext}")) %>%
        addPolygons(weight = 1.5, smoothFactor = 0.5, opacity = 0.6, fillOpacity = 0.5,
                    fillColor = ~colorNumeric("YlGn", dc.sf[[input$Columns]])(dc.sf[[input$Columns]]),
                    popup = paste("<div style='font-size: 16px;'><b>Name: </b>", dc.sf$Name, "<br>",
                                  "<b>Mobility Index: </b>", round(dc.sf$Mobility, digits = 2), "<br>",
                                  "<b>Income: </b>", round(dc.sf$Income, digits = 2), "<br>",
                                  "<b>Population: </b>", dc.sf$Population, "<br></div>"),
                    options = popupOptions(minWidth = 500, maxWidth=1000)
        ) %>% 
        addLegend(
          pal = colorNumeric("YlGn", dc.sf[[input$Columns]]),
          values = ~dc.sf[[input$Columns]],
          title = if (input$Columns == "Mobility") {paste(input$Columns, 'Index')} else{input$Columns},
          opacity = 0.8,
          position = "topleft"
        ) -> leafm
      leafm
    })
    output$Histogram <- renderPlot({
      data <- reactive_data()
      lowest_mi_5 <- data$lowest_mi_5
      # reorder based on low income group communities
      # pay attention to the name, since name is very long and seperated by comma in DC
      # other cities could be different
      if(input$Columns != 'Population'){
        ggplot(lowest_mi_5, aes(x = reorder(substring(Name, 1, ifelse(regexpr(",", Name) > 0, regexpr(",", Name) - 1, nchar(Name)))
                                            , Income), y=Mobility)) +
          geom_col(fill='#ccffcc') +
          geom_text(aes(label = round(Mobility, 2)), 
                    vjust=-0.5, size=6, position=position_stack(vjust=0.5)) + #fontface = "bold",
          labs(title = "5 Lowest Income Communities",
               x = NULL,
               y = "Moibility Index") +
          theme_minimal() +
          theme(axis.text.x=element_text(angle=45, hjust=1), 
                legend.position="none",
                text=element_text(siz=16))
      } else {
        ggplot(lowest_mi_5, aes(x=reorder(substring(Name, 1, ifelse(regexpr(",", Name) > 0, regexpr(",", Name) - 1, nchar(Name))), Population), y=Mobility)) +
          geom_col(fill='#ccffcc') +
          geom_text(aes(label = round(Mobility, 2)), 
                    vjust=-0.5, size=6, position=position_stack(vjust=0.5)) + #fontface = "bold", substring(Name, 1, regexpr(",", Name)-1)
          labs(title=paste("5 Lowest Population Communities"),
               x="Name",
               y="Moibility Index") +
          theme_minimal() +
          theme(axis.text.x=element_text(angle=30, hjust=1), 
                legend.position="none",
                text=element_text(size=14))
      }
    })
    output$Scatter <- renderPlot({
      # boxplot to only show income
      data <- reactive_data()
      dc.sf = data$dc.sf
      ggplot(dc.sf, aes(x = Income, y = Mobility)) +
        geom_boxplot(aes(fill = category), width = 0.5, color = "black") +
        geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5, color = "red") +
        geom_smooth(method = "lm", se = TRUE, color = "turquoise2", size = 1) +
        labs(title = "Mobility Index by Income Categories",
             x = "Income",
             y = "Mobility Index") +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.box = "horizontal",
              text=element_text(siz=16))
      # scatter plot
      # if(input$Columns != 'Population'){
      #   ggplot(low_inc_df, aes(x=Income, y=Mobility)) +
      #     geom_point(color="#00CED1") +
      #     geom_smooth(method=lm,  linetype="dashed", color="slateblue", fill="lightblue") +
      #     ylab("Mobility Index") +
      #     ggtitle("Lowest Income Community against Mobility Index")
      # } else {
      #   ggplot(low_inc_df, aes(x=Population, y=Mobility)) +
      #     geom_point(color="#00CED1") +
      #     geom_smooth(method=lm,  linetype="dashed", color="slateblue", fill="lightblue") +
      #     ylab("Mobility Index") + 
      #     ggtitle("Lowest Population Community against Mobility Index")
      # }
    })
    output$isochrone <- renderLeaflet({
      # data are uploaded to github for easier retrieval 
      # transport <- input$transport
      data <- reactive_data()
      transport <- c("bicycle", "drive", "walk", "transit")
      community <- input$community
      # time <- input$slider1
      time.bicycle <- input$slider1
      time.drive <- input$slider2
      time.transit <- input$slider3
      time.walk <- input$slider4

      # transport <- tolower(transport)
      community <- URLencode(gsub(",", "%2C", gsub(" ", "%20", community)))
      base_url <- 'https://raw.githubusercontent.com/Horatioj/R_visual/main'
      # fpath <- file.path(raw_url, 'dc', transport, paste0(community, "_dc_", time, ".json"))
      # read json on 4 transport modes
      st_objs <- lapply(transport, function(t) {
        url <- sprintf("%s/dc/%s/%s_dc_%d.json", base_url, t, community, 
                       switch(t, bicycle = time.bicycle, drive = time.drive, 
                              walk = time.walk, transit = time.transit))
        st_read(url)
      })
      st.multi <- do.call(bind_rows, st_objs) # combine to df
      st.multi$mode[st.multi$mode == "approximated_transit"] <- "public_transit"
      # set colors: bicycle, drive, transit, walk
      iso_all.colors <- c("yellow", "red", "#08306b", "green")
      iso_all.pal <- colorFactor(iso_all.colors, st.multi$mode)
      
      leaflet() %>%
        addProviderTiles(provider = "Stadia",
                        options = providerTileOptions(url = "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.{ext}")) %>%
        addPolygons(
          data = st.multi,
          fillColor = ~iso_all.pal(mode), # fill color based on modes
          fillOpacity = 0.5,
          color = "black",
          weight = 0.5,
          popup = ~mode, # click to show the isochrones names. It will display another name due to overlaps
          stroke = TRUE,
          group = 'Ischrone'
        ) %>%
        addCircles(data = st.multi,   # add the centroid point (geographical center)
                   lng = st.multi$lon,
                   lat = st.multi$lat,
                   radius = 5,
                   opacity = 1,
                   group = "Center") %>%
        addLegend("bottomright",
                  colors = iso_all.pal(unique(st.multi$mode)),
                  labels = unique(st.multi$mode),
                  opacity = 0.5,
                  title = "Transportation Modes") %>%
        # add texts
        addControl(html = paste('<div font-weight: bold; style="font-size: 24px;">', data$time.val, '</div>'),
                    position = "topright") %>%
        setView(lng=st.multi$lon[1], lat=st.multi$lat[1], zoom=12)
    })
    # output$myPlot <- renderImage({
    #   p <- input$P
    #   w <- input$W
    #   n <- input$N
    #   nw <- 1 - as.numeric(w)
    #   f.path <- paste0("DATA_Public", p, "NCR", n, "W", w, "_", nw, ".xlsx")
    #   df <- read_excel(file.path("data", f.path), col_names = FALSE)
    #   df2 <- read_excel(file.path("Metric_data", f.path), col_names = FALSE)
    #   colnames(df) <- c("origin", "dest", "flow")
    #   # metric (column 1) and the average travel time difference (column 2) between compliant and non compliant vehicles. 
    #   colnames(df2) <- c("MEM", "Diff")
    #   # average flows of bidrectional edges
    #   df <- df %>%
    #     group_by(origin, dest) %>%
    #     summarize(avg_flow = mean(flow)) %>%
    #     filter(origin < dest) %>%
    #     select(origin, dest, avg_flow)
    #   
    #   # 30% 70% percentiles flows to set colors
    #   # percentiles <- quantile(df$avg_flow, c(0.3, 0.7))
    #   df$category <- cut(df$avg_flow,
    #                      breaks = c(-Inf, 0.3, 0.75, Inf),
    #                      labels = c("Small", "Medium", "High"),
    #                      include.lowest = TRUE)
    #   graph.df <- graph_from_data_frame(df, directed=FALSE)
    #   layout <- layout_with_kk(graph.df)
    #   # Plot the graph with double arrows for bidirectional connections
    #   img_path <- "file.png"
    #   png(img_path, width = 1900, height = 900, bg = "transparent")
    #   plot(
    #     graph.df,
    #     layout = layout,
    #     edge.arrow.mode = 0,
    #     edge.width = 12,
    #     # edge.arrow.size = 0.4,   # Smaller arrow size
    #     # edge.arrow.width = 2,    # Thicker arrows
    #     edge.curved = 0,       # Adjust curvature as needed
    #     edge.color = ifelse(df$category == "Small", "forestgreen",
    #                         ifelse(df$category == "Medium", "tan2", "firebrick")),
    #     vertex.label.cex = 2,  # Adjust node label size as needed
    #     vertex.size = 14,        # Adjust node size
    #     vertex.color = 'grey',   # Set node color to grey
    #     margin = 0.0,            # Adjust margin
    #     bg = "transparent",
    #     vertex.label = NA
    #   )
    #   legend(
    #     x = 1.5,
    #     y = 1.1,
    #     legend = c("Small", "Medium", "Large"),
    #     fill = c("forestgreen", "tan2", "firebrick"),
    #     title = "Congested Level",
    #     border = "white",
    #     cex = 1.5
    #   )
    #   text(x = -2, y = -0.7, labels = paste("MEM =", round(df2$MEM, 4)), adj = c(0.5, 0), cex = 2)
    #   text(x = -2, y = -1, labels = paste("Time Difference between \n Compl. & Non-Compl. Vehicles =", round(df2$Diff, 4))
    #        , adj = c(0.5, 0), cex = 2)
    #   dev.off()
    #   
    #   list(
    #     src = img_path,
    #     contentType = "image/png",
    #     width = 1900,
    #     height = 900,
    #     alt = "Transport Network"
    #   )
    # }, deleteFile = TRUE)
    output$basemap <- renderLeaflet({
      p <- input$P
      w <- input$W
      n <- input$N
      nw <- 1 - as.numeric(w)
      f.path <- paste0("DATA_Public", p, "NCR", n, "W", w, "_", nw, ".xlsx")
      f.path1 <- file.path("data", f.path)
      if (!file.exists(f.path1)) {
        showNotification("Please choose the correct parameters", type = "default", duration = 5)
        return(NULL)
      }
      df <- read_excel(f.path1, col_names = FALSE)
      df2 <- read_excel(file.path("Metric_data", f.path), col_names = FALSE)
      flow_read <- read_excel("flow_combine.xlsx")
      colnames(df) <- c("origin", "dest", "flow") # this column is changed, "time" actually
      # metric (column 1) and the average travel time difference (column 2) between compliant and non compliant vehicles.
      colnames(df2) <- c("MEM", "Diff")
      # average flows of bidrectional edges
      df <- df %>%
        group_by(origin, dest) %>%
        summarize(avg_flow = mean(flow)) %>%
        filter(origin < dest) %>%
        select(origin, dest, avg_flow)
      
      df <- merge(flow_read, df, by=intersect(names(flow_read)[c(1,2)], names(df)[c(1,2)]))

      # 30% 70% percentiles flows to set colors
      # percentiles <- quantile(df$avg_flow, c(0.2, 0.5, ))
      df$category <- cut(df$avg_flow,
                         breaks = c(-Inf, 30, 60, 90, 120, 150, 180, Inf),
                         labels = c("Instantaneous", "Fast", "Short", "Moderate", "Medium", "Long", "Very Long"),
                         include.lowest = TRUE)
      
      nodes <- data.frame(
        longitude = c(-71.0484, -71.0538, -71.0630, -71.070425,
                      -71.05038, -71.0574, -71.06349, -71.07354,
                      -71.05977, -71.06468, -71.07032, -71.07952,
                      -71.06407, -71.07379, -71.07854, -71.08557),
        latitude = c(42.3621, 42.3669, 42.3666, 42.3638,
                     42.35643, 42.35875, 42.35757, 42.35736, 
                     42.35133, 42.35242, 42.35106, 42.34956,
                     42.34159, 42.34282, 42.34321, 42.34287),
        label = seq(1:16)
      )
      
      # Create Leaflet map with Stadia tiles
      map <- leaflet() %>% 
        setView(lng = -71.06455, lat = 42.353, zoom = 15) %>%
        addProviderTiles(provider = "Stadia",
                         options = providerTileOptions(url = "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.{ext}"))
      
      # Add markers for nodes
      map <- addCircleMarkers(map, lng = nodes$longitude, lat = nodes$latitude,
                              popup =  paste("<div style='font-size: 16px;'><b>Label: </b>", as.character(nodes$label), "<br>",
                                             "<b>Latitude: </b>", nodes$latitude, "<br>",
                                             "<b>Longitude: </b>", nodes$longitude, "</div>"), 
                              fillOpacity = 0.8, color = "grey",
                              radius = 8)
      
      # Define color palette
      palette <- colorRampPalette(c("forestgreen", "gold1", "brown3"))(100)
      palette.pal <- colorNumeric(palette, df$avg_flow)
      # Add legend
      legend_breaks <- quantile(df$avg_flow, probs = seq(0, 1, by = 0.2))
      legend_labels <- round(legend_breaks, 4)
      map <- map %>%
        addLegend(position = "topright", 
                  pal = palette.pal, 
                  values = df$avg_flow, 
                  title = "Time", 
                  opacity = 0.8,
                  # labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
                  ) %>%
        addControl(html = paste(paste('<div font-weight: bold; style="font-size: 18px; line-height: 1.5;"> MEM = ', round(df2$MEM, 4), '</div>'),
                                paste('<div font-weight: bold; style="font-size: 18px; line-height: 1.5;"> Time Difference between <br> Compl. & Non-Compl. Vehicles = ',
                                      round(df2$Diff, 4), '</div>')),
                   position = "topright")
      
      # Draw routes on the map
      for (i in 1:nrow(df)) {
        origin <- as.numeric(df[i, 1])
        destination <- as.numeric(df[i, 2])
        color <- colorNumeric(palette, domain = df$avg_flow)(df$avg_flow[i])
        path_lng <- as.numeric(unlist(strsplit(df$path_lon[i], ",")))
        path_lat <- as.numeric(unlist(strsplit(df$path_lat[i], ",")))
        route <- addPolylines(map, 
                              lng = path_lng,
                              lat = path_lat,
                              color = color,
                              opacity = 1,
                              weight = 6,
                              popup = paste("<div style='font-size: 16px;'><b>Time: </b>", round(df$avg_flow[i], 4), "</div>"))
        map <- route
      }
      
      map
      
    })
    
    ##############################################################
    # Boston
    ##############################################################
    output$mapbs <- renderLeaflet({
      data <- react.data.bs()
      dc.sf <- data$bs.sf
      
      leaflet(dc.sf) %>%
        addProviderTiles(provider = "Stadia", 
                         options = providerTileOptions(url = "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.{ext}")) %>%
        addPolygons(weight = 1.5, smoothFactor = 0.5, opacity = 0.6, fillOpacity = 0.5,
                    fillColor = ~colorNumeric("YlGn", dc.sf[[input$Columns]])(dc.sf[[input$Columns]]),
                    popup = paste("<div style='font-size: 16px;'><b>Name: </b>", dc.sf$Name, "<br>",
                                  "<b>Mobility Index: </b>", round(dc.sf$Mobility, digits = 2), "<br>",
                                  "<b>Income: </b>", round(dc.sf$Income, digits = 2), "<br>",
                                  "<b>Population: </b>", dc.sf$Population, "<br></div>"),
                    options = popupOptions(minWidth = 500, maxWidth=1000)
        ) %>% 
        addLegend(
          pal = colorNumeric("YlGn", dc.sf[[input$Columns]]),
          values = ~dc.sf[[input$Columns]],
          title = if (input$Columns == "Mobility") {paste(input$Columns, 'Index')} else{input$Columns},
          opacity = 0.8,
          position = "topleft"
        ) -> leafm
      leafm
    })
    output$Histbs <- renderPlot({
      data <- react.data.bs()
      lowest_mi_5 <- data$lowest_mi_5
      if(input$bColumns != 'Population'){
        ggplot(lowest_mi_5, aes(x = reorder(substring(Name, 1, ifelse(regexpr(",", Name) > 0, regexpr(",", Name) - 1, nchar(Name)))
                                            , Income), y=Mobility)) +
          geom_col(fill='#ccffcc') +
          geom_text(aes(label = round(Mobility, 2)), 
                    vjust=-0.5, size=6, position=position_stack(vjust=0.5)) + #fontface = "bold",
          labs(title = "5 Lowest Income Communities",
               x = NULL,
               y = "Moibility Index") +
          theme_minimal() +
          theme(axis.text.x=element_text(angle=45, hjust=1), 
                legend.position="none",
                text=element_text(siz=16))
      } else {
        ggplot(lowest_mi_5, aes(x=reorder(substring(Name, 1, ifelse(regexpr(",", Name) > 0, regexpr(",", Name) - 1, nchar(Name))), Population), y=Mobility)) +
          geom_col(fill='#ccffcc') +
          geom_text(aes(label = round(Mobility, 2)), 
                    vjust=-0.5, size=6, position=position_stack(vjust=0.5)) + #fontface = "bold", substring(Name, 1, regexpr(",", Name)-1)
          labs(title=paste("5 Lowest Population Communities"),
               x="Name",
               y="Moibility Index") +
          theme_minimal() +
          theme(axis.text.x=element_text(angle=30, hjust=1), 
                legend.position="none",
                text=element_text(size=14))
      }
    })
    output$Scatbs <- renderPlot({
      # boxplot to only show income
      data <- react.data.bs()
      dc.sf = data$bs.sf
      ggplot(dc.sf, aes(x = Income, y = Mobility)) +
        geom_boxplot(aes(fill = category), width = 0.5, color = "black") +
        geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5, color = "red") +
        geom_smooth(method = "lm", se = TRUE, color = "turquoise2", size = 1) +
        labs(title = "Mobility Index by Income Categories",
             x = "Income",
             y = "Mobility Index") +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.box = "horizontal",
              text=element_text(siz=16))
    })
    output$isobs <- renderLeaflet({
      data <- react.data.bs()
      transport <- c("bicycle", "drive", "walk", "transit")
      community <- input$bcommunity
      time.bicycle <- input$bslider1
      time.drive <- input$bslider2
      time.transit <- input$bslider3
      time.walk <- input$bslider4
      
      # transport <- tolower(transport)
      community <- URLencode(gsub(",", "%2C", gsub(" ", "%20", community)))
      base_url <- 'https://raw.githubusercontent.com/Horatioj/R_visual/main'
      # fpath <- file.path(raw_url, 'dc', transport, paste0(community, "_dc_", time, ".json"))
      # read json on 4 transport modes
      st_objs <- lapply(transport, function(t) {
        url <- sprintf("%s/boston/%s/%s_bs_%d.json", base_url, t, community, 
                       switch(t, bicycle = time.bicycle, drive = time.drive, 
                              walk = time.walk, transit = time.transit))
        st_read(url)
      })
      st.multi <- do.call(bind_rows, st_objs) # combine to df
      st.multi$mode[st.multi$mode == "approximated_transit"] <- "public_transit"
      # set colors: bicycle, drive, transit, walk
      iso_all.colors <- c("yellow", "red", "#08306b", "green")
      iso_all.pal <- colorFactor(iso_all.colors, st.multi$mode)
      
      leaflet() %>%
        addProviderTiles(provider = "Stadia",
                         options = providerTileOptions(url = "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.{ext}")) %>%
        addPolygons(
          data = st.multi,
          fillColor = ~iso_all.pal(mode), # fill color based on modes
          fillOpacity = 0.5,
          color = "black",
          weight = 0.5,
          popup = ~mode, # click to show the isochrones names. It will display another name due to overlaps
          stroke = TRUE,
          group = 'Ischrone'
        ) %>%
        addCircles(data = st.multi,   # add the centroid point (geographical center)
                   lng = st.multi$lon,
                   lat = st.multi$lat,
                   radius = 5,
                   opacity = 1,
                   group = "Center") %>%
        addLegend("bottomright",
                  colors = iso_all.pal(unique(st.multi$mode)),
                  labels = unique(st.multi$mode),
                  opacity = 0.5,
                  title = "Transportation Modes") %>%
        # add texts
        addControl(html = paste('<div font-weight: bold; style="font-size: 24px;">', data$time.val, '</div>'),
                   position = "topright") %>%
        setView(lng=st.multi$lon[1], lat=st.multi$lat[1], zoom=12)
    })
    
    ###########################
    ### New York City #########
    ###########################
    
    output$nymaps <- renderLeaflet({
      data <- react.data.ny()
      ny.sf <- data$ny.sf
      
      leaflet(ny.sf) %>%
        addProviderTiles(provider = "Stadia", 
                         options = providerTileOptions(url = "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.{ext}")) %>%
        addPolygons(weight = 1.5, smoothFactor = 0.5, opacity = 0.6, fillOpacity = 0.5,
                    fillColor = ~colorNumeric("YlGn", ny.sf[[input$Columns]])(ny.sf[[input$Columns]]),
                    popup = paste("<div style='font-size: 16px;'><b>Name: </b>", ny.sf$Name, "<br>",
                                  "<b>Mobility Index: </b>", round(ny.sf$Mobility, digits = 2), "<br>",
                                  "<b>Income: </b>", round(ny.sf$Income, digits = 2), "<br>",
                                  "<b>Population: </b>", ny.sf$Population, "<br></div>"),
                    options = popupOptions(minWidth = 500, maxWidth=1000)
        ) %>% 
        addLegend(
          pal = colorNumeric("YlGn", ny.sf[[input$Columns]]),
          values = ~ny.sf[[input$Columns]],
          title = if (input$Columns == "Mobility") {paste(input$Columns, 'Index')} else{input$Columns},
          opacity = 0.8,
          position = "topleft"
        ) -> leafm
      leafm
    })
    output$nyHist <- renderPlot({
      data <- react.data.ny()
      lowest_mi_5 <- data$lowest_mi_5
      if(input$nyColumns != 'Population'){
        ggplot(lowest_mi_5, aes(x = reorder(substring(Name, 1, ifelse(regexpr(",", Name) > 0, regexpr(",", Name) - 1, nchar(Name)))
                                            , Income), y=Mobility)) +
          geom_col(fill='#ccffcc') +
          geom_text(aes(label = round(Mobility, 2)), 
                    vjust=-0.5, size=6, position=position_stack(vjust=0.5)) + #fontface = "bold",
          labs(title = "5 Lowest Income Communities",
               x = NULL,
               y = "Moibility Index") +
          theme_minimal() +
          theme(axis.text.x=element_text(angle=45, hjust=1), 
                legend.position="none",
                text=element_text(siz=16))
      } else {
        ggplot(lowest_mi_5, aes(x=reorder(substring(Name, 1, ifelse(regexpr(",", Name) > 0, regexpr(",", Name) - 1, nchar(Name))), Population), y=Mobility)) +
          geom_col(fill='#ccffcc') +
          geom_text(aes(label = round(Mobility, 2)), 
                    vjust=-0.5, size=6, position=position_stack(vjust=0.5)) + #fontface = "bold", substring(Name, 1, regexpr(",", Name)-1)
          labs(title=paste("5 Lowest Population Communities"),
               x="Name",
               y="Moibility Index") +
          theme_minimal() +
          theme(axis.text.x=element_text(angle=30, hjust=1), 
                legend.position="none",
                text=element_text(size=14))
      }
    })
    output$nyScat <- renderPlot({
      # boxplot to only show income
      data <- react.data.ny()
      ny.sf = data$ny.sf
      ggplot(ny.sf, aes(x = Income, y = Mobility)) +
        geom_boxplot(aes(fill = category), width = 0.5, color = "black") +
        geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5, color = "red") +
        geom_smooth(method = "lm", se = TRUE, color = "turquoise2", size = 1) +
        labs(title = "Mobility Index by Income Categories",
             x = "Income",
             y = "Mobility Index") +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.box = "horizontal",
              text=element_text(siz=16))
    })
    output$nyiso <- renderLeaflet({
      data <- react.data.ny()
      transport <- c("bicycle", "drive", "walk", "transit")
      community <- input$nycommunity
      time.bicycle <- input$nyslider1
      time.drive <- input$nyslider2
      time.transit <- input$nyslider3
      time.walk <- input$nyslider4
      
      # transport <- tolower(transport)
      community <- URLencode(gsub(",", "%2C", gsub(" ", "%20", community)))
      base_url <- 'https://raw.githubusercontent.com/Horatioj/R_visual/main'
      # fpath <- file.path(raw_url, 'ny', transport, paste0(community, "_ny_", time, ".json"))
      # read json on 4 transport modes
      st_objs <- lapply(transport, function(t) {
        url <- sprintf("%s/nyc/%s/%s_ny_%d.json", base_url, t, community, 
                       switch(t, bicycle = time.bicycle, drive = time.drive, 
                              walk = time.walk, transit = time.transit))
        st_read(url)
      })
      st.multi <- do.call(bind_rows, st_objs) # combine to df
      st.multi$mode[st.multi$mode == "approximated_transit"] <- "public_transit"
      # set colors: bicycle, drive, transit, walk
      iso_all.colors <- c("yellow", "red", "#08306b", "green")
      iso_all.pal <- colorFactor(iso_all.colors, st.multi$mode)
      
      leaflet() %>%
        addProviderTiles(provider = "Stadia",
                         options = providerTileOptions(url = "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.{ext}")) %>%
        addPolygons(
          data = st.multi,
          fillColor = ~iso_all.pal(mode), # fill color based on modes
          fillOpacity = 0.5,
          color = "black",
          weight = 0.5,
          popup = ~mode, # click to show the isochrones names. It will display another name due to overlaps
          stroke = TRUE,
          group = 'Ischrone'
        ) %>%
        addCircles(data = st.multi,   # add the centroid point (geographical center)
                   lng = st.multi$lon,
                   lat = st.multi$lat,
                   radius = 5,
                   opacity = 1,
                   group = "Center") %>%
        addLegend("bottomright",
                  colors = iso_all.pal(unique(st.multi$mode)),
                  labels = unique(st.multi$mode),
                  opacity = 0.5,
                  title = "Transportation Modes") %>%
        # add texts
        addControl(html = paste('<div font-weight: bold; style="font-size: 24px;">', data$time.val, '</div>'),
                   position = "topright") %>%
        setView(lng=st.multi$lon[1], lat=st.multi$lat[1], zoom=12)
    })
    
    
  }

ui <-
  navbarPage("Mobility Equity Map", id = "nav",
             # first tab, a gif
             tabPanel("Intro",
                      div(class="outer"),
                      tags$head(
                        includeCSS("styles.css"),
                      ),
                      img(src = "Intro.gif", width = "100%", height = "100%", type = "image/gif", autoplay = TRUE, loop = FALSE),
                      ),
             # second tab, two leafletOutputs with height 50% each
             tabPanel("D.C.",
                      div(class="outer",
                          tags$head(
                            includeCSS("styles.css")
                          ),
                          leafletOutput("map", width="100%", height="50%"),
                          # border line
                          tags$hr(style = "border: none; border-top: 2px solid #333; margin: 0; padding: 0;"),
                          # select Mobility, Population, Income and a histogram
                          # need to click the submit button below
                          absolutePanel(
                            id = "controls", class = "panel panel-default", 
                            fixed = TRUE, draggable = TRUE, top = 60, left = "auto", 
                            right = 20, bottom = "auto", width = 450, height = "auto",
                            
                            # h2("Controller"),
                            
                            selectInput("Columns", "Select", choices = c("Mobility", "Population", "Income"),
                                        selected = "Mobility"),
                            # with spinner, 
                            # fail to work on leafletoutputs probably due to the shiny doesn't 
                            # send calculation signs and shinycssloaders cannot detect
                            withSpinner(
                              plotOutput("Histogram", height = 350), type = 7, 
                            ),
                            # withSpinner(plotOutput("Scatter", height = 300), type = 7)
                          ),
                          # the boxplot - jitter - smooth line
                          absolutePanel(
                            id = "controls", class = "panel panel-default", 
                            fixed = TRUE, draggable = TRUE, top = 60, left =  150, 
                            right = "auto", bottom = "auto", width = 500, height = "auto",
                            
                            withSpinner(
                              plotOutput("Scatter", height = 300), type = 7
                            )
                          ),
                          # isochrones, can select communities, and 4 time sidebars
                          # click submit button to see changes
                          leafletOutput("isochrone", width="100%", height="50%"), 
                          absolutePanel(
                            id = "controls", class = "panel panel-default", 
                            fixed = TRUE, draggable = TRUE, top = "auto", left = 20, 
                            right = "auto", bottom = 0, width = 350, height = "auto",
                            
                            # h2("Controller"),
                            selectInput("community", "Community", choices = c("Arboretum, Anacostia River", "Capitol Hill, Lincoln Park", "Cathedral Heights, McLean Gardens, Glover Park",
                                                                              "Downtown, Chinatown, Penn Quarters, Mount Vernon Square, North Capitol Street",
                                                                              "Dupont Circle, Connecticut Avenue K Street", "Edgewood, Bloomingdale, Truxton Circle, Eckington",
                                                                              "Georgetown, Burleith Hillandale", "Near Southeast, Navy Yard",
                                                                              "Observatory Circle", "Union Station, Stanton Park, Kingman Park")),
                            # selectInput("transport", "Transportation", choices = c("Bicycle", "Drive", "Transit", "Walk")),
                            sliderInput("slider1", "Bicycle Time", 10, 60, 10, 10),
                            sliderInput("slider2", "Drive Time", 10, 60, 10, 10),
                            sliderInput("slider3", "Transit Time", 10, 60, 10, 10),
                            sliderInput("slider4", "Walk Time", 10, 60, 10, 10),
                            submitButton("Submit", width = '100%')
                          )
                          
                      )
                      
             ),
             # third tab, two leafletOutputs with height 50% each
             tabPanel("New York City",
                      div(class="outer",
                          tags$head(
                            includeCSS("styles.css")
                          ),
                          leafletOutput("nymaps", width="100%", height="50%"),
                          # border line
                          tags$hr(style = "border: none; border-top: 2px solid #333; margin: 0; padding: 0;"),
                          # select Mobility, Population, Income and a histogram
                          # need to click the submit button below
                          absolutePanel(
                            id = "controls", class = "panel panel-default", 
                            fixed = TRUE, draggable = TRUE, top = 60, left = "auto", 
                            right = 20, bottom = "auto", width = 450, height = "auto",
                            
                            selectInput("nyColumns", "Select", choices = c("Mobility", "Population", "Income"),
                                        selected = "Mobility"),
                            
                            withSpinner(
                              plotOutput("nyHist", height = 350), type = 7, 
                            ),
                          ),
                          # the boxplot - jitter - smooth line
                          absolutePanel(
                            id = "controls", class = "panel panel-default", 
                            fixed = TRUE, draggable = TRUE, top = 60, left =  150, 
                            right = "auto", bottom = "auto", width = 500, height = "auto",
                            
                            withSpinner(
                              plotOutput("nyScat", height = 300), type = 7
                            )
                          ),
                          leafletOutput("nyiso", width="100%", height="50%"), 
                          absolutePanel(
                            id = "controls", class = "panel panel-default", 
                            fixed = TRUE, draggable = TRUE, top = "auto", left = 20, 
                            right = "auto", bottom = 0, width = 350, height = "auto",
                            
                            selectInput("nycommunity", "Community", choices = c("Midtown", "John F. Kennedy International Airport", "Long Island City", 
                                                                                "Ozone Park", "Washington Heights")),
                            sliderInput("nyslider1", "Bicycle Time", 10, 60, 10, 10),
                            sliderInput("nyslider2", "Drive Time", 10, 60, 10, 10),
                            sliderInput("nyslider3", "Transit Time", 10, 60, 10, 10),
                            sliderInput("nyslider4", "Walk Time", 10, 60, 10, 10),
                            submitButton("Submit", width = '100%')
                          )
                          
                      )
                      
             ),
             # fourth tab, two leafletOutputs with height 50% each
             tabPanel("Boston",
                      div(class="outer",
                          tags$head(
                            includeCSS("styles.css")
                          ),
                          leafletOutput("mapbs", width="100%", height="50%"),
                          # border line
                          tags$hr(style = "border: none; border-top: 2px solid #333; margin: 0; padding: 0;"),
                          # select Mobility, Population, Income and a histogram
                          # need to click the submit button below
                          absolutePanel(
                            id = "controls", class = "panel panel-default", 
                            fixed = TRUE, draggable = TRUE, top = 60, left = "auto", 
                            right = 20, bottom = "auto", width = 450, height = "auto",
                        
                            selectInput("bColumns", "Select", choices = c("Mobility", "Population", "Income"),
                                        selected = "Mobility"),

                            withSpinner(
                              plotOutput("Histbs", height = 350), type = 7, 
                            ),
                          ),
                          # the boxplot - jitter - smooth line
                          absolutePanel(
                            id = "controls", class = "panel panel-default", 
                            fixed = TRUE, draggable = TRUE, top = 60, left =  150, 
                            right = "auto", bottom = "auto", width = 500, height = "auto",
                            
                            withSpinner(
                              plotOutput("Scatbs", height = 300), type = 7
                            )
                          ),
                          leafletOutput("isobs", width="100%", height="50%"), 
                          absolutePanel(
                            id = "controls", class = "panel panel-default", 
                            fixed = TRUE, draggable = TRUE, top = "auto", left = 20, 
                            right = "auto", bottom = 0, width = 350, height = "auto",

                            selectInput("bcommunity", "Community", choices = c("Allston", "Back Bay", "Charlestown",
                                                                               "South Boston", "West End")),
                            sliderInput("bslider1", "Bicycle Time", 10, 60, 10, 10),
                            sliderInput("bslider2", "Drive Time", 10, 60, 10, 10),
                            sliderInput("bslider3", "Transit Time", 10, 60, 10, 10),
                            sliderInput("bslider4", "Walk Time", 10, 60, 10, 10),
                            submitButton("Submit", width = '100%')
                          )
                          
                      )
                      
             ),
             # fifth tab, network
             tabPanel("Transport Net",
                      div(class="outer"),
                      tags$head(
                        includeCSS("styles.css"),
                        tags$style(HTML("
      # #myPlot {
      # background-color: transparent !important;
      #   position: absolute;
      #   width: 80%;
      #   height: 80%;
      #   top: 25%;
      #   left: 5%;
      #   z-index: 1;
      # }

      #basemap {
        position: absolute;
        width: 100%;
        height: 100%;
        top: 5%;
        left: 0;
        z-index: 0;
      }
      
       /* Custom notification styles */
      .shiny-notification {
        position: fixed;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        background-color: #f8f9fa; /* Light gray background */
        border: 1px solid #ced4da; /* Light border */
        border-radius: 5px; /* Rounded corners */
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1); /* Shadow effect */
        padding: 15px 20px; /* Padding around the content */
        font-family: Arial, sans-serif; /* Font family */
        font-size: 16px; /* Font size */
        color: #333; /* Text color */
      }
      .shiny-notification .close {
        position: absolute;
        top: 5px;
        right: 10px;
        font-size: 16px;
        color: #888;
        cursor: pointer;
      }
      .shiny-notification .close:hover {
        color: #555;
      }
    "))
                      ),
                      
                      leafletOutput("basemap", width = "100%", height = "100%"),
                      # withSpinner(
                      #   imageOutput("Network"), type = 7,
                      # ),
                      # withSpinner(
                      #   imageOutput("myPlot", width = "70%", height = "400px"), type = 7
                      # ),
                      # img(src = "file.png", width = "100%", height = "100%", type = "image/png"),
                      absolutePanel(
                        id = "controls", class = "panel panel-default", 
                        fixed = TRUE, draggable = TRUE, top = 80, left =  60, 
                        right = "auto", bottom = "auto", width = 300, height = "auto",
                        selectInput("P", "Public Transit Rate", choices = c(0.3, 0.5, 0.7)),
                        selectInput("W", "Weight on Public", choices = c(0.6, 0.7, 0.8)),
                        selectInput("N", "Non-Compliance Rate", choices = c(0.1, 0.3, 0.5)),
                        submitButton("Submit", width = '100%')
                      ),

             ),
             
  )

shinyApp(ui, server)