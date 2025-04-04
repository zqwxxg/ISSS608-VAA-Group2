library(shiny)
library(shinyWidgets)
library(lubridate)
library(tidyverse)
library(ggstatsplot)
library(ggplot2)
library(plotly)
library(sf)
library(terra)
library(tmap)
library(gstat)
library(automap)
library(viridis)
library(dtwclust)

# ----------------------------------
# CDA Data

weather_active <- read.csv("data/weather_active.csv")
weather_active$Date <- as.Date(weather_active$Date)
weather_active$Daily.Rainfall.Total..mm. <- as.numeric(weather_active$Daily.Rainfall.Total..mm.)

# ----------------------------------
# Interpolation data

weather_sf <- readRDS("data/weather_sf.rds")
monthly_data_sf <- readRDS("data/monthly_data_sf.rds")
yearly_data_sf <- readRDS("data/yearly_data_sf.rds")

sg_boundary <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>%
 st_transform(crs = 3414)

grid <- terra::rast(sg_boundary, nrows = 690, ncols = 1075)

coop <- readRDS("data/coop.rds")

# ----------------------------------
# Clustering data

monthly_active_data_sf <- readRDS("data/monthly_active_data_sf.rds")
rain_list <- readRDS("data/rain_list.rds")

# ----------------------------------
# Forecasting data

weather_predicted_sf <- readRDS("data/weather_predicted_sf.rds")

# ----------------------------------
# Main Server

server <- function(input, output, session) {
  
    # ----------------------------------
    # Home station map

    output$station_map <- renderTmap({
      tm_check_fix()
      tmap_mode("view")
      
      tm_shape(weather_sf) +
        tm_symbols(
          col = "Status",
          palette = c("Active" = "#00b050", "Inactive" = "#ff4d4d"),
          size = 0.6,
          scale = 1,
          title.col = "Status",
          popup.vars = "Station",
          interactive = TRUE
        ) +
        tm_title("Weather Station Map") +
        tm_view(basemaps = "Esri.WorldTopoMap")
    })
    
    # ----------------------------------
    # CDA
    
    observe({
      station_choices <- c("All", unique(weather_active$Station))
      updateSelectInput(inputId = "anova_station", choices = station_choices)
      updateSelectInput(inputId = "poisson_station", choices = station_choices)
    })
    
    # ANOVA
    observeEvent(input$run_anova, {
      withProgress(message = "Running...", value = 0.1, {
        selected_station <- input$anova_station
        selected_year <- year(input$anova_year)
        
        req(selected_station, selected_year)
        
        Sys.sleep(0.3)
        incProgress(0.4, detail = "Filtering...")
        
        specific_anova_data <- weather_active %>%
          filter(
            (selected_station == "All" | Station == selected_station),
            Year == selected_year
          )
        
        # === Handle no data case ===
        if (nrow(specific_anova_data) == 0) {
          output$anova_plot <- renderPlotly({
            ggplotly(
              ggplot() +
                theme_minimal() +
                annotate("text", x = 1, y = 1, label = "No data found for this station and year") +
                xlim(0, 2) + ylim(0, 2) +
                labs(title = "ANOVA Result") +
                ylab("Daily rainfall (mm)")
            )
          })
          
          output$anova_summary <- renderText({
            paste("No data available for", selected_station, "in", year(selected_year))
          })
          
          return()  # Exit the observeEvent early
        }
        
        Sys.sleep(0.3)
        incProgress(0.4, detail = "Plotting...")
        
        output$anova_plot <- renderPlotly({
          p <- ggbetweenstats(
            data = specific_anova_data,
            x = Season,
            y = Daily.Rainfall.Total..mm.,
            type = "p",
            pairwise.comparisons = TRUE,
            pairwise.display = "s",
            output = "plot"
          ) +
            labs(title = paste0("ANOVA Result for ", selected_station, " in ", selected_year)) +
            ylab("Daily rainfall (mm)")
          
          ggplotly(p)
        })
        
        output$anova_summary <- renderText({
          means <- specific_anova_data %>%
            group_by(Season) %>%
            summarise(Mean = mean(Daily.Rainfall.Total..mm., na.rm = TRUE)) %>%
            arrange(desc(Mean))
          
          top_season <- means$Season[1]
          top_value <- round(means$Mean[1], 2)
          
          paste0(
            "In ", selected_year, ", at ", selected_station, ", the rainfall patterns across seasons showed distinct differences. ",
            "The highest average daily rainfall was recorded during the ", top_season,
            " with approximately ", top_value, " mm. ",
            "This CDA (Comparative Data Analysis) module visually compares seasonal rainfall patterns and tests their significance."
          )
        })
      })
    })
      
    # Poisson
    observeEvent(input$run_poisson, {
      withProgress(message = "Running...", value = 0.1, {
        selected_station <- input$poisson_station
        
        req(selected_station)
        
        Sys.sleep(0.3)
        incProgress(0.4, detail = "Filtering...")
        
        specific_poisson_data <- weather_active %>%
          filter(
            (selected_station == "All" | Station == selected_station),
            Daily.Rainfall.Total..mm. > 100
          ) %>%
          count(Year) %>%
          mutate(Year = as.integer(Year))
        
        # handle no data
        if (nrow(specific_poisson_data) == 0 || is.na(sd(specific_poisson_data$Year))) {
          output$poisson_plot <- renderPlotly({
            ggplotly(
              ggplot() +
                annotate("text", x = 1, y = 1, label = "No data or too few year data available for this station.") +
                xlim(0, 2) + ylim(0, 2) +
                theme_void()
            )
          })
          
          output$poisson_summary <- renderUI({
            paste("No extreme rainfall data available for", selected_station)
          })
          
          return()
        }
        
        Sys.sleep(0.3)
        incProgress(0.4, detail = "Fitting...")
        
        model <- glm(n ~ Year, family = "poisson", data = specific_poisson_data)
        
        coef <- summary(model)$coefficients
        beta <- round(coef["Year", "Estimate"], 4)
        pval <- signif(coef["Year", "Pr(>|z|)"], 3)
        
        output$poisson_plot <- renderPlotly({
          p <- ggplot(specific_poisson_data, aes(x = Year, y = n)) +
            geom_point(size = 3, color = "#134178") +
            geom_smooth(method = "glm", method.args = list(family = "poisson"), se = FALSE, color = "#fdae61") +
            labs(title = paste0("Trend of Extreme Rainfall Days for ", selected_station),
                 y = "Extreme Days (>100 mm)", x = "Year") +
            theme_minimal()
          
          ggplotly(p)
        })
        
        output$poisson_summary <- renderUI({
          HTML(paste0(
            "Poisson regression results for extreme rainfall days at <b>", selected_station, "</b>:<br>",
            "Trend coefficient: ", beta, "<br>",
            "p-value: ", pval, "<br>",
            if (pval < 0.05) {
              "<b>There is a statistically significant trend</b> in extreme rainfall days over the years."
            } else {
              "<b>There is no statistically significant trend</b> in extreme rainfall days over the years."
            }
          )
          )
        })
      })
    })
    
    # ----------------------------------
    # Interpolation

    # Kriging
    observe({
      gran <- input$`kriging-granularity`
      
      req(gran)
      
      output$`kriging-date_picker` <- renderUI({
        
        if (gran == "Daily") {
          airDatepickerInput(
            inputId = "kriging-selected_date",
            label = "Select Date:",
            minDate = "2018-01-01",
            maxDate = "2024-12-31",
            view = "days",
            dateFormat = "yyyy-MM-dd",
            autoClose = TRUE
          )
        } else if (gran == "Monthly") {
          airMonthpickerInput(
            inputId = "kriging-selected_date",
            label = "Select Month:",
            minDate = "2018-01",
            maxDate = "2024-12",
            view = "months",
            dateFormat = "yyyy-MM",
            autoClose = TRUE
          )
        } else if (gran == "Yearly") {
          airYearpickerInput(
            inputId = "kriging-selected_date",
            label = "Select Year:",
            minDate = "2018",
            maxDate = "2024",
            view = "years",
            dateFormat = "yyyy",
            autoClose = TRUE
          )
        }
      })
    })
    
    rain_map_result <- reactiveVal()
    rain_selected_date <- reactiveVal()
    rain_selected_gran <- reactiveVal()
    v_auto_result <- reactiveVal()
  
    observeEvent(input$submit_kriging, {
      withProgress(message = "Running interpolation...", value = 0.1, {
      
        granularity <- input$`kriging-granularity`
        selected_date <- input$`kriging-selected_date`
        
        req(granularity)
        req(selected_date)
        
        if (granularity == "Monthly") {
          specific_month <- monthly_data_sf %>%
            filter(floor_date(MonthYear, "month") == selected_date)
          
          if (nrow(specific_month) == 0) {
            showNotification("No data available for selected time.", type = "error")
            return(NULL)
          }
          
          # Simulate progress steps
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Predicting...")
          
          v_auto <- autofitVariogram(MonthlyRain ~ 1, specific_month)
          
          v_auto_result(v_auto)
          
          k <- gstat(formula = MonthlyRain ~ 1, model = v_auto$var_model, data = specific_month)
          
          resp <- predict(k, coop)
          resp$x <- st_coordinates(resp)[,1]
          resp$y <- st_coordinates(resp)[,2]
          resp$pred <- resp$var1.pred
          kpred <- terra::rasterize(resp, grid, field = "pred")
          
          # Simulate progress steps
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Generating map...")
          
          rain_selected_gran("monthly")
          rain_selected_date(selected_date)
          rain_map_result(kpred)
          
        } else if (granularity == "Daily") {
          specific_date <- weather_sf %>%
            filter(format(Date, "%Y-%m-%d") == selected_date)
          
          if (nrow(specific_date) == 0) {
            showNotification("No data available for selected time.", type = "error")
            return(NULL)
          }
          
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Predicting...")
          
          v_auto <- autofitVariogram(Daily.Rainfall.Total..mm. ~ 1, specific_date)
          
          v_auto_result(v_auto)
          
          k <- gstat(formula = Daily.Rainfall.Total..mm. ~ 1, model = v_auto$var_model, data = specific_date)
          
          resp <- predict(k, coop)
          resp$x <- st_coordinates(resp)[,1]
          resp$y <- st_coordinates(resp)[,2]
          resp$pred <- resp$var1.pred
          kpred <- terra::rasterize(resp, grid, field = "pred")
          
          # Simulate progress steps
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Generating map...")
          
          rain_selected_gran("daily")
          rain_selected_date(selected_date)
          rain_map_result(kpred)
          
        } else if (granularity == "Yearly") {
          specific_year <- yearly_data_sf %>%
            filter(Year == year(selected_date))
          
          if (nrow(specific_year) == 0) {
            showNotification("No data available for selected time.", type = "error")
            return(NULL)
          }
          
          # Simulate progress steps
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Predicting...")
          
          v_auto <- autofitVariogram(YearlyRain ~ 1, specific_year)
          
          v_auto_result(v_auto)
          
          k <- gstat(formula = YearlyRain ~ 1, model = v_auto$var_model, data = specific_year)
          
          resp <- predict(k, coop)
          resp$x <- st_coordinates(resp)[,1]
          resp$y <- st_coordinates(resp)[,2]
          resp$pred <- resp$var1.pred
          kpred <- terra::rasterize(resp, grid, field = "pred")
          
          # Simulate progress steps
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Generating map...")
          
          rain_selected_gran("yearly")
          rain_selected_date(selected_date)
          rain_map_result(kpred)
          
          }
        })
      })
    
    output$rain_map <- renderPlot({
      req(rain_map_result())
      req(rain_selected_date())
      req(rain_selected_gran())
      
      title_date <- if (rain_selected_gran() == "monthly") {
        format(rain_selected_date(), "%Y %b")
      } else if (rain_selected_gran() == "yearly") {
        format(rain_selected_date(), "%Y")
      }

      tmap_mode("plot")

      map <- tm_shape(rain_map_result()) +
        tm_raster(
          col.scale = tm_scale_continuous(values = "brewer.blues"),
          col.legend = tm_legend(title = "Total rainfall (mm)")
        ) +
        tm_title(text = paste("Rainfall Distribution in", title_date)) +
        tm_layout(frame = TRUE) +
        tm_compass(type = "8star", size = 2) +
        tm_scalebar(position = c("left", "bottom")) +
        tm_grid(alpha = 0.2)

      print(map)
    })
    
    output$kriging_summary <- renderUI({
      HTML(paste0(
        "<div style='font-size:16px;'>",
        "This map uses <b>Ordinary Kriging</b> to visualize the spatial distribution of total rainfall. ",
        "Ordinary Kriging is a geostatistical method that estimates rainfall in unmeasured areas based on known station data and their spatial relationships. ",
        "Darker areas indicate regions with higher estimated rainfall, while lighter areas suggest lower rainfall.",
        "</div>"
      ))
    })
    
    output$v_auto_plot <- renderPlot({
      req(v_auto_result())
      plot(v_auto_result())
    })
    
    output$variogram_summary <- renderUI({
      HTML(paste0(
        "<div style='font-size:16px;'>",
        "The variogram helps determine how spatially correlated rainfall values are over distance.",
        "A good fit improves the accuracy of spatial predictions.<br><br>",
        "Model Parameters:<br>",
        "<b>Nugget</b>: Measurement error or small-scale variability.<br>",
        "<b>Sill</b>: The level at which the variogram levels off - the total variance.<br>",
        "<b>Range</b>: The distance beyond which points are no longer spatially correlated.",
        "</div>"
      ))
    })
      
    
    output$kriging_results <- renderUI({
      req(rain_map_result())
      req(v_auto_result)
      
      div(
        h4("Ordinary Kringing Map"),
        plotOutput("rain_map"),
        
        br(),

        htmlOutput("kriging_summary"),
        
        br(),
        
        h4("Autofitted Variogram"),
        plotOutput("v_auto_plot"),
        
        br(),
        
        htmlOutput("variogram_summary")
      )
      })
    
    # IDW
    observe({
      gran <- input$`idw-granularity`
      
      req(gran)
      
      output$`idw-date_picker` <- renderUI({
        
        if (gran == "Daily") {
          airDatepickerInput(
            inputId = "idw-selected_date",
            label = "Select Date:",
            minDate = "2018-01-01",
            maxDate = "2024-12-31",
            view = "days",
            dateFormat = "yyyy-MM-dd",
            autoClose = TRUE
          )
        } else if (gran == "Monthly") {
          airMonthpickerInput(
            inputId = "idw-selected_date",
            label = "Select Month:",
            minDate = "2018-01",
            maxDate = "2024-12",
            view = "months",
            dateFormat = "yyyy-MM",
            autoClose = TRUE
          )
        } else if (gran == "Yearly") {
          airYearpickerInput(
            inputId = "idw-selected_date",
            label = "Select Year:",
            minDate = "2018",
            maxDate = "2024",
            view = "years",
            dateFormat = "yyyy",
            autoClose = TRUE
          )
        }
      })
    })
    
    idw_map_result <- reactiveVal()
    idw_selected_gran <- reactiveVal()
    idw_selected_date <- reactiveVal()
    
    observeEvent(input$submit_idw, {
      withProgress(message = "Running interpolation...", value = 0.1, {
        
        granularity <- input$`idw-granularity`
        selected_date <- input$`idw-selected_date`
        nmax <- input$idw_nmax
        idp <- input$idw_idp
        
        req(granularity)
        req(selected_date)
        
        if (granularity == "Monthly") {
          specific_month <- monthly_data_sf %>%
            filter(floor_date(MonthYear, "month") == selected_date)
          
          if (nrow(specific_month) == 0) {
            showNotification("No data available for selected time.", type = "error")
            return(NULL)
          }
          
          # Simulate progress steps
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Predicting...")
          
          res <- gstat(formula = MonthlyRain ~ 1, 
                       locations = specific_month, 
                       nmax = nmax,
                       set = list(idp = idp))
          
          resp <- predict(res, coop)
          resp$x <- st_coordinates(resp)[,1]
          resp$y <- st_coordinates(resp)[,2]
          resp$pred <- resp$var1.pred
          
          pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
          
          # Simulate progress steps
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Generating map...")
          
          idw_selected_gran("monthly")
          idw_selected_date(selected_date)
          idw_map_result(pred)
          
        } else if (granularity == "Daily") {
          specific_date <- weather_sf %>%
            filter(format(Date, "%Y-%m-%d") == selected_date)
          
          if (nrow(specific_date) == 0) {
            showNotification("No data available for selected time.", type = "error")
            return(NULL)
          }
          
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Predicting...")
          
          res <- gstat(formula = Daily.Rainfall.Total..mm. ~ 1, 
                       locations = specific_date, 
                       nmax = nmax,
                       set = list(idp = idp))
          
          resp <- predict(res, coop)
          resp$x <- st_coordinates(resp)[,1]
          resp$y <- st_coordinates(resp)[,2]
          resp$pred <- resp$var1.pred
          
          pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
          
          # Simulate progress steps
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Generating map...")
          
          idw_selected_gran("daily")
          idw_selected_date(selected_date)
          idw_map_result(pred)
          
        } else if (granularity == "Yearly") {
          specific_year <- yearly_data_sf %>%
            filter(Year == year(selected_date))
          
          if (nrow(specific_year) == 0) {
            showNotification("No data available for selected time.", type = "error")
            return(NULL)
          }
          
          # Simulate progress steps
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Predicting...")
          
          res <- gstat(formula = YearlyRain ~ 1, 
                       locations = specific_year, 
                       nmax = nmax,
                       set = list(idp = idp))
          
          resp <- predict(res, coop)
          resp$x <- st_coordinates(resp)[,1]
          resp$y <- st_coordinates(resp)[,2]
          resp$pred <- resp$var1.pred
          
          pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
          
          # Simulate progress steps
          Sys.sleep(0.3)
          incProgress(0.4, detail = "Generating map...")
          
          idw_selected_gran("yearly")
          idw_selected_date(selected_date)
          idw_map_result(pred)
          
        }
      })
    })
    
    output$idw_map <- renderPlot({
      req(idw_map_result())
      req(idw_selected_date())
      req(idw_selected_gran())
      
      title_date <- if (idw_selected_gran() == "monthly") {
        format(idw_selected_date(), "%Y %b")
      } else if (idw_selected_gran() == "yearly") {
        format(idw_selected_date(), "%Y")
      }
      
      tmap_mode("plot")
      
      map <- tm_shape(idw_map_result()) +
        tm_raster(
          col.scale = tm_scale_continuous(values = "brewer.blues"),
          col.legend = tm_legend(title = "Total rainfall (mm)")
        ) +
        tm_title(text = paste("Rainfall Distribution in", title_date)) +
        tm_layout(frame = TRUE) +
        tm_compass(type = "8star", size = 2) +
        tm_scalebar(position = c("left", "bottom")) +
        tm_grid(alpha = 0.2)
      
      print(map)
    })
    
    output$idw_summary <- renderUI({
      HTML(paste0(
        "<div style='font-size:16px;'>",
        "This map uses <b>Inverse Distance Weighting (IDW)</b> to estimate rainfall based on surrounding weather stations.",
        "IDW assumes that rainfall at unknown locations is more influenced by nearby stations than distant ones.<br><br>",
        "<b>nmax</b>: The maximum number of neighboring stations considered for each prediction.<br>",
        "Higher values use more stations (smoother results), while lower values make estimates more localized.<br><br>",
        "<b>idp</b>: The inverse distance power determines how much weight is given to closer stations.<br>",
        "Higher values emphasize nearby stations (sharper transitions), while lower values create smoother gradients.",
        "</div>"
      ))
    })
    
    output$idw_results <- renderUI({
      req(idw_map_result())
      
      div(
        h4("IDW Interpolation Map"),
        plotOutput("idw_map"),
        
        br(),

        htmlOutput("idw_summary")
      )
    })
    
    # ----------------------------------
    # Clustering
    
    cluster_map_result <- reactiveVal()
    cluster_model <- reactiveVal()
    
    centroid_method <- reactive({
      switch(input$clust_distance,
             "dtw_basic" = "dba",
             "sbd" = "shape",
             "euclidean" = "mean",
             "gak" = "pam")
    })
    
    output$clust_centroid_text <- renderText({
      req(centroid_method())
      centroid_method()
    })
    
    observeEvent(input$run_clustering, {
      withProgress(message = "Running interpolation...", value = 0.1, {
      
        type <- input$clust_type
        k = input$num_clusters
        distance = input$clust_distance
        
        req(type)
        req(k)
        req(distance)
        
        Sys.sleep(0.3)
        incProgress(0.4, detail = "Fitting model...")
        
        # Perform clustering
        model <- tsclust(
          series = rain_list,
          type = type,
          k = k,
          distance = distance,
          centroid = centroid_method(),
          seed = 123
        )
        
        cluster_model(model)
        
        # Add cluster labels to spatial data
        station_clusters <- data.frame(
          Station = names(rain_list),
          Cluster = as.factor(model@cluster)
        )
        
        clustered_stations <- monthly_active_data_sf %>%
          left_join(station_clusters, by = "Station")
        
        Sys.sleep(0.3)
        incProgress(0.4, detail = "Generating map...")
        
        tmap_mode("view")
        
        # Map
        map <- tm_shape(sg_boundary) +
            tm_polygons(col = "grey90", border.col = "white") +
            tm_shape(clustered_stations) +
            tm_symbols(
              col = "Cluster",
              palette = "hcl.set2",
              size = 0.6,
              scale = 1,
              title.col = "Cluster",
              popup.vars = "Station",
              interactive = TRUE
            ) +
            tm_layout(
              title = "Time Series Clusters by Station",
              legend.outside = TRUE,
              frame = FALSE
            ) +
            tm_view(basemaps = "Esri.WorldTopoMap")
        
        cluster_map_result(map)
      })
    })
    
    output$cluster_map <- renderTmap({
      req(cluster_map_result())
      cluster_map_result()
      })
    
    output$series_plot <- renderPlot({
      req(cluster_model())
      plot(cluster_model(), type = "series")
      })
    
    output$metrics <- renderPrint({
      req(cluster_model())
      cvi(cluster_model(), type = "internal")
      })
    
    output$clustering_results <- renderUI({
      req(cluster_map_result())
      
      div(
        h4("Spatial Cluster Map"),
        tmapOutput("cluster_map"),
        
        br(),
        
        h5("Clustered Series Plot"),
        plotOutput("series_plot"),
        
        br(),
        
        h5("Evaluation Metrics"),
        verbatimTextOutput("metrics")
      )
    })
    
    # ----------------------------------
    # Forecasting
    
    forecast_map_result <- reactiveVal()
    
    observeEvent(input$run_forecast, {
      selected_date <- as.Date(input$forecast_date)
      
      req(selected_date)
      
      forecast_data <- weather_predicted_sf %>%
        filter(Date == selected_date)
      
      print(forecast_data$Date)
      
      # Z-score anomaly detection
      mean_rain <- mean(forecast_data$Daily.Rainfall.Total..mm., na.rm = TRUE)
      sd_rain <- sd(forecast_data$Daily.Rainfall.Total..mm., na.rm = TRUE)
      
      forecast_data <- forecast_data %>%
        mutate(
          z_score = (Daily.Rainfall.Total..mm. - mean_rain) / sd_rain,
          is_anomaly = abs(z_score) > 2
        )
      
        tmap_mode("view")
        
        map <- tm_shape(sg_boundary) +
          tm_polygons(col = "grey90", border.col = "white") +
          
          tm_shape(forecast_data) +
          tm_symbols(
            col = "is_anomaly",
            palette = c("FALSE" = "#2c7fb8", "TRUE" = "#d7191c"),
            size = "Daily.Rainfall.Total..mm.",
            scale = 1,
            title.size = "Rainfall (mm)",
            title.col = "Anomaly",
            popup.vars = c("Station", "Daily.Rainfall.Total..mm.")
          ) +
          tm_layout(
            title = paste("Rainfall Forecast & Anomalies for", selected_date),
            legend.outside = TRUE,
            frame = FALSE
          ) +
          tm_view(basemaps = "Esri.WorldTopoMap")
        
        forecast_map_result(map)
    })
    
    output$forecast_map <- renderTmap({
      req(forecast_map_result())
      forecast_map_result()
    })
    
    output$forecasting_results <- renderUI({
      req(forecast_map_result())
      tmapOutput("forecast_map")
    })
    
}

