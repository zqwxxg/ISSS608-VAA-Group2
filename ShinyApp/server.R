library(shiny)
library(shinyWidgets)
library(lubridate)
library(tidyverse)
library(tsibble)
library(sf)
library(terra)
library(tmap)
library(gstat)
library(automap)
library(viridis)

tm_check_fix()

weather <- read.csv("data/weather.csv")
monthly_data <- read.csv("data/monthly_data.csv")
yearly_data <- read.csv("data/yearly_data.csv")

weather$Date <- as.Date(weather$Date)
monthly_data$MonthYear <- as.Date(monthly_data$MonthYear)
yearly_data$Year <- as.numeric(yearly_data$Year)

weather_sf <- st_as_sf(weather,
                       coords = c("Longitude", "Latitude"),
                       crs = 4326) %>%
  st_transform(crs = 3414)

# monthly_data <- weather %>%
#   mutate(MonthYear = floor_date(Date, "month")) %>%
#   group_by(Station, MonthYear) %>%
#   summarise(
#     MonthlyRain = sum(Daily.Rainfall.Total..mm., na.rm = TRUE),
#     Latitude = first(Latitude),
#     Longitude = first(Longitude),
#     .groups = "drop"
#     )
# 
# yearly_data <- weather %>%
#   mutate(Year = year(Date)) %>%
#   group_by(Station, Year) %>%
#   summarise(
#     YearlyRain = sum(Daily.Rainfall.Total..mm., na.rm = TRUE),
#     Latitude = first(Latitude),
#     Longitude = first(Longitude),
#     .groups = "drop"
#     )

monthly_data_sf <- st_as_sf(monthly_data, coords = c("Longitude", "Latitude"), crs = 4326) %>%
 st_transform(crs = 3414)

yearly_data_sf <- st_as_sf(yearly_data, coords = c("Longitude", "Latitude"), crs = 4326) %>%
 st_transform(crs = 3414)

sg_boundary <- st_read(dsn = "data/geospatial", layer = "MPSZ-2019") %>%
 st_transform(crs = 3414)

grid <- terra::rast(sg_boundary, nrows = 690, ncols = 1075)
xy <- terra::xyFromCell(grid, 1:ncell(grid))
coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = st_crs(sg_boundary))
coop <- st_filter(coop, sg_boundary)

server <- function(input, output, session) {
  
    # ----------------------------------
    # Home station map
    output$station_map <- renderTmap({
      
      tmap_mode("view")
      tm_shape(weather_sf) +
        tm_symbols(
          col = "Status",
          palette = c("Active" = "#00b050", "Inactive" = "#ff4d4d"),
          size = 0.6,                                                
          scale = 1,                                                 
          title.col = "Status",
          popup.vars = "Station",
          interactive = TRUE) +
        tm_title("Weather Station Map") +
        tm_view(basemaps.server = "Esri.WorldTopoMap")
    })
    
    # Dynamically render the correct date selector
    observe({
      gran <- input$`interpolation-granularity`
      
      req(gran)
      
      output$`interpolation-date_picker` <- renderUI({
        
        if (gran == "Daily") {
          airDatepickerInput(
            inputId = "interpolation-selected_date",
            label = "Select Date:",
            minDate = "2018-01-01",
            maxDate = "2024-12-31",
            view = "days",
            dateFormat = "yyyy-MM-dd",
            autoClose = TRUE
          )
        } else if (gran == "Monthly") {
          airMonthpickerInput(
            inputId = "interpolation-selected_date",
            label = "Select Month:",
            minDate = "2018-01",
            maxDate = "2024-12",
            view = "months",
            dateFormat = "yyyy-MM",
            autoClose = TRUE
          )
        } else if (gran == "Yearly") {
          airYearpickerInput(
            inputId = "interpolation-selected_date",
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
  
    observeEvent(input$submit_interpolation, {
      showNotification("Submit button clicked!", type = "message")
      
      granularity <- input$`interpolation-granularity`
      selected_date <- input$`interpolation-selected_date`
      
      req(granularity)
      req(selected_date)
      
      tmap_mode("plot")
      
      if (granularity == "Monthly") {
        specific_month <- monthly_data_sf %>%
          filter(floor_date(MonthYear, "month") == selected_date)
        
        if (nrow(specific_month) == 0) return(NULL)
        
        v_auto <- autofitVariogram(MonthlyRain ~ 1, specific_month)
        k <- gstat(formula = MonthlyRain ~ 1, model = v_auto$var_model, data = specific_month)
        
        resp <- predict(k, coop)
        resp$x <- st_coordinates(resp)[,1]
        resp$y <- st_coordinates(resp)[,2]
        resp$pred <- resp$var1.pred
        kpred <- terra::rasterize(resp, grid, field = "pred")
        
        map <- tm_shape(kpred) +
          tm_raster(
            col.scale = tm_scale_continuous(values = "brewer.blues"),
            col.legend = tm_legend(title = "Total rainfall (mm)")
            ) +
          tm_title(text = paste("Monthly Rainfall Distribution -", format(selected_date, "%Y %b"))) +
          tm_layout(frame = TRUE, outer.bg.color = "#f0f5fa") +
          tm_compass(type = "8star", size = 2) +
          tm_scalebar(position = c("left", "bottom")) +
          tm_grid(alpha = 0.2)
        
        rain_map_result(map)
        
      } else if (granularity == "Daily") {
        specific_date <- weather_sf %>%
          filter(format(Date, "%Y-%m-%d") == selected_date)
        
        if (nrow(specific_date) == 0) return(NULL)
        
        v_auto <- autofitVariogram(Daily.Rainfall.Total..mm. ~ 1, specific_date)
        k <- gstat(formula = Daily.Rainfall.Total..mm. ~ 1, model = v_auto$var_model, data = specific_date)
        
        resp <- predict(k, coop)
        resp$x <- st_coordinates(resp)[,1]
        resp$y <- st_coordinates(resp)[,2]
        resp$pred <- resp$var1.pred
        kpred <- terra::rasterize(resp, grid, field = "pred")
        
        map <- tm_shape(kpred) +
          tm_raster(
            col.scale = tm_scale_continuous(values = "brewer.blues"),
            col.legend = tm_legend(title = "Total rainfall (mm)")
          ) +
          tm_title(text = paste("Daily Rainfall Distribution -", selected_date)) +
          tm_layout(frame = TRUE, outer.bg.color = "#f0f5fa") +
          tm_compass(type = "8star", size = 2) +
          tm_scalebar(position = c("left", "bottom")) +
          tm_grid(alpha = 0.2)
        
        rain_map_result(map)
        
      } else if (granularity == "Yearly") {
        specific_year <- yearly_data_sf %>%
          filter(Year == selected_date)
        
        if (nrow(specific_year) == 0) return(NULL)
        
        v_auto <- autofitVariogram(YearlyRain ~ 1, specific_year)
        k <- gstat(formula = YearlyRain ~ 1, model = v_auto$var_model, data = specific_year)
        
        resp <- predict(k, coop)
        resp$x <- st_coordinates(resp)[,1]
        resp$y <- st_coordinates(resp)[,2]
        resp$pred <- resp$var1.pred
        kpred <- terra::rasterize(resp, grid, field = "pred")
        
        map <- tm_shape(kpred) +
          tm_raster(
            col.scale = tm_scale_continuous(values = "brewer.blues"),
            col.legend = tm_legend(title = "Total rainfall (mm)")
          ) +
          tm_title(text = paste("Yearly Rainfall Distribution -", selected_date)) +
          tm_layout(frame = TRUE, outer.bg.color = "#f0f5fa") +
          tm_compass(type = "8star", size = 2) +
          tm_scalebar(position = c("left", "bottom")) +
          tm_grid(alpha = 0.2)
        
        rain_map_result(map)
      }
      
    
      output$rain_map <- renderTmap({
        req(rain_map_result())
        rain_map_result() 
        })
      })
    
    }

