library(tidyverse)
library(sf)
library(tidyverse)
library(stringr)
library(spData)
library(scales)
library(RColorBrewer)
library(shiny)
library(plotly)

setwd("C:/Users/guill/OneDrive/Documents/Data and Programming II/HW2")
df_educ_chi <- read_csv("educ_shi.csv")
zip_list <- df_educ_chi$`ZIP Code`

# shiny
ui <- fluidPage(
  fluidRow(
    column(width = 3,
           tags$img(src = "https://d11jve6usk2wa9.cloudfront.net/platform/10747/assets/logo.png",
                    height = 90, width = 250)),
    column(width = 4,
           align = "center",
           tags$h3("Performance Level of Chicago Public Schools - School Year 2011"),
           tags$hr())
  ),
  fluidRow(
    column(width = 12,
           offset = 2,
           align = "center",
           checkboxInput(inputId = "bot",
                         label = "Toggle Streets",
                         value = FALSE))
  ),
  fluidRow(
    column(width = 12,
           align = "center",
           plotlyOutput("street"))
  ),
  fluidRow(
    column(width = 4,
           align = "center",
           selectizeInput(inputId = "zipcode",
                          label = "Choose a zipcode to see more information",
                          choices = zip_list))
  ),
  fluidRow(
    column(width = 6,
           plotlyOutput("plot")),
     column(width = 6,
           tableOutput("table"))
  )
  )

server <- function(input, output) {
  
  df_educ_chi <- read_csv("educ_shi.csv")
  shape_chi <- st_read("geo_export_fbe7a12f-9b8f-4ed9-b1eb-9b5cc7027af5.shp")
  shape_chi$zip <- as.numeric(shape_chi$zip)
  educ_shape_chi <- inner_join(df_educ_chi, shape_chi, by = c("ZIP Code" = "zip"))
  
  shape_sts <- st_read("Major_Streets.shp")
  st_crs(shape_chi) == st_crs(shape_sts)
  shape_chi <- st_transform(shape_chi, 4326)
  shape_sts <- st_transform(shape_sts, 4326)
  
  output$street <- renderPlotly({
    if (input$bot == FALSE) {
      map_peformance <- ggplot() +
        geom_sf(data = educ_shape_chi, aes(geometry = geometry,
                                    fill = `CPS Performance Policy Level`)) +
        theme_minimal() +
        labs(title = "Overview of the Performance of Chicago Public Schools in 2011") +
        theme(plot.title = element_text(hjust = 0.5))
      map_peformance
         
    } else if (input$bot == TRUE) {
      map_peformance <- ggplot() +
        geom_sf(data = educ_shape_chi, aes(geometry = geometry,
                                           fill = `CPS Performance Policy Level`)) +
        geom_sf(data = shape_sts) +
        theme_minimal() +
        labs(title = "Overview of the Performance of Chicago Public Schools in 2011") +
        theme(plot.title = element_text(hjust = 0.5))
      map_peformance
    }
  })
  
  data_plot <- reactive({
    dst <- df_educ_chi %>%
      filter(`ZIP Code` == input$zipcode) %>% 
      ggplot(aes(`CPS Performance Policy Level`)) +
      geom_bar() +
      labs(title = "Number of Schools by Performance Level")
    ggplotly(dst)
    })
  
  output$plot <- renderPlotly({data_plot()
    })
  
  data_table <- reactive({df_educ_chi %>% 
      filter(`ZIP Code` == input$zipcode) %>%
      select(-`ZIP Code`) %>% 
      rename("# Participants in Library Summer Program" = `2011 PARTICIPANTS`)
  })
  
  output$table <- renderTable({data_table()
  })
}

shinyApp(ui = ui, server = server)

