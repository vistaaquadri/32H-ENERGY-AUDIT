# Load required libraries
library(ggradar)
library(ggplot2)
library(dplyr)
library(tibble)
library(scales)
library(ggrepel)
library(shiny)
library(fmsb)
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(gridExtra)  # For arranging radar chart and table
library(grid)       # For graphical layout
library(rsconnect)
library(shinyjs)
library(slickR)
library(leaflet)

###############################################################################################################################################
main_audit_df <- read_csv("database/Main_Audit.csv")




###############################################################################################################################################
# Sample hospital data
hospital_data_sample <- data.frame(
  id = 1:32,
  name = paste("Hospital", LETTERS[1:3], rep(1:11, length.out = 32)),
  total_demand = rep(500, 32),
  peak_load = rep(2000, 32),
  roof_size = rep("300 square meter", 32),
  solar_capacity = rep("1,000 kWp", 32),
  air_quality = sample(1:5, 32, replace = TRUE)
)





server <- function(input, output, session) {
  
  runjs("console.log('Shiny app loaded');")
  
  output$finance_plot <- renderPlotly({
    # Sample data
    months <- month.abb
    consumption <- c(70, 50, 35, 30, 40, 20, 55, 25, 60, 65, 75, 40)
    spend <- c(40, 50, 35, 80, 35, 20, 50, 40, 85, 90, 45, 20)
    
    plot_ly() %>%
      add_trace(x = months, y = consumption, name = "12 months historical consumption from Grid (kWh)",
                type = 'scatter', mode = 'lines+markers',
                fill = 'tozeroy',
                line = list(color = 'green'), marker = list(color = 'green')) %>%
      add_trace(x = months, y = spend, name = "12 months historical spend from Grid (kWh)",
                type = 'scatter', mode = 'lines+markers',
                fill = 'tonexty',
                line = list(color = 'red'), marker = list(color = 'red')) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""),
             legend = list(orientation = 'h', x = 0.1, y = -0.2),
             margin = list(t = 10))
  })
  
  output$consumption_chart <- renderPlot({
    months <- month.abb
    values <- round(runif(12, 20, 90))
    
    plot(1:12, values, type = "o", col = "forestgreen", lwd = 2,
         xaxt = "n", xlab = "", ylab = "kWh", main = "2025")
    axis(1, at = 1:12, labels = months)
  })
  
  
  output$avg_monthly_comparison <- renderPlot({
    barplot(height = c(56, 64),
            names.arg = c("Current solution", "Solar"),
            col = c("deepskyblue", "lightgreen"),
            ylim = c(0, 100),
            main = "",
            xlab = "", ylab = "kWh")
    text(x = c(0.7, 1.9), y = c(56, 64), labels = c("56 kWh", "64 kWh"), pos = 3)
  })
  
  output$avg_monthly_spend <- renderPlot({
    barplot(height = c(80, 60),
            names.arg = c("Current solution", "Solar"),
            col = c("deepskyblue", "lightgreen"),
            ylim = c(0, 100),
            main = "",
            xlab = "", ylab = "₦")
    text(x = c(0.7, 1.9), y = c(80, 60), labels = c("₦80,000", "₦60,000"), pos = 3)
  })
  
  output$energy_mix_chart <- renderPlot({
    labels <- c("Grid", "Generator", "Outage")
    values <- c(20, 30, 50)
    colors <- c("orange", "seagreen", "royalblue")
    
    pie(values,
        labels = paste0(labels, " ", values, "%"),
        col = colors,
        main = "",
        init.angle = 90,
        clockwise = TRUE,
        border = "white"
    )
    
    # Draw center white circle to create donut effect
    symbols(0, 0, circles = 0.4, add = TRUE, inches = FALSE, bg = "white")
    text(0, 0, "100%", cex = 2.5, font = 2)
  })
  
  output$rooftopCarousel <- renderSlickR({
    imgs <- c(
      "https://via.placeholder.com/800x300?text=Drone+View+1",
      "https://via.placeholder.com/800x300?text=Drone+View+2",
      "https://via.placeholder.com/800x300?text=Map+View"
    )
    slickR(imgs, height = 300)
  })
  
  output$monthly_gen_forecast <- renderPlot({
    values <- c(56, 64, 76, 78, 70, 37)
    names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
    barplot(values,
            names.arg = names,
            col = "lightgreen",
            ylim = c(0, 100),
            main = "", ylab = "kWh")
    text(x = seq_along(values), y = values, labels = values, pos = 3)
  })
  
  output$pv_gen_mix <- renderPlot({
    bar_values <- c(56, 64, 76, 78, 70, 37)
    line_values <- c(90, 75, 85, 95, 90, 70)
    names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
    
    barplot(bar_values,
            names.arg = names,
            col = "firebrick3",
            ylim = c(0, 100),
            beside = TRUE)
    
    lines(x = seq_along(line_values), y = line_values, type = "o", col = "lightgreen", lwd = 2, pch = 16)
    legend("topright", legend = c("Generator", "Solar PV"), fill = c("firebrick3", "lightgreen"), bty = "n")
  })
  
  
  output$geo_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 3.3792, lat = 6.5244, zoom = 12) %>%  # Centered on Lagos
      addMarkers(lng = 3.3500, lat = 6.5095, popup = "Lagos University Teaching Hospital") %>%
      addMarkers(lng = 3.3216, lat = 6.4617, popup = "Echo Spring Hotel") %>%
      addMarkers(lng = 3.3775, lat = 6.4718, popup = "Apapa Amusement Park") %>%
      addMarkers(lng = 3.4061, lat = 6.4216, popup = "Federal Palace Hotel and Casino")
  })
  
  page_size <- 9
  current_page <- reactiveVal(1)
  
  observeEvent(input$nextPage, {
    if (current_page() * page_size < nrow(hospital_data_sample)) {
      current_page(current_page() + 1)
    }
  })
  
  observeEvent(input$prevPage, {
    if (current_page() > 1) {
      current_page(current_page() - 1)
    }
  })
  
  output$page_numbers <- renderUI({
    pages <- ceiling(nrow(hospital_data_sample) / page_size)
    tagList(
      lapply(1:pages, function(i) {
        actionButton(paste0("page_", i), label = i, class = ifelse(i == current_page(), "btn-success", "btn-default"))
      })
    )
  })
  
  observe({
    pages <- ceiling(nrow(hospital_data_sample) / page_size)
    lapply(1:pages, function(i) {
      observeEvent(input[[paste0("page_", i)]], {
        current_page(i)
      })
    })
  })
  
  output$hospital_cards <- renderUI({
    start <- (current_page() - 1) * page_size + 1
    end <- min(current_page() * page_size, nrow(hospital_data_sample))
    rows <- split(hospital_data_sample[start:end, ], f = ceiling(seq_len(end - start + 1) / 3))

    tagList(
      lapply(rows, function(row) {
        fluidRow(
          lapply(1:nrow(row), function(i) {
            hosp <- row[i, ]
            column(4,
                   div(class = "card", onclick = sprintf("Shiny.setInputValue('showDetail', %d, {priority: 'event'})", hosp$id),
                       div(class = "hospital-title", hosp$name),
                       p(HTML(sprintf("<b>Total Demand:</b> %s kW<br><b>Peak Load:</b> %s kW<br><b>Roof Top Size:</b> %s<br><b>Solar Capacity:</b> %s",
                                      hosp$total_demand, hosp$peak_load, hosp$roof_size, hosp$solar_capacity))),
                       span("Air Quality: "),
                       HTML(paste(rep("<span class='star'>&#9733;</span>", hosp$air_quality),
                                  rep("<span class='star star-grey'>&#9733;</span>", 5 - hosp$air_quality)))
                   )
            )
          })
        )
      })
    )
  })
  
  output$energyChart <- renderPlot({
    slices <- c(40, 30, 20, 10)
    labels <- c("Grid", "Generator", "Solar", "IPP")
    colors <- c("#28a745", "#ffc107", "#007bff", "#6c757d")
    
    pie(slices, labels = labels, col = colors, main = "Energy Source Distribution")
  })
  
  
  
  # output$hospital_cards <- renderUI({
  #   tagList(
  #     lapply(1:nrow(hospital_data_sample), function(i) {
  #       hosp <- hospital_data_sample[i, ]
  #       div(class = "card",
  #           style = "cursor:pointer;",
  #           onclick = sprintf("Shiny.setInputValue('showDetail', %d, {priority: 'event'})", hosp$id),
  #           div(class = "hospital-title", hosp$name),
  #           p(HTML(sprintf("<b>Total Demand:</b> %s<br><b>Peak Load:</b> %s", 
  #                          hosp$total_demand, hosp$peak_load)))
  #       )
  #     })
  #   )
  # })
  
  
  # observeEvent(input$showDetail, {
  #   hosp <- hospital_data_sample[hospital_data_sample$id == input$showDetail, ]
  #   output$detailContent <- renderUI({
  #     tagList(
  #       h3(hosp$name),
  #       tags$img(src = "https://placehold.co/800x300", width = "100%"),
  #       br(), br(),
  #       
  #       tabsetPanel(
  #         id = "hospital_tabs",
  #         
  #         tabPanel("Site Assessment",
  #                  br(),
  #                  fluidRow(
  #                    column(6,
  #                           h4("General Information"),
  #                           p("Address: Address line, John Doe street, Neo Avenue, Lagos"),
  #                           p("LGA: Neo Marshal LGA"),
  #                           p("Contact: Kenny Ken, 08098271188"),
  #                           p("Email: Kennyken@hospital.com.ng"),
  #                           p("Facility Manager"),
  #                           p("Bed Capacity: 3,000 beds"),
  #                           p("Size: 3,000 square meters")
  #                    ),
  #                    column(6,
  #                           h4("Power Info"),
  #                           p("Grid Connection: YES"),
  #                           p("Generator Use: YES"),
  #                           p("Solar Installed: YES"),
  #                           p("Alt Power: YES"),
  #                           p("Roof Accessible: YES")
  #                    )
  #                  )
  #         ),
  #         
  #         tabPanel("Environmental Assessment",
  #                  br(),
  #                  p("Environmental assessment details go here.")
  #         ),
  #         
  #         tabPanel("Energy Assessment",
  #                  br(),
  #                  p("Energy metrics and visualization go here.")
  #         ),
  #         
  #         tabPanel("Viability Assessment",
  #                  br(),
  #                  p("Viability and economic analysis here.")
  #         ),
  #         
  #         tabPanel("Summary & Recommendations",
  #                  br(),
  #                  p("Summary and action points.")
  #         )
  #       )
  #     )
  #   })
  #   
  #   runjs("$('#detailPanel').addClass('open');")
  #   
  # })
  
  
  observeEvent(input$showDetail, {
    hosp <- hospital_data_sample[hospital_data_sample$id == input$showDetail, ]
    
    output$detailContent <- renderUI({
      tagList(
        h3(hosp$name),
        tabsetPanel(
          tabPanel("Site Assessment",
                   tags$head(
                     tags$style(HTML("
      .section-box {
        border: 1px solid #e0e0e0;
        border-radius: 10px;
        padding: 20px;
        margin-bottom: 30px;
        background-color: #fefefe;
      }
      .info-label {
        font-weight: bold;
        margin-bottom: 5px;
        display: block;
        color: #555;
      }
      .info-box {
        background-color: #f7f7f7;
        padding: 10px 15px;
        border-radius: 6px;
        margin-bottom: 15px;
      }
      .status-yes {
        display: inline-block;
        background-color: #28a745;
        color: white;
        padding: 5px 12px;
        border-radius: 20px;
        font-weight: bold;
      }
      .status-no {
        display: inline-block;
        background-color: #dc3545;
        color: white;
        padding: 5px 12px;
        border-radius: 20px;
        font-weight: bold;
      }
      .info-section-title {
        font-size: 18px;
        font-weight: bold;
        margin-bottom: 15px;
      }
    "))
                   ),
                   
                   h3("Hospital A"),
                   img(src = "https://placehold.co/1200x400", width = "100%"),
                   br(), br(),
                   
                   ## General Information
                   div(class = "section-box",
                       div(class = "info-section-title", "General Information"),
                       fluidRow(
                         column(6,
                                div(class = "info-label", "Address"),
                                div(class = "info-box", "Address line, John Doe street, Neo Avenue, Lagos")
                         ),
                         column(3,
                                div(class = "info-label", "Total Departments"),
                                div(class = "info-box", "4")
                         ),
                         column(3,
                                div(class = "info-label", "Employee Count"),
                                div(class = "info-box",
                                    HTML("<i class='fa fa-male'></i> Male: 4,500<br>
                               <i class='fa fa-female'></i> Female: 1,300<br>
                               <i class='fa fa-users'></i> Total: 5,800"))
                         )
                       ),
                       fluidRow(
                         column(3,
                                div(class = "info-label", "LGA"),
                                div(class = "info-box", "Neo Marshal LGA")
                         ),
                         column(3,
                                div(class = "info-label", "Contact Person"),
                                div(class = "info-box",
                                    HTML("Kenny Ken<br>08098271188<br>Kennyken@hospital.com.ng<br>Facility Manager"))
                         ),
                         column(3,
                                div(class = "info-label", "Size (No of buildings)"),
                                div(class = "info-box", "3,000 square meters")
                         ),
                         column(3,
                                div(class = "info-label", "Bed Capacity"),
                                div(class = "info-box", "3,000 beds")
                         )
                       ),
                       fluidRow(
                         column(6,
                                div(class = "info-label", "SLD"),
                                div(class = "info-box", HTML("<i>View/download file</i>"))
                         ),
                         column(6,
                                div(class = "info-label", "Avg number of daily patient"),
                                div(class = "info-box",
                                    HTML("<i class='fa fa-male'></i> Male: 2,000<br>
                               <i class='fa fa-female'></i> Female: 1,000<br>
                               <i class='fa fa-users'></i> Total: 3,000"))
                         )
                       )
                   ),
                   
                   ## Power Info Section
                   div(class = "section-box",
                       div(class = "info-section-title", "General Information (Power)"),
                       fluidRow(
                         column(4,
                                div(class = "info-label", "Grid Connection"),
                                div(class = "status-yes", icon("check-circle"), " Yes")
                         ),
                         column(4,
                                div(class = "info-label", "Generator Use"),
                                div(class = "status-yes", icon("check-circle"), " Yes")
                         ),
                         column(4,
                                div(class = "info-label", "Existing Solar"),
                                div(class = "status-no", icon("times-circle"), " No")
                         )
                       ),
                       fluidRow(
                         column(4,
                                div(class = "info-label", "Alternative power source (not listed above)"),
                                div(class = "status-no", icon("times-circle"), " No")
                         ),
                         column(4,
                                div(class = "info-label", "Shading risks"),
                                div(class = "status-no", icon("times-circle"), " No")
                         ),
                         column(4,
                                div(class = "info-label", "Roof Accessibility"),
                                div(class = "status-yes", icon("check-circle"), " Yes")
                         )
                       )
                   )
          ),
          tabPanel("Environmental Assessment",
                   tags$head(
                     tags$style(HTML("
             .section-box {
               border-radius: 10px;
               padding: 25px;
               margin-bottom: 30px;
               background-color: #fff;
               box-shadow: 0 2px 8px rgba(0,0,0,0.05);
             }
             .metric-box {
               background-color: #f5f5f5;
               padding: 20px;
               border-radius: 10px;
               text-align: center;
               box-shadow: 0 1px 3px rgba(0,0,0,0.05);
             }
             .metric-value {
               font-size: 30px;
               font-weight: bold;
             }
             .unit {
               font-size: 14px;
               color: #888;
             }
             .star {
               color: #ffc107;
               font-size: 20px;
             }
             .star-empty {
               color: #e0e0e0;
               font-size: 20px;
             }
             .badge {
               padding: 6px 12px;
               font-weight: bold;
               border-radius: 6px;
               display: inline-block;
             }
             .badge-good {
               background-color: #fdd835;
               color: #fff;
             }
             .badge-verygood {
               background-color: #ffa000;
               color: #fff;
             }
             .attr-label {
               font-weight: bold;
               margin-right: 10px;
             }
             .attr-box {
               background-color: #eee;
               padding: 4px 12px;
               border-radius: 4px;
               display: inline-block;
               font-size: 13px;
             }
             .kpi-box {
               background: #f9f9f9;
               padding: 20px;
               text-align: center;
               border-radius: 10px;
               box-shadow: 0 1px 3px rgba(0,0,0,0.05);
             }
             .kpi-title {
               color: #888;
             }
             .kpi-value {
               font-size: 26px;
               font-weight: bold;
             }
           "))
                   ),
                   
                   div(class = "section-box",
                       h4("Air Quality/Climatology and Noise Level"),
                       p("Lorem ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s..."),
                       br(),
                       fluidRow(
                         column(4,
                                div(class = "metric-box",
                                    div("CO₂ Emission"),
                                    div(class = "metric-value", "300", span(class = "unit", "kg"))
                                )
                         ),
                         column(4,
                                div("Air Quality Rating"),
                                span(class = "star", HTML("&#9733;&#9733;&#9733;&#9733;")),
                                span(class = "star-empty", HTML("&#9733;"))
                         ),
                         column(4,
                                div("Noise Level Rating"),
                                span(class = "star", HTML("&#9733;&#9733;&#9733;&#9733;")),
                                span(class = "star-empty", HTML("&#9733;"))
                         )
                       )
                   ),
                   
                   div(class = "section-box",
                       h4("Physiochemical Analysis (Surface and Ground Water)"),
                       p("Lorem ipsum is simply dummy text of the printing and typesetting industry..."),
                       br(),
                       fluidRow(
                         column(6,
                                div(class = "metric-box",
                                    span(class = "star", HTML("&#9733;&#9733;&#9733;&#9733;&#9733;")),
                                    br(), br(),
                                    span(class = "badge badge-verygood", "Very Good"),
                                    br(), br(),
                                    div(span(class = "attr-label", "Colour:"), span(class = "attr-box", "Good")),
                                    div(span(class = "attr-label", "pH:"), span(class = "attr-box", "High")),
                                    div(span(class = "attr-label", "Turbidity:"), span(class = "attr-box", "High")),
                                    div(span(class = "attr-label", "Salinity:"), span(class = "attr-box", "Low")),
                                    div(span(class = "attr-label", "Hardness:"), span(class = "attr-box", "Low")),
                                    div(span(class = "attr-label", "Heavy Metals:"), span(class = "attr-box", "None"))
                                )
                         ),
                         column(6,
                                div(class = "metric-box",
                                    span(class = "star", HTML("&#9733;&#9733;&#9733;&#9733;&#9734;")),
                                    br(), br(),
                                    span(class = "badge badge-good", "Good"),
                                    br(), br(),
                                    div(span(class = "attr-label", "Colour:"), span(class = "attr-box", "Good")),
                                    div(span(class = "attr-label", "pH:"), span(class = "attr-box", "High")),
                                    div(span(class = "attr-label", "Turbidity:"), span(class = "attr-box", "High")),
                                    div(span(class = "attr-label", "Salinity:"), span(class = "attr-box", "Low")),
                                    div(span(class = "attr-label", "Hardness:"), span(class = "attr-box", "Low")),
                                    div(span(class = "attr-label", "Heavy Metals:"), span(class = "attr-box", "None"))
                                )
                         )
                       )
                   ),
                   
                   div(class = "section-box",
                       h4("Physiochemical Analysis (Surface and Ground Water)"),
                       p("Lorem ipsum is simply dummy text of the printing and typesetting industry..."),
                       br(),
                       fluidRow(
                         column(6,
                                div(style = "background-color: white; border-radius: 10px; padding: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.05); text-align: center;",
                                    
                                    # Stars and badge
                                    span(class = "rating", HTML("&#9733;&#9733;&#9733;&#9733;&#9733;")),
                                    br(), br(),
                                    div(style = "display: inline-block; background-color: #fdc23e; color: white; padding: 5px 15px; border-radius: 6px; font-weight: bold;", "Very Good"),
                                    br(), br(),
                                    
                                    # Attributes
                                    div(style = "text-align: left; padding-left: 20%;",
                                        div(tags$b("Colour:"), span(class = "attr-box", "Good")),
                                        div(tags$b("pH:"), span(class = "attr-box", "High")),
                                        div(tags$b("Turbidity:"), span(class = "attr-box", "High")),
                                        div(tags$b("Salinity:"), span(class = "attr-box", "Low")),
                                        div(tags$b("Hardness:"), span(class = "attr-box", "Low")),
                                        div(tags$b("Heavy Metals:"), span(class = "attr-box", "None"))
                                    )
                                )
                         ),
                         column(6,
                                div(style = "background-color: white; border-radius: 10px; padding: 20px; box-shadow: 0 2px 6px rgba(0,0,0,0.05); text-align: center;",
                                    
                                    # Stars and badge
                                    span(class = "rating", HTML("&#9733;&#9733;&#9733;&#9733;&#9733;")),
                                    br(), br(),
                                    div(style = "display: inline-block; background-color: #ffe057; color: black; padding: 5px 15px; border-radius: 6px; font-weight: bold;", "Good"),
                                    br(), br(),
                                    
                                    # Attributes
                                    div(style = "text-align: left; padding-left: 20%;",
                                        div(tags$b("Colour:"), span(class = "attr-box", "Good")),
                                        div(tags$b("pH:"), span(class = "attr-box", "High")),
                                        div(tags$b("Turbidity:"), span(class = "attr-box", "High")),
                                        div(tags$b("Salinity:"), span(class = "attr-box", "Low")),
                                        div(tags$b("Hardness:"), span(class = "attr-box", "Low")),
                                        div(tags$b("Heavy Metals:"), span(class = "attr-box", "None"))
                                    )
                                )
                         )
                       )
                   ),
                   
                   
                   div(class = "section-box",
                       h4("Socioeconomics and Health Survey"),
                       p("Lorem ipsum is simply dummy text of the printing and typesetting industry..."),
                       br(),
                       fluidRow(
                         column(3,
                                div(class = "kpi-box",
                                    div(class = "kpi-title", "Education"),
                                    div(class = "kpi-value", "30 kWp")
                                )
                         ),
                         column(3,
                                div(class = "kpi-box",
                                    div(class = "kpi-title", "Culture"),
                                    div(class = "kpi-value", "30 kWp")
                                )
                         ),
                         column(3,
                                div(class = "kpi-box",
                                    div(class = "kpi-title", "Employment Status"),
                                    div(class = "kpi-value", "30 kWp")
                                )
                         ),
                         column(3,
                                div(class = "kpi-box",
                                    div(class = "kpi-title", "Income level"),
                                    div(class = "kpi-value", "30 kWp")
                                )
                         )
                       ),
                       br(),
                       fluidRow(
                         column(6, plotOutput("health_issues_plot")),
                         column(6, plotOutput("culture_plot"))
                       )
                   )
          ),
          # tabPanel("Energy Assessment",
          #          tags$head(
          #            tags$style(HTML("      
          #    .metric-card {
          #      background-color: white;
          #      padding: 20px;
          #      border-radius: 10px;
          #      box-shadow: 0 2px 6px rgba(0,0,0,0.08);
          #      text-align: center;
          #      margin-bottom: 20px;
          #    }
          #    .metric-title {
          #      color: #888;
          #      font-size: 14px;
          #    }
          #    .metric-value {
          #      font-size: 26px;
          #      font-weight: bold;
          #    }
          #    .metric-unit {
          #      font-size: 12px;
          #      color: #999;
          #    }
          #    .green-box {
          #      background-color: #e9fbe9;
          #      border-radius: 8px;
          #      padding: 8px 12px;
          #      color: #1f8e1f;
          #      font-weight: bold;
          #      display: inline-block;
          #      font-size: 12px;
          #    }
          #    .image-box {
          #      margin-top: 10px;
          #    }
          #    .info-box-light {
          #      background-color: #f9f9f9;
          #      padding: 20px;
          #      border-radius: 10px;
          #      text-align: center;
          #      box-shadow: 0 1px 3px rgba(0,0,0,0.05);
          #      margin-bottom: 15px;
          #    }
          #  "))
          #          ),
          #          
          #          h4("Power information (Load, Grid, etc)"),
          #          p("Lorem ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s..."),
          #          br(),
          #          
          #          fluidRow(
          #            column(2, div(class = "metric-card", div(class = "metric-title", "Total Demand"), div(class = "metric-value", "500", span(class = "metric-unit", " kW")))),
          #            column(2, div(class = "metric-card", div(class = "metric-title", "Avg. Energy Consumption"), div(class = "metric-value", "330", span(class = "metric-unit", " kWh")))),
          #            column(2, div(class = "metric-card", div(class = "metric-title", "Peak Load"), div(class = "metric-value", "2,000", span(class = "metric-unit", " kWh")))),
          #            column(2, div(class = "metric-card", div(class = "metric-title", "Off Peak Load"), div(class = "metric-value", "1,220", span(class = "metric-unit", " kWh")))),
          #            column(2, div(class = "metric-card", div(class = "metric-title", "Avg. Peak Load"), div(class = "metric-value", "400", span(class = "metric-unit", " kWh"))))
          #          ),
          #          
          #          fluidRow(
          #            column(8,
          #                   div(class = "info-box-light",
          #                       h5("Demand Load vs Peak Load"),
          #                       selectInput("load_type", NULL, choices = c("Weekly", "Monthly"), selected = "Weekly", width = "150px"),
          #                       plotOutput("loadPlot", height = "280px")
          #                   )
          #            ),
          #            column(4,
          #                   fluidRow(
          #                     column(6, div(class = "info-box-light", h6("Grid Capacity"), h3("2,000", span(class = "metric-unit", "MW")) )),
          #                     column(6, div(class = "info-box-light", h6("Generator Capacity"), h3("300", span(class = "metric-unit", "kVA")) )),
          #                     column(6, div(class = "info-box-light", h6("Alternative Capacity (IPP/Solar) - if any"), h3("2,000", span(class = "metric-unit", "MW")) )),
          #                     column(6, div(class = "info-box-light", h6("Hours of Supply"), h3("20", span(class = "metric-unit", "hrs")) ))
          #                   )
          #            )
          #          ),
          #          
          #          br(),
          #          h4("Number of dedicated Transformer and Distribution Panel"),
          #          
          #          fluidRow(
          #            column(2, div(class = "metric-card", div(class = "metric-title", "Transformer"), div(class = "metric-value", "4"))),
          #            column(3, div(class = "metric-card", div(class = "metric-title", "Total Transformer voltage"), div(class = "metric-value", "15,000", span(class = "metric-unit", "volts")))),
          #            column(2, div(class = "metric-card", div(class = "metric-title", "Distribution Panel"), div(class = "metric-value", "5"))),
          #            column(2, div(class = "metric-card", div(class = "metric-title", "Info 4"), div(class = "metric-value", "4")))
          #          ),
          #          
          #          fluidRow(
          #            column(3, div(class = "info-box-light", HTML("Transformer 1 <i class='fa fa-chevron-right'></i>"))),
          #            column(3, div(class = "info-box-light", HTML("Transformer 2 <i class='fa fa-chevron-right'></i>"))),
          #            column(3, div(class = "info-box-light", HTML("Transformer 3 <i class='fa fa-chevron-right'></i>"))),
          #            column(3, div(class = "info-box-light", HTML("Transformer 4 <i class='fa fa-chevron-right'></i>")))
          #          ),
          #          
          #          fluidRow(
          #            column(12,
          #                   div(class = "info-box-light",
          #                       span(class = "green-box", "Transformer 1"),
          #                       br(),
          #                       img(src = "https://via.placeholder.com/600x250", width = "100%", class = "image-box")
          #                   )
          #            )
          #          )
          # ),
          

            

          tabPanel("Energy Assessment",
                   tags$head(
                     tags$style(HTML("
             .metric-box {
               background-color: #fff;
               border-radius: 10px;
               box-shadow: 0 2px 5px rgba(0,0,0,0.05);
               padding: 20px;
               text-align: center;
               margin-bottom: 20px;
             }
             .metric-title {
               font-size: 14px;
               color: #888;
               margin-bottom: 5px;
             }
             .metric-value {
               font-size: 24px;
               font-weight: bold;
               color: #000;
             }
             .metric-unit {
               font-size: 14px;
               color: #666;
             }
             .section-box {
               background-color: #fefefe;
               border: 1px solid #e0e0e0;
               border-radius: 10px;
               padding: 20px;
               margin-bottom: 30px;
             }
             .transformer-card {
               background-color: #f8f8f8;
               border: 1px solid #e0e0e0;
               border-radius: 10px;
               padding: 10px 15px;
               margin-bottom: 10px;
               display: flex;
               justify-content: space-between;
               align-items: center;
               font-weight: bold;
             }
             .dropdown-icon {
               font-size: 16px;
               color: #28a745;
             }
             .equipment-entry {
               display: flex;
               justify-content: space-between;
               align-items: center;
               padding: 10px 15px;
               background-color: #f7f7f7;
               border: 1px solid #e0e0e0;
               border-radius: 10px;
               margin-bottom: 10px;
               font-weight: bold;
             }
             .download-button {
               margin-top: 10px;
               background-color: #28a745;
               color: white;
               padding: 8px 15px;
               border-radius: 5px;
               text-decoration: none;
               display: inline-block;
             }
             .toggle-header {
              display: flex;
              justify-content: space-between;
              align-items: center;
              background-color: #f8f9fa;
              padding: 8px 12px;
              margin-top: 8px;
              border-radius: 6px;
              cursor: pointer;
              border: 1px solid #e0e0e0;
            }
            .toggle-icon {
              margin-right: 10px;
              color: #28a745;
              transition: transform 0.3s ease;
            }
            .toggle-label {
              flex-grow: 1;
              font-weight: bold;
              color: #444;
            }
            .toggle-content {
              display: none;
              margin-left: 20px;
              padding: 5px 0 10px 10px;
              background-color: #f1f1f1;
              border-radius: 6px;
            }
            .toggle-content ul {
              list-style-type: disc;
              padding-left: 20px;
              margin: 0;
            }
            .toggle-content li {
              margin-bottom: 4px;
            }
                 
           "),
            
          )
                   ),
                   
                   h4("Power information (Load, Grid, etc)"),
                   p("Lorem ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make."),
                   
                   ## Top Summary Metrics
                   fluidRow(
                     column(2, div(class = "metric-box", div(class = "metric-title", "Total Demand"), div(class = "metric-value", "500"), div(class = "metric-unit", "kW"))),
                     column(2, div(class = "metric-box", div(class = "metric-title", "Avg. Energy Consumption"), div(class = "metric-value", "330"), div(class = "metric-unit", "kWh"))),
                     column(2, div(class = "metric-box", div(class = "metric-title", "Peak Load"), div(class = "metric-value", "2,000"), div(class = "metric-unit", "kWh"))),
                     column(2, div(class = "metric-box", div(class = "metric-title", "Off Peak Load"), div(class = "metric-value", "1,220"), div(class = "metric-unit", "kWh"))),
                     column(2, div(class = "metric-box", div(class = "metric-title", "Avg. Peak Load"), div(class = "metric-value", "400"), div(class = "metric-unit", "kWh")))
                   ),
                   
                   ## Demand Load vs Peak Load Chart + Right Column Metrics
                   fluidRow(
                     column(8,
                            div(class = "section-box",
                                fluidRow(
                                  column(10, h5("Demand Load vs Peak Load")),
                                  column(2, selectInput("timeSelect", NULL, choices = c("Weekly", "Monthly"), selected = "Weekly"))
                                ),
                                plotOutput("loadPlot", height = "250px"),
                                tags$div(style = "padding-left: 20px;",
                                         tags$span(style = "color: red;", icon("circle"), " Demand load (kW) "),
                                         tags$span(style = "color: green; margin-left: 10px;", icon("circle"), " Peak load (kW)")
                                )
                            )
                     ),
                     
                     column(4,
                            div(class = "metric-box", div(class = "metric-title", "Grid Capacity"), div(class = "metric-value", "2,000"), div(class = "metric-unit", "MW")),
                            div(class = "metric-box", div(class = "metric-title", "Generator Capacity"), div(class = "metric-value", "300"), div(class = "metric-unit", "kVA")),
                            div(class = "metric-box", div(class = "metric-title", "Alternative Capacity (IPP/Solar) - if any"), div(class = "metric-value", "2,000"), div(class = "metric-unit", "MW")),
                            div(class = "metric-box", div(class = "metric-title", "Hours of Supply"), div(class = "metric-value", "20"), div(class = "metric-unit", "hrs"))
                     )
                   ),
                   
                   ## Transformer and Distribution Panel Section
                   div(class = "section-box",
                       h5("Number of dedicated Transformer and Distribution Panel"),
                       fluidRow(
                         column(3, div(class = "metric-box", div(class = "metric-title", "Transformer"), div(class = "metric-value", "4"))),
                         column(3, div(class = "metric-box", div(class = "metric-title", "Total Transformer Voltage"), div(class = "metric-value", "15,000"), div(class = "metric-unit", "volts"))),
                         column(3, div(class = "metric-box", div(class = "metric-title", "Distribution Panel"), div(class = "metric-value", "5"))),
                         column(3, div(class = "metric-box", div(class = "metric-title", "Info 4"), div(class = "metric-value", "4")))
                       ),
                       
                       br(),
                       
                       column(4, div(class = "transformer-card", "Transformer 1", icon("chevron-down", class = "dropdown-icon"))),
                       column(4, div(class = "transformer-card", "Transformer 2", icon("chevron-down", class = "dropdown-icon"))),
                       column(4, div(class = "transformer-card", "Transformer 3", icon("chevron-down", class = "dropdown-icon"))),
                       column(4, div(class = "transformer-card", "Transformer 4", icon("chevron-down", class = "dropdown-icon"))),
                       
                       br(),
                       
                       img(src = "https://via.placeholder.com/400x200", width = "100%", style = "border-radius: 10px;")
                   ),
                   
                   ## Equipment Quantity and Load
                   ## Equipment Quantity and Load
                   div(class = "section-box",
                       h5("Equipment Quantity and Load (Critical, Non-critical and Backup)"),
                       
                       fluidRow(
                         column(6,
                                div(class = "metric-box",
                                    div(class = "metric-title", "Total Quantity of Equipment"),
                                    div(class = "metric-value", "3,000")
                                ),
                                br(),
                                
                                # CRITICAL Equipment with dropdown
                                div(id = "critical_toggle", class = "equipment-entry toggle-header",
                                    icon("chevron-right", class = "toggle-icon", id = "critical_icon"),
                                    span("Critical Equipment", class = "toggle-label"),
                                    span("1,500", class = "equipment-count", style = "float: right; font-weight: bold;")
                                ),
                                div(id = "critical_list", class = "toggle-content",
                                    tags$ul(
                                      tags$li("X-ray Machine"),
                                      tags$li("MRI Scanner"),
                                      tags$li("Ventilator")
                                    )
                                ),
                                
                                # NON-CRITICAL Equipment with dropdown
                                div(id = "non_critical_toggle", class = "equipment-entry toggle-header",
                                    icon("chevron-right", class = "toggle-icon", id = "non_critical_icon"),
                                    span("Non-critical Equipment", class = "toggle-label"),
                                    span("1,000", class = "equipment-count", style = "float: right; font-weight: bold;")
                                ),
                                div(id = "non_critical_list", class = "toggle-content",
                                    tags$ul(
                                      tags$li("Water Dispenser"),
                                      tags$li("Lighting Units"),
                                      tags$li("Printer")
                                    )
                                ),
                                
                                # BACKUP Equipment with dropdown
                                div(id = "backup_toggle", class = "equipment-entry toggle-header",
                                    icon("chevron-right", class = "toggle-icon", id = "backup_icon"),
                                    span("Backup", class = "toggle-label"),
                                    span("500", class = "equipment-count", style = "float: right; font-weight: bold;")
                                ),
                                div(id = "backup_list", class = "toggle-content",
                                    tags$ul(
                                      tags$li("UPS Unit"),
                                      tags$li("Battery Pack"),
                                      tags$li("Spare Generator")
                                    )
                                )
                         ),
                         column(6,
                                plotOutput("energyChart", height = "250px")
                         )
                       ),
                       
                       br(),
                       h5("SLD"),
                       img(src = "https://via.placeholder.com/800x200", width = "100%",
                           style = "border: 2px solid #28a745; border-radius: 10px;"),
                       br(),
                       a("Download SLD", href = "#", class = "download-button")
                   )
                   
          ),

          tabPanel("Viability Assessment",
                   useShinyjs(),
                   tags$script(HTML("Shiny.setInputValue('selected_subtab', 'econ');")),
                   
                   ###
                   tags$script(HTML("
                      function selectSubTab(tab) {
                        Shiny.setInputValue('selected_subtab', tab);
                        
                        // Remove 'active' class from all
                        document.querySelectorAll('.sub-tab').forEach(el => el.classList.remove('active'));
                    
                        // Add 'active' to the clicked one
                        if (tab === 'economic') {
                          document.getElementById('econ_tab').classList.add('active');
                        } else if (tab === 'structural') {
                          document.getElementById('structural_tab').classList.add('active');
                        } else if (tab === 'technical') {
                          document.getElementById('technical_tab').classList.add('active');
                        }
                      }
                    ")),
                   ###
                   
                   tags$head(
                     tags$style(HTML("
             .sub-tab {
               border-left: 3px solid transparent;
               padding: 10px 15px;
               cursor: pointer;
               margin-bottom: 10px;
               color: #000;
               font-weight: bold;
               border-radius: 5px;
             }
             .sub-tab.active {
               border-left: 3px solid #28a745;
               background-color: #f5f5f5;
             }
             .section-box {
               border-radius: 10px;
               padding: 25px;
               background: white;
               box-shadow: 0 2px 6px rgba(0, 0, 0, 0.05);
               margin-bottom: 30px;
             }
             .cost-table {
               width: 100%;
               margin-top: 20px;
             }
             .cost-table thead th {
               color: #666;
               font-weight: 600;
               font-size: 14px;
             }
             .cost-table td, .cost-table th {
               padding: 12px 10px;
               border-bottom: 1px solid #eee;
             }
             .total-row {
               background-color: #e6f4ea;
               font-weight: bold;
               font-size: 16px;
             }
             .custom-select {
               margin-bottom: 20px;
             }
             .chart-section {
               padding-top: 20px;
             }
           "))
                   ),
                   
                   fluidRow(
                     column(2,
                            # div(id = "subTabs",
                            #     div(class = "sub-tab active", id = "econ_tab", "Economic/Financial", onclick = "Shiny.setInputValue('selected_subtab', 'econ')"),
                            #     div(class = "sub-tab", id = "structural_tab", "Structural", onclick = "Shiny.setInputValue('selected_subtab', 'structural')"),
                            #     div(class = "sub-tab", id = "technical_tab", "Technical", onclick = "Shiny.setInputValue('selected_subtab', 'technical')")
                            # )
                            div(id = "subTabs",
                                div(class = "sub-tab active", id = "econ_tab", onclick = "selectSubTab('econ')", "Economic/Financial"),
                                div(class = "sub-tab", id = "structural_tab", onclick = "selectSubTab('structural')", "Structural"),
                                div(class = "sub-tab", id = "technical_tab", onclick = "selectSubTab('technical')", "Technical")
                            )
                            
                     ),
                     
                     column(10,
                            
                            # ECONOMIC PANEL
                            conditionalPanel(
                              condition = "input.selected_subtab == 'econ'",
                              div(class = "section-box",
                                  h4("Economic/Financial Simulation"),
                                  fluidRow(
                                    column(6,
                                           selectInput("proposed_system", "Proposed system",
                                                       choices = c("System with storage", "System without storage"),
                                                       selected = "System with storage",
                                                       width = "100%")
                                    ),
                                    column(6,
                                           selectInput("hospital_load", "Hospital load",
                                                       choices = c("Peak load", "Average load"),
                                                       selected = "Peak load",
                                                       width = "100%")
                                    )
                                  )
                              ),
                              
                              div(class = "section-box",
                                  h5("Initial cost of deployment"),
                                  tags$table(class = "cost-table",
                                             tags$thead(
                                               tags$tr(
                                                 tags$th("Items"),
                                                 tags$th("NGN")
                                               )
                                             ),
                                             tags$tbody(
                                               tags$tr(tags$td("Solar panels"), tags$td("97,900")),
                                               tags$tr(tags$td("Batteries"), tags$td("97,900")),
                                               tags$tr(tags$td("Inverters"), tags$td("97,900")),
                                               tags$tr(tags$td("Solar rails, cables, protection and other accessories"), tags$td("97,900")),
                                               tags$tr(tags$td("Installation"), tags$td("97,900")),
                                               tags$tr(tags$td("Erection of solar carport"), tags$td("97,900")),
                                               tags$tr(class = "total-row", tags$td("Total initial cost of deployment"), tags$td("97,900,000"))
                                             )
                                  )
                              ),
                              
                              div(class = "section-box chart-section",
                                  tabsetPanel(
                                    tabPanel("Historical consumption",
                                             h5("12 months historical consumption from Grid (kWh)"),
                                             plotOutput("consumption_chart", height = "250px")
                                    ),
                                    tabPanel("Historical send", p("Demo data placeholder")),
                                    tabPanel("Consumption vs Spend", p("Demo data placeholder"))
                                  )
                              ),
                              # div(class = "section-box chart-section",
                              #     h5("Average monthly consumption"),
                              #     plotOutput("avg_monthly_comparison", height = "250px")
                              # ),
                              # 
                              # div(class = "section-box chart-section",
                              #     fluidRow(
                              #       column(6,
                              #              h5("Energy Supply Mix (Spend)"),
                              #              plotOutput("energy_mix_chart", height = "300px")
                              #       ),
                              #       column(6,
                              #              selectInput("energy_option", "Select an option", choices = c("Current", "Solar"))
                              #       )
                              #     )
                              # )
                              div(class = "section-box chart-section",
                                  tabsetPanel(
                                    tabPanel("Average monthly consumption",
                                             h5("Current solution vs Solar solutions"),
                                             plotOutput("avg_monthly_comparison", height = "250px")
                                    ),
                                    tabPanel("Average monthly spend",
                                             h5("Current solution vs Solar solutions"),
                                             plotOutput("avg_monthly_spend", height = "250px")
                                    )
                                  )
                              ),
                              
                              div(class = "section-box chart-section",
                                  fluidRow(
                                    column(6,
                                           h5("Energy Supply Mix (Spend)"),
                                           # plotOutput("energy_mix_chart", height = "300px")
                                           plotOutput("energy_mix_chart", height = "450px")
                                    ),
                                    column(6,
                                           selectInput("energy_option", "Select an option", choices = c("Current", "Solar"), selected = "Current", width = "100%")
                                    )
                                  )
                              )
                              
                            ),
                            
                            # STRUCTURAL PANEL
                            # conditionalPanel(
                            #   condition = "input.selected_subtab == 'structural'",
                            #   div(class = "section-box", h4("Structural Info"), p("This is structural information."))
                            # ),
                            # 
                            # STRUCTURAL PANEL
                            conditionalPanel(
                              condition = "input.selected_subtab == 'structural'",
                              
                              div(class = "section-box",
                                  h4("Rooftop Suitability Image (Drone and Map view)"),
                                  slickROutput("rooftopCarousel", width = "100%", height = "300px")
                              ),
                              
                              div(class = "section-box",
                                  h4("Structural Details"),
                                  tags$table(style = "width: 100%; margin-top: 15px;",
                                             tags$thead(
                                               tags$tr(
                                                 tags$th(style = "background-color: #6c757d; color: white; padding: 10px; border-radius: 6px 6px 0 0;", "Title 1"),
                                                 tags$th(style = "background-color: #6c757d; color: white; padding: 10px; border-radius: 6px 6px 0 0;", "Title 1")
                                               )
                                             ),
                                             tags$tbody(
                                               lapply(1:6, function(i) {
                                                 tags$tr(
                                                   tags$td("LCDE"),
                                                   tags$td(style = "font-weight: bold; font-size: 18px;", "30")
                                                 )
                                               })
                                             )
                                  )
                              ),
                              
                              div(class = "section-box",
                                  h4("Available Space vs Space Required for Deployment"),
                                  plotOutput("space_chart", height = "350px")
                              )
                            ),
                            
                            # TECHNICAL PANEL
                            conditionalPanel(
                              condition = "input.selected_subtab == 'technical'",
                              tagList(
                                div(class = "section-box",
                                    h4("Solar Yield Simulation"),
                                    fluidRow(
                                      column(4,
                                             div(class = "metric-box",
                                                 h5("Estimated PV Capacity"),
                                                 div(style = "font-size: 24px; font-weight: bold;", "30 kWp")
                                             )
                                      ),
                                      column(4,
                                             div(class = "metric-box",
                                                 h5("Shaded Irradiance"),
                                                 div(style = "font-size: 24px; font-weight: bold;", "1,438.2 kWh/m²")
                                             )
                                      ),
                                      column(4,
                                             div(class = "metric-box",
                                                 h5("Solar Access"),
                                                 div(style = "font-size: 24px; font-weight: bold;", "70%")
                                             )
                                      )
                                    )
                                ),
                                
                                div(class = "section-box",
                                    h5("Monthly generation forecast (kWh/month)"),
                                    selectInput("tech_time_select", NULL, choices = c("Monthly", "Yearly"), selected = "Monthly", width = "150px"),
                                    plotOutput("monthly_gen_forecast", height = "300px")
                                ),
                                
                                div(class = "section-box",
                                    h5("PV/Gen Mix"),
                                    fluidRow(
                                      column(9,
                                             plotOutput("pv_gen_mix", height = "300px")
                                      ),
                                      column(3,
                                             div(style = "background-color: #f9f9f9; padding: 20px; border-radius: 10px;",
                                                 h6("Solar"),
                                                 div(style = "font-size: 22px; font-weight: bold;", "80%"),
                                                 br(),
                                                 h6("Generator"),
                                                 div(style = "font-size: 22px; font-weight: bold;", "20%")
                                             )
                                      )
                                    )
                                ),
                                
                                div(class = "section-box",
                                    h5("Sources of system loss"),
                                    plotOutput("system_loss_chart", height = "300px")
                                ),
                                
                                div(class = "section-box",
                                    h5("Solar access by month"),
                                    tags$table(class = "table table-bordered",
                                               tags$thead(
                                                 tags$tr(
                                                   tags$th("Description"),
                                                   lapply(month.abb[1:10], tags$th)
                                                 )
                                               ),
                                               tags$tbody(
                                                 tags$tr(
                                                   tags$td("Rooftop 1"),
                                                   lapply(rep("98%", 10), tags$td)
                                                 ),
                                                 tags$tr(
                                                   tags$td("Rooftop 2"),
                                                   lapply(rep("98%", 10), tags$td)
                                                 ),
                                                 tags$tr(
                                                   tags$td("Solar access weighted by kWP"),
                                                   lapply(rep("98%", 10), tags$td)
                                                 ),
                                                 tags$tr(
                                                   tags$td("AC power (kWh)"),
                                                   lapply(rep("100", 10), tags$td)
                                                 )
                                               )
                                    )
                                )
                              )
                            )
                            
                            
                     )
                   )
          ),
          tabPanel("Summary & Recommendations", p("Summary & Recommendations content")),

        )
      )
    })
    
    runjs("document.getElementById('detailPanel').classList.add('open');")
  })
  
  # observeEvent(input$closePanel, {
  #   runjs("$('#detailPanel').removeClass('open');")
  #   
  # })
  
  
  observeEvent(input$closePanel, {
    runjs("document.getElementById('detailPanel').classList.remove('open');")
  })
  
  
  
  output$health_issues_plot <- renderPlot({
    barplot(
      height = c(60, 70, 85, 90, 100, 50),
      names.arg = c("Figma", "Sketch", "XD", "Photoshop", "Illustrator", "AfterEffect"),
      col = "#7da0fa",
      border = NA,
      main = NULL,
      ylab = NULL,
      ylim = c(0, 120)
    )
  })
  
  output$culture_plot <- renderPlot({
    barplot(
      height = c(55, 60, 70, 95, 90, 40),
      names.arg = c("Figma", "Sketch", "XD", "Photoshop", "Illustrator", "AfterEffect"),
      col = "#7da0fa",
      border = NA,
      main = NULL,
      ylab = NULL,
      ylim = c(0, 120)
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  #################################################################################################################
## hospital count (Unique)
 output$hospital_count <-  renderText({
   c <- length(unique(main_audit_df$`Hospital Name`))
   c
 })
  
## LOCAL GOVERNMENT UNIQUE COUNT
  output$lga_count <-  renderText({
    c <- length(unique(main_audit_df$`Local Government`))
    c
  })
  
  
  
  
  
  
  
  
}