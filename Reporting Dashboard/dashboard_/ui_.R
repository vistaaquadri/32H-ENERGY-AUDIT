# app.R
library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(DT)
library(leaflet)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = span("Overall Summary", style = "font-weight: bold; font-size: 20px")),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Overall Summary", tabName = "summary", icon = icon("home")),
      menuItem("All Hospitals", tabName = "all_hospitals", icon = icon("hospital")),
      menuItem("Geospatial Information", tabName = "geo_info", icon = icon("globe"))
    )
  ),
  
  # dashboardBody(
  #   
  #   useShinyjs(),
  #   
  #   
  # #   tags$head(tags$style(HTML(".star { color: #ffc107; font-size: 20px; } .star-grey { color: #e0e0e0; } .card { border: 1px solid #ddd; padding: 10px; margin: 5px; border-radius: 10px; background-color: #fff; box-shadow: 0 2px 4px rgba(0,0,0,0.05); } .hospital-title { background-color: #333; color: #fff; padding: 5px; border-radius: 5px 5px 0 0; font-weight: bold; } .pagination-btn { margin: 5px; } #detailPanel { position: fixed; top: 0; right: 0; width: 100%; height: 100%; background: white; z-index: 1000; overflow-y: scroll; padding: 20px; display: none; }"))),
  # #   
  # #   tags$head(
  # #     tags$style(HTML("
  # #   #detailPanel {
  # #     position: fixed;
  # #     top: 0;
  # #     right: 0;
  # #     width: 100%;
  # #     height: 100%;
  # #     background: white;
  # #     z-index: 1000;
  # #     overflow-y: auto;
  # #     transform: translateX(100%);
  # #     transition: transform 0.4s ease-in-out;
  # #   }
  # #   #detailPanel.open {
  # #     transform: translateX(0%);
  # #   }
  # # "))
  #   # ),
  #   tags$head(
  #     tags$style(HTML("
  #       .small-box {
  #         border-radius: 15px;
  #         padding: 30px;
  #         color: black !important;
  #         background-color: #f9f9f9 !important;
  #         box-shadow: 0 2px 6px rgba(0,0,0,0.1);
  #         text-align: center;
  #       }
  #       
  #       .section-box {
  #         border-radius: 20px;
  #         background-color: white;
  #         padding: 20px;
  #         margin-bottom: 30px;
  #         box-shadow: 0 2px 6px rgba(0,0,0,0.1);
  #       }
  #       .rating {
  #         font-size: 20px;
  #         color: #ffc107;
  #       }
  #       .rating-grey {
  #         color: #e0e0e0;
  #       }
  #       .metric-box {
  #         border: 1px solid #e5e5e5;
  #         border-radius: 12px;
  #         padding: 20px;
  #         text-align: center;
  #         background-color: #fdfdfd;
  #         box-shadow: 0 1px 3px rgba(0,0,0,0.05);
  #       }
  #       .metric-title {
  #         font-size: 14px;
  #         color: #888;
  #         margin-bottom: 8px;
  #       }
  #       .metric-value {
  #         font-weight: bold;
  #         font-size: 24px;
  #       }
  #       .unit {
  #         font-size: 14px;
  #       }
  #       
  #       .star { color: #ffc107; font-size: 20px; } .star-grey { color: #e0e0e0; } 
  #       
  #       # .card {
  #       #     border: 1px solid #ddd; 
  #       #     padding: 10px; 
  #       #     margin: 5px; 
  #       #     border-radius: 10px;
  #       #     background-color: #fff; 
  #       #     box-shadow: 0 2px 4px rgba(0,0,0,0.05); 
  #       # } 
  #       
  #       .card {
  #       border: 1px solid #ccc; 
  #       padding: 15px; 
  #       margin: 10px; 
  #       border-radius: 8px; 
  #       background-color: #fff;
  #       box-shadow: 0 2px 6px rgba(0,0,0,0.1);
  #     }
  #       
  #       .hospital-title { background-color: #333; color: #fff; padding: 5px; border-radius: 5px 5px 0 0; font-weight: bold; } .pagination-btn { margin: 5px; } #detailPanel { position: fixed; top: 0; right: 0; width: 100%; height: 100%; background: white; z-index: 1000; overflow-y: scroll; padding: 20px; display: none; }
  #       
  #       
  #       
  #    #detailPanel {
  #       position: fixed;
  #       top: 0;
  #       right: 0;
  #       width: 100%;
  #       height: 100%;
  #       background-color: white;
  #       z-index: 9999;
  #       transform: translateX(100%);
  #       transition: transform 0.4s ease-in-out;
  #       padding: 20px;
  #       overflow-y: auto;
  #    }
  #     
  #     
  #     #detailPanel.open {
  #       transform: translateX(0%);
  #     }
  #   
  #     "))
  #   ),
  #   
  #   tabItems(
  #     tabItem(tabName = "summary",
  #             
  #             ## Hospital Metrics
  #             fluidRow(
  #               column(12,
  #                      selectInput("hospital_type", "Select Hospital Type:", 
  #                                  choices = c("All Hospital", "General", "Maternity", "Orthopedic", "Pediatric"),
  #                                  selected = "All Hospital")
  #               )
  #             ),
  #             br(),
  #             fluidRow(
  #               column(3, div(class = "small-box", h3("5"), p("General Hospitals"))),
  #               column(3, div(class = "small-box", h3("20"), p("Maternity Hospital"))),
  #               column(3, div(class = "small-box", h3("3"), p("Orthopedic Hospital"))),
  #               column(3, div(class = "small-box", h3("4"), p("Pediatric Hospital")))
  #             ),
  #             
  #             ## Environmental Assessment
  #             div(class = "section-box",
  #                 h3("Environmental Assessment"),
  #                 br(),
  #                 fluidRow(
  #                   column(4,
  #                          div(class = "metric-box",
  #                              div(class = "metric-title", "CO₂ Emission"),
  #                              div(class = "metric-value", "300", span(class = "unit", "kg")),
  #                              tags$img(src = "https://cdn-icons-png.flaticon.com/512/166/166259.png", width = "30px")
  #                          )
  #                   ),
  #                   column(4,
  #                          div(class = "metric-box",
  #                              div(class = "metric-title", "Air Quality Rating"),
  #                              span(class = "rating", HTML("&#9733;&#9733;&#9733;&#9733;")),
  #                              span(class = "rating-grey", HTML("&#9734;")),
  #                              tags$br(),
  #                              tags$img(src = "https://cdn-icons-png.flaticon.com/512/166/166259.png", width = "30px")
  #                          )
  #                   ),
  #                   column(4,
  #                          div(class = "metric-box",
  #                              div(class = "metric-title", "Noise Level Rating"),
  #                              span(class = "rating", HTML("&#9733;&#9733;&#9733;&#9733;")),
  #                              span(class = "rating-grey", HTML("&#9734;")),
  #                              tags$br(),
  #                              tags$img(src = "https://cdn-icons-png.flaticon.com/512/166/166259.png", width = "30px")
  #                          )
  #                   )
  #                 )
  #             ),
  #             
  #             ## Energy Assessment
  #             div(class = "section-box",
  #                 h3("Energy Assessment"),
  #                 br(),
  #                 fluidRow(
  #                   column(2, div(class = "metric-box", div(class = "metric-title", "Total Demand"), div(class = "metric-value", "500", span(class = "unit", "kW")))),
  #                   column(2, div(class = "metric-box", div(class = "metric-title", "Avg. Energy Consumption"), div(class = "metric-value", "330", span(class = "unit", "kWh")))),
  #                   column(2, div(class = "metric-box", div(class = "metric-title", "Peak Load"), div(class = "metric-value", "2,000", span(class = "unit", "kWh")))),
  #                   column(2, div(class = "metric-box", div(class = "metric-title", "Off Peak Load"), div(class = "metric-value", "1,220", span(class = "unit", "kWh")))),
  #                   column(2, div(class = "metric-box", div(class = "metric-title", "Avg. Peak Load"), div(class = "metric-value", "400", span(class = "unit", "kWh"))))
  #                 )
  #             ),
  #             
  #             ## Financial Assessment Chart
  #             div(class = "section-box",
  #                 h3("Financial Assessment"),
  #                 p("12 months historical consumption vs spend from Grid (kWh)"),
  #                 plotlyOutput("finance_plot", height = "400px")
  #             )
  #     ),
  #     
  #     tabItem(tabName = "all_hospitals",
  #             fluidRow(
  #               column(12,
  #                      h3("32 Hospitals"),
  #                      p("This is a demo view."),
  #                      textInput("search", NULL, placeholder = "Search", width = "100%")
  #               )
  #             ),
  #             uiOutput("hospital_cards"),
  #             
  #             div(id = "detailPanel",
  #                 actionButton("closePanel", "Close", class = "btn btn-danger"),
  #                 uiOutput("detailContent")
  #             ),
  #             
  #             br(),
  #             fluidRow(
  #               column(12, align = "center",
  #                      actionButton("prevPage", "<", class = "pagination-btn"),
  #                      uiOutput("page_numbers"),
  #                      actionButton("nextPage", ">", class = "pagination-btn")
  #               )
  #             )
  #     ),
  #     
  #     
  #     
  #     tabItem(tabName = "geo_info",
  #             h2("Geospatial Information Page (Coming Soon)")
  #     )
  #   ),
  # 
  # # OUTSIDE of tabItems!
  # div(id = "detailPanel",
  #     actionButton("closePanel", "Close"),
  #     uiOutput("detailContent")
  # )
  # )
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .small-box {
          border-radius: 15px;
          padding: 30px;
          color: black !important;
          background-color: #f9f9f9 !important;
          box-shadow: 0 2px 6px rgba(0,0,0,0.1);
          text-align: center;
        }

        .section-box {
          border-radius: 20px;
          background-color: white;
          padding: 20px;
          margin-bottom: 30px;
          box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        }
        .rating {
          font-size: 20px;
          color: #ffc107;
        }
        .rating-grey {
          color: #e0e0e0;
        }
        .metric-box {
          border: 1px solid #e5e5e5;
          border-radius: 12px;
          padding: 20px;
          text-align: center;
          background-color: #fdfdfd;
          box-shadow: 0 1px 3px rgba(0,0,0,0.05);
        }
        .metric-title {
          font-size: 14px;
          color: #888;
          margin-bottom: 8px;
        }
        .metric-value {
          font-weight: bold;
          font-size: 24px;
        }
        .unit {
          font-size: 14px;
        }

        .star { color: #ffc107; font-size: 20px; } 
        .star-grey { color: #e0e0e0; } 

        .card {
          border: 1px solid #ccc; 
          padding: 15px; 
          margin: 10px; 
          border-radius: 8px; 
          background-color: #fff;
          box-shadow: 0 2px 6px rgba(0,0,0,0.1);
          cursor: pointer;
        }

        .hospital-title { 
          background-color: #333; 
          color: #fff; 
          padding: 5px; 
          border-radius: 5px 5px 0 0; 
          font-weight: bold; 
        } 

        .pagination-btn { margin: 5px; } 

        #detailPanel {
          position: fixed;
          top: 0;
          right: 0;
          width: 100%;
          height: 100%;
          background-color: white;
          z-index: 9999;
          transform: translateX(100%);
          transition: transform 0.4s ease-in-out;
          padding: 20px;
          overflow-y: auto;
        }

        #detailPanel.open {
          transform: translateX(0%);
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "summary",
              fluidRow(
                column(12,
                       selectInput("hospital_type", "Select Hospital Type:", 
                                   choices = c("All Hospital", "General", "Maternity", "Orthopedic", "Pediatric"),
                                   selected = "All Hospital")
                )
              ),
              br(),
              fluidRow(
                column(3, div(class = "small-box", h3("5"), p("General Hospitals"))),
                column(3, div(class = "small-box", h3("20"), p("Maternity Hospital"))),
                column(3, div(class = "small-box", h3("3"), p("Orthopedic Hospital"))),
                column(3, div(class = "small-box", h3("4"), p("Pediatric Hospital")))
              ),
              
              div(class = "section-box",
                  h3("Environmental Assessment"),
                  br(),
                  fluidRow(
                    column(4, div(class = "metric-box", div(class = "metric-title", "CO₂ Emission"), div(class = "metric-value", "300", span(class = "unit", "kg")), tags$img(src = "https://cdn-icons-png.flaticon.com/512/166/166259.png", width = "30px"))),
                    column(4, div(class = "metric-box", div(class = "metric-title", "Air Quality Rating"), span(class = "rating", HTML("&#9733;&#9733;&#9733;&#9733;")), span(class = "rating-grey", HTML("&#9734;")), tags$br(), tags$img(src = "https://cdn-icons-png.flaticon.com/512/166/166259.png", width = "30px"))),
                    column(4, div(class = "metric-box", div(class = "metric-title", "Noise Level Rating"), span(class = "rating", HTML("&#9733;&#9733;&#9733;&#9733;")), span(class = "rating-grey", HTML("&#9734;")), tags$br(), tags$img(src = "https://cdn-icons-png.flaticon.com/512/166/166259.png", width = "30px")))
                  )
              ),
              
              div(class = "section-box",
                  h3("Energy Assessment"),
                  br(),
                  fluidRow(
                    column(2, div(class = "metric-box", div(class = "metric-title", "Total Demand"), div(class = "metric-value", "500", span(class = "unit", "kW")))),
                    column(2, div(class = "metric-box", div(class = "metric-title", "Avg. Energy Consumption"), div(class = "metric-value", "330", span(class = "unit", "kWh")))),
                    column(2, div(class = "metric-box", div(class = "metric-title", "Peak Load"), div(class = "metric-value", "2,000", span(class = "unit", "kWh")))),
                    column(2, div(class = "metric-box", div(class = "metric-title", "Off Peak Load"), div(class = "metric-value", "1,220", span(class = "unit", "kWh")))),
                    column(2, div(class = "metric-box", div(class = "metric-title", "Avg. Peak Load"), div(class = "metric-value", "400", span(class = "unit", "kWh"))))
                  )
              ),
              
              div(class = "section-box",
                  h3("Financial Assessment"),
                  p("12 months historical consumption vs spend from Grid (kWh)"),
                  plotlyOutput("finance_plot", height = "400px")
              )
      ),
      
      tabItem(tabName = "all_hospitals",
              fluidRow(
                column(12,
                       h3("32 Hospitals"),
                       p("This is a demo view."),
                       textInput("search", NULL, placeholder = "Search", width = "100%")
                )
              ),
              uiOutput("hospital_cards"),
              br(),
              fluidRow(
                column(12, align = "center",
                       actionButton("prevPage", "<", class = "pagination-btn"),
                       uiOutput("page_numbers"),
                       actionButton("nextPage", ">", class = "pagination-btn")
                )
              )
      ),
      
      # tabItem(tabName = "geo_info",
      #         h2("Geospatial Information Page (Coming Soon)")
      # )
      
      tabItem(tabName = "geo_info",
              fluidRow(
                box(title = "Geospatial Heatmap", width = 12, solidHeader = TRUE, status = "primary",
                    leafletOutput("geo_map", height = 1000)
                )
              )
      )
      
    ),
    
    div(id = "detailPanel",
        actionButton("closePanel", "Close", class = "btn btn-danger"),
        uiOutput("detailContent")
    )
  )
)


