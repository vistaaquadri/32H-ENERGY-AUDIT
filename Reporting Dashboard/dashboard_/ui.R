# app.R
library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(DT)
library(leaflet)
library(bslib)
library(shinyWidgets)



# Define UI for application that draws a histogram
fluidPage(theme = "css/button.css",
          useShinyjs(),
          tags$script(src = "https://kit.fontawesome.com/cd04ebad6e.js"),
          tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@400;700;900&display=swap"),
          tags$head(tags$script(type="text/javascript", src = "scripts/code.js")),
          tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/button.css")),
          tags$style(type="text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"
          ),
          
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

        
        page_navbar(
          nav_panel(
            "Home",
            
            
            
            ##############################
              tabName = "project_summary",
              fluidRow(
                box(
                  width = 12,
                  solidHeader = FALSE,
                  status = NULL,
                  style = "background:transparent; border:none; padding:10px 0 30px 0;",
                  
                  tags$head(
                    tags$style(HTML("
/* Project Summary Card */
.project-summary {
  position: relative;
  border-radius: 18px;
  overflow: hidden;
    background-repeat: repeat;   /* tile the image */
    background-size: auto;       /* keep original size for tiling */
  min-height: 300px;
  padding: 32px;
  background-image: url('images/landing_page_img.png'); /* your local image in www/ */
  # background-size: cover;
  background-position: center;
  box-shadow: 0 12px 28px rgba(0,0,0,0.35);
  color: white;
}




/* Inner content above background */
.project-summary .ps-inner { 
  position: relative; 
  z-index: 1; 
}

/* Title & paragraph styling */
.ps-title { 
  font-size: 28px; 
  font-weight: 700; 
  margin-bottom: 12px; 
  text-shadow: 0 2px 4px rgba(0,0,0,0.6); 
}

.ps-desc { 
  font-size: 14px; 
  line-height: 1.6; 
  margin-bottom: 18px; 
  text-shadow: 0 1px 3px rgba(0,0,0,0.5); 
}

/* Divider */
.ps-hr { 
  border: 0; 
  height: 2px; 
  background: rgba(255,255,255,0.2); 
  margin: 18px 0 22px; 
  border-radius: 2px; 
}

/* Bottom row: logos left, metrics right */
.ps-bottom {
  display: flex;
  gap: 18px;
  align-items: center;
  justify-content: space-between;
  flex-wrap: wrap;
}

/* Logos section */
.ps-logos {
  display: flex;
  gap: 14px;
  align-items: center;
  flex-wrap: wrap;
  width: 60%;
}

.logo-card {
  background: rgba(255,255,255,0.98);
  padding: 10px;
  border-radius: 10px;
  display: flex;
  align-items: center;
  justify-content: center;
  min-width: 90px;
  min-height: 64px;
  box-shadow: 0 6px 18px rgba(0,0,0,0.12);
}

.logo-card img { 
  max-height: 50px; 
  max-width: 100%; 
  object-fit: contain; 
}

/* Metrics section */
.ps-metrics {
  display: flex;
  gap: 16px;
  align-items: center;
  justify-content: flex-end;
  width: 38%;
  flex-wrap: wrap;
}

.metric-card {
  background: rgba(255,255,255,0.06);
  border-radius: 12px;
  padding: 14px 18px;
  min-width: 120px;
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 6px;
  text-align: center;
  backdrop-filter: blur(4px);
  box-shadow: 0 6px 14px rgba(0,0,0,0.12);
}

.metric-icon {
  display:inline-flex;
  align-items:center;
  justify-content:center;
  width:44px;
  height:44px;
  border-radius:50%;
  background: rgba(255,255,255,0.08);
  font-size:18px;
}

.metric-number { font-size: 22px; font-weight: 700; color: #ffffff; }
.metric-label { font-size: 12px; opacity: 0.95; }

/* Mobile responsive */
@media (max-width: 880px) {
  .ps-logos { width: 100%; justify-content: flex-start; margin-bottom: 10px; }
  .ps-metrics { width: 100%; justify-content: space-between; }
  .project-summary { padding: 22px; }
}
        "))
                  ),
                  
                  # Card content
                  div(class = "project-summary",
                      div(class = "ps-inner",
                          tags$h2(class = "ps-title", "Project Summary"),
                          tags$p(class = "ps-desc",
                                 "Lorem Ipsum is simply dummy text of the printing and typesetting industry.
                      Lorem Ipsum has been the industry's standard dummy text ever since the 1500s."
                          ),
                          tags$hr(class = "ps-hr"),
                          
                          div(class = "ps-bottom",
                              # Logos (left)
                              div(class = "ps-logos",
                                  div(class = "logo-card", img(src = "logo_lga.png")),
                                  div(class = "logo-card", img(src = "logo_power.png")),
                                  div(class = "logo-card", img(src = "logo_rea.png")),
                                  div(class = "logo-card", img(src = "logo_wb.png"))
                              ),
                              
                              # Metrics (right)
                              div(class = "ps-metrics",
                                  div(class = "metric-card",
                                      div(class = "metric-icon", icon("hospital")),
                                      # div(class = "metric-number", textOutput("total_hospitals")),
                                      div(class = "metric-number", 34),
                                      div(class = "metric-label", "Total Hospitals")
                                  ),
                                  div(class = "metric-card",
                                      div(class = "metric-icon", icon("map-marker")),
                                      # div(class = "metric-number", textOutput("total_lgas")),
                                      div(class = "metric-number", 56),
                                      div(class = "metric-label", "Total LGA's")
                                  ),
                                  div(class = "metric-card",
                                      div(class = "metric-icon", icon("layer-group")),
                                      # div(class = "metric-number", textOutput("total_senatorial")),
                                      div(class = "metric-number", 45),
                                      div(class = "metric-label", "Total Senatorial Districts")
                                  )
                              )
                          )
                      )
                  )
                )
              )
            ,
            ##############################
            
            fluidRow(
              column(12,
                     selectInput("hospital_type", "Select Hospital Type:", 
                                 choices = c("All Hospital", "General", "Maternity", "Orthopedic", "Pediatric"),
                                 selected = "All Hospital")
              )
            ),
            br(),
            fluidRow(
              column(6,),
              column(3, div(class = "small-box", h3(uiOutput("hospital_count")), p("General Hospitals"))),
              column(3, div(class = "small-box", h3(uiOutput("lga_count")), p("LGA Coverage")))
            ),
            
            div(class = "section-box",
                h3("Environmental Assessment General Overview (Lagos State)"),
                br(),
                fluidRow(
                  column(4, div(class = "metric-box", div(class = "metric-title", "CO₂ Emission"), div(class = "metric-value", "300", span(class = "unit", "kg")), tags$img(src = "https://cdn-icons-png.flaticon.com/512/166/166259.png", width = "30px"))),
                  column(4, div(class = "metric-box", div(class = "metric-title", "Air Quality Rating"), span(class = "rating", HTML("&#9733;&#9733;&#9733;&#9733;")), span(class = "rating-grey", HTML("&#9734;")), tags$br(), tags$img(src = "https://cdn-icons-png.flaticon.com/512/166/166259.png", width = "30px"))),
                  column(4, div(class = "metric-box", div(class = "metric-title", "Noise Level Rating"), span(class = "rating", HTML("&#9733;&#9733;&#9733;&#9733;")), span(class = "rating-grey", HTML("&#9734;")), tags$br(), tags$img(src = "https://cdn-icons-png.flaticon.com/512/166/166259.png", width = "30px")))
                )
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
        
        
          nav_panel(
            "All Hospitals",
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
                    ),
            
            div(id = "detailPanel",
                actionButton("closePanel", "Close", class = "btn btn-danger"),
                uiOutput("detailContent")
            ),
          ),

          nav_panel(
            "Geopspatial",
                    fluidRow(
                      box(title = "Geospatial Heatmap", width = 12, solidHeader = TRUE, status = "primary",
                          leafletOutput("geo_map", height = 1000)
                      )
                    )
          ),
          


        title = "32H Survey", 
        id = "page", 
        
        ),

) 


