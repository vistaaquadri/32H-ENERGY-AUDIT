#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/


library(shiny)
library(bslib)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(shinyjs)

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
                          .container {
            display: grid;
            grid-template-columns: 0.5fr 3fr;
            grid-template-rows: 0.5fr 3fr;
            gap: 10px;
            width: 100%;
            background: white;
            padding: 20px;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        }
        .axis-label {
            writing-mode: vertical-rl;
            text-align: center;
            font-weight: bold;
        }
        .x-axis-label {
            grid-column: 2;
            text-align: center;
            font-weight: bold;
            margin-top: 10px;
        }
        .grid {
            display: grid;
            grid-template-columns: 1fr 1fr;
            grid-template-rows: 1fr 1fr;
            gap: 10px;
        }
        .box {
            background: #e8f4ff;
            padding: 15px;
            border-radius: 5px;
            display: flex;
            flex-direction: column;
            justify-content: space-between;
            box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
        }
        .score {
            background: #4CAF50;
            color: white;
            text-align: center;
            font-size: 18px;
            font-weight: bold;
            padding: 10px;
            border-radius: 5px;
            align-self: flex-end;
        }
        .axis-lines {
            #position: absolute;
            width: 1px;
            height: 100%;
            background: black;
            left: 120px;
            top: 50px;
        }
        .horizontal-line {
            #position: absolute;
            width: 80%;
            height: 2px;
            background: black;
            left: 150px;
            top: 10px;
        }

                "))
          ),
          
          tags$head(
            tags$style(HTML("
                  .box-container {
                    display: flex;
                    #align-items: center;
                    background-color: #F5FAFF;
                    border-radius: 8px;
                    padding: 10px;
                    margin: 5px;
                  }
                  .box-container-2 {
                    #display: block;
                    display: flex;
                    align-items: left;
                    margin-right: 50px;
                  }
                  .box-container-3 {
                    #display: block;
                    display: block;
                    align-items: right;
                    align: right;
                    justify-content: space-between;
                    text-align: right;
                  }
                  .number-box {
                    background-color: #4CAF50;
                    color: white;
                    font-weight: bold;
                    font-size: 18px;
                    padding: 10px 15px;
                    border-radius: 8px 0 0 8px;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    width: 50px;
                    height: 50px;
                  }
                  .text-box {
                    flex: 1;
                    padding-left: 17px;
                    font-weight: bold;
                    font-size: 12px;
                    color: #333;
                  }
                  .text-box-2 {
                    flex: 1;
                    padding-left: 14px;
                    font-weight: light;
                    font-size: 12px;
                    color: #333;
                  }
                  .info-icon {
                    margin-left: auto;
                    color: #888;
                    font-size: 16px;
                  }
                  .card {
                    background: white;
                    border-radius: 12px;
                    padding: 15px;
                    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
                  }

                  .progress-label {
                    font-size: 14px;
                    font-weight: bold;
                  }

                  .score-box {
                  font-size: 30px;
                  font-weight: bold;
                  color: #2d8b57;
                  text-align: right;
                  padding-right: 20px;
                }
                .section-title {
                  font-weight: bold;
                  font-size: 20px;
                  margin-bottom: 10px;
                }
                .metric-label {
                  font-weight: 600;
                }
                .comment-box {
                  background-color: #f9f9f9;
                  padding: 15px;
                  border-top: 1px solid #e0e0e0;
                  margin-top: 15px;
                  font-style: italic;
                }
                  .icon-circle {
                    width: 35px;
                    height: 35px;
                    background: #f0f0f0;
                    border-radius: 50%;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    margin-right: 10px;
                  }
                  .policy-header {
                    display: flex;
                    justify-content: space-between;
                    align-items: center;
                    font-weight: bold;
                    font-size: 16px;
                  }
                  .axis-label-left {
                    position: relative;
                    left: 0px;
                    margin-right: 5px;
                    font-weight: bold;
                    font-sixe: 12px;
                    #border-top: 6px solid transparent;
                    #border-bottom: 6px solid transparent;
                    border-right: 3px solid #ccc;
                    height: 35%
                  }
                  .axis-label-left-2 {
                    #position: relative;
                   # writing-mode: vertical-rl;
                   # transform: rotate(180deg);
                    #position: absolute;
                    #top: 100px;
                    left: 0px;
                    margin-right: 5px;
                    font-weight: light;
                    #border-top: 6px solid transparent;
                    #border-bottom: 6px solid transparent;
                    border-right: 3px solid #ccc;
                    font-size: 12px;
                    margin-top : 0px;
                    height: 30%
                  }
                  .bottom-label-border {
                  border-top: 3px solid #ccc;
                  }
                  .axis-label-bottom {
                    position: relative;
                    text-align: center;
                    margin-top: 10px;
                    font-weight: bold;
                    font-sixe: 12px;
                    #border-top: 3px solid #ccc;
                    }
                  .axis-label-bottom-2 {
                    text-align: left;
                    margin-top: 10px;
                    font-weight: light;
                    font-size: 12px;
                    #border-top: 3px solid #ccc;
                    }
                  .arrow-up::before {
                    content: '↑';
                    display: block;
                    font-size: 18px;
                  }
                  .arrow-right::after {
                    content: '→';
                    display: inline-block;
                    font-size: 18px;
                    margin-left: 5px;
                  }
                  
                  .timeline {
                    display: flex;
                    align-items: center;
                    justify-content: space-between;
                    margin-top: 50px;
                    margin-bottom: 50px;
                  }
            
                  .timeline-step {
                    text-align: center;
                    flex: 1;
                    position: relative;
                  }
            
                  .timeline-line {
                    position: absolute;
                    top: 18px;
                    left: 50%;
                    transform: translateX(-50%);
                    height: 10px;
                    width: 100%;
                    background: linear-gradient(to right, #E6F7F3 0%, #A6E2E1 25%, #6AC7A7 50%, #1F9B57 75%, #157F38 100%);
                    z-index: -1;
                    border-radius: 50px;
                  }
            
                  .timeline-circle {
                    width: 20px;
                    height: 20px;
                    background-color: #ffffff;
                    border: 6px solid;
                    border-radius: 50%;
                    display: inline-block;
                    z-index: 1;
                  }
            
                  .circle-1 { border-color: #A6E2E1; }
                  .circle-2 { border-color: #6AC7A7; }
                  .circle-3 { border-color: #1F9B57; }
                  .circle-4 { border-color: #157F38; }
            
                  .timeline-label {
                    margin-top: 10px;
                    font-family: 'Poppins', Poppins;
                    font-weight: 500;
                    font-size: 14px;
                  }
                  .timeline-container {
                    width: 100%;
                    text-align: center;
                    margin-top: 40px;
                  }
            
                  .milestone-labels {
                    display: flex;
                    justify-content: space-between;
                    margin-top: 8px;
                    font-family: 'Poppins', Poppins;
                    font-weight: 500;
                    font-size: 13px;
                    padding: 0 20px;
                  }
            
                  .milestone-labels div {
                    flex: 1;
                    text-align: center;
                  }
                  
                  .section-title {
                    font-size: 28px;
                    font-weight: 700;
                    line-height: 1.4;
                    color: 075D04;
                  }
                  .highlight {
                    color: #3aaa35; /* green for 'Phase 1' */
                  }
   
                  
                  .metric-box {
                      display: flex;                /* Enables flexbox */
                      justify-content: center;     /* Centers horizontally */
                      align-items: center;         /* Centers vertically */
                      flex-direction: column;      /* Ensures stacked layout if there are multiple lines */
                      
                      width: 200px;
                      height: 100px;               /* Set fixed height to center within */
                      margin-right: 20px;
                      padding: 20px;
                      border-radius: 5px;
                      text-align: center;
                      
                      color: white;
                      font-weight: 500;
                      font-size: 25px;
                    }

                  .light-box {
                    background-color: #eef6fa;
                    color: #333;
                  }
                  .green-box {
                    background-color: #62c98d;
                  }
                  .dark-green-box {
                    background-color: #126b1e;
                  }
                  .label {
                    font-size: 14px;
                    color: #666;
                    margin-top: 10px;
                    
                  }
                  .goal-box {
                    background-color: #f0f8ff;
                    padding: 10px 20px;
                    border-radius: 6px;
                    font-weight: bold;
                    font-size: 25px;
                    color: #1c4b27;
                  }

                .timeline-wrapper {
                  display: flex;
                  flex-direction: column;
                  align-items: center;
                  margin-top: 40px;
                  font-family: 'Poppins', Poppins;
                }
          
                .labels-top, .labels-bottom {
                  display: flex;
                  justify-content: space-between;
                  width: 90%;
                  margin: 0 auto;
                  font-weight: 500;
                  font-size: 13px;
                }
          
                .labels-top {
                  margin-bottom: -10px;
                }
          
                .labels-bottom {
                  margin-top: -10px;
                }
          
                .labels-top div, .labels-bottom div {
                  width: 25%;
                  text-align: center;
                }
          
                svg {
                  display: block;
                  margin: 0 auto;
                }
                
                      .card-grid {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        gap: 20px;
        margin-top: 30px;
      }

      .card {
        background-color: white;
        border-radius: 12px;
        padding: 20px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.05);
        text-align: center;
        font-family: 'Poppins', Poppins;
      }

      .card-icon {
        width: 40px;
        height: 40px;
        background-color: #EAF0F8;
        border-radius: 50%;
        margin: 0 auto 10px auto;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 20px;
        color: #64748B;
      }

      .card-title {
        font-size: 14px;
        color: #595959;
        margin-bottom: 10px;
      }

      .card-number {
        font-size: 24px;
        font-weight: 600;
        color: #111827;
      }
      .nesip-title {
        font-family: 'Poppins', sans-serif;
        font-size: 40px;
        line-height: 1.4;
        color: #595959;
      }
      .first-letter {
        font-weight: 900;
        font-size: 45px;
        color: #004d00;
      }
      .phase {
        color: #00cc00;
        font-weight: bold;
      }


              .box-in {
        width: 75px;
        height: 50px;
        background-color: #b2e2e2;
        color: white;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        font-weight: bold;
        font-size: 18px;
        margin: 10px;
        border-radius: 8px;
              }
            .box-in-1 {
        width: 75px;
        height: 50px;
        background-color: #66C2A4;
        color: white;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        font-weight: bold;
        font-size: 18px;
        margin: 10px;
        border-radius: 8px;
            }
            .box-in-2 {
        width: 75px;
        height: 50px;
        background-color: #238b45;
        color: white;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        font-weight: bold;
        font-size: 18px;
        margin: 10px;
        border-radius: 8px;
      }
        .arrow-1 {
    display: inline-block;
            align-items: center;
        justify-content: center;
    font-size: 40px;         /* Bigger arrow */
    color:  #66C2A4;
    font-weight: 700;        /* Bold */
    margin: 0 10px;
    transform: translateY(-20px);
        }
  
          .arrow-2 {
    display: inline-block;
            align-items: center;
        justify-content: center;
    font-size: 40px;         /* Bigger arrow */
    color: #238b45;
    font-weight: 700;        /* Bold */
    margin: 0 10px;
    transform: translateY(-20px);
          }
  
  
        .tabbable > .nav > li > a {
        color: #555;
        font-weight: 500;
        font-size: 16px;
      }
      .tabbable > .nav > li[class=active] > a {
        border: none;
        border-bottom: 3px solid green;
        color: black;
        font-weight: 600;
      }
      
      
      
            .leaderboard-entry {
            width: 70%;
        display: flex;
        align-items: center;
        justify-content: space-between;
        border: 1px solid #eee;
        border-radius: 8px;
        padding: 10px 15px;
        margin-bottom: 10px;
        background-color: white;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
      }
      .leaderboard-rank {
        font-weight: bold;
        margin-right: 10px;
      }
      .leaderboard-logo {
        height: 30px;
        margin-right: 10px;
      }
      .leaderboard-name {
        font-weight: 600;
        color: #1B5E20;
        margin-right: auto;
      }
      .leaderboard-score {
        font-weight: bold;
        font-size: 18px;
        color: #1B5E20;
        background-color: #f3f6f9;
        padding: 5px 10px;
        border-radius: 6px;
        margin-right: 15px;
      }
      .leaderboard-status {
        color: #666;
        margin-right: 10px;
      }

      .pagination-link {
        margin: 0 5px;
        cursor: pointer;
        color: blue;
        font-weight: bold;
      }
      .pagination-active {
        text-decoration: underline;
        color: darkblue;
      }
      

    "))
          ),
          
          
          
          page_navbar(
            nav_panel(
              "Home",
              
              fluidRow(
                column(
                  width = 6,
                  div(
                    class = "section-title",
                    div(class = "nesip-title",
                        HTML(
                          paste(
                            "<span class='first-letter'>N</span>ational<br>",
                            "<span class='first-letter'>E</span>lectrification<br>",
                            "<span class='first-letter'>S</span>trategy and<br>",
                            "<span class='first-letter'>I</span>mplementation<br>",
                            "<span class='first-letter'>P</span>lan – <span class='phase'>Phase 1</span>"
                          )
                        )
                    )
                    # p(HTML("National<br>Electrification<br>Strategy and<br>Implementation<br>Plan <b>(NESIP)</b> - <span class='highlight'>Phase 1</span>"),
                    #   style = 'font-size: 45px')
                  ),
                  br(),
                  p("As part of the NESIP, stakeholders involved in the electrification process have been identified, engaged, and surveyed. This site presents the findings of that engagement and survey, including assessing the capacity and readiness of sub-national governments to contribute to the NESIP mission of universal electricity access.",
                    style = 'color: #595959;'),
                  
                  
                  fluidRow(
                    column(
                      width = 3,
                      align = "center",
                      tags$a(href = "https://statehouse.gov.ng/", target = "_blank",
                             tags$img(src = "images/fon.png")
                      )
                    ),
                    column(
                      width = 3,
                      align = "center",
                      tags$a(href = "https://power.gov.ng/", target = "_blank",
                             tags$img(src = "images/mop.png")
                      )
                    ),
                    column(
                      width = 3,
                      align = "center",
                      tags$a(href = "https://rea.gov.ng/", target = "_blank",
                             tags$img(src = "images/rea.png")
                      )
                    ),
                    column(
                      width = 3,
                      align = "center",
                      tags$a(href = "https://www.worldbank.org/ext/en/home", target = "_blank",
                             tags$img(src = "images/wb.png")
                      )
                    ),
                    style = 'margin-top: 20px;',
                  ),
                  
                ),
                
                # Right Side
                column(
                  width = 6,
                  h6("The Goal of The NESIP",
                     style = 'color: #595959;'),
                  div(class = "goal-box", "Universal Electricity Access by 2030"),
                  br(),
                  hr(),
                  h6("Summary Insights of the NESIP (Phase 1)",
                     style = 'color: #595959;'),
                  p("The NESIP is a strategic undertaking and initiative that aims to provide a well-defined roadmap for transforming Nigeria’s vision of universal electricity access into reality.",
                    style = 'color: #595959;'),

                  
                  fluidRow(
                    div(class = "card-grid",
                        # You can reuse this pattern for all cards
                        div(class = "card",
                            div(class = "card-icon", icon("handshake")),
                            div(class = "card-title", "Total Identified Stakeholders"),
                            div(class = "card-number", textOutput("stakeholder_count_home_2"))
                        ),
                        div(class = "card",
                            div(class = "card-icon", icon("hourglass-half")),
                            div(class = "card-title", "Average Sub-National Readiness Score"),
                            div(class = "card-number", "2/5")
                        ),
                        div(class = "card",
                            div(class = "card-icon", icon("bolt")),
                            div(class = "card-title", "Energy Access (National Level"),
                            div(class = "card-number", "67.2 %")
                        ),
                    )
                    ),
                  fluidRow(
                    column(
                      width = 12, 
                      p("",
                        style = 'margin:15px')                      
                    )
                  ),
                  
                  fluidRow(
                    column(
                      width = 11,
                      actionButton("about_link_home", "View More Information About NESIP", width = "100%",
                                   style="color: #075D04; background-color: #FFFFFF; border-color: #075D04; margin: 20px;")
                    )
                  )
                )
              ),
              
              
              br(),
              hr(),
              fluidRow(
                column(
                  width = 7,
                  
                  fluidRow(
                    column(
                      width = 12, 
                      p("",
                        style = 'margin:5px')                      
                    )
                  ),
                  fluidRow(
                    column(
                      width = 6,
                      
                      fluidRow(
                        p('Identified Stakeholders by Category',
                          # style = 'font:Poppins; font-weight: 500; font-size: 20px; line-height: 1.6; letter-spacing: 2%; color: #595959;')
                        style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;')
                      ),
                      
                      fluidRow(
                        column(
                          width = 12, 
                          p("",
                            style = 'margin:35px')                      
                        )
                      ),
                      
                      fluidRow(
                        column(
                          width = 6,
                          p("Count of Identified Stakeholders",
                            style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 1.6; letter-spacing: 2%; color: #595959;')
                        ),
                        column(
                          width = 6,
                          span(textOutput("stakeholder_count_home"),
                               style = 'font:Poppins; font-weight: 600; font-size: 160px; line-height: 100%; letter-spacing: 0%; color: #075D04;')
                        )
                      )
                    ),
                    column(
                      width = 6
                    )
                  ),
                  br(),
                  fluidRow(
                    column(
                      width = 12, 
                      p("",
                        style = 'margin:35px')                      
                    )
                  ),
                  
                  fluidRow(
                    div(class = "card-grid",
                        # You can reuse this pattern for all cards
                        div(class = "card",
                            div(class = "card-icon", icon("user")),
                            div(class = "card-title", "National (Non-Energy Related)"),
                            div(class = "card-number", textOutput("summary_nner_stakeholder_home"))
                        ),
                        div(class = "card",
                            div(class = "card-icon", icon("user")),
                            div(class = "card-title", "National Energy Related"),
                            div(class = "card-number", textOutput("summary_ner_stakeholder_home"))
                        ),
                        div(class = "card",
                            div(class = "card-icon", icon("user")),
                            div(class = "card-title", "State Energy Related"),
                            div(class = "card-number", textOutput("summary_state_stakeholder_home"))
                        ),
                        div(class = "card",
                            div(class = "card-icon", icon("user")),
                            div(class = "card-title", "Regulatory"),
                            div(class = "card-number", textOutput("summary_regulatory_stakeholder_home"))
                        ),
                        div(class = "card",
                            div(class = "card-icon", icon("user")),
                            div(class = "card-title", "Utilities & Private Sectors"),
                            div(class = "card-number", textOutput("summary_utilities_stakeholder_home"))
                        ),
                        div(class = "card",
                            div(class = "card-icon", icon("user")),
                            div(class = "card-title", "DFIs"),
                            div(class = "card-number", textOutput("summary_dfi_stakeholder_home"))
                        ),
                        div(class = "card",
                            div(class = "card-icon", icon("user")),
                            div(class = "card-title", "Other Financial Institutions"),
                            div(class = "card-number", textOutput("summary_ofi_stakeholder_home"))
                        ),
                        div(class = "card",
                            div(class = "card-icon", icon("user")),
                            div(class = "card-title", "Sector Support Institutions"),
                            div(class = "card-number", textOutput("summary_psi_stakeholder_home"))
                        ),
                        div(class = "card",
                            div(class = "card-icon", icon("user")),
                            div(class = "card-title", "Local Communities"),
                            div(class = "card-number", textOutput("summary_communities_stakeholder_home"))
                        )
                    )
                  ),
                  
                ####################################


                ###################################
                  
                ),
                column(
                  width = 5,
                  align = "left",
                  
                  fluidRow(
                    column(
                      width = 9,
   
                      h2("Status of Engagement Levels by State",
                    
                      style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;'),
                    ),
                    column(
                      width = 3,
                      p('',
                        style = 'font:Poppins; font-weight: 800; font-size: 20px; line-height: 1.6; letter-spacing: 2%; color: #595959; align:left;')
                    )
                    
                  ),
                  

                  fluidRow(
                    column(
                      width = 12, 
                      p("",
                        style = 'margin:5px')                      
                    )
                  ),

                  uiOutput("map_home"),
                  
                  fluidRow(
                    column(
                      width = 12, 
                      p("",
                        style = 'margin:20px')                      
                    )
                  ),
                  
                  # Layout with 3 boxes and 2 arrows between
                  fluidRow(
                    column(12, align = "center",
                           div(class = "box-in", ""),
                           span(class = "arrow-1", "\u2192"),  # Unicode arrow →
                           div(class = "box-in-1", ""),
                           span(class = "arrow-2", "\u2192"),  # Another arrow
                           div(class = "box-in-2", "")
                    )
                  ),
                  
                  hr(),
                  
                  fluidRow(
                    align = "left",
     
                    fluidRow(
                      column(
                        width = 1,
                        tags$i(class = "fa-solid fa-square fa-2x",
                               style = 'color: #b2e2e2; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                      ),
                      column(
                        width = 11,
                        fluidRow(
                          align = "left",
                          p(
                            span("Initiated Engagement", 
                                 style = 'font-family: Poppins; font-weight:700; font-size: 13px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                            span("First contact made, introducing letters, talking points and objectives.",
                                 style = 'font-family: Poppins; font-weight: 500; font-size: 11px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                          )
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        width = 1,
                        tags$i(class = "fa-solid fa-square fa-2x",
                               style = 'color: #66C2A4; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                      ),
                      column(
                        width = 11,
                        fluidRow(
                          align = "left",
                          p(
                            span("Engagement Ongoing", 
                                 style = 'font-family: Poppins; font-weight: 700; font-size: 13px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                            span("Active discussions, consultations and collaborations in progress.",
                                 style = 'font-family: Poppins; font-weight: 500; font-size: 11px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                          )
                        )
                      )
                    ),
                    
                    fluidRow(
                      column(
                        width = 1,
                        tags$i(class = "fa-solid fa-square fa-2x",
                               style = 'color: #238b45; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                      ),
                      column(
                        width = 11,
                        fluidRow(
                          align = "left",
                          p(
                            span("Engagement Completed", 
                                 style = 'font-family: Poppins; font-weight: 700; font-size: 13px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                            span("Stakeholder inputs received, and engagement phase concluded.",
                                 style = 'font-family: Poppins; font-weight: 500; font-size: 11px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                          )
                        )
                      )
                    ),

                    
                    
                  ),
                  
                  br(),
                  
                  fluidRow(
                    column(
                      width = 11,
                      actionButton("stakeholder_link_home", "View More Stakeholders Information", width = "100%",
                                   style="color: #075D04; background-color: #FFFFFF; border-color: #075D04; margin: 5px;")
                    )
                  ),
                  tags$script(HTML("
                      Shiny.addCustomMessageHandler('open_new_tab', function(message) {
                        window.open(message.url, '_blank');
                      });
                    ")),
                  
                  
                  fluidRow(
                    column(
                      width = 12, 
                      p("",
                        style = 'margin:5px')                      
                    )
                  )

                )
              ),
              
              hr(),
              
              fluidRow(
                h2("Sub-National Readiness Assessment Score",
                   # style = 'font:Poppins; font-weight: 500; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;'),
                
                fluidRow(
                  column(
                    width = 5,
                    align = "center",
                    
                    # plotOutput("readiness_score_home_2", width = "100%", height = "75%"),
                    plotOutput("readiness_score_home_2A", width = "100%", height = "70%"),
                    fluidRow(
                      align = "center",
                      margin = "20px",
                      column(
                        width = 3,

                        tags$i(class = "fa-solid fa-1 fa-2x",
                               style = 'color: #16D10F; margin: 10px;'),
                        p("Early Stage"),
                        popover(
                          tags$i(class = "fa-solid fa-circle-info fa-1x",
                                 style = 'color: #383A3D; margin: 5px;'),
                          title = "Early Stage",
                          p("
                            The state is at the earliest stage of readiness, having made no concrete efforts to localize electricity governance under the Electricity Act 2023. There is no state-level electricity policy or regulatory framework, and electrification receives less than 0.1% of the state’s annual budget. There is no active public-private partnership (PPP) unit for energy, and no foreign direct investment (FDI) or donor-backed energy projects have been secured. Infrastructure is severely limited, with less than 20% of households having access to reliable electricity, no mini-grid or embedded generation initiatives, and no state-driven grid expansion projects underway. Metering coverage is negligible, and the state lacks any modern or diverse energy systems. Capacity is minimal, with no personnel training initiatives, no electricity agency, and no structured processes for project planning, procurement, or stakeholder engagement. Data collection is virtually nonexistent, with no centralized repository and no systems for tracking household or productive-use electricity demand
                            ")
                        )
                      ),
                      column(
                        width = 3,
                        tags$i(class = "fa-solid fa-2 fa-2x",
                               style = 'color: #16D10F; margin: 10px;'),
                        p("Developing"),
                        popover(
                          tags$i(class = "fa-solid fa-circle-info fa-1x",
                                 style = 'color: #383A3D; margin: 5px;'),
                          title = "Developing",
                         
                        p("The state has begun engaging with the Electricity Act 2023, often through initial stakeholder consultations or the drafting of early-stage electrification policies. However, these efforts remain fragmented and have not yet led to meaningful implementation or enforcement mechanisms. Government funding for energy is still modest, typically ranging between 0.1% and 1% of the state budget. A PPP office may have been established, but it remains in its infancy, having facilitated fewer than five energy-related transactions. FDI inflows are minimal, often below US$10 million, and donor support remains limited. Infrastructure development is underway but sporadic, with household electricity access hovering between 20% and 40%. Most projects are small-scale and uncoordinated, such as isolated solar systems or early grid extension pilots. Grid reliability is weak, and metering infrastructure is still underdeveloped. While some basic energy planning structures or agencies exist, they lack the capacity and resources for consistent execution. Data systems are emergent but poorly organized, often housed in separate departments without central oversight or real-time accessibility")
                         )
                      ),
                      column(
                        width = 2,
                        tags$i(class = "fa-solid fa-3 fa-2x",
                               style = 'color: #16D10F; margin: 10px;'),
                        p("Progressing"),
                        popover(
                          tags$i(class = "fa-solid fa-circle-info fa-1x",
                                 style = 'color: #383A3D; margin: 5px;'),
                          title = "Progressing",
                          p("The state has developed a draft electricity market policy or law and is beginning to shift from planning to partial implementation. A legal framework is in place, and a state electricity regulatory commission may have been established, although regulatory enforcement remains weak or inconsistent. Budget allocations for energy have improved, typically ranging from 1% to 3% of the state’s total expenditure. The PPP office is functional and has supported between five and ten transactions, and the state has begun to attract FDI in the range of US$10 million to US$20 million, often tied to renewable energy or rural electrification programs. Infrastructure access has expanded, with between 40% and 60% of households connected to electricity, either through centralized grids, embedded generation, or mini-grid systems. Grid reinforcement and metering upgrades are ongoing, but service reliability remains uneven. Institutions responsible for electricity planning and execution are in place, but they remain underfunded and understaffed, though collaboration with federal agencies, development partners, and the private sector is increasing. Data systems are becoming more structured, with efforts to digitize and standardize energy access information, though integration and accessibility still pose major challenges")
                          #p("The state has drafted an electricity market policy or law in response to the Electricity Act 2023 but has not fully operationalized it. Some licensing and regulatory structures exist, but enforcement is still developing. Funding for electrification is improving, with increasing private sector participation and donor support, though financing gaps remain. Infrastructure is expanding, with growing investments in mini-grids, renewable energy projects, and state-supported grid enhancements, but supply is still insufficient. Institutions responsible for electricity planning and regulation are in place but remain underfunded and understaffed, though collaboration with federal agencies, development finance institutions (DFIs), and private sector partners is increasing. Energy data collection is more structured, with efforts to digitize and update information, but significant gaps still hinder real-time monitoring and planning")
                        )
                      ),
                      column(
                        width = 2,
                        tags$i(class = "fa-solid fa-4 fa-2x",
                               style = 'color: #16D10F; margin: 10px;'),
                        p("Advanced"),
                        popover(
                          tags$i(class = "fa-solid fa-circle-info fa-1x",
                                 style = 'color: #383A3D; margin: 5px;'),
                          title = "Advanced",
                          p("The state has passed a comprehensive electricity law and established a functional regulatory framework aligned with the Electricity Act 2023. A fully operational state electricity regulatory commission is overseeing licensing, tariff setting, and consumer protection. Funding for electrification is robust, with energy consistently receiving between 3% and 4% of the state’s budget. The PPP office is well-organized, having completed between ten and fifteen infrastructure transactions, and FDI inflows now exceed US$20 million, including investments in solar parks, gas-to-power plants, and grid modernization. Infrastructure is expanding rapidly, reaching 60%–80% of household demand. There is an increasing diversity of supply through grid extensions, mini-grids, and embedded generation, supported by targeted investments in metering infrastructure and load management systems. The state has professionalized its electricity institutions, developed clear procurement guidelines, and established partnerships for technical assistance and workforce training. Energy data is systematically collected and analyzed, with a centralized agency responsible for regular updates, geospatial mapping, and energy audits that inform policy and investment decisions")
                          #p("The state has passed its own electricity law or developed a well-defined electricity market framework in alignment with the Electricity Act 2023, with a functional state regulatory commission. Funding mechanisms for electrification are well-established, attracting significant private sector investment and donor-backed programs. Infrastructure is expanding rapidly, incorporating a diverse mix of generation sources, including on-grid, off-grid, mini-grids, and embedded generation, with electricity access reaching a majority of the population. Regulatory and planning institutions are well-structured, with efficient procurement processes, stakeholder engagement, and technical expertise to support electrification. The state maintains an organized energy database with periodic updates, geospatial mapping, and energy audits, ensuring data-driven decision-making.")
                        )
                      ),
                      column(
                        width = 2,
                        tags$i(class = "fa-solid fa-5 fa-2x",
                               style = 'color: #16D10F; margin: 10px;'),
                        p("Optimised"),
                        popover(
                          tags$i(class = "fa-solid fa-circle-info fa-1x",
                                 style = 'color: #383A3D; margin: 5px;'),
                          title = "Optimised",
                          p("The state has fully operationalized the Electricity Act 2023 and now runs a mature, decentralized electricity market. Its electricity law is fully implemented, supported by a dynamic and independent state regulatory commission with strong enforcement powers, transparent licensing processes, and cost-reflective tariff-setting. Energy funding is exemplary, with over 4% of the state’s annual budget allocated to electrification. The PPP office is highly active, completing over fifteen transactions, and the state attracts FDI exceeding US$100 million, driven by a strong investment climate and blended financing mechanisms, including green bonds and concessional financing. Infrastructure is modern, efficient, and reliable, with electricity access rates exceeding 80% and wide-scale adoption of smart meters, embedded renewable generation, and smart grid technologies. Energy institutions operate at full capacity with streamlined procurement, well-trained personnel, and ongoing regulatory reforms. A real-time digital energy platform integrates data from across the energy value chain, enabling predictive planning, performance monitoring, and adaptive policy development. The state’s energy governance is fully data-driven, investor-ready, and centered on inclusive, sustainable electrification.")
                          #p("The state has a fully functional electricity market under the Electricity Act 2023, with a strong regulatory commission, clear licensing procedures, and an efficient tariff structure. Sustainable financing models, including Public-Private Partnerships (PPPs), green bonds, and concessional financing, are in place, attracting major energy investments. Infrastructure is modern and well-maintained, with a reliable electricity network, widespread smart grid adoption, and seamless integration of renewable energy sources. The state's institutional capacity is highly advanced, with a skilled workforce, efficient governance, and proactive energy sector planning. Real-time data monitoring systems, open-access energy platforms, and advanced digital tools support electricity planning and operations, enabling fully data-driven decision-making and a robust energy market.")
                        )
                      )
                    ),
                    fluidRow(
                      p("Note: This assessment reflects aggregated national-level readiness across all 36 states and the Federal Capital Territory (FCT). Scores represent an average and are not state-specific.",
                        style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #595959;')
                      # p("Note: This assessment reflects aggregated national-level readiness across all 36 states and the Federal Capital Territory (FCT). Scores represent an average and are not state-specific.")
                    )
                  ),
                  
                  column(
                    width = 1,
                    
                    
                      fluidRow(
                        span(uiOutput("score_readiness_national_home_3"),
                             style = 'font:Poppins; font-weight: 700; font-size: 70px; line-height: 100%; letter-spacing: 2%; color: #075D04; padding-right:20px;')
                        
                      ),
                    fluidRow(
                      align = 'left',
                      style = 'margin-top: 110px;, height: 80%',
                      tableOutput("readiness_table"
                      )
                    ),

                  ),
                  column(
                    width = 1,
                    fluidRow(
                      align = 'center',
                      style = 'margin: 110px;, height: 80%',
                    ),
                    
                  ),

                  column(
                    width = 5,
                    fluidRow(
                      column(
                        width = 2,
                        tags$i(class = "fa-solid fa-gavel fa-2x",
                               style = 'color: #16D10F; margin: 10px;')
                      ),
                      column(
                        width = 10,
                        p("Policy Readiness Assessment:",
                          style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;'),
                        p("Evaluation of the state's policy framework and alignment with the Electricity Act 2023.",
                          style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 122%; letter-spacing: 2%; color: #595959;')
                      ),
                      style = 'margin: 20px;'
                    ),
                    fluidRow(
                      column(
                        width = 2,
                        tags$i(class = "fa-solid fa-hand-holding-dollar fa-2x",
                               style = 'color: #16D10F; margin: 10px;')
                      ),
                      column(
                        width = 10,
                        p("Funding and Investment Readiness Assessment:",
                          style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;'),
                        p("Evaluation of funding mechanisms and investment readiness for electrification projects.",
                          style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 122%; letter-spacing: 2%; color: #595959;')
                      ),
                      style = 'margin: 20px;'
                    ),
                    fluidRow(
                      column(
                        width = 2,
                        tags$i(class = "fa-solid fa-tower-observation fa-2x",
                               style = 'color: #16D10F; margin: 10px;')
                      ),
                      column(
                        width = 10,
                        p("Infrastructure Readiness Assessment:",
                          style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;'),
                        p("Evaluation of the state's energy infrastructure and its readiness for expanding electricity access.",
                          style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 122%; letter-spacing: 2%; color: #595959;')
                      ),
                      style = 'margin: 20px;'
                    ),
                    fluidRow(
                      column(
                        width = 2,
                        tags$i(class = "fa-solid fa-user-shield fa-2x",
                               style = 'color: #16D10F; margin: 10px;')
                      ),
                      column(
                        width = 10,
                        p("Capacity Readiness Assessment:",
                          style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;'),
                        p("Evaluation of the state's ability to execute, monitor, and manage electrification projects effectively.",
                          style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 122%; letter-spacing: 2%; color: #595959;')
                      ),
                      style = 'margin: 20px;'
                    ),
                    fluidRow(
                      column(
                        width = 2,
                        tags$i(class = "fa-solid fa-magnifying-glass-chart fa-2x",
                               style = 'color: #16D10F; margin: 1px;')
                      ),
                      column(
                        width = 10,
                        p("Data Readiness Assessment:",
                          style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;'),
                        p("Evaluation of the state's capacity for data collection, management, and utilization in energy-related decision making.",
                          style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 122%; letter-spacing: 2%; color: #595959;')
                      ),
                      style = 'margin: 10px;'
                    ),
                    br(),
                    fluidRow(
                      column(
                        width = 11,
                        actionButton("readiness_link_home", "View More Readiness Information", width = "100%",
                                     style="color: #075D04; background-color: #fff; border-color: #075D04; margin: 20px;")
                      )
                    )
                  )
                )
              ),
              
              hr(),
              
              fluidRow(
                h2("Electricity Access",
                   # style = 'font:Poppins; font-weight: 500; font-size: 25px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;'),
                h4("All non-percentage metrics are derived using the Reported median",
                   style = 'font:Poppins; font-weight: 250; font-size: 12px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                
                h6("----------------------------------------------------------------------------------------------",
                   style = 'font:Poppins; font-weight: 250; font-size: 10px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                
                fluidRow(
                  ######################
                  column(
                    width = 6,
                  
                    fluidRow(
                      column(
                        width = 5,
                        tags$i(class = "fa-solid fa-clock fa-2x",
                               style = 'color: #ffffff; margin: 10px;'),
                        span(textOutput("access_wdr_home"),
                             style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #FFFFFF;'),
                        p("Daily Hours of Grid Supply (Median)",
                          style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF; margin-top: 20px;'),
                        style = 'background-color: #6fd195; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                      ),
                      column(
                        width = 5,
                        tags$i(class = "fa-solid fa-bolt fa-2x",
                               style = 'color: #ffffff; margin: 10px;'),
                        span(textOutput("access_ecp_home"),
                             style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #FFFFFF;'),
                        p("Daily Energy Consumption (Median)",
                          style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF; margin-top: 20px;'),
                        style = 'background-color: #66C2A4; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                      )
                    ),
                    
                    fluidRow(
                      column(
                        width = 5,
                        tags$i(class = "fa-solid fa-money-bill-wave fa-2x",
                               style = 'color: #ffffff; margin: 10px;'),
                        span(textOutput("access_egs_home"),
                             style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #FFFFFF;'),
                        p("Daily Spend on Grid Supply (Median)",
                          style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF; margin-top: 20px;'),
                        style = 'background-color: #6bc16e; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                      ),
                      column(
                        width = 5,
                        tags$i(class = "fa-solid fa-money-bill-wave fa-2x",
                               style = 'color: #ffffff; margin: 10px;'),
                        span(textOutput("access_gen_spend_home"),
                             style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #FFFFFF;'),
                        p("Daily Spend on Alternate Supply (Median)",
                          style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF; margin-top: 20px;'),
                        style = 'background-color: #238b45; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                      )
                    ),
                    br(),
                    fluidRow(
                      column(
                        width = 11,
                        actionButton("access_link_home", "View More Energy Access Information", width = "100%",
                                     style="color: #075D04; background-color: #fff; border-color: #075D04; margin: 20px;")
                      )
                    ),
                  ),
                  column(
                    width = 6,
                    navset_tab( 
                      nav_panel(
                        "Considering only Capacity & Grid Availability",
                        plotOutput("access_dist_home_2")
                      ),
                      nav_panel(
                        "Considering All 8 Metrics",
                        plotOutput("access_dist_home")
                      )
                    ),
                    p("Percentage of households represented in each Tier of energy access across the country",
                      style = 'font:Poppins; font-weight: 600; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                    p("The World Bank’s Multi-Tier Framework (MTF) for Energy Access is a comprehensive approach to measuring energy access beyond a simple “yes” or “no” binary. It categorizes access into five tiers (Tier 0 to Tier 5) based on attributes like capacity, duration, reliability, affordability, legality, quality, and safety. Lower tiers (0-2) indicate limited access, such as solar lanterns or intermittent grid supply, while higher tiers (3-5) represent reliable, affordable, and high-capacity electricity for productive use.",
                      style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #595959;'),
                    br(),
  
                    style = 'margin-top:20px; margin-bottom: 30px;'
                    
                  )
                ),
                #########################
                br(),
                
                hr(),
                
                fluidRow(
                  column(
                    width = 5
                  ),
                  column(
                    width = 2,
                    align = "center",
                    tags$a(href = "https://rea.gov.ng/", target = "_blank",
                           tags$img(src = "images/rea.png")
                    )
                  ),
                  column(
                    width = 5
                  )

                )
              )
            ), 
            nav_panel(
              "Sub-National Readiness Assessment",
              value = 'readiness',
              fluidRow(
                column(
                  width = 3,
                  uiOutput("regions_readiness", width = "100%")
                ),
                column(
                  width = 3,
                  uiOutput("states_readiness", width = "100%")
                ),

                column(
                  width = 3,

                )
              ),
              
              hr(),
              
              
              
              
              fluidRow(
                
                ##########################################
                column(
                  width = 6,
                  
                  fluidRow(
                    column(
                      width = 9,
                      # h2(
                      #   "Sub-National Readiness",
                      #   align = "left",
                      #   style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #16D10F; margin-bottom: 20px;'
                      # ),
                      fluidRow(
                        # p('Sub-National Readiness Scoring',
                        p("Sub-National Readiness Chart By Scoring Criteria",
                              #style = 'font:Poppins; font-weight: 500; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                          style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;'),
                      
                      ),
                      #plotOutput("readiness_score_readiness", width = "100%"),
                      plotOutput("readiness_score_readiness_2A", width = "100%"),
                      
                    ),
                    # column(
                    #   width = 2,
                    #   height = "50%",
                    #     align = 'center',
                    #     style = 'margin: 110px;, height: 80%'
                    #     #tableOutput("readiness_table_2")
                    #   ),

                    # ),
                    column(
                      width = 2,
                      
                      fluidRow(
                        align = 'left',
                        style = 'margin-top: 180px;, height: 50%',
                        
                        
                        # tableOutput("readiness_table_2")
                        # p(textOutput("score_readiness_national_2"),
                        #   style= 'font-size: 350px; color: #16D10F; padding-left: 20px;')
                        span(textOutput("score_readiness_national_2"),
                             style = 'font:Poppins; font-weight: 700; font-size: 70px; line-height: 100%; letter-spacing: 2%; color: #075D04; padding-right:20px;')
                        
                      )
                    ),
                    column(
                      width = 1,
                    )
                    
                  ),


                  fluidRow(
                    align = "center",
                    column(
                      width = 3,

                      tags$i(class = "fa-solid fa-1 fa-2x",
                             style = 'color: #16D10F; margin: 10px;'),
                      p("Early Stage"),
                      popover(
                        tags$i(class = "fa-solid fa-circle-info fa-1x",
                               style = 'color: #383A3D; margin: 5px;'),
                        title = "Early Stage",
                        p("
                          The state is at the earliest stage of readiness, having made no concrete efforts to localize electricity governance under the Electricity Act 2023. There is no state-level electricity policy or regulatory framework, and electrification receives less than 0.1% of the state’s annual budget. There is no active public-private partnership (PPP) unit for energy, and no foreign direct investment (FDI) or donor-backed energy projects have been secured. Infrastructure is severely limited, with less than 20% of households having access to reliable electricity, no mini-grid or embedded generation initiatives, and no state-driven grid expansion projects underway. Metering coverage is negligible, and the state lacks any modern or diverse energy systems. Capacity is minimal, with no personnel training initiatives, no electricity agency, and no structured processes for project planning, procurement, or stakeholder engagement. Data collection is virtually nonexistent, with no centralized repository and no systems for tracking household or productive-use electricity demand
                          ")
                      )
                    ),
                    column(
                      width = 2,
                      tags$i(class = "fa-solid fa-2 fa-2x",
                             style = 'color: #16D10F; margin: 10px;'),
                      p("Developing"),
                      popover(
                        tags$i(class = "fa-solid fa-circle-info fa-1x",
                               style = 'color: #383A3D; margin: 5px;'),
                        title = "Developing",
                        p("
                          The state has begun engaging with the Electricity Act 2023, often through initial stakeholder consultations or the drafting of early-stage electrification policies. However, these efforts remain fragmented and have not yet led to meaningful implementation or enforcement mechanisms. Government funding for energy is still modest, typically ranging between 0.1% and 1% of the state budget. A PPP office may have been established, but it remains in its infancy, having facilitated fewer than five energy-related transactions. FDI inflows are minimal, often below US$10 million, and donor support remains limited. Infrastructure development is underway but sporadic, with household electricity access hovering between 20% and 40%. Most projects are small-scale and uncoordinated, such as isolated solar systems or early grid extension pilots. Grid reliability is weak, and metering infrastructure is still underdeveloped. While some basic energy planning structures or agencies exist, they lack the capacity and resources for consistent execution. Data systems are emergent but poorly organized, often housed in separate departments without central oversight or real-time accessibility
                          ")
                      )
                    ),
                    column(
                      width = 2,
                      tags$i(class = "fa-solid fa-3 fa-2x",
                             style = 'color: #16D10F; margin: 10px;'),
                      p("Progressing"),
                      popover(
                        tags$i(class = "fa-solid fa-circle-info fa-1x",
                               style = 'color: #383A3D; margin: 5px;'),
                        title = "Progressing",
                        p("
                          The state has developed a draft electricity market policy or law and is beginning to shift from planning to partial implementation. A legal framework is in place, and a state electricity regulatory commission may have been established, although regulatory enforcement remains weak or inconsistent. Budget allocations for energy have improved, typically ranging from 1% to 3% of the state’s total expenditure. The PPP office is functional and has supported between five and ten transactions, and the state has begun to attract FDI in the range of US$10 million to US$20 million, often tied to renewable energy or rural electrification programs. Infrastructure access has expanded, with between 40% and 60% of households connected to electricity, either through centralized grids, embedded generation, or mini-grid systems. Grid reinforcement and metering upgrades are ongoing, but service reliability remains uneven. Institutions responsible for electricity planning and execution are in place, but they remain underfunded and understaffed, though collaboration with federal agencies, development partners, and the private sector is increasing. Data systems are becoming more structured, with efforts to digitize and standardize energy access information, though integration and accessibility still pose major challenges
                          ")
                      )
                    ),
                    column(
                      width = 2,
                      tags$i(class = "fa-solid fa-4 fa-2x",
                             style = 'color: #16D10F; margin: 10px;'),
                      p("Advanced"),
                      popover(
                        tags$i(class = "fa-solid fa-circle-info fa-1x",
                               style = 'color: #383A3D; margin: 5px;'),
                        title = "Advanced",
                        p("
                          The state has passed a comprehensive electricity law and established a functional regulatory framework aligned with the Electricity Act 2023. A fully operational state electricity regulatory commission is overseeing licensing, tariff setting, and consumer protection. Funding for electrification is robust, with energy consistently receiving between 3% and 4% of the state’s budget. The PPP office is well-organized, having completed between ten and fifteen infrastructure transactions, and FDI inflows now exceed US$20 million, including investments in solar parks, gas-to-power plants, and grid modernization. Infrastructure is expanding rapidly, reaching 60%–80% of household demand. There is an increasing diversity of supply through grid extensions, mini-grids, and embedded generation, supported by targeted investments in metering infrastructure and load management systems. The state has professionalized its electricity institutions, developed clear procurement guidelines, and established partnerships for technical assistance and workforce training. Energy data is systematically collected and analyzed, with a centralized agency responsible for regular updates, geospatial mapping, and energy audits that inform policy and investment decisions
                          ")
                      )
                    ),
                    column(
                      width = 2,
                      tags$i(class = "fa-solid fa-5 fa-2x",
                             style = 'color: #16D10F; margin: 10px;'),
                      p("Optimised"),
                      popover(
                        tags$i(class = "fa-solid fa-circle-info fa-1x",
                               style = 'color: #383A3D; margin: 5px;'),
                        title = "Optimised",
                        p("
                          The state has fully operationalized the Electricity Act 2023 and now runs a mature, decentralized electricity market. Its electricity law is fully implemented, supported by a dynamic and independent state regulatory commission with strong enforcement powers, transparent licensing processes, and cost-reflective tariff-setting. Energy funding is exemplary, with over 4% of the state’s annual budget allocated to electrification. The PPP office is highly active, completing over fifteen transactions, and the state attracts FDI exceeding US$100 million, driven by a strong investment climate and blended financing mechanisms, including green bonds and concessional financing. Infrastructure is modern, efficient, and reliable, with electricity access rates exceeding 80% and wide-scale adoption of smart meters, embedded renewable generation, and smart grid technologies. Energy institutions operate at full capacity with streamlined procurement, well-trained personnel, and ongoing regulatory reforms. A real-time digital energy platform integrates data from across the energy value chain, enabling predictive planning, performance monitoring, and adaptive policy development. The state’s energy governance is fully data-driven, investor-ready, and centered on inclusive, sustainable electrification.
                          ")
                      )
                    )
                  ),
                  hr(),

                  fluidRow(
                    column(
                      width = 5,
                      style = 'padding: 15px; margin: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);',
                      fluidRow(
                        column(
                          width = 6,
                          p("Policy",
                            style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                        ),

                        column(
                          width = 4,
                          offset = 2,
                          align = "center",
                          style = "background-color: #F1F6FD; padding: 5px;",
                          p("Score",
                            style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                          fluidRow(
                            column(
                              width = 7,
                              align = "right",
                              span(textOutput("score_policy_readiness_2"),
                                   style = 'font:Poppins; font-weight: 700; font-size: 28px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            ),
                            column(
                              width = 5,
                              align = "left",
                              p("/5",
                                style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            )
                          )
                        )
                      ),
                   
###########################################################################
                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-gavel fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,
                            
                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Electrification Law", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_2")),
                                 span(uiOutput("score_readiness_22"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                            popover(
                              tags$i(class = "fa-solid fa-circle-info fa-1x",
                                     style = 'color: #088c1c;'),
                              title = "Electrification Law",
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 12,
                                  p("No color code",
                                    style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("No law has been drafted or initiated.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              #########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Stakeholder talks on electrification law started, no draft.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Draft electrification law developed, not yet submitted.	",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Law has been passed but not yet  widely publicized.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Law passed, gazetted, and under active implementation.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Law passed, gazetted, and under active implementation.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                            )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  
############################################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-file-contract fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Regulatory Framework", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_3")),
                                 span(uiOutput("score_readiness_33"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                            popover(
                              tags$i(class = "fa-solid fa-circle-info fa-1x",
                                     style = 'color: #088c1c;'),
                              title = "Regulatory Framework",
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 12,
                                  p("No color code",
                                    style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("No regulatory framework has been undertaken.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              #########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("State aware of Electricity Act 2023, discussions begun.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("SERC concept or policy proposal in development.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("SERC legally established, NERC transferred oversight.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("SERC structure set, staffing or training ongoing.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("SERC is fully operational and staffed",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                            )
                            
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  
############################################################################


############################################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-toolbox fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Electrification Plan/Strategy", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_4")),
                                span(uiOutput("score_readiness_44"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                                popover(
                                  tags$i(class = "fa-solid fa-circle-info fa-1x",
                                         style = 'color: #088c1c;'),
                                  title = "Electrification Plan/Strategy",
                                  ##########################################################################################
                                  fluidRow(
                                    column(
                                      width = 12,
                                      p("No color code",
                                        style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                    ),
                                    column(
                                      width = 11,
                                      fluidRow(
                                        align = "left",
                                        p(
                                          span("No electrification or rural plan; no action taken.",
                                               style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                        )
                                      )
                                    )
                                  ),
                                  #########################################################################################
                                  fluidRow(
                                    column(
                                      width = 1,
                                      tags$i(class = "fa-solid fa-square fa-2x",
                                             style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                    ),
                                    column(
                                      width = 11,
                                      fluidRow(
                                        align = "left",
                                        p(
                                          span("State recognizes need, discussions on plan initiated.",
                                               style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                        )
                                      )
                                    )
                                  ),
                                  ##########################################################################################
                                  fluidRow(
                                    column(
                                      width = 1,
                                      tags$i(class = "fa-solid fa-square fa-2x",
                                             style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                    ),
                                    column(
                                      width = 11,
                                      fluidRow(
                                        align = "left",
                                        p(
                                          span("Draft electrification plan developed, not yet adopted.",
                                               style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                        )
                                      )
                                    )
                                  ),
                                  ##########################################################################################
                                  fluidRow(
                                    column(
                                      width = 1,
                                      tags$i(class = "fa-solid fa-square fa-2x",
                                             style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                    ),
                                    column(
                                      width = 11,
                                      fluidRow(
                                        align = "left",
                                        p(
                                          span("Electrification plan adopted, early implementation underway.",
                                               style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                        )
                                      )
                                    )
                                  ),
                                  ##########################################################################################
                                  fluidRow(
                                    column(
                                      width = 1,
                                      tags$i(class = "fa-solid fa-square fa-2x",
                                             style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                    ),
                                    column(
                                      width = 11,
                                      fluidRow(
                                        align = "left",
                                        p(
                                          span("Plan implementation begun, funding secured, pilot projects launched.",
                                               style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                        )
                                      )
                                    )
                                  ),
                                  ##########################################################################################
                                  
                                  fluidRow(
                                    column(
                                      width = 1,
                                      tags$i(class = "fa-solid fa-square fa-2x",
                                             style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                    ),
                                    column(
                                      width = 11,
                                      fluidRow(
                                        align = "left",
                                        p(
                                          span("Electrification plan operational, aligned with Electricity Act 2023, and includes PPPs.",
                                               style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                        )
                                      )
                                    )
                                  ),
                                  ##########################################################################################
                                )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  
############################################################################
                      hr()
                      # p("Comment",
                      #   style = 'font:Poppins; font-weight: 500; font-style: italic; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #595959;'),
                      # span(textOutput("score_policy_comms_readiness"),
                      #      style = 'font:Poppins; font-weight: 500; font-style: italic; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
                    ),


                    column(
                      width = 1,
                    ),
                  
                    column(
                      width = 5,
                      style = 'padding: 15px; margin: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);',
                      fluidRow(
                        column(
                          width = 6,
                          p("Funding",
                            style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                        ),
                        column(
                          width = 4,
                          offset = 2,
                          align = "center",
                          style = "background-color: #F1F6FD; padding: 5px;",
                          p("Score",
                            style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                          fluidRow(
                            column(
                              width = 7,
                              align = "right",
                              span(textOutput("score_funding_readiness_2"),
                                   style = 'font:Poppins; font-weight: 700; font-size: 28px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            ),
                            column(
                              width = 5,
                              align = "left",
                              p("/5",
                                style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            )
                          )
                        )
                      ),
                      
########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-hand-holding-dollar fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Budget Allocation", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_5")),
                                span(uiOutput("score_readiness_55"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                              popover(
                                tags$i(class = "fa-solid fa-circle-info fa-1x",
                                       style = 'color: #088c1c;'),
                                title = "Budget Allocation",
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 12,
                                    p("No color code",
                                      style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("State allocates <0.1% of budget to energy projects.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                #########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Energy receives <1.1% of the budget.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("State allocates 1.1% to 2.1% of budget to energy.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Energy projects receive 2.1% to 3.1% of the budget.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("State allocates 3.1% to 4.1% of budget to energy infrastructure.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("State allocates >4.1% to energy, with strong PPPs and FDI.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                              )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################


########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-building-circle-check fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("PPP Office", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_6")),
                                span(uiOutput("score_readiness_66"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                            popover(
                              tags$i(class = "fa-solid fa-circle-info fa-1x",
                                     style = 'color: #088c1c;'),
                              title = "PPP Office",
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 12,
                                  p("No color code",
                                    style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("No Initiation: No PPP office or private sector engagement.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              #########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Early Stage: PPP office established, no transactions.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Developing: Fewer than five PPPs, evolving office.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Progressing: Fewer than ten PPPs, growing experience.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Advanced: Fewer than 15 PPPs, structured office.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Optimised: Over 15 PPPs, mature framework, high private investment.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                            )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################




########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-sack-dollar fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("External Funding", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_7")),
                                span(uiOutput("score_readiness_77"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                            popover(
                              tags$i(class = "fa-solid fa-circle-info fa-1x",
                                     style = 'color: #088c1c;'),
                              title = "External Funding",
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 12,
                                  p("No color code",
                                    style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("No Initiation: No FDI in energy, minimal engagement.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              #########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Early Stage: Less than $10 million in FDI, limited engagement.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Developing: $10M-$16M in FDI, investor confidence growing.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Progressing: $16M-$20M in FDI, increasing foreign interest.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Advanced: $20M-$100M in FDI, strong investment potential.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Optimised: Over $100M in FDI, sustained investor confidence.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                            )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################
                      hr(),
                      # p("Comment",
                      #   style = 'font:Poppins; font-weight: 500; font-style: italic; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #595959;'),
                      # span(textOutput("score_funding_comms_readiness"),
                      #      style = 'font:Poppins; font-weight: 500; font-style: italic; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
                    )
                  ),
###### empty spae for entry     
                  fluidRow(
                    column(
                      width = 11,
                      style = 'padding: 15px; margin: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);',
                      fluidRow(
                        column(
                          width = 6,
                          p("Infrastructure",
                            style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                        ),
                        column(
                          width = 4,
                          offset = 2,
                          align = "center",
                          style = "background-color: #F1F6FD; padding: 5px;",
                          p("Score",
                            style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                          fluidRow(
                            column(
                              width = 7,
                              align = "right",
                              span(textOutput("score_infrastructure_readiness_2"),
                                   style = 'font:Poppins; font-weight: 700; font-size: 28px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            ),
                            column(
                              width = 5,
                              align = "left",
                              p("/5",
                                style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            )
                          )
                        )
                      ),
                      fluidRow(
                        style = "margin-top: 20px;",
                        column(
                          width = 6,
                          p("Estimated Demand",
                            style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                          span(textOutput("score_readiness_8"), 
                               style = 'font:Poppins; font-weight: 600; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #075D04;'),

                          fluidRow(
                            column(
                              width = 3,
                              tags$i(class = "fa-solid fa-tower-observation fa-2x",
                                     style = 'color: #16D10F; margin: 10px;')
                            ),
                            column(
                              width = 9,
                              p("Grid Capacity",
                                style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                              span(textOutput("score_readiness_9"), 
                                   style = 'font:Poppins; font-weight: 600; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            )
                          ),
                          fluidRow(
                            column(
                              width = 3,
                              tags$i(class = "fa-solid fa-bolt fa-2x",
                                     style = 'color: #16D10F; margin: 10px;')
                            ),
                            column(
                              width = 9,
                              p("Licensed DisCo",
                                style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                              span(textOutput("score_infrastructure_dsc_readiness"),
                                   style = 'font:Poppins; font-weight: 600; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #075D04;')
                            )
                          ),
                          fluidRow(
                            column(
                              width = 3,
                              tags$i(class = "fa-solid fa-bolt fa-2x",
                                     style = 'color: #16D10F; margin: 10px;')
                            ),
                            column(
                              width = 9,
                              p("Customers Metered",
                                style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-top: 10px;'),
                              span(textOutput("score_readiness_10"), 
                                   style = 'font:Poppins; font-weight: 600; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            )
                          )

                        ),

                        column(
                          width = 6,
                          
########################################################


########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-gavel fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Transmission Capacity as a Percentage of Household Demand", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_11")),
                                span(uiOutput("score_readiness_111"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                            popover(
                              tags$i(class = "fa-solid fa-circle-info fa-1x",
                                     style = 'color: #088c1c;'),
                              title = "Transmission Capacity as a Percentage of Household Demand",
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 12,
                                  p("No color code",
                                    style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("0%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              #########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Less than 20%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Between 20% and 40%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Greater than 40% but not more than 60%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Greater than 60% but not more than 80%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Greater than 80%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                            )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################



#######################################################
                          


########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-file-contract fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Grid Penetration Rate (%)", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_12")),
                                span(uiOutput("score_readiness_122"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                            popover(
                              tags$i(class = "fa-solid fa-circle-info fa-1x",
                                     style = 'color: #088c1c;'),
                              title = "Grid Penetration Rate (%)",
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 12,
                                  p("No color code",
                                    style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("0%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              #########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Less than 20%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Between 20% and 40%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Greater than 40% but not more than 60%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Greater than 60% but not more than 80%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Greater than 80%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                            )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################


########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-toolbox fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Metering Rate", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_13")),
                                span(uiOutput("score_readiness_133"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                            popover(
                              tags$i(class = "fa-solid fa-circle-info fa-1x",
                                     style = 'color: #088c1c;'),
                              title = "Metering Rate",
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 12,
                                  p("No color code",
                                    style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("0%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              #########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Less than 20%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Between 20% and 40%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Greater than 40% but not more than 60%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Greater than 60% but not more than 80%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Greater than 80%",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                            )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################
                        )
                      ),
                      hr(),
                      # p("Comment",
                      #   style = 'font:Poppins; font-weight: 500; font-style: italic; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #595959;'),
                      # span(textOutput("score_infrastructure_comms_readiness"),
                      #      style = 'font:Poppins; font-weight: 500; font-style: italic; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
                    )
                  ),
                  fluidRow(
                    column(
                      width = 5,
                      style = 'padding: 15px; margin: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);',
                      fluidRow(
                        column(
                          width = 6,
                          p("Capacity",
                            style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                        ),
                        column(
                          width = 4,
                          offset = 2,
                          align = "center",
                          style = "background-color: #F1F6FD; padding: 5px;",
                          p("Score",
                            style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                          fluidRow(
                            column(
                              width = 7,
                              align = "right",
                              span(textOutput("score_capacity_readiness_2"),
                                   style = 'font:Poppins; font-weight: 700; font-size: 28px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            ),
                            column(
                              width = 5,
                              align = "left",
                              p("/5",
                                style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            )
                          )
                        )
                      ),
                      
                      
                      
########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-user-shield fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Stakeholder Engagement", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_14")),
                                span(uiOutput("score_readiness_144"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                              popover(
                                tags$i(class = "fa-solid fa-circle-info fa-1x",
                                       style = 'color: #088c1c;'),
                                title = "Stakeholder Engagement",
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 12,
                                    p("No color code",
                                      style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("No Initiation: No community partnerships or awareness efforts.	",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                #########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Early Stage: Informal engagement, no structured campaigns.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Developing: Some partnerships, occasional public messaging.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Progressing: Formal partnerships, weak federal collaboration.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Advanced: Active partnerships, ongoing awareness campaigns.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Optimised: Institutionalized framework, strong local and federal partnerships.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                              )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################




                      
########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-user-shield fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Pre-Development Capacity", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_15")),
                                span(uiOutput("score_readiness_155"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                              popover(
                                tags$i(class = "fa-solid fa-circle-info fa-1x",
                                       style = 'color: #088c1c;'),
                                title = "Pre-Development Capacity",
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 12,
                                    p("No color code",
                                      style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("No Initiation: No agency, resources, or technical partnerships.	",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                #########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Early Stage: No functional agency, minimal resources, no training.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Developing: Agencies developing, weak regulatory presence, limited collaborations.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Progressing: Operational agency, limited resources, occasional training.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Advanced: Functional agency, adequate resources, regular training, active collaboration.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Optimised: Fully resourced, strategic approach, strong institutions, active partnerships.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                              )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################

                      
########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-user-shield fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Development Capacity", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_16")),
                                span(uiOutput("score_readiness_166"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                              popover(
                                tags$i(class = "fa-solid fa-circle-info fa-1x",
                                       style = 'color: #088c1c;'),
                                title = "Development Capacity",
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 12,
                                    p("No color code",
                                      style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("No Initiation: No planning, procurement, or monitoring structures..",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                #########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Early Stage: No agency, no team, basic monitoring.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Developing: Planning bodies under development, informal monitoring.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Progressing: Under-resourced agencies, partial processes, limited monitoring.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Advanced: Staffed structures, active but incomplete monitoring.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Optimised: Integrated capacity, active tracking and enforcement.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                              )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################

                                            
########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-user-shield fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Operational Capacity", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_17")),
                                span(uiOutput("score_readiness_177"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                              popover(
                                tags$i(class = "fa-solid fa-circle-info fa-1x",
                                       style = 'color: #088c1c;'),
                                title = "Operational Capacity",
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 12,
                                    p("No color code",
                                      style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("No Initiation: No sustainability evaluation or reporting systems.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                #########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Early Stage: No sustainability systems, rare performance reports.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Developing: Sustainability systems developing, irregular reports.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Progressing: Limited systems, quarterly reports lacking consistency.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Advanced: Functional systems, actionable reports generated regularly.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                                
                                fluidRow(
                                  column(
                                    width = 1,
                                    tags$i(class = "fa-solid fa-square fa-2x",
                                           style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                  ),
                                  column(
                                    width = 11,
                                    fluidRow(
                                      align = "left",
                                      p(
                                        span("Optimised: Comprehensive systems, frequent reports used for decision-making.",
                                             style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                      )
                                    )
                                  )
                                ),
                                ##########################################################################################
                              )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################

                      
                      hr()
                    ),
#######################################################

                    column(
                      width = 1,
                    ),
#######################################################

                      column(
                      width = 5,
                    
                      style = 'padding: 15px; margin: 10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);',
                      
                      fluidRow(
                        column(
                          width = 6,
                          p("Data",
                            style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                        ),
                        column(
                          width = 4,
                          offset = 2,
                          align = "center",
                          style = "background-color: #F1F6FD; padding: 5px;",
                          p("Score",
                            style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                          fluidRow(
                            column(
                              width = 7,
                              align = "right",
                              span(textOutput("score_data_readiness_2"),
                                   style = 'font:Poppins; font-weight: 700; font-size: 28px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            ),
                            column(
                              width = 5,
                              align = "left",
                              p("/5",
                                style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #075D04;')
                            )
                          )
                        )
                      ), 
                      
                                   
########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-database fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Dedicated Agency", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_19")),
                                span(uiOutput("score_readiness_199"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                            popover(
                              tags$i(class = "fa-solid fa-circle-info fa-1x",
                                     style = 'color: #088c1c;'),
                              title = "Dedicated Agency",
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 12,
                                  p("No color code",
                                    style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("No Initiation: No Agency or Department",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              #########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Early Stage: Department or unit within  an agency",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Developing: Plans to establish dedicated  Agency",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Progressing: Dedicated agency but lacks resources.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Advanced: Fully established agency with defined roles but minimal operations",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Optimised: Fully functional  and consistent agency",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                            )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################

########################################################

                        fluidRow(
                          style = "margin-top: 25px;",
                          column(
                            width = 2,
                            tags$i(class = "fa-solid fa-database fa-2x",
                                   style = 'color: #16D10F; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                          ),
                          column(
                            width = 1,

                          ),
                          column(
                            width = 7,
                            fluidRow(
                              align = "left",
                              tagList(
                                span("Data Accessibility", 
                                     style = 'font-family: Poppins; font-weight: 600; font-size: 11px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'), 
                                tags$div(style = "margin-top: -5px;",  # adjust to control space
                                         #progressBar(id = "elecLaw", value = 5, total = 5, display_pct = FALSE, status = "warning")
                                         uiOutput("score_readiness_20")),
                                span(uiOutput("score_readiness_200"),
                                     style = 'font-family: Poppins; font-weight: 500; font-size: 12px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px; margin-top:5px;')
                              )
                            )
                            
                          ),
                          column(
                            width = 1,
                            popover(
                              tags$i(class = "fa-solid fa-circle-info fa-1x",
                                     style = 'color: #088c1c;'),
                              title = "Data Accessibility",
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 12,
                                  p("No color code",
                                    style = 'font-family: Poppins; font-weight: 500; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 15px;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("No Initiation: No centralized repository, data is scattered and inaccessible",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              #########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Early Stage: Data exists but  no structured process to access",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Developing: Plans to improve data management exist, but no implementation yet.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Progressing: Some data is accessible, but the process is slow and inconsistent.	",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Advanced: Data is mostly accessible, with structured but limited digital access. Plans for full integration.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                              
                              fluidRow(
                                column(
                                  width = 1,
                                  tags$i(class = "fa-solid fa-square fa-2x",
                                         style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                ),
                                column(
                                  width = 11,
                                  fluidRow(
                                    align = "left",
                                    p(
                                      span("Optimised: Fully digitized, centralized system with seamless, real-time data access.",
                                           style = 'font-family: Poppins; font-weight: 600; font-size: 13px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                    )
                                  )
                                )
                              ),
                              ##########################################################################################
                            )
                          ),
                          column(
                            width = 1,
                            
                          )
                          
                        ),  


#######################################################
                  hr()
                      
                    ),
#######################################################

                  ),



                ),
      #############################
      column( 
        width = 6,
        align = "center",
        style = "background-color: #FFFFFF; padding: 0px;",

        fluidRow(

          p('Sub-National Readiness Map, Average Score By State',
            # style = 'font:Poppins; font-weight: 500; font-size: 20px; line-height: 120%; letter-spacing: 2%; color: #595959;')
            style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px; align: left;'),
        ),
        
        fluidRow(
          
          selectInput("ranking_metric", "Select Metric to Rank By:",
                      choices = c("All", "Policy", "Funding", "Infrastructure", "Capacity", "Data"),
                      selected = "All"),   
          
          tabsetPanel(type = "tabs", id = "view_tab",
                      tabPanel("Map view",
                               uiOutput("map_readiness"),
                               hr(),
                               
                               fluidRow(
                                 align = "left",
                                 
                                 fluidRow(
                                   column(
                                     width = 1,
                                     tags$i(class = "fa-solid fa-square fa-2x",
                                            style = 'color: #C1241D; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                   ),
                                   column(
                                     width = 11,
                                     fluidRow(
                                       align = "left",
                                       p(
                                         span("Early Stage",
                                              style = 'font-family: Poppins; font-weight: 800; font-size: 13px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'),
                                         span("Awareness and preliminary actions in policy, infrastructure, funding, capacity, or data aligning with EA 23",
                                              style = 'font-family: Poppins; font-weight: 500; font-size: 11px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                       )
                                     )
                                   )
                                 ),
                                 fluidRow(
                                   column(
                                     width = 1,
                                     tags$i(class = "fa-solid fa-square fa-2x",
                                            style = 'color: #f7cfcd; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                   ),
                                   column(
                                     width = 11,
                                     fluidRow(
                                       align = "left",
                                       p(
                                         span("Developing",
                                              style = 'font-family: Poppins; font-weight: 800; font-size: 13px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'),
                                         span("Foundational work underway in key readiness areas.",
                                              style = 'font-family: Poppins; font-weight: 500; font-size: 11px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                       )
                                     )
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(
                                     width = 1,
                                     tags$i(class = "fa-solid fa-square fa-2x",
                                            style = 'color: #daa400; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                   ),
                                   column(
                                     width = 11,
                                     fluidRow(
                                       align = "left",
                                       p(
                                         span("Progressing",
                                              style = 'font-family: Poppins; font-weight: 800; font-size: 13px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'),
                                         span("Draft frameworks, electrifcation projects  and investment plans in place.",
                                              style = 'font-family: Poppins; font-weight: 500; font-size: 11px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                       )
                                     )
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(
                                     width = 1,
                                     tags$i(class = "fa-solid fa-square fa-2x",
                                            style = 'color: #35b36d; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                   ),
                                   column(
                                     width = 11,
                                     fluidRow(
                                       align = "left",
                                       p(
                                         span("Advanced",
                                              style = 'font-family: Poppins; font-weight: 800; font-size: 13px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'),
                                         span("Significant progress in policy, infrastructure, funding, capacity, and data.",
                                              style = 'font-family: Poppins; font-weight: 500; font-size: 11px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                       )
                                     )
                                   )
                                 ),
                                 
                                 fluidRow(
                                   column(
                                     width = 1,
                                     tags$i(class = "fa-solid fa-square fa-2x",
                                            style = 'color: #247749; margin: 3px; width: 50px; height: 40px; display: inline-block;')
                                   ),
                                   column(
                                     width = 11,
                                     fluidRow(
                                       align = "left",
                                       p(
                                         span("Optimised",
                                              style = 'font-family: Poppins; font-weight: 800; font-size: 13px; letter-spacing: 2%; color: #075D04; display: block; line-height: 18px;'),
                                         span("Active execution with tangible outcomes across multiple readiness areas.",
                                              style = 'font-family: Poppins; font-weight: 500; font-size: 11px; letter-spacing: 2%; color: #595959; display: block; line-height: 18px;')
                                       )
                                     )
                                   )
                                 ),
                               )
                      ),
                      tabPanel("Readiness Leaderboard",
                               # h5("Top states"),
  
                               uiOutput("leaderboard_ui"),
                               uiOutput("pagination_ui")
                      )
          )
        ),

        hr(),
        h2(
          span(textOutput("map_info_readiness")),
          align = "left",
          # style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #16D10F; margin-bottom: 20px;'
          style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px; align: left;',
        ),
        fluidRow(
          align = "left",
          style = "margin: 10px; border-top: 1px solid rgba(101, 153, 205, 0.3); padding: 10px;",
          column(
            width = 4,
            p("Data Source",
              style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #595959;')
          ),
          column(
            width = 8,
            span(textOutput("data_source"),
                 style = 'font:Poppins; font-weight: 700; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #075D04;')
          )
        ),
        fluidRow(
          align = "left",
          style = "margin: 10px; border-top: 1px solid rgba(101, 153, 205, 0.3); padding: 10px;",
          column(
            width = 4,
            p("Size",
              style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #595959;')
          ),
          column(
            width = 8,
            span(textOutput("map_area_readiness"),
                 style = 'font:Poppins; font-weight: 700; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #075D04;')
          )
        ),
        fluidRow(
          align = "left",
          style = "margin: 10px; border-top: 1px solid rgba(101, 153, 205, 0.3); padding: 10px;",
          column(
            width = 4,
            p("Population",
              style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #595959;')
          ),
          column(
            width = 8,
            span(textOutput("map_population_readiness"),
                 style = 'font:Poppins; font-weight: 700; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #075D04;')
          )
        ),
        fluidRow(
          align = "left",
          style = "margin: 10px; border-top: 1px solid rgba(101, 153, 205, 0.3); padding: 10px;",
          column(
            width = 4,
            p("Sub-National Readiness",
              style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #595959;')
          ),
          column(
            width = 8,
            span(uiOutput("score_readiness_national"),
                 style = 'font:Poppins; font-weight: 700; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #075D04;')
          )
        ),
        fluidRow(
          align = "left",
          style = "margin: 10px; border-top: 1px solid rgba(101, 153, 205, 0.3); padding: 10px;",
          column(
            width = 4,
            p("Commited to Electrification Pact",
              style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #595959;')
          ),
          column(
            width = 8,
            span("❌",
                 style = 'font:Poppins; font-weight: 700; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #075D04;')
          )
        ),
        fluidRow(
          align = "left",
          style = "margin: 10px; border-top: 1px solid rgba(101, 153, 205, 0.3); padding: 10px;",
          column(
            width = 4,
            p("Number of Stakeholders",
              style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #595959;')
          ),
          column(
            width = 8,
            span(textOutput("map_register_readiness"),
                 style = 'font:Poppins; font-weight: 700; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #075D04;')
          )
        ),
        fluidRow(
          align = "left",
          style = "margin: 10px; border-top: 1px solid rgba(101, 153, 205, 0.3); padding: 10px;",
          column(
            width = 4,
            p("Energy Access Tier",
              style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #595959;')
          ),
          column(
            width = 8,
            span(textOutput("map_tier_readiness"),
                 style = 'font:Poppins; font-weight: 700; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #075D04;')
          )
        ),
        fluidRow(
          align = "left",
          style = "margin: 10px; border-top: 1px solid rgba(101, 153, 205, 0.3); padding: 10px;",
          column(
            width = 4,
            p("Region(s)",
              style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #595959;')
          ),
          column(
            width = 8,
            span(textOutput("map_region_readiness"),
              style = 'font:Poppins; font-weight: 400; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #075D04;')
          )
        ),
        fluidRow(
          align = "left",
          style = "margin: 10px; border-top: 1px solid rgba(101, 153, 205, 0.3); padding: 10px;",
          column(
            width = 4,
            p("States",
              style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #595959;')
          ),
          column(
            width = 8,
            span(textOutput("map_state_readiness"),
              style = 'font:Poppins; font-weight: 700; font-size: 15px; line-height: 120%; letter-spacing: 2%; color: #075D04;')
          )
        ),


      ),
                        hr(),
                        fluidRow(
                          column(
                            width = 5
                          ),
                          column(
                            width = 2,
                            align = "center",
                            tags$a(href = "https://rea.gov.ng/", target = "_blank",
                                   tags$img(src = "images/rea.png")
                            )
                          ),
                          column(
                            width = 5
                          )
                          
                        )
              )
            ),
            nav_panel(
              "Energy Access Assessment",
              value = 'access',
              fluidRow(
                
                column(
                  width = 3,
                  uiOutput("regions_access", width = "100%")
                ),
                column(
                  width = 3,
                  uiOutput("states_access", width = "100%")
                ),
                column(
                  width = 2,
                  div(
                    style = "visibility: hidden; height: 0; overflow: hidden;",
                    uiOutput("lgas_access")
                  )
                ),
                column(
                  width = 3,
                      actionButton("rea_iept_link", "REA's IEPT Tool", width = "100%",
                                   style="color: #075D04; background-color: #66C2A4; border-color: #075D04; margin: 20px;")
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 6,
                  h2(
                    "Energy Access Distribution", 
                    align = "left",
                    style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;'
                  ),
                  h4(
                    align = "left",
                    p("This section presents an analysis of energy access based on household survey results, benchmarked against the World Bank’s Multi-Tier Framework (MTF). It highlights the distribution of energy access across different regions, capturing variations in service quality, reliability, and affordability beyond a simple grid connection.",
                      style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;')
                  ),
                  navset_tab( 
                    nav_panel(
                      "Considering only Capacity & Grid Availability",
                      plotOutput("access_dist_access")
                    ),
                    nav_panel(
                      "Considering All 8 Metrics",
                      plotOutput("access_dist_access_2")
                    )
                  ),
                  p("Percentage of households represented in each Tier of energy access across the country",
                    style = 'font:Poppins; font-weight: 600; font-size: 16px; line-height: 100%; letter-spacing: 2% color: #595959;'),
                  hr(),
                  h4("All non-percentage metrics are derived using the median",
                     style = 'font:Poppins; font-weight: 250; font-size: 15px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                  
                  hr(),
                  
                  fluidRow(
                    h3("Access to Primary Electricity Supply (Grid/Mini-Grid)",
                      # style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 100%; letter-spacing: 2%; color: #383a3d;'),
                    style = 'font:Poppins; font-weight: 700; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;'),
                  ),
                  fluidRow(##############################################
                           column(
                             width = 5,
                             tags$i(class = "fa-solid fa-power-off fa-2x",
                                    style = 'color: #383a3d; margin: 10px;'),
                             span(textOutput("grid_connect_rt_eap"),
                                  style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #383a3d;'),
                             p("Reported access to Grid Electricity Supply",
                               style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #383a3d; margin-top: 20px;'),
                             style = 'background-color: #5fbc5c; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                           ),
                           column(
                             width = 5,
                             tags$i(class = "fa-solid fa-bolt fa-2x",
                                    style = 'color: #383a3d; margin: 10px;'),
                             span(textOutput("access_ecp_home_eap"),
                                  style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #383a3d;'),
                             p("Daily Energy Consumption (Median)",
                               style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #383a3d; margin-top: 20px;'),
                             style = 'background-color: #5fbc5c; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                           )
                  ),
                  fluidRow(
                    column(
                      width = 5,
                      tags$i(class = "fa-solid fa-money-bill-wave fa-2x",
                             style = 'color: #383a3d; margin: 10px;'),
                      span(textOutput("access_egs_home_eap"),
                           style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #383a3d;'),
                      p("Estimated Daily Spend on Grid Supply (Median)",
                        style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #383a3d; margin-top: 20px;'),
                      style = 'background-color: #5fbc5c; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                    ),
                    column(
                      width = 5,
                      tags$i(class = "fa-solid fa-clock fa-2x",
                             style = 'color: #383a3d; margin: 10px;'),
                      span(textOutput("access_wdr_home_eap"),
                           style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #383a3d;'),
                      p("Total Daily Grid Supply Hours",
                        style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #383a3d; margin-top: 20px;'),
                      style = 'background-color: #5fbc5c; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                    )
                  ),
                  
                  fluidRow(
                    column(
                      width = 12,
),
                      h3("Household Income distribution Vs Percentage spend on Electricity",
                         #style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 100%; letter-spacing: 2%; color: #383a3d; margin-top : 25px;'),
                         style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;'),

                      plotOutput("perc_inc_ener_spe", width = "100%"),

                    )
                    
                  )

                  
                ), 
                
                column(
                  width = 6,
                  align = "center",
                  
                  h2(
                    "Energy Access Multi Tier Framework", 
                    align = "left",
                    style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;'
                  ),
                  
                  h4(
                    align = "left",
                    p("The Multi-Tier Framework (MTF), developed by the World Bank, provides a comprehensive approach to measuring and tracking energy access. Unlike traditional binary metrics, the MTF assesses access based on multiple factors, including capacity, duration, reliability, affordability, and safety. This method enables a more detailed understanding of electrification progress and service quality.",
                      style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 10px;')
                  ),
                  hr(),
                  img(src="images/MTF__.png",
                      style = 'width: 100%, height:350px, margin-bottom:10px;'),
                  hr(),
                  fluidRow(
                   align = "left",
                   
                   fluidRow(
                     h3("Access to Alternate Electricity Supply (Generator, Solar Home System)",
                        style = 'font:Poppins; font-weight: 700; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;'),
                        # style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 100%; letter-spacing: 2%; color: #595959;'),
                   ),   
                  fluidRow(
                    column(
                      width = 5,
                      tags$i(class = "fa-solid fa-car-battery fa-2x",
                             style = 'color: #383a3d; margin: 10px;'),
                      span(textOutput("access_aeo_home_eap"),
                           style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #383a3d;'),
                      p("Reported access to Alternate Electricity Supply",
                        style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #383a3d; margin-top: 20px;'),
                      style = 'background-color: #92e290; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                    ),
                    column(
                      width = 5,
                      tags$i(class = "fa-solid fa-money-bill-wave fa-2x",
                             style = 'color: #383a3d; margin: 10px;'),
                      span(textOutput("access_gen_spend_home_eap"),
                           style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #383a3d;'),
                      p("Estimated Daily Spend on Alternate Supply (Median)",
                        style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #383a3d; margin-top: 20px;'),
                      style = 'background-color: #92e290; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                    )
                  ),
                  
                  fluidRow(
                    column(
                      width = 5,
                      tags$i(class = "fa-solid fa-cogs fa-2x",
                             style = 'color: #383a3d; margin: 10px;'),
                      span(textOutput("access_aet_gen_home_eap"),
                           style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #383a3d;'),
                      p("Reported generator Ownership",
                        style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #383a3d; margin-top: 20px;'),
                      style = 'background-color: #92e290; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                    ),
                    column(
                      width = 5,
                      tags$i(class = "fa-solid fa-solar-panel fa-2x",
                             style = 'color: #383a3d; margin: 10px;'),
                      span(textOutput("access_aet_solar_home_eap"),
                           style = 'font:Poppins; font-weight: 700; font-size: 40px; line-height: 100%; letter-spacing: 2%; color: #383a3d;'),
                      p("Reported solar system Ownership",
                        style = 'font:Poppins; font-weight: 500; font-size: 16px; line-height: 120%; letter-spacing: 2%; color: #383a3d; margin-top: 20px;'),
                      style = 'background-color: #92e290; padding: 20px; margin:20px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                    )
                  ),
                  
                  ),
                  
                  fluidRow(
                    h3("Household Demographic",
                      # style = 'font:Poppins; font-weight: 500; font-size: 15px; line-height: 100%; letter-spacing: 2%; color: #383a3d; margin-top : 25px;'),
                    style = 'font:Poppins; font-weight: 700; font-size: 24px; line-height: 100%; letter-spacing: 2%; color: #595959; margin-bottom: 20px;'),
                    
                    
                    column(
                      width = 6,
                     
                      fluidRow(
                        tags$i(class = "fa-solid fa-house fa-1x",
                               style = 'color: #ffffff; margin-top: 10px; margin-bottom: 10px; width:900px;'),
                        span(textOutput("access_hhs_access"),
                             style = 'font:Poppins; font-weight: 700; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #FFFFFF;'),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  p("Household Size",
                          style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF; margin-top:10px;'),
                        style = 'background-color: #075d04; padding: 5px; margin:10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                      ),
                      fluidRow(
                        tags$i(class = "fa-solid fa-user-tie fa-1x",
                               style = 'color: #ffffff; margin-top: 10px; margin-bottom: 10px;'),
                        span(textOutput("access_occup_access"),
                             style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #FFFFFF;'),
                        p("Main Occupation",
                          style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF; margin-top:10px;'),
                        style = 'background-color: #075d04; padding: 5px; margin:10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                      ),
                      fluidRow(
                        tags$i(class = "fa-solid fa-utensils fa-1x",
                               style = 'color: #ffffff; margin-top: 10px; margin-bottom: 10px;'),
                        span(textOutput("access_cfl_access"),
                             style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #FFFFFF;'),
                        p("Most Common Primary Cooking Fuel",
                          style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF; margin-top:10px;'),
                        style = 'background-color: #075d04; padding: 5px; margin:10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                      )
                    ),
                    column(
                      width = 6,
                      fluidRow(
                        tags$i(class = "fa-solid fa-house fa-1x",
                               style = 'color: #ffffff; margin-top: 10px; margin-bottom: 10px;'),
                        span(textOutput("access_how_access"),
                             style = 'font:Poppins; font-weight: 700; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #FFFFFF;'),
                        p("Home Ownership",
                          style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF; margin-top: 10px;'),
                        style = 'background-color: #075D04; padding: 5px; margin:10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                      ),
                      fluidRow(
                        tags$i(class = "fa-solid fa-store fa-1x",
                               style = 'color: #ffffff; margin-top: 10px; margin-bottom: 10px;'),
                        span(textOutput("access_hbr_access"),
                             style = 'font:Poppins; font-weight: 700; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #FFFFFF;'),
                        p("Run Home Business",
                          style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF; margin-top:10px;'),
                        style = 'background-color: #075D04; padding: 5px; margin:10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                      ),
                      fluidRow(
                        tags$i(class = "fa-solid fa-car fa-1x",
                               style = 'color: #ffffff; margin-top: 10px; margin-bottom: 10px;'),
                        span(textOutput("access_mot_access"),
                             style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #FFFFFF;'),
                        p("Most Common Mode of Transport",
                          style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF; margin-top:10px;'),
                        style = 'background-color: #075D04; padding: 5px; margin:10px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);'
                      )
                    )
                    ),
                  hr(),
                  )
              ),
              fluidRow(
                div(class = "custom-dashboard-section", 
                    tags$head(tags$style(HTML("
        .custom-dashboard-section {
          background-color: #f6fbf4; 
          padding: 20px; 
          border-radius: 10px;
        }
        .custom-box {
          border: 1px solid #ddeedf;
          padding: 15px;
          background-color: #f6fbf4;
          border-radius: 10px;
          margin-bottom: 10px;
        }
        .big-text { font-size: 16px; font-weight: bold; color: #064b17; }
        .value-text { font-size: 22px; font-weight: bold; color: #064b17; }
        .money-text { font-size: 22px; font-weight: bold; color: #064b17; }
        .money-text-2 { font-size: 14px; font-weight: bold; color: #064b17; }
        .highlight-text { font-size: 20px; font-weight: bold; color: #064b17; }
        .emoji { font-size: 22px; }
        .star { color: #fbc02d; font-size: 22px; } 
        .star-empty { color: #ddd; font-size: 22px; }
      "))),
                    
                    fluidRow(
                      column(6, div(class = "custom-box",
                                    fluidRow(
                                      column(6, div(" % Income Spend on Electricity (Median)", class = "big-text")),
                                      column(6, div("No. of Weekly Grid Disruption", class = "big-text"))
                                    ),
                                    fluidRow(
                                      column(6, div(textOutput("affordability_eap"), class = "value-text")),
                                      column(6, div(textOutput("access_wdrn_access"), class = "value-text"))
                                    ),
                                    br(),
                                    fluidRow(
                                      column(6, div("Grid Electricity Supply Quality", class = "big-text")),
                                      column(6, div("Grid Electricity Customer Satisfaction", class = "big-text"))
                                    ),
                                    fluidRow(
                                      column(6, div(uiOutput("access_geq_access"), class = "money-text")),
                                      column(6, div(uiOutput("access_ges_access"), class = "money-text"))
                                    ),
                                    fluidRow(
                                      column(6, div("", class = "money-text")),
                                      column(6, div("", class = "money-text"))
                                    )
                      ),
                      ),
                      
                      column(6, div(class = "custom-box",
                                    fluidRow(
                                      column(6, div(" % of Alternative Supply (Gen/Solar)", class = "big-text")),
                                      column(6, div("Average Daily Alternative Supply Hours", class = "big-text"))
                                    ),
                                    fluidRow(
                                      column(6, div(textOutput("access_aeo_access"), class = "value-text")),
                                      column(6, div(textOutput("access_agd_access"), class = "highlight-text"))
                                    ),
                                    br(),
                                    fluidRow(
                                      column(6, div("Reason for no Grid Connection", class = "big-text")),
                                      column(6, div("Alternative Generator Supply Quality", class = "big-text"))
                                    ),
                                    fluidRow(
                                      column(6, div(textOutput("access_res_access"), class = "money-text-2")),
                                      column(6, div(uiOutput("access_agc_access"), class = "money-text"))
                                    )
                      ))
                    ),
                    hr(),
                    fluidRow(
                      column(
                        width = 5
                      ),
                      column(
                        width = 2,
                        align = "center",
                        tags$a(href = "https://rea.gov.ng/", target = "_blank",
                               tags$img(src = "images/rea.png")
                        )
                      ),
                      column(
                        width = 5
                      )
                      
                    )
                )
                
              )
            ),
            nav_panel(
              "About Project",
              value = 'about',
              fluidRow(
                p("Summary",
                  style = 'font:Poppins; font-weight: 700; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #075D04;'),
                p("Nigeria has made progress in expanding electricity access but faces significant challenges in achieving universal access. These challenges stem from issues like a lack of standardized definitions, inconsistent data collection, and variations in sub-national measurement criteria. The decentralization of decision-making and involvement of multiple stakeholders further complicate coordination efforts. In response, the Federal Ministry of Power and the Rural Electrification Agency, with support from the World Bank, are developing a National Electrification Strategy and Implementation Plan (NESIP). This plan will be implemented in three phases: Phase 1 focuses on establishing the necessary inputs and geospatial model, Phase 2 will develop the NESIP roadmap, and Phase 3 will focus on implementation support and consensus building. Phase 1 is being led by a McKinsey consortium and aims to define the NESIP boundaries and create a least-cost development plan, with a key milestone being the signing of an electrification pact between stakeholders.",
                  style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D;')
              ),
              fluidRow(
                p("NESIP Phases",
                  style = 'font:Poppins; font-weight: 700; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #075D04;'),
                fluidRow(
                  column(
                    width = 3,
                    style = 'margin:10px; border-right: 1px solid #16D10F;',
                    p("Phase 1"),
                    img(src="images/ph_1.png", width="30%"),
                    p("Phase 1 will be focused on establishing all of the inputs into the NESIP itself, including the geospatial model and the overall structure for the Sector Wide Approach for electrification;",
                      style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;')
                  ),
                  column(
                    width = 3,
                    style = 'margin:10px; border-right: 1px solid #16D10F;',
                    p("Phase 2"),
                    img(src="images/ph_2.png", width="30%"),
                    p("Phase 2 will focus on the actual NESIP Roadmap development for universal energy access, including
detailed articulation of the strategy itself, as well as detailing out the engagement approach between different institutions;",
                      style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;')
                  ),
                  column(
                    width = 3,
                    style = 'margin:10px;',
                    p("Phase 3"),
                    img(src="images/ph_3.png", width="30%"),
                    p("Phase 3 will focus on implementation support, including building broad consensus for the NESIP, and testing the interaction model between the Federal and Sub-National institutions.",
                      style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;')
                  )
                ),
                fluidRow(
                  p("Phase 1 Objectives",
                    style = 'font:Poppins; font-weight: 700; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #075D04;'),
                  p("This consultancy focuses on Phase 1 of the NESIP, which involves defining its initial boundaries and creating key components, such as a geospatial model and a least-cost development plan. It will also develop the overall framework for a Sector Wide Approach to electrification. The main goal by the end of Phase 1 is to have an electrification pact signed by key stakeholders, including the REA, Ministry of Power, and State Governments.",
                    style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                  fluidRow(
                    column(
                      width = 2,
                      align = "center",
                      img(src="images/ph_1_obj_1.png", width="30%"),
                    ),
                    column(
                      width = 10,
                      p("Development of inputs into the National Electrification Strategy and Implementation Plan:",
                        style = 'font:Poppins; font-weight: 600; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                      tags$ul(
                        tags$li("Development of a clear and measurable definition of energy access specifically for the NESIP, considering factors like access to grid, reliability, affordability, and quality of service, based on a review of definitions of energy access used globally by institutions like the World Bank and International Energy Agency (IEA).",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                        tags$li("Recommendation of appropriate metrics for monitoring and tracking progress towards achieving universal energy access under the NESIP.",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                        tags$li("Identification and categorization of all relevant stakeholders involved in the electrification process at the national, sub-national, and community levels, along with an approach on how these stakeholders will be engaged in the NESIP development process.",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;')
                      )
                    ),
                    style = 'background-color: #F1FDF2; padding: 5px; margin:5px;'
                  ),
                  fluidRow(
                    column(
                      width = 2,
                      align = "center",
                      img(src="images/ph_1_obj_2.png", width="30%"),
                    ),
                    column(
                      width = 10,
                      p("Development of Geospatial Tool for electrification planning:",
                        style = 'font:Poppins; font-weight: 600; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                      tags$ul(
                        tags$li("An assessment of the current status of geospatial tools for electrification planning, to determine what data layers and functionalities can be used for REA’s NESIP geospatial information system (GIS) platform",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                        tags$li("The development of a data collection and management strategy, based on an understanding of the quality, availability and accessibility of existing data.",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                        tags$li("The development of a comprehensive geospatial database of Nigeria's electrification network, including existing grid infrastructure, mini-grids, and off-grid solutions, and ensure the database is operable and functional at the individual State level, to inform more local decision making.",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                        tags$li("For specific sectors (e.g., agriculture and health), inclusion of geospatial layers to enable provision of least cost generation pathways for the sector specific facilities.",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;')
                      )
                    ),
                    style = 'background-color: #F1FDF2; padding: 5px; margin:5px;'
                  ),
                  fluidRow(
                    column(
                      width = 2,
                      align = "center",
                      img(src="images/ph_1_obj_3.png", width="30%"),
                    ),
                    column(
                      width = 10,
                      p("Development of the Interaction Model for NESIP:",
                        style = 'font:Poppins; font-weight: 600; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                      tags$ul(
                        tags$li("Engagement with relevant stakeholders, including national and sub-national government agencies, private sector entities, civil society, and local communities, to ensure a participatory and inclusive strategy development process.",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                        tags$li("An assessment of the current capacity and readiness of sub-national governments (state and local governments) to contribute effectively to the NESIP, including an evaluation of existing policies, regulations, and institutional frameworks at the sub-national level that may impact electrification efforts.",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                        tags$li("Definition of the overall architecture for the interaction model whereby the Federal Government (through both the Ministry of Power and the Rural Electrification Agency) engage with the States on an on-going basis to support them in their delivery of universal electrification.",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;'),
                        tags$li("Development and signature of an Electrification Pact that aligns the key Federal and State-level stakeholders in the sector on a pathway forward.",
                                style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 120%; letter-spacing: 2%; color: #383A3D; margin: 10px;')
                      )
                    ),
                    style = 'background-color: #F1FDF2; padding: 5px; margin:5px;'
                  )
                )
              ),
              # fluidRow(
              #   p("Working Team & Roles",
              #     style = 'font:Poppins; font-weight: 700; font-size: 20px; line-height: 100%; letter-spacing: 2%; color: #075D04;'),
              #   fluidRow(
              #     column(
              #       width = 3,
              #       p("Organisation",
              #         style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #ffffff; margin:5px;')
              #     ),
              #     column(
              #       width = 3,
              #       p("Role",
              #         style = 'font:Poppins; font-weight: 700; font-size: 16px; line-height: 100%; letter-spacing: 2%; color: #ffffff; margin:5px;')
              #     ),
              #     style = 'background-color: #383A3D; padding: 5px; margin:5px;'
              #   ),
              #   fluidRow( #### ministry of power
              #     column(
              #       width = 3,
              #       img(
              #         src="images/mop.png", width="30%",
              #         style = 'margin:10px;'
              #       )
              #     ),
              #     column(
              #       width = 9,
              #       p("",
              #         style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 100%; letter-spacing: 2%; color: #383A3D; margin:5px;')
              #     ),
              #     style = 'margin:10px; border-bottom: 1px solid #16D10F;',
              #   ),
              #   fluidRow(#### rea
              #     column(
              #       width = 3,
              #       align = "center",
              #       tags$a(href = "https://rea.gov.ng/", target = "_blank",
              #              tags$img(src = "images/rea.png")
              #       )
              #     ),
              #     column(
              #       width = 9,
              #       p("",
              #         style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 100%; letter-spacing: 2%; color: #383A3D; margin:5px;')
              #     ),
              #     style = 'margin:10px; border-bottom: 1px solid #16D10F;',
              #   ),
              #   fluidRow(
              #     column(
              #       width = 3,
              #       img(
              #         src="images/mckinsey.png", width="30%",
              #         style = 'margin:10px;'
              #       )
              #     ),
              #     column(
              #       width = 9,
              #       p("",
              #         style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 100%; letter-spacing: 2%; color: #383A3D; margin:5px;')
              #     ),
              #     style = 'margin:10px; border-bottom: 1px solid #16D10F;',
              #   ),
              #   fluidRow(
              #     column(
              #       width = 3,
              #       img(
              #         src="images/vista.svg", width="50%",
              #         style = 'margin:10px;'
              #       )
              #     ),
              #     column(
              #       width = 9,
              #       p("",
              #         style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 100%; letter-spacing: 2%; color: #383A3D; margin:5px;')
              #     ),
              #     style = 'margin:10px; border-bottom: 1px solid #16D10F;',
              #   ),
              #   fluidRow(
              #     column(
              #       width = 3,
              #       img(
              #         src="images/fraym.png", width="30%",
              #         style = 'margin:10px;'
              #       )
              #     ),
              #     column(
              #       width = 9,
              #       p("",
              #         style = 'font:Poppins; font-weight: 400; font-size: 14px; line-height: 100%; letter-spacing: 2%; color: #383A3D; margin:5px;')
              #     ),
              #     style = 'margin:10px; border-bottom: 1px solid #16D10F;',
              #   )
              # )
              
              hr(),
              fluidRow(
                column(
                  width = 5
                ),
                column(
                  width = 2,
                  align = "center",
                  tags$a(href = "https://rea.gov.ng/", target = "_blank",
                         tags$img(src = "images/rea.png")
                  )
                ),
                column(
                  width = 5
                )
                
              )
            ),
            title = "NESIP", 
            id = "page", 
          )

