#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
# https://shiny.posit.co/
#
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

#####

national_ea <- read_csv("database/tier_charts_ea/energy_access_calc.csv")

regional_ea <- read_csv("database/tier_charts_ea/region_tier_percent.csv")
state_ea <- read_csv("database/tier_charts_ea/state_tier_percent.csv")
lga_ea <- read_csv("database/tier_charts_ea/LGA_tier_percent.csv")

regional_ea_2 <- read_csv("database/tier_charts_ea/region_tier_percent_av_cap.csv")
state_ea_2 <- read_csv("database/tier_charts_ea/state_tier_percent_av_cap.csv")
lga_ea_2 <- read_csv("database/tier_charts_ea/LGA_tier_percent_av_cap.csv")

#####

region_state <- read_csv("database/region_state.csv")

region_state_lga <- read_csv("database/region_state_lga.csv")

regions <- unique(region_state_lga$region)

states <- unique(region_state_lga$state)

lgas <- unique(region_state_lga$lga)

register <- read_csv("database/register.csv")

register <- register %>% mutate(color = ifelse(status == "Desk Research", "#C0C0C0", 
                                               ifelse(status == "Engagement Initiated", "#69CCE5", 
                                                      ifelse(status == "Engagement Ongoing", "#E36A61",
                                                             ifelse(status == "Engagement Complete", "#6BC16E",
                                                                    "#DD2620")))),
                                phone = ifelse(is.na(phone), phone, "***********"),
                                email = ifelse(is.na(email), email, "***********"))
readiness <- read_csv("database/readiness.csv")

access <- read_csv("database/access.csv")


access_cs_score <- access %>%
  mutate(ges = case_when(
    ges == "1 - Very Dissatisfied" ~ 1,
    ges == "2 - Dissatisfied" ~ 2,
    ges == "3 - Neutral" ~ 3,
    ges == "4 - Satisfied" ~ 4,
    ges == "5 - Very Satisfied" ~ 5,
    ges == "NA" ~ NA_real_,  # Convert "NA" string to actual NA
    TRUE ~ as.numeric(ges)   # Convert remaining values to numeric
  ))


access_tier <- read_csv("database/access_tier.csv")



access_tier_2 <- read_csv(("database/access_tier____.csv"))

access_tier_df_1 <- data.frame(tier = c("Tier 0", "Tier 1", "Tier 2", "Tier 3", "Tier 4", "Tier 5"),
                               value = c(39, 41, 4, 15, 1, 0),
                               per = c("39%", "41%", "4%", "15%", "1%", "0%"))

access_tier_df_2 <- data.frame(tier = c("Tier 0", "Tier 1", "Tier 2", "Tier 3", "Tier 4", "Tier 5"),
                               value = c(26, 33, 8, 20, 13, 0),
                               per = c("26%", "33%", "8%", "20%", "13%", "0%"))

state_readiness <- read_csv(("database/state_readiness.csv"))
infrastructure_df <- read_csv(("database/infrastructure_df.csv"))



states_data <- data.frame(
  State = paste("State", 1:37),
  Score = sample(1:5, 37, replace = TRUE),
  Status = rep("Developing", 37),
  Logo = rep("lagos.png", 37),
  Policy = sample(1:5, 37, replace = TRUE),
  Funding = sample(1:5, 37, replace = TRUE),
  Infrastructure = sample(1:5, 37, replace = TRUE),
  Capacity = sample(1:5, 37, replace = TRUE),
  Data = sample(1:5, 37, replace = TRUE),
  stringsAsFactors = FALSE
)



# Define server logic 
function(input, output, session) {
  
  
  output$lgas_access <- renderUI({
    selectInput(
      inputId = "lga_access",
      label = NULL,
      choices = unique(df$LGA),  # your actual LGA column
      selected = unique(df$LGA)[1],  # or whatever default you want
      width = "100%"
    )
  })
  
  
  output$stakeholder_count_home <- renderText(
    {
      # text <- sum((register %>% group_by(level) %>% summarise(count = n()))$count)
      text <- nrow(register)
      text
    }
  )
  
  output$stakeholder_count_home_2 <- renderText(
    {
      text <- sum((register %>% group_by(level) %>% summarise(count = n()))$count)
      text
    }
  )
  
  output$stakeholder_federal_home <- renderText(
    {
      text <- (subset(register, level == "Federal") %>% group_by(level) %>% summarise(count = n()))$count
      text
    }
  )
  
  output$stakeholder_state_home <- renderText(
    {
      text <- (subset(register, level == "State") %>% group_by(level) %>% summarise(count = n()))$count
      text
    }
  )
  
  output$stakeholder_lga_home <- renderText(
    {
      text <- (subset(register, level == "LGA") %>% group_by(level) %>% summarise(count = n()))$count
      text
    }
  )
  
  output$stakeholder_other_home <- renderText(
    {
      text <- (subset(register, level == "Other") %>% group_by(level) %>% summarise(count = n()))$count
      text
    }
  )
  
  output$engage_count <- renderText(
    {
      text <- (subset(register, role == "Engage") %>% group_by(role) %>% summarise(count = n()))$count
      text
    }
  )
  
  
  output$engage_count_2 <- renderText(
    {
      
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        
        text <- (subset(register, role == "Engage") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        
        text <- (subset(subset(register, region == input$regions_stakeholder), role == "Engage") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        
        text <- (subset(subset(register, state == input$states_stakeholder), role == "Engage") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else {
        
        text <- (subset(subset(register, state == input$states_stakeholder), role == "Engage") %>% group_by(role) %>% summarise(count = n()))$count
      }
      
      text
    }
  )
  
  

  
  output$int_prt_count <- renderText(
    {
      text <- (subset(register, role == "Inform- Interested Parties") %>% group_by(role) %>% summarise(count = n()))$count
      text
    }
  )
  
  
  output$int_prt_count_2 <- renderText(
    {
      
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        
        text <- (subset(register, role == "Inform- Interested Parties") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        
        text <- (subset(subset(register, region == input$regions_stakeholder), role == "Inform- Interested Parties") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        
        text <- (subset(subset(register, state == input$states_stakeholder), role == "Inform- Interested Parties") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else {
        
        text <- (subset(subset(register, state == input$states_stakeholder), role == "Inform- Interested Parties") %>% group_by(role) %>% summarise(count = n()))$count
      }
      
      text
    }
  )
  
  
  
  output$fp_count <- renderText(
    {
      text <- (subset(register, role == "Communicate- Future Partners") %>% group_by(role) %>% summarise(count = n()))$count
      text
    }
  )
  
  
  output$fp_count_2 <- renderText(
    {
      
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        
        text <- (subset(register, role == "Communicate- Future Partners") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        
        text <- (subset(subset(register, region == input$regions_stakeholder), role == "Communicate- Future Partners") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        
        text <- (subset(subset(register, state == input$states_stakeholder), role == "Communicate- Future Partners") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else {
        
        text <- (subset(subset(register, state == input$states_stakeholder), role == "Communicate- Future Partners") %>% group_by(role) %>% summarise(count = n()))$count
      }
      
      text
    }
  )
  
  
  
  
  output$ki_count <- renderText(
    {
      text <- (subset(register, role == "Accomodate - Key Informant") %>% group_by(role) %>% summarise(count = n()))$count
      text
    }
  )
  
  
  
  output$ki_count_2 <- renderText(
    {
      
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        
        text <- (subset(register, role == "Accomodate - Key Informant") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        
        text <- (subset(subset(register, region == input$regions_stakeholder), role == "Accomodate - Key Informant") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        
        text <- (subset(subset(register, state == input$states_stakeholder), role == "Accomodate - Key Informant") %>% group_by(role) %>% summarise(count = n()))$count
        
      } else {
        
        text <- (subset(subset(register, state == input$states_stakeholder), role == "Accomodate - Key Informant") %>% group_by(role) %>% summarise(count = n()))$count
      }
      
      text
    }
  )
  
  
  
  observeEvent(input$stakeholder_link_home, {
    updateTabsetPanel(session, "page",
                      selected = "stakeholder")
  })
  
  observeEvent(input$about_link_home, {
    updateTabsetPanel(session, "page",
                      selected = "about")
  })
  
  
  output$readiness_score_home <- renderPlot({ 
    # Add some margin around the plot area
    par(mar = c(2, 2, 2, 2))
    
    data <- data.frame(
      policy         = c(5, 0, round(mean(state_readiness$Policy_Score, na.rm = TRUE), 0)), 
      funding        = c(5, 0, round(mean(state_readiness$Funding_Score, na.rm = TRUE), 0)), 
      infrastructure = c(5, 0, round(mean(state_readiness$Infrastructure_Score, na.rm = TRUE), 0)), 
      capacity       = c(5, 0, round(mean(state_readiness$Capacity_Score, na.rm = TRUE), 0)), 
      data           = c(5, 0, round(mean(state_readiness$Data_Score, na.rm = TRUE), 0))
    )
    
    # Scaling for the radar chart
    data <- rbind(rep(4, 5), rep(0, 5), data)

    
    radarchart(data,
               axistype = 1,
               vlabels = rep("", 5),  
               vlcex = 1.5,                     # Label font size
               centerzero = TRUE,              # Keep center aligned
               seg = 4,                        # Number of axis segments
               pcol = scales::alpha("#075D04", 1),
               pfcol = scales::alpha("#075D04", 0.4),
               plwd = 2,
               cglcol = "green",
               cglty = 1,
               cglwd = 0.8,
               axislabcol = "darkgreen",
               caxislabels = seq(0, 4, 1),
               calcex = 0.9
    )
    
    # Add custom labels farther away using polar coordinates
    n_vars <- 5
    angles <- seq(0, 2 * pi, length.out = n_vars + 1)[- (n_vars + 1)]
    
    labels <- c('policy', 'data', 'capacity', 'infrastucture', 'funding') # colnames(data)
    
    # Adjust radius for label placement (increase from 1.2 to push them out)
    radius <- 1.4
    
    for (i in 1:n_vars) {
      x <- radius * sin(angles[i])
      y <- radius * cos(angles[i])
      text(x, y, labels[i], cex = 0.9)
    }

  })
  
  

  ###############


  output$readiness_score_home_2 <- renderPlot({
# 
#     # Load fmsb
#     library(fmsb)
# 
#     
    # Policy_score =as.integer(format(round(mean(state_readiness$Policy_Score),0)))
    # Data_score = as.integer(format(round(mean(state_readiness$Data_Score),0)))
    # Capacity_score = as.integer(format(round(mean(state_readiness$Capacity_Score),0)))
    # Infrastructure_score = as.integer(format(round(mean(state_readiness$Infrastructure_Score),0)))
    # Funding_score = as.integer(format(round(mean(state_readiness$Funding_Score),0)))
#     
#     
#     # Create a data frame
#     data <- data.frame(
#       Policy = c(5, 0, Policy_score),
#       Data = c(5, 0, Data_score),
#       Capacity = c(5, 0, Capacity_score),
#       Infrastructure = c(5, 0, Infrastructure_score),
#       Funding = c(5, 0, Funding_score)
#     )
# 
#     # data$Policy <- format(round(data$Policy, 0))
#     # data$Data <- format(round(data$Data, 0))
#     # data$Capacity <- format(round(data$Capacity, 0))
#     # data$Infrastructure <- format(round(data$Infrastructure, 0))
#     # data$Funding <- format(round(data$Funding, 0))
#     
#     # Radar chart
#     radarchart(data,
#                axistype = 0,
#                pcol = rgb(0, 100, 0, max = 255),        # Border color
#                pfcol = rgb(0, 100, 0, 150, max = 255),  # Fill color
#                plwd = 2,                                # Border thickness
#                cglcol = "grey",                         # Grid line color
#                cglty = 1,                               # Grid line type
#                axislabcol = "black",                    # Axis labels color
#                cglwd = 0.9,                             # Grid line width
#                vlcex = 1.2                              # Axis label size
#     )

    # Your score calculations
    Policy_score = as.integer(format(round(mean(state_readiness$Policy_Score), 0)))
    Data_score = as.integer(format(round(mean(state_readiness$Data_Score), 0)))
    Capacity_score = as.integer(format(round(mean(state_readiness$Capacity_Score), 0)))
    Infrastructure_score = as.integer(format(round(mean(state_readiness$Infrastructure_Score), 0)))
    Funding_score = as.integer(format(round(mean(state_readiness$Funding_Score), 0)))
    
    # Create the dataset
    data <- data.frame(
      Category = c("Policy", "Funding", "Infrastructure", "Capacity", "Data"),
      Score = c(Policy_score, Funding_score, Infrastructure_score, Capacity_score, Data_score)
    )
    
    # Reorder for sorting
    data$Category <- factor(data$Category, levels = data$Category[order(-data$Score)])
  
    
    # Color mapping based on score
    score_colors <- c(
      "1" = "#EDF8FB",
      "2" = "#B2E2E2",
      "3" = "#66C2A4",
      "4" = "#6BC16E",
      "5" = "#31A935"
    )
    
    # Calculate average score
    avg_score <- mean(data$Score)
    
# 
#     # Plot
#     ggplot(data, aes(x = Category, y = Score)) +
#       geom_segment(aes(xend = Category, y = 0, yend = Score), color = "gray60", size = 2) +
#       geom_point(aes(color = as.factor(Score)), size = 10) +  # color-coded by score
#       scale_color_manual(values = score_colors) +
#       geom_hline(yintercept = avg_score, linetype = "dashed", color = "green", size = 1) +
#       scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
#       theme_minimal(base_size = 14) +
#       theme(
#         panel.grid = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 12),
#         axis.text = element_text(size = 12),
#         legend.position = "none"  # Hide legend if not needed
#       ) +
#       labs(title = "", y = "Score")
    

    # Plot
    ggplot(data, aes(x = Category, y = Score)) +
      geom_segment(aes(xend = Category, y = 0, yend = Score), color = "black", size = 2) +
      geom_point(aes(color = as.factor(Score)), size = 16) +  # enlarged circle
      geom_text(aes(label = Score), color = "black", size = 5, fontface = "bold") +  # label inside circle
      scale_color_manual(values = score_colors) +
      #geom_hline(yintercept = avg_score, linetype = "dashed", color = "green", size = 1) +
      scale_y_continuous(limits = c(0, 5), breaks = 0:5, expand = c(0, 0)) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),       # remove y-axis title
        #axis.text.y = element_blank(),        # remove y-axis labels
        axis.ticks.y = element_blank(),       # remove y-axis ticks
        axis.text.x = element_text(size = 12),
        legend.position = "none"
      ) +
      labs(title = "", y = "Readiness Score")
    
  })



      # 
      # output$readiness_score_home_2A <- renderPlot({
      # 
      # 
      #   # Load required library
      #   library(fmsb)
      # 
      #   # Prepare data
      #   
      #   new_df <- state_readiness[, c("Policy_Score", "Funding_Score", "Infrastructure_Score", "Capacity_Score", "Data_Score")]
      #   
      #   readiness_score_df<- data.frame(t(round(colMeans(new_df, na.rm = TRUE), 0)))
      #     
      #   names(readiness_score_df)[names(readiness_score_df) == "Policy_Score"] <- "Policy"
      #   names(readiness_score_df)[names(readiness_score_df) == "Funding_Score"] <- "Funding"
      #   names(readiness_score_df)[names(readiness_score_df) == "Infrastructure_Score"] <- "Infrastructure"
      #   names(readiness_score_df)[names(readiness_score_df) == "Capacity_Score"] <- "Capacity"
      #   names(readiness_score_df)[names(readiness_score_df) == "Data_Score"] <- "Data"
      #   
      #   
      #   max_min <- data.frame(
      #     Policy = c(5, 0), Funding = c(5, 0), Infrastructure = c(5, 0),
      #     Capacity = c(5, 0), Data = c(5, 0)
      #   )
      #   
      #   
      #   # Bind the variable ranges to the data
      #   df <- rbind(max_min, readiness_score_df)
      # 
      #   
      #   create_beautiful_radarchart <- function(data, color = "#238b45", 
      #                                           vlabels = colnames(data), vlcex = 0.9,
      #                                           caxislabels = NULL, title = NULL, ...){
      #     radarchart(
      #       data, axistype = 1,
      #       # Customize the polygon
      #       pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
      #       # Customize the grid
      #       cglcol = "grey", cglty = 1, cglwd = 0.8,
      #       # Customize the axis
      #       axislabcol = "#238b45", 
      #       # Variable labels
      #       vlcex = vlcex, vlabels = vlabels,
      #       caxislabels = caxislabels, title = title, ...
      #     )
      #   }
      #   
      #   
      #   # Reduce plot margin using par()
      #   op <- par(mar = c(1, 2, 2, 1))
      #   create_beautiful_radarchart(df, caxislabels = c(0, 1.25, 2.5, 3.75, 5))
      #   
      #   
      #   
      #   par(op)
      # 
      # 
      # 
      # })
      # 
      # 
      
      output$readiness_score_home_2A <- renderPlot({
        
        library(fmsb)
        
        # Prepare data
        new_df <- state_readiness[, c("Policy_Score", "Funding_Score", "Infrastructure_Score", "Capacity_Score", "Data_Score")]
        readiness_score_df <- data.frame(t(round(colMeans(new_df, na.rm = TRUE), 0)))
        
        names(readiness_score_df) <- c("Policy", "Funding", "Infrastructure", "Capacity", "Data")
        
        max_min <- data.frame(
          Policy = c(5, 0), Funding = c(5, 0), Infrastructure = c(5, 0),
          Capacity = c(5, 0), Data = c(5, 0)
        )
        
        # Radar chart data
        df <- rbind(max_min, readiness_score_df)
        
        # Create radar chart function
        create_beautiful_radarchart <- function(data, color = "#238b45", 
                                                vlabels = colnames(data), vlcex = 0.9,
                                                caxislabels = NULL, title = NULL, ...) {
          radarchart(
            data, axistype = 1,
            pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
            cglcol = "grey", cglty = 1, cglwd = 0.8,
            axislabcol = "#238b45", 
            vlcex = vlcex, vlabels = vlabels,
            caxislabels = caxislabels, title = title, ...
          )
        }
        
        # Reduce margin
        op <- par(mar = c(1, 2, 2, 1))
        
        # Draw radar chart
        create_beautiful_radarchart(df, caxislabels = c(0, 1.25, 2.5, 3.75, 5))
        
        # ---- Add labels to radar points ----
        # scores <- as.numeric(df[3, ])
        # n_vars <- length(scores)
        # angles <- seq(0, 2 * pi, length.out = n_vars + 1)[- (n_vars + 1)]
        # 
        # # Rescale the scores (0 to 1) since radarchart plots in [0, 1] scale internally
        # scaled_scores <- scores / 5
        # 
        # # Calculate coordinates for each label
        # label_radius <- 1.1  # Just outside the radar polygon
        # x <- label_radius * scaled_scores * sin(angles)
        # y <- label_radius * scaled_scores * cos(angles)
        # 
        # # Plot text at the correct positions
        # text(x, y, labels = scores, cex = 1, font = 3, col = "#238b45")
        
        par(op)  # Reset margin
        
      })
      

#################

  # output$readiness_table <- renderTable({
  #   data <- data.frame(
  #     Category       = c("Policy", "Funding", "Infrastructure", "Capacity", "Data"),
  #     Score  = c(
  #       round(mean(state_readiness$Policy_Score, na.rm = TRUE), 0),
  #       round(mean(state_readiness$Funding_Score, na.rm = TRUE), 0),
  #       round(mean(state_readiness$Infrastructure_Score, na.rm = TRUE), 0),
  #       round(mean(state_readiness$Capacity_Score, na.rm = TRUE), 0),
  #       round(mean(state_readiness$Data_Score, na.rm = TRUE), 0)
  #     )
  #   )
  #   
  #   return(data)
  # })
  # 
  # 
  
  output$readiness_table <- renderTable({
    data <- data.frame(
      Category = c("Policy", "Funding", "Infrastructure", "Capacity", "Data"),
      Score = c(
        mean(state_readiness$Policy_Score, na.rm = TRUE),
        mean(state_readiness$Funding_Score, na.rm = TRUE),
        mean(state_readiness$Infrastructure_Score, na.rm = TRUE),
        mean(state_readiness$Capacity_Score, na.rm = TRUE),
        mean(state_readiness$Data_Score, na.rm = TRUE)
      )
    )
    
    # Format the Score column to have 0 decimal places
    data$Score <- format(round(data$Score, 0))
    
    return(data)
  })
  
  
  output$readiness_score_readiness <- renderPlot({
    
    # Load fmsb
    library(fmsb)
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      
            Policy_score = as.integer(format(round(mean(state_readiness$Policy_Score), 0)))
            Data_score = as.integer(format(round(mean(state_readiness$Data_Score), 0)))
            Capacity_score = as.integer(format(round(mean(state_readiness$Capacity_Score), 0)))
            Infrastructure_score = as.integer(format(round(mean(state_readiness$Infrastructure_Score), 0)))
            Funding_score = as.integer(format(round(mean(state_readiness$Funding_Score), 0)))
      
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      
      
          Policy_score = as.integer(format(round(mean(subset(state_readiness, region==input$regions_readiness)$Policy_Score), 0)))
          Data_score = as.integer(format(round(mean(subset(state_readiness, region==input$regions_readiness)$Data_Score), 0)))
          Capacity_score = as.integer(format(round(mean(subset(state_readiness, region==input$regions_readiness)$Capacity_Score), 0)))
          Infrastructure_score = as.integer(format(round(mean(subset(state_readiness, region==input$regions_readiness)$Infrastructure_Score), 0)))
          Funding_score = as.integer(format(round(mean(subset(state_readiness, region==input$regions_readiness)$Funding_Score), 0)))
      
      
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      
      
          Policy_score = as.integer(format(round(mean(subset(state_readiness, state==input$states_readiness)$Policy_Score), 0)))
          Data_score = as.integer(format(round(mean(subset(state_readiness, state==input$states_readiness)$Data_Score), 0)))
          Capacity_score = as.integer(format(round(mean(subset(state_readiness, state==input$states_readiness)$Capacity_Score), 0)))
          Infrastructure_score = as.integer(format(round(mean(subset(state_readiness, state==input$states_readiness)$Infrastructure_Score), 0)))
          Funding_score = as.integer(format(round(mean(subset(state_readiness, state==input$states_readiness)$Funding_Score), 0)))
      
    } else {
      
      
          Policy_score = as.integer(format(round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Policy_Score), 0)))
          Data_score = as.integer(format(round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Data_Score), 0)))
          Capacity_score = as.integer(format(round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Capacity_Score), 0)))
          Infrastructure_score = as.integer(format(round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Infrastructure_Score), 0)))
          Funding_score = as.integer(format(round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Funding_Score), 0)))
          
    }
    
    # 
    # # Create a data frame
    # data <- data.frame(
    #   data
    # )
    # 
    # # Radar chart
    # radarchart(data,
    #            axistype = 0,
    #            pcol = rgb(0, 100, 0, max = 255),        # Border color
    #            pfcol = rgb(0, 100, 0, 150, max = 255),  # Fill color
    #            plwd = 2,                                # Border thickness
    #            cglcol = "grey",                         # Grid line color
    #            cglty = 1,                               # Grid line type
    #            axislabcol = "black",                    # Axis labels color
    #            cglwd = 0.8,                             # Grid line width
    #            vlcex = 1.2                              # Axis label size
    # )
    # 
    # 
    
    
    
    # Create the dataset
    data <- data.frame(
      Category = c("Policy", "Funding", "Infrastructure", "Capacity", "Data"),
      Score = c(Policy_score, Funding_score, Infrastructure_score, Capacity_score, Data_score)
    )
    
    # Reorder for sorting
    data$Category <- factor(data$Category, levels = data$Category[order(data$Score)])
    
    # Color mapping based on score
    score_colors <- c(
      "1" = "#EDF8FB",
      "2" = "#B2E2E2",
      "3" = "#66C2A4",
      "4" = "#6BC16E",
      "5" = "#31A935"
    )
    
    # Calculate average score
    avg_score <- mean(data$Score)
    
    
    # Plot
    ggplot(data, aes(x = Category, y = Score)) +
      geom_segment(aes(xend = Category, y = 0, yend = Score), color = "gray60", size = 2) +
      geom_point(aes(color = as.factor(Score)), size = 16) +  # enlarged circle
      geom_text(aes(label = Score), color = "black", size = 5, fontface = "bold") +  # label inside circle
      scale_color_manual(values = score_colors) +
      geom_hline(yintercept = avg_score, linetype = "dashed", color = "green", size = 1) +
      scale_y_continuous(limits = c(0, 5), breaks = 0:5, expand = c(0, 0)) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),       # remove y-axis title
        #axis.text.y = element_blank(),        # remove y-axis labels
        axis.ticks.y = element_blank(),       # remove y-axis ticks
        axis.text.x = element_text(size = 12),
        legend.position = "none"
      ) +
      labs(title = "", y = "Readiness Score")
    
    
  })
  
  
  
  
  
  
  output$readiness_score_readiness_2A <- renderPlot({
    
    # Load fmsb
    library(fmsb)
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      
      Policy_score = as.integer(format(round(mean(state_readiness$Policy_Score), 0)))
      Data_score = as.integer(format(round(mean(state_readiness$Data_Score), 0)))
      Capacity_score = as.integer(format(round(mean(state_readiness$Capacity_Score), 0)))
      Infrastructure_score = as.integer(format(round(mean(state_readiness$Infrastructure_Score), 0)))
      Funding_score = as.integer(format(round(mean(state_readiness$Funding_Score), 0)))
      
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      
      
      Policy_score = as.integer(format(round(mean(subset(state_readiness, region==input$regions_readiness)$Policy_Score), 0)))
      Data_score = as.integer(format(round(mean(subset(state_readiness, region==input$regions_readiness)$Data_Score), 0)))
      Capacity_score = as.integer(format(round(mean(subset(state_readiness, region==input$regions_readiness)$Capacity_Score), 0)))
      Infrastructure_score = as.integer(format(round(mean(subset(state_readiness, region==input$regions_readiness)$Infrastructure_Score), 0)))
      Funding_score = as.integer(format(round(mean(subset(state_readiness, region==input$regions_readiness)$Funding_Score), 0)))
      
      
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      
      
      Policy_score = as.integer(format(round(mean(subset(state_readiness, state==input$states_readiness)$Policy_Score), 0)))
      Data_score = as.integer(format(round(mean(subset(state_readiness, state==input$states_readiness)$Data_Score), 0)))
      Capacity_score = as.integer(format(round(mean(subset(state_readiness, state==input$states_readiness)$Capacity_Score), 0)))
      Infrastructure_score = as.integer(format(round(mean(subset(state_readiness, state==input$states_readiness)$Infrastructure_Score), 0)))
      Funding_score = as.integer(format(round(mean(subset(state_readiness, state==input$states_readiness)$Funding_Score), 0)))
      
    } else {
      
      
      Policy_score = as.integer(format(round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Policy_Score), 0)))
      Data_score = as.integer(format(round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Data_Score), 0)))
      Capacity_score = as.integer(format(round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Capacity_Score), 0)))
      Infrastructure_score = as.integer(format(round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Infrastructure_Score), 0)))
      Funding_score = as.integer(format(round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Funding_Score), 0)))
      
    }

    
    # Create data frame with custom column names
    summary_scores <- data.frame(
      Policy_Score = Policy_score,
      Funding_Score = Funding_score,
      Infrastructure_Score = Infrastructure_score,
      Capacity_Score = Capacity_score,
      Data_Score = Data_score
    )
    
    # Prepare data
    readiness_score_df <- summary_scores[, c("Policy_Score", "Funding_Score", "Infrastructure_Score", "Capacity_Score", "Data_Score")]
    
    
    #readiness_score_df <- data.frame(t(round(colMeans(new_df, na.rm = TRUE), 0)))
    
    names(readiness_score_df) <- c("Policy", "Funding", "Infrastructure", "Capacity", "Data")
    
    max_min <- data.frame(
      Policy = c(5, 0), Funding = c(5, 0), Infrastructure = c(5, 0),
      Capacity = c(5, 0), Data = c(5, 0)
    )
    
    # Radar chart data
    df <- rbind(max_min, readiness_score_df)
    
    # Create radar chart function
    create_beautiful_radarchart <- function(data, color = "#238b45", 
                                            vlabels = colnames(data), vlcex = 0.9,
                                            caxislabels = NULL, title = NULL, ...) {
      radarchart(
        data, axistype = 1,
        pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
        cglcol = "grey", cglty = 1, cglwd = 0.8,
        axislabcol = "#238b45", 
        vlcex = vlcex, vlabels = vlabels,
        caxislabels = caxislabels, title = title, ...
      )
    }
    
    # Reduce margin
    op <- par(mar = c(1, 2, 2, 1))
    
    # Draw radar chart
    create_beautiful_radarchart(df, caxislabels = c(0, 1.25, 2.5, 3.75, 5))
    
    # # ---- Add labels to radar points ----
    # scores <- as.numeric(df[3, ])
    # n_vars <- length(scores)
    # angles <- seq(0, 2 * pi, length.out = n_vars + 1)[- (n_vars + 1)]
    # 
    # # Rescale the scores (0 to 1) since radarchart plots in [0, 1] scale internally
    # scaled_scores <- scores / 5
    # 
    # # Calculate coordinates for each label
    # label_radius <- 1.1  # Just outside the radar polygon
    # x <- label_radius * scaled_scores * sin(angles)
    # y <- label_radius * scaled_scores * cos(angles)
    # 
    # # Plot text at the correct positions
    # text(x, y, labels = scores, cex = 1, font = 3, col = "#238b45")
    # 
    par(op)  # Reset margin
    
    
  })
  
  
  
  # 
  # output$readiness_score_readiness <- renderPlot( 
  #   { 
  #     if((input$regions_readiness == "All") & (input$states_readiness == "All")){
  #       
  #       data <- data.frame(      policy = c(5, 0, round(mean(state_readiness$Policy_Score, na.rm = TRUE), 0)), 
  #                                funding = c(5, 0, round(mean(state_readiness$Funding_Score, na.rm = TRUE), 0)), 
  #                                infrastructure = c(5, 0, round(mean(state_readiness$Infrastructure_Score, na.rm = TRUE), 0)), 
  #                                capacity = c(5, 0, round(mean(state_readiness$Capacity_Score, na.rm = TRUE), 0)), 
  #                                data = c(5, 0, round(mean(state_readiness$Data_Score, na.rm = TRUE), 0)))
  #       
  #     } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
  #       
  #       data <- data.frame(      policy = c(5, 0, round(mean(subset(state_readiness, region==input$regions_readiness)$Policy_Score, na.rm = TRUE), 0)), 
  #                                funding = c(5, 0, round(mean(subset(state_readiness, region==input$regions_readiness)$Funding_Score, na.rm = TRUE), 0)), 
  #                                infrastructure = c(5, 0, round(mean(subset(state_readiness, region==input$regions_readiness)$Infrastructure_Score, na.rm = TRUE), 0)), 
  #                                capacity = c(5, 0, round(mean(subset(state_readiness, region==input$regions_readiness)$Capacity_Score, na.rm = TRUE), 0)), 
  #                                data = c(5, 0, round(mean(subset(state_readiness, region==input$regions_readiness)$Data_Score, na.rm = TRUE), 0)))
  #       
  #     } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
  # 
  #       data <- data.frame(      policy = c(5, 0, round(mean(subset(state_readiness, state==input$states_readiness)$Policy_Score, na.rm = TRUE), 0)), 
  #                                funding = c(5, 0, round(mean(subset(state_readiness, state==input$states_readiness)$Funding_Score, na.rm = TRUE), 0)), 
  #                                infrastructure = c(5, 0, round(mean(subset(state_readiness, state==input$states_readiness)$Infrastructure_Score, na.rm = TRUE), 0)), 
  #                                capacity = c(5, 0, round(mean(subset(state_readiness, state==input$states_readiness)$Capacity_Score, na.rm = TRUE), 0)), 
  #                                data = c(5, 0, round(mean(subset(state_readiness, state==input$states_readiness)$Data_Score, na.rm = TRUE), 0)))
  #     } else {
  #       
  #       data <- data.frame(      policy = c(5, 0, round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Policy_Score, na.rm = TRUE), 0)), 
  #                                funding = c(5, 0, round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Funding_Score, na.rm = TRUE), 0)), 
  #                                infrastructure = c(5, 0, round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Infrastructure_Score, na.rm = TRUE), 0)), 
  #                                capacity = c(5, 0, round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Capacity_Score, na.rm = TRUE), 0)), 
  #                                data = c(5, 0, round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Data_Score, na.rm = TRUE), 0)))
  #       
  #     }
  #     
  #     # Scaling for the radar chart
  #     data <- rbind(rep(4, 5), rep(0, 5), data)
  #     
  #     
  #     radarchart(data,
  #                axistype = 1,
  #                vlabels = rep("", 5),  
  #                vlcex = 1.5,                     # Label font size
  #                centerzero = TRUE,              # Keep center aligned
  #                seg = 4,                        # Number of axis segments
  #                pcol = scales::alpha("#075D04", 1),
  #                pfcol = scales::alpha("#075D04", 0.4),
  #                plwd = 2,
  #                cglcol = "green",
  #                cglty = 1,
  #                cglwd = 0.8,
  #                axislabcol = "darkgreen",
  #                caxislabels = seq(0, 4, 1),
  #                calcex = 0.9
  #     )
  #     
  #     # Add custom labels farther away using polar coordinates
  #     n_vars <- 5
  #     angles <- seq(0, 2 * pi, length.out = n_vars + 1)[- (n_vars + 1)]
  #     
  #     labels <- c('policy', 'data', 'capacity', 'infrastucture', 'funding') # colnames(data)
  #     
  #     # Adjust radius for label placement (increase from 1.2 to push them out)
  #     radius <- 1.4
  #     
  #     for (i in 1:n_vars) {
  #       x <- radius * sin(angles[i])
  #       y <- radius * cos(angles[i])
  #       text(x, y, labels[i], cex = 0.9)
  #     }
  #     
  #   })
  # 

  
  



  output$readiness_table_2 <- renderTable(
    {
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){

        data <- data.frame(
          Category       = c("Policy", "Funding", "Infrastructure", "Capacity", "Data"),
          Score  = c(
            round(mean(state_readiness$Policy_Score, na.rm = TRUE), 0),
            round(mean(state_readiness$Funding_Score, na.rm = TRUE), 0),
            round(mean(state_readiness$Infrastructure_Score, na.rm = TRUE), 0),
            round(mean(state_readiness$Capacity_Score, na.rm = TRUE), 0),
            round(mean(state_readiness$Data_Score, na.rm = TRUE), 0)
          )
        )

      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){

        data <- data.frame(
          Category       = c("Policy", "Funding", "Infrastructure", "Capacity", "Data"),
          Score  = c(
            round(mean(subset(state_readiness, region==input$regions_readiness)$Policy_Score, na.rm = TRUE), 0),
            round(mean(subset(state_readiness, region==input$regions_readiness)$Funding_Score, na.rm = TRUE), 0),
            round(mean(subset(state_readiness, region==input$regions_readiness)$Infrastructure_Score, na.rm = TRUE), 0),
            round(mean(subset(state_readiness, region==input$regions_readiness)$Capacity_Score, na.rm = TRUE), 0),
            round(mean(subset(state_readiness, region==input$regions_readiness)$Data_Score, na.rm = TRUE), 0)
          )
        )

      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){

        data <- data.frame(
          Category       = c("Policy", "Funding", "Infrastructure", "Capacity", "Data"),
          Score  = c(
            round(mean(subset(subset(state_readiness, state==input$states_readiness))$Policy_Score, na.rm = TRUE), 0),
            round(mean(subset(subset(state_readiness, state==input$states_readiness))$Funding_Score, na.rm = TRUE), 0),
            round(mean(subset(subset(state_readiness, state==input$states_readiness))$Infrastructure_Score, na.rm = TRUE), 0),
            round(mean(subset(subset(state_readiness, state==input$states_readiness))$Capacity_Score, na.rm = TRUE), 0),
            round(mean(subset(subset(state_readiness, state==input$states_readiness))$Data_Score, na.rm = TRUE), 0)
          )
        )


      } else {


        data <- data.frame(
          Category       = c("Policy", "Funding", "Infrastructure", "Capacity", "Data"),
          Score  = c(
            round(mean(subset(subset(state_readiness, state==input$states_readiness))$Policy_Score, na.rm = TRUE), 0),
            round(mean(subset(subset(state_readiness, state==input$states_readiness))$Funding_Score, na.rm = TRUE), 0),
            round(mean(subset(subset(state_readiness, state==input$states_readiness))$Infrastructure_Score, na.rm = TRUE), 0),
            round(mean(subset(subset(state_readiness, state==input$states_readiness))$Capacity_Score, na.rm = TRUE), 0),
            round(mean(subset(subset(state_readiness, state==input$states_readiness))$Data_Score, na.rm = TRUE), 0)
          )
        )
        


      }

      return(format(data))


    })

  
  
  
  observeEvent(input$readiness_link_home, {
    updateTabsetPanel(session, "page",
                      selected = "readiness")
  })
  
  output$access_esp_home <- renderText(
    {
      text <- paste(subset(access %>% group_by(esp) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), esp=="TRUE")$per, "%", sep="")
      #text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      text
    }
  )
  
  
  output$access_esp_home_nat_grd <- renderText(
    {
      # Count occurrences
      national_grid_count <- sum(access$egc_ntngrd == "National grid", na.rm = TRUE)
      total_count <- nrow(access)  # Includes NA values
      
      # Calculate percentage
      text <- paste0(round((national_grid_count / total_count) * 100, 1), "%")
      
      
      text
    }
  )
  
  output$grid_connect_rt <- renderText(
    {
      #text <- paste(subset(access %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
      text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), egc=="TRUE")$per, "%", sep="")
      text
    }
  )

  output$grid_connect_rt_eap <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
  
        text <- paste(round(mean(access_tier$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
  
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
  
        text <- paste(round(mean(subset(access_tier, region==input$regions_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
  
        #text <- paste(subset(subset(access, region==input$regions_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), egc=="TRUE")$per, "%", sep="")
  
        #text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(egc, na.rm=T)))$average, 1), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
  
  
        text <- paste(round(mean(subset(access_tier, state==input$states_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
  
        #text <- paste(subset(subset(access, state==input$states_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), egc=="TRUE")$per, "%", sep="")
  
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 1), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
  
        text <- paste(round(mean(subset(access_tier, state==input$states_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
  
        #text <- paste(subset(subset(access, state==input$states_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), egc=="TRUE")$per, "%", sep="")
  
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 1), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
  
        text <- paste(round(mean(subset(access_tier, lga==input$lgas_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
  
        #text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), egc=="TRUE")$per, "%", sep="")
  
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 1), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
  
        text <- paste(round(mean(subset(access_tier, lga==input$lgas_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
  
        #text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), egc=="TRUE")$per, "%", sep="")
  
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
  
        text <- paste(round(mean(subset(access_tier, lga==input$lgas_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
  
        #text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), egc=="TRUE")$per, "%", sep="")
  
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 1), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
  
  
            text <- paste(round(mean(subset(access_tier, lga==input$lgas_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
             #text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), egc=="TRUE")$per, "%", sep="")
  
          #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 1), big.mark = ","), sep=" ")
        } else {
  
          text <- paste(round(mean(access_tier$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
  
          #text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), egc=="TRUE")$per, "%", sep="")
  
          #text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        }
        text
      }
    )


  
  output$grid_connect_rt_eap_1 <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All")){
        
        text <- paste(round(mean(access_tier$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
        
      } else if((input$regions_access != "All") & (input$states_access == "All")){
        
        text <- paste(round(mean(subset(access_tier, region==input$regions_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
      
      } else if((input$regions_access == "All") & (input$states_access != "All")){
        
        text <- paste(round(mean(subset(access_tier, state==input$states_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste(round(mean(subset(access_tier, state==input$states_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
        
      } else {
        
        text <- paste(round(mean(subset(access_tier, state==input$states_access)$Tier_avail_cap != "Tier 0", na.rm = TRUE) * 100, 1), " %")
        
        }
      text
    }
  )
  
  
  
  
  output$access_alt_esp_home <- renderText(
    {
      text <- paste(subset(access %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), aeo=="TRUE")$per, "%", sep="")
      #text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      text
    }
  )
  
  output$access_ecp_home <- renderText(
    {
      
      text <- paste0(round(median(access$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
      
      # text <- paste(round((access %>% summarise(average = median(Daily_Capacity_in_Wh)))$average/1000, 2), "kWh", sep=" ")
      text
    }
  )
  
  
  output$access_ecp_home_eap <- renderText(
    {
      
      #      text <- paste0(round(median(access$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
      
      # text <- paste(round((access %>% summarise(average = median(Daily_Capacity_in_Wh)))$average/1000, 2), "kWh", sep=" ")
      #     text
      
      
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste0(round(median(access$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
        
        
        #text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste0(round(median(subset(access, region==input$regions_access)$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
        
        #text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste0(round(median(subset(access, state==input$states_access)$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
        
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste0(round(median(subset(access, state==input$states_access)$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
        
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste0(round(median(subset(access, lga==input$lgas_access)$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
        
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste0(round(median(subset(access, lga==input$lgas_access)$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
        
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste0(round(median(subset(access, lga==input$lgas_access)$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
        
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste0(round(median(subset(access, lga==input$lgas_access)$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
        
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else {
        text <- paste0(round(median(access$Daily_Capacity_in_Wh, na.rm = TRUE)/1000, 1), "kWh")
        
        #text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      }
      text
      
      
    }
  )
  
  output$access_egs_home <- renderText(
    {
      
      text <- paste("₦", format(round((access %>% summarise(median = median(grid_spend, na.rm=T)/30))$median, 0), big.mark = ","), sep=" ")
      
      #text <- paste("₦", format(round((access %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      
      text
      #grid_hrs
    }
  )
  
  
  output$access_egs_home_eap <- renderText(
    {
      
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- paste("₦", format(round((access %>% summarise(median = median(grid_spend, na.rm=T)/30))$median, 0), big.mark = ","), sep=" ")
        #text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(median = median(grid_spend, na.rm=T)/30))$median, 0), big.mark = ","), sep=" ")
        #text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(median = median(grid_spend, na.rm=T)/30))$median, 0), big.mark = ","), sep=" ")
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(median = median(grid_spend, na.rm=T)/30))$median, 0), big.mark = ","), sep=" ")
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(median = median(grid_spend, na.rm=T)/30))$median, 0), big.mark = ","), sep=" ")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(median = median(grid_spend, na.rm=T)/30))$median, 0), big.mark = ","), sep=" ")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(median = median(grid_spend, na.rm=T)/30))$median, 0), big.mark = ","), sep=" ")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(median = median(grid_spend, na.rm=T)/30))$median, 0), big.mark = ","), sep=" ")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else {
        
        text <- paste("₦", format(round((access %>% summarise(median = median(grid_spend, na.rm=T)/30))$median, 0), big.mark = ","), sep=" ")
        #text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      }
      text
      
    }
  )
  
  output$access_wdr_home <- renderText(
    {
      
      #text <- paste(100 - round((access %>% summarise(median = median(weekly_disrupt, na.rm=T)))$median / 168 * 100 , 0), "%")
      text <- paste(format(24 - round((access %>% summarise(average = median(weekly_disrupt, na.rm=T)/7))$average, 0), big.mark = ","), "Hrs", sep=" ")
      
      text
    }
  )
  # 
  # output$access_wdr_home_eap <- renderText(
  #   {
  #     
  #     text <- c()
  #     if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
  #       
  #       text <- paste(100 - round((access %>% summarise(median = median(weekly_disrupt, na.rm=T)))$median / 168 * 100 , 0), "%")
  #       
  #     } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
  #       
  #       text <- paste(100 - round((subset(access, region==input$regions_access) %>% summarise(median = median(weekly_disrupt, na.rm=T)))$median / 168 * 100 , 0), "%")
  #    
  #     } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
  #       
  #       text <- paste(100 - round((subset(access, state==input$states_access) %>% summarise(median = median(weekly_disrupt, na.rm=T)))$median / 168 * 100 , 0), "%")
  #       
  #     } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
  #       
  #       text <- paste(100 - round((subset(access, state==input$states_access) %>% summarise(median = median(weekly_disrupt, na.rm=T)))$median / 168 * 100 , 0), "%")
  #       
  #     } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
  #       
  #       text <- paste(100 - round((subset(access, lga==input$lgas_access) %>% summarise(median = median(weekly_disrupt, na.rm=T)))$median / 168 * 100 , 0), "%")
  # 
  #     } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
  #       
  #       text <- paste(100 - round((subset(access, lga==input$lgas_access) %>% summarise(median = median(weekly_disrupt, na.rm=T)))$median / 168 * 100 , 0), "%")
  #      
  #     } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
  #       
  #       text <- paste(100 - round((subset(access, lga==input$lgas_access) %>% summarise(median = median(weekly_disrupt, na.rm=T)))$median / 168 * 100 , 0), "%")
  #       
  #     } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
  #       
  #       text <- paste(100 - round((subset(access, lga==input$lgas_access) %>% summarise(median = median(weekly_disrupt, na.rm=T)))$median / 168 * 100 , 0), "%")
  #     
  #     } else {
  #       
  #       text <- paste(100 - round((access %>% summarise(median = median(weekly_disrupt, na.rm=T)))$median / 168 * 100 , 0), "%")
  # 
  #     }
  #     text
  #     
  #   }
  # )
  # 
  # 
  # 
  
  output$access_wdr_home_eap <- renderText(
    {
      
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All")){
        
        text <- paste(format(24 - round((access %>% summarise(average = median(weekly_disrupt, na.rm=T)/7))$average, 0), big.mark = ","), "Hrs", sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access == "All")){
        
         text <- paste(format(24 - round((subset(access, region==input$regions_access) %>% summarise(average = median(weekly_disrupt, na.rm=T)/7))$average, 0), big.mark = ","), "Hrs", sep=" ")
         
     
      } else if((input$regions_access == "All") & (input$states_access != "All")){
        
        text <- paste(format(24 - round((subset(access, state==input$states_access) %>% summarise(average = median(weekly_disrupt, na.rm=T)/7))$average, 0), big.mark = ","), "Hrs", sep=" ")
      
        
      } else {
        
        text <- paste(format(24 - round((subset(access, state==input$states_access) %>% summarise(average = median(weekly_disrupt, na.rm=T)/7))$average, 0), big.mark = ","), "Hrs", sep=" ")
        

      }
      text
      
    }
  )
  
  
  # 
  # 
  # output$affordability_eap <- renderText(
  #   {
  #     
  #     access_tier_aff <- access_tier[!is.na(access_tier$Percentage_income_spend_on_energy) & access_tier$Percentage_income_spend_on_energy != 0, ]
  #     text <- c()
  #     if((input$regions_access == "All") & (input$states_access == "All")){
  #       
  #       text <- paste(format(round((access_tier_aff %>% summarise(average = mean(Percentage_income_spend_on_energy, na.rm=T)/7))$average, 0), big.mark = ","), "%", sep=" ")
  #       
  #     } else if((input$regions_access != "All") & (input$states_access == "All")){
  #       
  #       text <- paste(format(round((subset(access_tier_aff, region==input$regions_access) %>% summarise(average = mean(Percentage_income_spend_on_energy, na.rm=T)/7))$average, 0), big.mark = ","), "%", sep=" ")
  #       
  #       
  #     } else if((input$regions_access == "All") & (input$states_access != "All")){
  #       
  #       text <- paste(format(round((subset(access_tier_aff, state==input$states_access) %>% summarise(average = mean(Percentage_income_spend_on_energy, na.rm=T)/7))$average, 0), big.mark = ","), "%", sep=" ")
  #       
  #       
  #     } else {
  #       
  #       text <- paste(format(round((subset(access_tier_aff, state==input$states_access) %>% summarise(average = mean(Percentage_income_spend_on_energy, na.rm=T)/7))$average, 0), big.mark = ","), "%", sep=" ")
  #       
  #       
  #     }
  #     text
  #     
  #   }
  # )
  # 
  
  
  output$affordability_eap <- renderText({
    
    access_tier_aff <- access_tier %>%
      filter(!is.na(Percentage_income_spend_on_energy), Percentage_income_spend_on_energy != 0)
    
    # Default to all
    filtered_data <- access_tier_aff
    
    if (input$regions_access != "All" & input$states_access == "All") {
      filtered_data <- filter(access_tier_aff, region == input$regions_access)
    } else if (input$states_access != "All") {
      filtered_data <- filter(access_tier_aff, state == input$states_access)
    }
    
    average_val <- median(filtered_data$Percentage_income_spend_on_energy, na.rm = TRUE)
    
    paste(format(round(average_val, 0), big.mark = ","), "%")
  })
  
  
  output$access_aeo_home <- renderText(
    {
      text <- paste(subset(access %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
      text
    }
  )
  
  
  output$access_aeo_home_eap <- renderText(
    {
      
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(access %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
        #text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(subset(access, region==input$regions_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
        #text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else {
        
        text <- paste(subset(access %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 1)), aeo=="TRUE")$per, "%", sep="")
        #text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      }
      text
      
    }
  )
  
  
  output$access_gen_spend_home <- renderText(
    {
      text <- paste("₦",format(round((access %>% summarise(median = median(gen_spend, na.rm=T)/30))$median, 0), big.mark = ","),  sep=" ")
      text
    }
  )
  
  
  output$access_gen_spend_home_eap <- renderText(
    {
      
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        
        text <- paste("₦",format(round((access %>% summarise(median = median(gen_spend, na.rm=T)/30))$median, 0), big.mark = ","),  sep=" ")
        #text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- paste("₦",format(round((subset(access, region==input$regions_access) %>% summarise(median = median(gen_spend, na.rm=T)/30))$median, 0), big.mark = ","),  sep=" ")
        #text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste("₦",format(round((subset(access, state==input$states_access) %>% summarise(median = median(gen_spend, na.rm=T)/30))$median, 0), big.mark = ","),  sep=" ")
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste("₦",format(round((subset(access, state==input$states_access) %>% summarise(median = median(gen_spend, na.rm=T)/30))$median, 0), big.mark = ","),  sep=" ")
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- paste("₦",format(round((subset(access, lga==input$lgas_access) %>% summarise(median = median(gen_spend, na.rm=T)/30))$median, 0), big.mark = ","),  sep=" ")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- paste("₦",format(round((subset(access, lga==input$lgas_access) %>% summarise(median = median(gen_spend, na.rm=T)/30))$median, 0), big.mark = ","),  sep=" ")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- paste("₦",format(round((subset(access, lga==input$lgas_access) %>% summarise(median = median(gen_spend, na.rm=T)/30))$median, 0), big.mark = ","),  sep=" ")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- paste("₦",format(round((subset(access, lga==input$lgas_access) %>% summarise(median = median(gen_spend, na.rm=T)/30))$median, 0), big.mark = ","),  sep=" ")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else {
        
        text <- paste("₦",format(round((access %>% summarise(median = median(gen_spend, na.rm=T)/30))$median, 0), big.mark = ","),  sep=" ")
        #text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      }
      text
      
    }
  )
  
  output$access_aet_solar_home <- renderText(
    {
      text <- paste(subset(access %>% 
                             group_by(aet) %>% 
                             summarise(count = n()) %>% 
                             mutate(per = round(count / sum(count) * 100, 0)), 
                           aet == "Solar home system")$per, "%", sep = "")
      text
    }
  )
  
  
  output$access_aet_solar_home_eap <- renderText(
    {
      
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(access %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Solar home system")$per, "%", sep = "")
        #text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(subset(access, region==input$regions_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Solar home system")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(subset(access, state==input$states_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Solar home system")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(subset(access, state==input$states_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Solar home system")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Solar home system")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Solar home system")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Solar home system")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Solar home system")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else {
        
        text <- paste(subset(access %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Solar home system")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      }
      text
      
    }
  )
  output$access_aet_gen_home <- renderText(
    {
      text <- paste(subset(access %>% 
                             group_by(aet) %>% 
                             summarise(count = n()) %>% 
                             mutate(per = round(count / sum(count) * 100, 0)), 
                           aet == "Generator")$per, "%", sep = "")
      text
    }
  )
  
  
  output$access_aet_gen_home_eap <- renderText(
    {
      
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(access %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Generator")$per, "%", sep = "")
        #text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(subset(access, region==input$regions_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Generator")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(subset(access, state==input$states_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Generator")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- paste(subset(subset(access, state==input$states_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Generator")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Generator")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Generator")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Generator")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Generator")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(egc, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      } else {
        
        text <- paste(subset(access %>% 
                               group_by(aet) %>% 
                               summarise(count = n()) %>% 
                               mutate(per = round(count / sum(count) * 100, 0)), 
                             aet == "Generator")$per, "%", sep = "")
        #text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
        
      }
      text
      
    }
  )
  
  output$access_aso_home <- renderText(
    {
      text <- c("3 %")
      text
    }
  )
  
  
  output$access_dist_home_2 <- renderPlot(
    {
      access_tier_access <- access_tier %>%
        group_by(Tier_avail_cap) %>%
        summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
        mutate(value = (count / sum(count)) * 100,
               per = paste0(round(value, 1), "%"))
      
      # Define all possible tiers (Tier 0 to Tier 5)
      required_tiers <- paste0("Tier ", 0:5)
      
      # Ensure all tiers (0-5) are present, filling missing ones with 0
      access_tier_access <- access_tier_access %>%
        complete(Tier_avail_cap = required_tiers, fill = list(count = 0, value = 0)) %>%
        mutate(per = paste0(round(value, 1), "%"))  # Format percentage column
      
      
      ggplot(access_tier_access, aes(Tier_avail_cap, value, fill=Tier_avail_cap)) + 
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("Tier 0" = "#C2251C",
                                     "Tier 1" = "#F47A1F",
                                     "Tier 2" = "#FAA73B",
                                     "Tier 3" = "#FFDB67",
                                     "Tier 4" = "#8AC366",
                                     "Tier 5" = "#01634F")) +
        geom_text(
          aes(label = per, y = value + 0.05),
          position = position_dodge(0.9),
          vjust = 0
        ) +
        scale_x_discrete(name="") +
        scale_y_continuous(name="") +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(face="bold", colour="#075D04", size=16),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) 
    }
  )
  
  output$access_dist_home <- renderPlot(
    {
      access_tier_access <- access_tier %>%
        group_by(Tier_all_metrics) %>%
        summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
        mutate(value = (count / sum(count)) * 100,
               per = paste0(round(value, 1), "%"))
      
      # Define all possible tiers (Tier 0 to Tier 5)
      required_tiers <- paste0("Tier ", 0:5)
      
      # Ensure all tiers (0-5) are present, filling missing ones with 0
      access_tier_access <- access_tier_access %>%
        complete(Tier_all_metrics = required_tiers, fill = list(count = 0, value = 0)) %>%
        mutate(per = paste0(round(value, 1), "%"))  # Format percentage column
      
      
      ggplot(access_tier_access, aes(Tier_all_metrics, value, fill=Tier_all_metrics)) + 
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("Tier 0" = "#C2251C",
                                     "Tier 1" = "#F47A1F",
                                     "Tier 2" = "#FAA73B",
                                     "Tier 3" = "#FFDB67",
                                     "Tier 4" = "#8AC366",
                                     "Tier 5" = "#01634F")) +
        geom_text(
          aes(label = per, y = value + 0.05),
          position = position_dodge(0.9),
          vjust = 0
        ) +
        scale_x_discrete(name="") +
        scale_y_continuous(name="") +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(face="bold", colour="#075D04", size=16),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) 
    }
  )
  
  
  observeEvent(input$access_link_home, {
    updateTabsetPanel(session, "page",
                      selected = "access")
  })
  
  output$map_home <- renderUI(
    {
      img(src="images/maps/nation_1.png", width="100%")
    }
  )
  
  ################### Stakeholder #############
  
  output$regions_stakeholder <- renderUI({
    
    selectInput("regions_stakeholder", "Select a region : ", c('All', na.omit(regions)))
    
  })
  
  output$states_stakeholder <- renderUI({
    
    if (input$regions_stakeholder == 'All') {
      
      selectInput("states_stakeholder", "Select a state : ", c('All', na.omit(states)))
      
    } else {
      
      selectInput("states_stakeholder", "Select a state : ", c('All', unique(na.omit(subset(region_state_lga, region == input$regions_stakeholder)$state))))
      
    }
    
  })
  
  output$lgas_stakeholder <- renderUI({
    
    if (input$states_stakeholder == 'All') {
      
      selectInput("lgas_stakeholder", "Select a LGA : ", c('All', na.omit(lgas)))
      
    } else {
      
      selectInput("lgas_stakeholder", "Select a LGA : ", c('All', unique(na.omit(subset(region_state_lga, state == input$states_stakeholder)$lga))))
      
    }
    
  })
  
  output$summary_nner_stakeholder <- renderText(
    {
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- (subset(register, category == "National (Non-energy related)") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- (subset(subset(register, region == input$regions_stakeholder), category == "National (Non-energy related)") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- (subset(subset(register, state == input$states_stakeholder), category == "National (Non-energy related)") %>% group_by(category) %>% summarise(count = n()))$count
      } else {
        text <- (subset(subset(register, state == input$states_stakeholder), category == "National (Non-energy related)") %>% group_by(category) %>% summarise(count = n()))$count
      }
      text
    }
  )
  
  
  output$summary_nner_stakeholder_home <- renderText(
    {
        text <- (subset(register, category == "National (Non-energy related)") %>% group_by(category) %>% summarise(count = n()))$count
      
        text
    }
  )
  
  output$summary_ner_stakeholder <- renderText(
    {
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- (subset(register, category == "National (Energy Related)") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- (subset(subset(register, region == input$regions_stakeholder), category == "National (Energy Related)") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- (subset(subset(register, state == input$states_stakeholder), category == "National (Energy Related)") %>% group_by(category) %>% summarise(count = n()))$count
      } else {
        text <- (subset(subset(register, state == input$states_stakeholder), category == "National (Energy Related)") %>% group_by(category) %>% summarise(count = n()))$count
      }
      text
    }
  )
  
  output$summary_ner_stakeholder_home <- renderText(
    {
        text <- (subset(register, category == "National (Energy Related)") %>% group_by(category) %>% summarise(count = n()))$count
     
      text
    }
  )
  
  output$summary_state_stakeholder <- renderText(
    {
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- (subset(register, category == "State") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- (subset(subset(register, region == input$regions_stakeholder), category == "State") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- (subset(subset(register, state == input$states_stakeholder), category == "State") %>% group_by(category) %>% summarise(count = n()))$count
      } else {
        text <- (subset(subset(register, state == input$states_stakeholder), category == "State") %>% group_by(category) %>% summarise(count = n()))$count
      }
      text
    }
  )
  
  
  output$summary_state_stakeholder_home <- renderText(
    {
        text <- (subset(register, category == "State") %>% group_by(category) %>% summarise(count = n()))$count
        text
    }
  )
  
  output$summary_regulatory_stakeholder <- renderText(
    {
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- (subset(register, category == "Regulatory") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- (subset(subset(register, region == input$regions_stakeholder), category == "Regulatory") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- (subset(subset(register, state == input$states_stakeholder), category == "Regulatory") %>% group_by(category) %>% summarise(count = n()))$count
      } else {
        text <- (subset(subset(register, state == input$states_stakeholder), category == "Regulatory") %>% group_by(category) %>% summarise(count = n()))$count
      }
      text
    }
  )
  
  
  output$summary_regulatory_stakeholder_home <- renderText(
    {
        text <- (subset(register, category == "Regulatory") %>% group_by(category) %>% summarise(count = n()))$count
     
      text
    }
  )
  
  output$summary_utilities_stakeholder <- renderText(
    {
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- (subset(register, category == "Utilities") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- (subset(subset(register, region == input$regions_stakeholder), category == "Utilities") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- (subset(subset(register, state == input$states_stakeholder), category == "Utilities") %>% group_by(category) %>% summarise(count = n()))$count
      } else {
        text <- (subset(subset(register, state == input$states_stakeholder), category == "Utilities") %>% group_by(category) %>% summarise(count = n()))$count
      }
      text
    }
  )
  
  
  output$summary_utilities_stakeholder_home <- renderText(
    {
      text <- (subset(register, category == "Utilities & Private Sector") %>% group_by(category) %>% summarise(count = n()))$count
        text
    }
  )
  
  output$summary_dfi_stakeholder_home <- renderText(
    {
        text <- (subset(register, category == "DFIs") %>% group_by(category) %>% summarise(count = n()))$count
        text
    }
  )
  
  output$summary_ofi_stakeholder_home <- renderText(
    {
        text_1 <- (subset(register, category == "Other Financial Institutions") %>% group_by(category) %>% summarise(count = n()))$count
        text_2 <- (subset(register, category == "Other") %>% group_by(category) %>% summarise(count = n()))$count
        text <- text_1 + text_2
        
        text
    }
  )
  
  output$summary_psi_stakeholder <- renderText(
    {
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- (subset(register, category == "Power Sector Program Support Institutions") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- (subset(subset(register, region == input$regions_stakeholder), category == "Power Sector Program Support Institutions") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- (subset(subset(register, state == input$states_stakeholder), category == "Power Sector Program Support Institutions") %>% group_by(category) %>% summarise(count = n()))$count
      } else {
        text <- (subset(subset(register, state == input$states_stakeholder), category == "Power Sector Program Support Institutions") %>% group_by(category) %>% summarise(count = n()))$count
      }
      text
    }
  )
  
  
  
  output$summary_psi_stakeholder_home <- renderText(
    {
        text <- (subset(register, category == "Power Sector Program Support Institutions") %>% group_by(category) %>% summarise(count = n()))$count
      
      text
    }
  )
  
  
  output$summary_communities_stakeholder <- renderText(
    {
      
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- (subset(register, category == "Community Level") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- (subset(subset(register, region == input$regions_stakeholder), category == "Community Level") %>% group_by(category) %>% summarise(count = n()))$count
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- (subset(subset(register, state == input$states_stakeholder), category == "Community Level") %>% group_by(category) %>% summarise(count = n()))$count
      } else {
        text <- (subset(subset(register, state == input$states_stakeholder), category == "Community Level") %>% group_by(category) %>% summarise(count = n()))$count
      }
      text
    }
  )
  
  
  output$summary_communities_stakeholder_home <- renderText(
    {
      
        text <- (subset(register, category == "Community Level") %>% group_by(category) %>% summarise(count = n()))$count
    
      text
    }
  )

  items_per_page <- 20  # Number of cards per page
  total_pages <- reactive(ceiling(nrow(register) / items_per_page))
  current_page <- reactiveVal(1)
  
  
  # Handle previous/next buttons
  observeEvent(input$prev_page, {
    if (current_page() > 1) {
      current_page(current_page() - 1)
    }
  })
  
  observeEvent(input$next_page, {
    if (current_page() < total_pages()) {
      current_page(current_page() + 1)
    }
  })
  
  
  observeEvent(input$stakeholder_link_home, {
    updateActionButton(session, "stakeholder_link_home",
                       label = "View More Stakeholders Information") # Optional: Provide user feedback
    
    session$sendCustomMessage(type = 'open_new_tab',
                              message = list(url = "https://docs.google.com/spreadsheets/d/1eXRrbBayH5VJmyJkTJbldZWq57yhM6uT/edit?usp=sharing&ouid=100267662799051112237&rtpof=true&sd=true"))
  })
  
  
  observeEvent(input$rea_iept_link, {
    updateActionButton(session, "rea_iept_link",
                       label = "Checkout REA's IEPT Tool") # Optional: Provide user feedback
    
    session$sendCustomMessage(type = 'open_new_tab',
                              message = list(url = "https://nigeria-iep.sdg7energyplanning.org/"))
  })
  
  # Update page numbers dynamically
  output$page_numbers <- renderUI({
    tagList(
      lapply(1:total_pages(), function(i) {
        actionButton(paste0("page_", i), label = i, class = ifelse(i == current_page(), "btn btn-success", "btn btn-light"))
      })
    )
  })
  
  
  observe({
    lapply(1:total_pages(), function(i) {
      observeEvent(input[[paste0("page_", i)]], {
        current_page(i)
      })
    })
  })
  
  
# 
#   output$stakeholder_list <- renderUI({
# 
#     counter <- 0
#     # if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
#     #   register <- register
#     # } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
#     #   register <- subset(register, region == input$regions_stakeholder)
#     # } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
#     #   register <- subset(register, state == input$states_stakeholder)
#     # } else if (input$textSearch == "") {
#     #   register  # Show all stakeholders if search box is empty
#     # } else if (input$textSearch != "") {
#     #   register <- register[grepl(input$textSearch, register$name, ignore.case = TRUE), ]
#     # } else {
#     #   register <- subset(register, state == input$states_stakeholder)
#     # }
#     #
#     # Start with full dataset
#     register <- register
# 
#     # Apply Region Filter
#     if (input$regions_stakeholder != "All") {
#       register <- subset(register, region == input$regions_stakeholder)
#     }
# 
#     # Apply State Filter
#     if (input$states_stakeholder != "All") {
#       register <- subset(register, state == input$states_stakeholder)
#     }
# 
#     # Apply Search Filter
#     if (input$textSearch != "") {
#       register <- register[grepl(input$textSearch, register$name, ignore.case = TRUE), ]
#     }
# 
#     # Return the final filtered dataset
#     register
# 
# 
# 
# 
# 
#     start_row <- (current_page() - 1) * items_per_page + 1
#     end_row <- min(start_row + items_per_page - 1, nrow(register))
# 
#     register <- register[start_row:end_row, ]
# 
#     lapply(seq(1, nrow(register), by = 4), function(i) {
#     #lapply(seq_len(nrow(register)), function(a) {
#       #counter <<- counter + 3
#       layout_columns(
#         if(i <= nrow(register)) card(
#           width = 3,
#           card_header(
#             style = paste('background-color: ', register$color[i], ' !important;'),
#             fluidRow(
#               style = 'padding-top: 20px;',
#               column(
#                 width = 3,
#                 img(src="images/sp.png")
#               ),
#               column(
#                 width = 7,
#                 p("Stakeholder Name",
#                   style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #EBEBEB;'),
#                 p(register$name[i],
#                   style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF;')
#               ),
#               column(
#                 width = 2,
#                 popover(
#                   tags$i(class = "fa-solid fa-circle-info fa-1x",
#                          style = 'color: #383A3D; margin: 5px;'),
#                   title = "Role in Nigerian Electrification Value Chain",
#                   p(register$roles_and_responsibilities[i]),
# 
#                   h5("Role in NESIP Development & Implementation"),
#                   p(register$roles_and_responsibilities_nesip[i]),
#                 )
#               )
#             )
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Level",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$level[i],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Category",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$category[i],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("State",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$state[i],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           ),
#           fluidRow(
#             p("Organisation",
#               style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#             p(register$organisation[i],
#               style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#           ),
#           ###influence	importance
#           fluidRow(
#             column(
#               width = 4,
#               p("Influence",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$influence[i],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Importance",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$importance[i],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Strategy",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$role[i],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Phone",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$phone[i],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 5,
#               p("Email",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$email[i],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 3,
#               p("Link",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(tags$i(class = "fa-solid fa-link fa-1x",
#                        style = 'color: #16D10F'),
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           )
#         ),
#         if(i + 1 <= nrow(register)) card(
#           width = 3,
#           card_header(
#             style = paste('background-color: ', register$color[i + 1], ' !important;'),
#             fluidRow(
#               style = 'padding-top: 20px;',
#               column(
#                 width = 3,
#                 img(src="images/sp.png")
#               ),
#               column(
#                 width = 7,
#                 p("Stakeholder Name",
#                   style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #EBEBEB;'),
#                 p(register$name[i + 1],
#                   style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF;')
#               ),
#               column(
#                 width = 2,
#                 popover(
#                   tags$i(class = "fa-solid fa-circle-info fa-1x",
#                          style = 'color: #383A3D; margin: 5px;'),
#                   title = "Role in Nigerian Electrification Value Chain",
#                   p(register$roles_and_responsibilities[i+1]),
# 
#                   h5("Role in NESIP Development & Implementation"),
#                   p(register$roles_and_responsibilities_nesip[i+1]),
#                 )
#               )
#             )
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Level",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$level[i + 1],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Category",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$category[i + 1],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("State",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$state[i + 1],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           ),
#           fluidRow(
#             p("Organisation",
#               style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#             p(register$organisation[i + 1],
#               style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Influence",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$influence[i + 1],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Importance",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$importance[i + 1],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Strategy",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$role[i + 1],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Phone",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$phone[i + 1],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 5,
#               p("Email",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$email[i + 1],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 3,
#               p("Link",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(tags$i(class = "fa-solid fa-link fa-1x",
#                        style = 'color: #16D10F'),
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           )
#         ),
#         if(i + 2 <= nrow(register)) card(
#           width = 3,
#           card_header(
#             style = paste('background-color: ', register$color[i + 2], ' !important;'),
#             fluidRow(
#               style = 'padding-top: 20px;',
#               column(
#                 width = 3,
#                 img(src="images/sp.png")
#               ),
#               column(
#                 width = 7,
#                 p("Stakeholder Name",
#                   style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #EBEBEB;'),
#                 p(register$name[i + 2],
#                   style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF;')
#               ),
#               column(
#                 width = 2,
#                 popover(
#                   tags$i(class = "fa-solid fa-circle-info fa-1x",
#                          style = 'color: #383A3D; margin: 5px;'),
#                   title = "Role in Nigerian Electrification Value Chain",
#                   p(register$roles_and_responsibilities[i+2]),
# 
#                   h5("Role in NESIP Development & Implementation"),
#                   p(register$roles_and_responsibilities_nesip[i+2]),
#                 )
#               )
#             )
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Level",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$level[i + 2],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Category",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$category[i + 2],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("State",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$state[i + 2],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           ),
#           fluidRow(
#             p("Organisation",
#               style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#             p(register$organisation[i + 2],
#               style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Influence",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$influence[i + 2],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Importance",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$importance[i + 2],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Strategy",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$role[i + 2],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Phone",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$phone[i + 2],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 5,
#               p("Email",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$email[i + 2],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 3,
#               p("Link",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(tags$i(class = "fa-solid fa-link fa-1x",
#                        style = 'color: #16D10F'),
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           )
#         ),
#         if(i + 3 <= nrow(register)) card(
#           width = 3,
#           card_header(
#             style = paste('background-color: ', register$color[i + 3], ' !important;'),
#             fluidRow(
#               style = 'padding-top: 20px;',
#               column(
#                 width = 3,
#                 img(src="images/sp.png")
#               ),
#               column(
#                 width = 7,
#                 p("Stakeholder Name",
#                   style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #EBEBEB;'),
#                 p(register$name[i + 3],
#                   style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #FFFFFF;')
#               ),
#               column(
#                 width = 2,
#                 popover(
#                   tags$i(class = "fa-solid fa-circle-info fa-1x",
#                          style = 'color: #383A3D; margin: 5px;'),
#                   title = "Role in Nigerian Electrification Value Chain",
#                   p(register$roles_and_responsibilities[i+3]),
# 
#                   h5("Role in NESIP Development & Implementation"),
#                   p(register$roles_and_responsibilities_nesip[i+3]),
#                 )
#               )
#             )
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Level",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$level[i + 3],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Category",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$category[i + 3],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("State",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$state[i + 3],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           ),
#           fluidRow(
#             p("Organisation",
#               style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#             p(register$organisation[i + 3],
#               style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Influence",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$influence[i + 3],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Importance",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$importance[i + 3],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 4,
#               p("Strategy",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$role[i + 3],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           ),
#           fluidRow(
#             column(
#               width = 4,
#               p("Phone",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$phone[i + 3],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 5,
#               p("Email",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(register$email[i + 3],
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             ),
#             column(
#               width = 3,
#               p("Link",
#                 style = 'font:Poppins; font-weight: 500; font-size: 10.89px; line-height: 100%; letter-spacing: 2%; color: #888C95;'),
#               p(tags$i(class = "fa-solid fa-link fa-1x",
#                        style = 'color: #16D10F'),
#                 style = 'font:Poppins; font-weight: 700; font-size: 12px; line-height: 120%; letter-spacing: 2%; color: #16D10F;')
#             )
#           )
#         )
# 
#       )
#     })
#   })
  
  
  
  
  # 
  # 
  # per_page <- 3  # number of entries per page
  # 
  # paged_data <- reactive({
  #   states_data %>%
  #     arrange(desc(Score)) %>%
  #     slice(((input$page - 1) * per_page + 1):(input$page * per_page))
  # })
  # 
  # output$leaderboard_ui <- renderUI({
  #   df <- paged_data()
  #   
  #   # tagList(
  #   #   lapply(1:nrow(df), function(i) {
  #   #     div(class = "leaderboard-entry",
  #   #         span(class = "leaderboard-rank", paste0(i + (input$page - 1) * per_page, ".")),
  #   #         img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/Lagos_State_Seal.png/120px-Lagos_State_Seal.png", 
  #   #             class = "leaderboard-logo"),
  #   #         span(class = "leaderboard-name", df$State[i]),
  #   #         span(class = "leaderboard-score", paste0(df$Score[i], "/5")),
  #   #         span(class = "leaderboard-status", df$Status[i]),
  #   #         icon("chevron-right")
  #   #     )
  #   #   })
  #   #   
  #   # )
  #   
  #   tagList(
  #     lapply(1:nrow(df), function(i) {
  #       div(class = "leaderboard-entry",
  #           span(class = "leaderboard-rank", paste0(i + (input$page - 1) * per_page, ".")),
  #           img(src = df$Logo[i], class = "leaderboard-logo"),
  #           span(class = "leaderboard-name", df$State[i]),
  #           span(class = "leaderboard-score", paste0(df$Score[i], "/5")),
  #           span(class = "leaderboard-status", df$Status[i]),
  #           icon("chevron-right")
  #       )
  #     })
  #   )
  # })
  
    #################################################
    per_page <- 10
    total_pages_ <- ceiling(nrow(state_readiness) / per_page)
    
    current_page <- reactiveVal(1)
    
    observeEvent(input$page_click, {
      current_page(as.numeric(input$page_click))
    })
    
    output$pagination_ui <- renderUI({
      tagList(
        lapply(1:total_pages_, function(i) {
          class <- if (i == current_page()) "pagination-link pagination-active" else "pagination-link"
          # actionLink(inputId = "page_click", label = i, class = class, 
          #            onclick = sprintf("Shiny.setInputValue('page_click', %d, {priority: 'event'})", i))
          
          tags$a(
            i,
            href = "#",
            class = class,
            onclick = sprintf("Shiny.setInputValue('page_click', %d, {priority: 'event'})", i)
          )
          
        })
      )
    })
    
    # paged_data <- reactive({
    #   state_readiness %>%
    #     arrange(desc(`State__Score`)) %>%
    #     slice(((current_page() - 1) * per_page + 1):(current_page() * per_page))
    # })
    
    # paged_data <- reactive({
    #   df <- state_readiness
    #   
    #   if (input$ranking_metric != "All") {
    #     req(input$ranking_metric)
    #     
    #     sort_col <- input$ranking_metric
    #     
    #     df <- df %>%
    #       arrange(desc(.data[[sort_col]])) %>%
    #       mutate(`State Score` = .data[[sort_col]])  # So leaderboard shows selected metric score
    #   } else {
    #     df <- df %>%
    #       arrange(desc(`State__Score`))
    #   }
    #   
    #   df %>%
    #     slice(((current_page() - 1) * per_page + 1):(current_page() * per_page))
    # })
    # 
    
    paged_data <- reactive({
      df <- state_readiness
      
      if (input$ranking_metric != "All") {
        req(input$ranking_metric)
        
        sort_col <- input$ranking_metric
        score_col <- paste0(sort_col, "_Score")
        
        df <- df %>%
          arrange(desc(.data[[sort_col]])) %>%
          mutate(`State Score` = .data[[score_col]])  # Display the score column
      } else {
        df <- df %>%
          arrange(desc(`State__Score`))  # Default overall score
      }
      
      df %>%
        slice(((current_page() - 1) * per_page + 1):(current_page() * per_page))
    })
    
    
    # output$leaderboard_ui <- renderUI({
    #   df <- paged_data()
    # 
    #   tagList(
    #     lapply(1:nrow(df), function(i) {
    #       div(class = "leaderboard-entry",
    #           span(class = "leaderboard-rank", paste0(i + (current_page() - 1) * per_page, ".")),
    #           #img(src = df$Logo[i], class = "leaderboard-logo"),
    #           span(class = "leaderboard-name", df$State[i]),
    #           span(class = "leaderboard-score", paste0(df$`State Score`[i], "/5")),
    #           span(class = "leaderboard-status", df$Status[i]),
    #           # icon("chevron-right")
    #       )
    #     })
    #   )
    # })
    
    
    output$leaderboard_ui <- renderUI({
      df <- paged_data()
      metric <- input$ranking_metric
      metric_score <- if (metric == "All") df$`State Score` else df[[paste0(metric, "_Score")]]
      # metric_comment <- if (metric == "All") df$`Status` else df[[metric]]
      metric_comment <- if (metric == "All") df$Status else df[[paste0(metric, "_Comment")]]
      
      
      tagList(
        lapply(1:nrow(df), function(i) {
          div(class = "leaderboard-entry",
              span(class = "leaderboard-rank", paste0(i + (current_page() - 1) * per_page, ".")),
              span(class = "leaderboard-name", df$State[i]),
              span(class = "leaderboard-score", paste0(metric_score[i], "/5")),
              span(class = "leaderboard-status", paste0(metric_comment[i]))
          )
        })
      )
    })
    
    # 
    # output$leaderboard_ui <- renderUI({
    #   df <- paged_data()
    #   metric <- input$ranking_metric
    #   score_col <- if (metric != "All") paste0(metric, "_Score") else "State__Score"
    #   metric_comment <- if (metric == "All") df$Status else df[[paste0(metric, "_Comment")]]
    #   
    #   tagList(
    #     lapply(1:nrow(df), function(i) {
    #       div(class = "leaderboard-entry",
    #           span(class = "leaderboard-rank", paste0(i + (current_page() - 1) * per_page, ".")),
    #           span(class = "leaderboard-name", df$State[i]),
    #           span(class = "leaderboard-score", paste0(score_col[i], "/5")),
    #           span(class = "leaderboard-status", paste0(metric_comment[i]))
    #       )
    #     })
    #   )
    # })
    # 
    # 
    

    # 
    # output$leaderboard_ui <- renderUI({
    #   df <- paged_data()
    #   
    #   tagList(
    #     lapply(1:nrow(df), function(i) {
    #       state_id <- paste0("state_", i + (current_page() - 1) * per_page)
    #       div(
    #         class = "leaderboard-entry",
    #         span(class = "leaderboard-rank", paste0(i + (current_page() - 1) * per_page, ".")),
    #         # img(src = df$Logo[i], class = "leaderboard-logo"),
    #         span(class = "leaderboard-name", df$State[i]),
    #         span(class = "leaderboard-score", paste0(df$Score[i], "/5")),
    #         span(class = "leaderboard-status", df$Status[i]),
    #         span(class = "toggle-icon", 
    #              icon("chevron-right"), 
    #              onclick = sprintf("$('#%s').toggle();", state_id)),
    #         
    #         div(
    #           id = state_id,
    #           class = "details",
    #           HTML(sprintf("
    #         <div><strong>Policy:</strong> %d/5</div>
    #         <div><strong>Funding:</strong> %d/5</div>
    #         <div><strong>Infrastructure:</strong> %d/5</div>
    #         <div><strong>Capacity:</strong> %d/5</div>
    #         <div><strong>Data:</strong> %d/5</div>
    #       ", df$Policy[i], df$Funding[i], df$Infrastructure[i], df$Capacity[i], df$Data[i]))
    #         )
    #       )
    #     })
    #   )
    # })
    
    
    
    
    #############################
  
  output$map_stakeholder <- renderUI(
    {
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        img(src="images/maps/nation.png", width="100%")
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        img(src=paste0("images/maps/region/", input$regions_stakeholder, ".png", sep=""), width="100%")
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        img(src=paste0("images/maps/state/", input$states_stakeholder, ".png", sep=""), width="100%")
      } else {
        img(src=paste0("images/maps/state/", input$states_stakeholder, ".png", sep=""), width="100%")
      }
    }
  )
  
  output$map_info_stakeholder <- renderText(
    {
      text <- c()
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- c("National Info")
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- c("Regional Info")
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- c("State Info")
      } else {
        text <- c("State Info")
      }
      
      text
    }
  )
  
  output$map_area_stakeholder <- renderText(
    {
      text <- c()
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- paste(format((region_state %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- paste(format((subset(region_state, region==input$regions_stakeholder) %>% group_by(region) %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- paste(format((subset(region_state, state==input$states_stakeholder) %>% group_by(state) %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      } else {
        text <- paste(format((subset(region_state, state==input$states_stakeholder) %>% group_by(state) %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      }
      
      text
    }
  )
  
  output$map_population_stakeholder <- renderText(
    {
      text <- c()
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- paste(round((region_state %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- paste(round((subset(region_state, region==input$regions_stakeholder) %>% group_by(region) %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- paste(round((subset(region_state, state==input$states_stakeholder) %>% group_by(state) %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      } else {
        text <- paste(round((subset(region_state, state==input$states_stakeholder) %>% group_by(state) %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      }
      
      text
    }
  )
  
  output$map_rank_stakeholder <- renderUI(
    {
      n <- c()
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        n <- ceiling(mean(mean(readiness$ps), mean(readiness$fs), mean(readiness$ins), mean(readiness$cs), mean(readiness$ds)))
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        n <- ceiling(mean(mean(subset(readiness, region==input$regions_stakeholder)$ps), mean(subset(readiness, region==input$regions_stakeholder)$fs), mean(subset(readiness, region==input$regions_stakeholder)$ins), mean(subset(readiness, region==input$regions_stakeholder)$cs), mean(subset(readiness, region==input$regions_stakeholder)$ds)))
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        n <- ceiling(mean(mean(subset(readiness, state==input$states_stakeholder)$ps), mean(subset(readiness, state==input$states_stakeholder)$fs), mean(subset(readiness, state==input$states_stakeholder)$ins), mean(subset(readiness, state==input$states_stakeholder)$cs), mean(subset(readiness, state==input$states_stakeholder)$ds)))
      } else {
        n <- ceiling(mean(mean(subset(readiness, state==input$states_stakeholder)$ps), mean(subset(readiness, state==input$states_stakeholder)$fs), mean(subset(readiness, state==input$states_stakeholder)$ins), mean(subset(readiness, state==input$states_stakeholder)$cs), mean(subset(readiness, state==input$states_stakeholder)$ds)))
      }
      
      # lapply(1:n, function(a) {
      #   tags$i(class = "fa-solid fa-star fa-1x",
      #          style = 'color: #075D04')
      # })
      n
    }
  )
  
  output$map_register_stakeholder <- renderText(
    {
      text <- c()
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- sum((register %>% group_by(level) %>% summarise(count = n()))$count)
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- sum((subset(register, region==input$regions_stakeholder) %>% group_by(level) %>% summarise(count = n()))$count)
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- sum((subset(register, state==input$states_stakeholder) %>% group_by(level) %>% summarise(count = n()))$count)
      } else {
        text <- sum((subset(register, state==input$states_stakeholder) %>% group_by(level) %>% summarise(count = n()))$count)
      }
      
      text
    }
  )
  
  output$map_tier_stakeholder <- renderText(
    {
      text <- c()
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- na.omit(access_tier_2 %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- na.omit(subset(access_tier_2, region==input$regions_stakeholder) %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- na.omit(subset(access_tier_2, state==input$states_stakeholder) %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      } else {
        text <- na.omit(subset(access_tier_2, state==input$states_stakeholder) %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      }
      
      text
    }
  )
  

  output$map_region_stakeholder <- renderText(
    {
      text <- c()
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- paste(c("North central", "North east", "North west", "South west", "South east", "South south"), collapse=", ")
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- paste(str_to_title(str_split(input$regions_stakeholder, "_")[[1]][1]), str_split(input$regions_stakeholder, "_")[[1]][2], sep=" ")
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- paste(str_to_title(str_split(subset(region_state, state == input$states_stakeholder)$region, "_")[[1]][1]), str_split(subset(region_state, state == input$states_stakeholder)$region, "_")[[1]][2], sep=" ")
      } else {
        text <- paste(str_to_title(str_split(subset(region_state, state == input$states_stakeholder)$region, "_")[[1]][1]), str_split(subset(region_state, state == input$states_stakeholder)$region, "_")[[1]][2], sep=" ")
      }
      
      text
    }
  )
  
  output$map_state_stakeholder <- renderText(
    {
      text <- c()
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- c("36 & FCT/Abuja")
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        text <- paste(subset(region_state, region == input$regions_stakeholder)$state, collapse = ", ")
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- input$states_stakeholder
      } else {
        text <- input$states_stakeholder
      }
      
      text
    }
  )
  
  output$map_summary_stakeholder <- renderText(
    {
      text <- c()
      if((input$regions_stakeholder == "All") & (input$states_stakeholder == "All")){
        text <- c("Nigeria has about 60% electricity access, with rural areas facing the greatest shortages. The country’s power generation capacity is 12,500 MW, but actual supply is often lower due to infrastructure and fuel issues. While the government and private sector focus on solar and off-grid solutions, challenges remain with grid reliability, infrastructure, and funding. Renewable energy potential, especially solar, is high, but rural electrification and consistent power supply are still major hurdles.")
      } else if((input$regions_stakeholder != "All") & (input$states_stakeholder == "All")){
        if(input$regions_stakeholder == "north_east"){
          text <- c("This region has the lowest electrification rates due to security challenges, weak grid infrastructure, and limited investments. Many rural communities lack access to the national grid and rely on expensive diesel generators. Solar mini-grids and standalone systems are gaining traction as alternative solutions. Government and donor-funded projects aim to expand energy access, but progress remains slow due to security risks.")
        } else if(input$regions_stakeholder == "north_west"){
          text <- c("Grid electricity access is expanding in major cities, but rural communities still face significant gaps. Many households depend on small solar home systems and mini-grids. Power infrastructure is improving, but vandalism and weak distribution networks limit reliability. The region has high solar potential, making off-grid electrification a promising alternative. Affordability remains a barrier for low-income communities.")
        } else if(input$regions_stakeholder == "north_west"){
          text <- c("Electrification is moderate, with urban centers enjoying better access than rural areas. The region hosts major hydropower plants like Kainji and Shiroro, but transmission bottlenecks limit efficiency. Rural communities still rely on diesel generators and standalone solar solutions. Distribution infrastructure is expanding, but affordability remains a challenge. Efforts to integrate renewable energy solutions are growing, but implementation is slow.")
        } else if(input$regions_stakeholder == "south_west"){
          text <- c("The region has the highest electrification rate in Nigeria, with a well-developed grid network. Lagos, the economic hub, has relatively better access, but demand still surpasses supply, leading to widespread generator use. Rural electrification is expanding, driven by private sector investments in off-grid solutions. Industrial and commercial sectors rely heavily on backup power due to grid instability. Renewable energy projects, particularly solar, are gaining traction to supplement existing capacity.")
        } else if(input$regions_stakeholder == "south_east"){
          text <- c("Urban areas have high grid access, but rural electrification lags behind. Many communities depend on diesel generators, while mini-grids and solar home systems are gradually emerging. Power supply is unreliable, with frequent blackouts affecting businesses and households. Economic activities, particularly in manufacturing and trade, are constrained by unstable electricity. Distributed renewable energy solutions are being explored to bridge the gap.")
        } else {
          text <- c("Despite strong grid infrastructure, electricity supply remains unreliable due to aging transmission lines and gas supply constraints. The region hosts several power plants but suffers from systemic inefficiencies. Rural electrification is improving, with growing adoption of solar mini-grids. Flood-prone areas face additional challenges in maintaining infrastructure. Efforts to strengthen grid reliability and integrate renewable energy sources are ongoing.")
        }
      } else if((input$regions_stakeholder == "All") & (input$states_stakeholder != "All")){
        text <- subset(readiness, state==input$states_stakeholder)$state_comms[1]
      } else {
        text <- subset(readiness, state==input$states_stakeholder)$state_comms[1]
      }
      
      text
    }
  )
  
  ################### Readiness #############
  
  output$regions_readiness <- renderUI({
    
    selectInput("regions_readiness", "Select a region : ", c('All', na.omit(regions)))
    
  })
  
  output$states_readiness <- renderUI({
    
    if (input$regions_readiness == 'All') {
      
      selectInput("states_readiness", "Select a state : ", c('All', na.omit(states)))
      
    } else {
      
      selectInput("states_readiness", "Select a state : ", c('All', unique(na.omit(subset(region_state_lga, region == input$regions_readiness)$state))))
      
    }
    
  })
  
  output$lgas_readiness <- renderUI({
    
    if (input$states_readiness == 'All') {
      
      selectInput("lgas_readiness", "Select a LGA : ", c('All', na.omit(lgas)))
      
    } else {
      
      selectInput("lgas_readiness", "Select a LGA : ", c('All', unique(na.omit(subset(region_state_lga, state == input$states_readiness)$lga))))
      
    }
    
  })
  
  
  output$score_policy_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- ceiling((readiness %>% summarise(ps=mean(ps)))$ps)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(ps=mean(ps)))$ps)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- ceiling((subset(readiness, state==input$states_readiness) %>% summarise(ps=mean(ps)))$ps)
      } else {
        text <- ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(ps=mean(ps)))$ps)
      }
      text
    }
  )
  
  output$score_policy_readiness_2 <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        
        text <- round(mean(state_readiness$Policy_Score, na.rm = TRUE), 0)
        
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        
        text <- round(mean(subset(state_readiness, region==input$regions_readiness)$Policy_Score, na.rm = TRUE), 0)
        
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        
        text <- round(mean(subset(state_readiness, state==input$states_readiness)$Policy_Score, na.rm = TRUE), 0)
        
      } else {
        
        text <- round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Policy_Score, na.rm = TRUE), 0)
        
      }
      text
    }
  )
  
  output$score_funding_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- ceiling((readiness %>% summarise(fs=mean(fs)))$fs)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(fs=mean(fs)))$fs)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- ceiling((subset(readiness, state==input$states_readiness) %>% summarise(fs=mean(fs)))$fs)
      } else {
        text <- ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(fs=mean(fs)))$fs)
      }
      text
    }
  )
  
  
  output$score_funding_readiness_2 <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        
        text <- round(mean(state_readiness$Funding_Score, na.rm = TRUE), 0)
        
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        
        text <- round(mean(subset(state_readiness, region==input$regions_readiness)$Funding_Score, na.rm = TRUE), 0)
        
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        
        text <- round(mean(subset(state_readiness, state==input$states_readiness)$Funding_Score, na.rm = TRUE), 0)
        
      } else {
        
        text <- round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Funding_Score, na.rm = TRUE), 0)
        
      }
      text
    }
  )
  
  output$score_infrastructure_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- ceiling((readiness %>% summarise(ins=mean(ins)))$ins)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(ins=mean(ins)))$ins)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- ceiling((subset(readiness, state==input$states_readiness) %>% summarise(ins=mean(ins)))$ins)
      } else {
        text <- ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(ins=mean(ins)))$ins)
      }
      text
    }
  )
  
  
  output$score_infrastructure_readiness_2 <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        
        text <- round(mean(state_readiness$Infrastructure_Score, na.rm = TRUE), 0)
        
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        
        text <- round(mean(subset(state_readiness, region==input$regions_readiness)$Infrastructure_Score, na.rm = TRUE), 0)
        
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        
        text <- round(mean(subset(state_readiness, state==input$states_readiness)$Infrastructure_Score, na.rm = TRUE), 0)
        
      } else {
        
        text <- round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Infrastructure_Score, na.rm = TRUE), 0)
        
      }
      text
    }
  )
  
  
  output$score_capacity_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- ceiling((readiness %>% summarise(cs=mean(cs)))$cs)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(cs=mean(cs)))$cs)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- ceiling((subset(readiness, state==input$states_readiness) %>% summarise(cs=mean(cs)))$cs)
      } else {
        text <- ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(cs=mean(cs)))$cs)
      }
      text
    }
  )
  
  
  output$score_capacity_readiness_2 <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        
        text <- round(mean(state_readiness$Capacity_Score, na.rm = TRUE), 0)
        
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        
        text <- round(mean(subset(state_readiness, region==input$regions_readiness)$Capacity_Score, na.rm = TRUE), 0)
        
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        
        text <- round(mean(subset(state_readiness, state==input$states_readiness)$Capacity_Score, na.rm = TRUE), 0)
        
      } else {
        
        text <- round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Capacity_Score, na.rm = TRUE), 0)
        
      }
      text
    }
  )
  
  output$score_data_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- ceiling((readiness %>% summarise(ds=mean(ds)))$ds)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(ds=mean(ds)))$ds)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- ceiling((subset(readiness, state==input$states_readiness) %>% summarise(ds=mean(ds)))$ds)
      } else {
        text <- ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(ds=mean(ds)))$ds)
      }
      text
    }
  )
  
  
  output$score_data_readiness_2 <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        
        text <- round(mean(state_readiness$Data_Score, na.rm = TRUE), 0)
        
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        
        text <- round(mean(subset(state_readiness, region==input$regions_readiness)$Data_Score, na.rm = TRUE), 0)
        
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        
        text <- round(mean(subset(state_readiness, state==input$states_readiness)$Data_Score, na.rm = TRUE), 0)
        
      } else {
        
        text <- round(mean(subset(state_readiness, (region==input$regions_readiness) & (state==input$states_readiness))$Data_Score, na.rm = TRUE), 0)
        
      }
      text
    }
  )
  
  output$score_policy_el_readiness <- renderUI(
    {
      el_value <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        el_value <- floor((ceiling((readiness %>% summarise(ps_els_code=mean(ps_els_code)))$ps_els_code)/3)*100)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        el_value <- floor((ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(ps_els_code=mean(ps_els_code)))$ps_els_code)/3)*100)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        el_value <- floor((ceiling((subset(readiness, state==input$states_readiness) %>% summarise(ps_els_code=mean(ps_els_code)))$ps_els_code)/3)*100)
      } else {
        el_value <- floor((ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(ps_els_code=mean(ps_els_code)))$ps_els_code)/3)*100)
      }
      
      progressBar(
        id = "el",
        value = el_value, 
        status = ifelse(el_value < 33, "danger",
                        ifelse((el_value > 33) & (el_value < 66), "warning",
                               "success"))
      )
      
    }
    
  )
  
  
  
  
  
  
  
  
  
###########score readiness
  
  
  output$data_source <- renderText({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- "Desktop Reseach, Interview & Questionnaire Responses"
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- "Desktop Reseach, Interview & Questionnaire Responses"
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Data Source`)
      
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Data Source`)
    }
    
    el_value
    
  })
  
  
  

  output$score_readiness_2 <- renderUI({

    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      
      el_value <- round(mean(state_readiness$`Electricity Law`, na.rm = TRUE) , 0)
      
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Electricity Law`, na.rm = TRUE), 0) 
      
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Electricity Law`, na.rm = TRUE), 0) 
      
    } else {
      
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Electricity Law`, na.rm = TRUE), 0) 
    }
    
 

    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
      ),
      
    )
    
  })
  
  
  
  
  
  output$score_readiness_22 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Electricity Law Text`)
      
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Electricity Law Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  
  
  
  output$score_readiness_3 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Regulatory Framework`, na.rm = TRUE) , 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Regulatory Framework`, na.rm = TRUE), 0)
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Regulatory Framework`, na.rm = TRUE), 0)
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Regulatory Framework`, na.rm = TRUE), 0)
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  output$score_readiness_33 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Regulatory Framework Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Regulatory Framework Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  output$score_readiness_4 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Electrification Plan/Strategy`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Electrification Plan/Strategy`, na.rm = TRUE), 0)
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Electrification Plan/Strategy`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Electrification Plan/Strategy`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  output$score_readiness_44 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Electrification Plan/ Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Electrification Plan/ Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  
  output$score_readiness_5 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Budget Allocation`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Budget Allocation`, na.rm = TRUE), 0)
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Budget Allocation`, na.rm = TRUE), 0)
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Budget Allocation`, na.rm = TRUE), 0)
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  output$score_readiness_55 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Budget Allocation Text`)[1]
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Budget Allocation Text`)[1]
    }
    
    el_value
    
  })
  
  
  
  
  
  
  output$score_readiness_6 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`PPP Office`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`PPP Office`, na.rm = TRUE), 0)
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`PPP Office`, na.rm = TRUE), 0)
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`PPP Office`, na.rm = TRUE), 0)
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  output$score_readiness_66 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`PPP Office Text`)[1]
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`PPP Office Text`)[1]
    }
    
    el_value
    
  })
  
  
  
  
  
  
  
  
  
  output$score_readiness_7 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`External Funding`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`External Funding`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`External Funding`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`External Funding`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  
  output$score_readiness_77 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`External Funding Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`External Funding Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  output$score_readiness_8 <- renderText({

    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(sum(infrastructure_df$`Total Estimated Demand (MW)`, na.rm = TRUE) , 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(sum(subset(infrastructure_df, region == input$regions_readiness)$`Total Estimated Demand (MW)`, na.rm = TRUE), 0)
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(sum(subset(infrastructure_df, state == input$states_readiness)$`Total Estimated Demand (MW)`, na.rm = TRUE), 0)
    } else {
      el_value <- round(sum(subset(infrastructure_df, state == input$states_readiness)$`Total Estimated Demand (MW)`, na.rm = TRUE), 0)
    }
    
    
    
    paste0(el_value, " MW")
    
  })
  
  
  
  
  output$score_readiness_9 <- renderText({
    
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(sum(infrastructure_df$`State Grid Transmission Capacity (MW)`, na.rm = TRUE) , 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(sum(subset(infrastructure_df, region == input$regions_readiness)$`State Grid Transmission Capacity (MW)`, na.rm = TRUE), 0)
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(sum(subset(infrastructure_df, state == input$states_readiness)$`State Grid Transmission Capacity (MW)`, na.rm = TRUE), 0)
    } else {
      el_value <- round(sum(subset(infrastructure_df, state == input$states_readiness)$`State Grid Transmission Capacity (MW)`, na.rm = TRUE), 0)
    }
    
    
    
    
    paste0(el_value, " MW")
    
    
  })
  
  
  
  output$score_readiness_10 <- renderText({
    
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(infrastructure_df$`Disco's Metering Rate`, na.rm = TRUE) , 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(infrastructure_df, region == input$regions_readiness)$`Disco's Metering Rate`, na.rm = TRUE), 0)
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(infrastructure_df, state == input$states_readiness)$`Disco's Metering Rate`, na.rm = TRUE), 0)
    } else {
      el_value <- round(mean(subset(infrastructure_df, state == input$states_readiness)$`Disco's Metering Rate`, na.rm = TRUE), 0)
    }
    
    
    
    paste0(el_value, " %")
    
  })
  
  
  
  
  

  
  
  
  
  output$score_readiness_11 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`% of Transmission Capacity to Household Demand`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`% of Transmission Capacity to Household Demand`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`% of Transmission Capacity to Household Demand`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`% of Transmission Capacity to Household Demand`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  output$score_readiness_111 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`% of Transmission Capacity to Household Demand Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`% of Transmission Capacity to Household Demand Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  
  
  output$score_readiness_12 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`% of Grid Connection`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`% of Grid Connection`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`% of Grid Connection`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`% of Grid Connection`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  
  output$score_readiness_122 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`% of Grid Connection Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`% of Grid Connection Text`)
    }
    
    el_value
    
  })
  
  
  
  
  output$score_readiness_13 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Metering Rate`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Metering Rate`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Metering Rate`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Metering Rate`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  output$score_readiness_133 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Metering Rate Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Metering Rate Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  output$score_readiness_14 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Stakeholder Engagement`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Stakeholder Engagement`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Stakeholder Engagement`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Stakeholder Engagement`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  output$score_readiness_144 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Stakeholder Engagement Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Stakeholder Engagement Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  output$score_readiness_15 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Pre-Development Capacity`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Pre-Development Capacity`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Pre-Development Capacity`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Pre-Development Capacity`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  output$score_readiness_155 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Pre-Development Capacity Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Pre-Development Capacity Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  
  
  
  output$score_readiness_16 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Development Capacity`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Development Capacity`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Development Capacity`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Development Capacity`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  
  output$score_readiness_166 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Development Capacity Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Development Capacity Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  output$score_readiness_17 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Operational Capacity`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Operational Capacity`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Operational Capacity`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Operational Capacity`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  
  
  output$score_readiness_177 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Operational Capacity Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Operational Capacity Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  output$score_readiness_18 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Research and Development (R&D) and Innovation Support`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Research and Development (R&D) and Innovation Support`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Research and Development (R&D) and Innovation Support`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Research and Development (R&D) and Innovation Support`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  
  
  output$score_readiness_188 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Research and Development (R&D) and Innovation Support Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Research and Development (R&D) and Innovation Support Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  output$score_readiness_19 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Dedicated Agency`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Dedicated Agency`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Dedicated Agency`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Dedicated Agency`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  output$score_readiness_199 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Dedicated Agency Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Dedicated Agency Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  output$score_readiness_20 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`Data Accessibility`, na.rm = TRUE), 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`Data Accessibility`, na.rm = TRUE), 0) 
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Data Accessibility`, na.rm = TRUE), 0) 
    } else {
      el_value <- round(mean(subset(state_readiness, state == input$states_readiness)$`Data Accessibility`, na.rm = TRUE), 0) 
    }
    
    
    
    tags$div(
      class = if (el_value == 1) { "custom-20" } 
      else if (el_value == 2) { "custom-40" } 
      else if (el_value == 3) { "custom-60" } 
      else if (el_value == 4) { "custom-80" } 
      else if (el_value == 5) { "custom-100" } 
      else { "" },
      progressBar(
        id = "el",
        value = el_value,
        total = 5,
        display_pct = FALSE
        
      ),
      
    )
    
  })
  
  
  
  
  
  
  
  
  
  output$score_readiness_200 <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- ""
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Data Accessibility Text`)
    } else {
      el_value <- as.character(subset(state_readiness, state == input$states_readiness)$`Data Accessibility Text`)
    }
    
    el_value
    
  })
  
  
  
  
  
  output$score_readiness_national_home_3 <- renderUI({
    el_value <- c()
    
      el_value <- round(mean(state_readiness$`State Score`, na.rm = TRUE) , 0)

    
    paste0(el_value, "/5")
    
    
  })
  
  
    output$score_readiness_national_home <- renderUI({
    el_value <- c()
    
      el_value <- round(mean(state_readiness$`State Score`, na.rm = TRUE) , 0)

    
    paste0(el_value, "/5")
    
    
  })
  
  
  
  output$score_readiness_national_home_text <- renderUI({
    el_value <- c()
    el_value_text <- c()
    
    el_value <- round(mean(state_readiness$`State Score`, na.rm = TRUE) , 0)
    
    if(el_value == 1){
      el_value_text = "Early Stage" 
    }else if(el_value == 2){
      el_value_text = "Developing" 
    }else if(el_value == 3){
      el_value_text = "Progressing" 
    }else if(el_value == 4){
      el_value_text = "Advanced" 
    }else if(el_value == 5){
      el_value_text = "Optimized" 
    }
    
    el_value_text
     
    
  })
  
  
  
  output$score_readiness_national <- renderUI({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`State Score`, na.rm = TRUE) , 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`State Score`, na.rm = TRUE), 0)
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- subset(state_readiness, state == input$states_readiness)$`State Score`[1]
    } else {
      el_value <- subset(state_readiness, state == input$states_readiness)$`State Score`[1]
    }
    
    el_value
    
    
  })
  
  
  
  
  output$score_readiness_national_2 <- renderText({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`State Score`, na.rm = TRUE) , 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`State Score`, na.rm = TRUE), 0)
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- subset(state_readiness, state == input$states_readiness)$`State Score`[1]
    } else {
      el_value <- subset(state_readiness, state == input$states_readiness)$`State Score`[1]
    }
    
    el_value = paste0(el_value, "/5")
    
    
  })
  
  
  
  output$score_readiness_national_home_2 <- renderText({
    el_value <- c()
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      el_value <- round(mean(state_readiness$`State Score`, na.rm = TRUE) , 0)
    } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      el_value <- round(mean(subset(state_readiness, region == input$regions_readiness)$`State Score`, na.rm = TRUE), 0)
    } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      el_value <- subset(state_readiness, state == input$states_readiness)$`State Score`[1]
    } else {
      el_value <- subset(state_readiness, state == input$states_readiness)$`State Score`[1]
    }
    
    el_value = paste0(el_value, "/5")
    
    
  })
  
  
  
  
  
  ################################################
  
  output$score_policy_el_tx_readiness <- renderText(
    {
      text <- c('')
      # if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      #   text <- paste0("Most ", (readiness %>% group_by(ps_els) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$per[1], "% of states have Bill ", (readiness %>% group_by(ps_els) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_els[1])
      # } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      #   text <- paste0("Most ", (subset(readiness, region==input$regions_readiness) %>% group_by(ps_els) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$per[1], "% of states have Bill ", (subset(readiness, region==input$regions_readiness) %>% group_by(ps_els) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_els[1])
      # } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      #   text <- paste0("Bill ", (subset(readiness, state==input$states_readiness) %>% group_by(ps_els) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_els[1])
      # } else {
      #   text <- paste0("Bill ", (subset(readiness, state==input$states_readiness) %>% group_by(ps_els) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_els[1])
      # }
      text
    }
  )
  
  output$score_policy_rf_readiness <- renderUI(
    {
      ef_value <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        ef_value <- floor((ceiling((readiness %>% summarise(ps_efs_code=mean(ps_efs_code)))$ps_efs_code)/5)*100)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        ef_value <- floor((ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(ps_efs_code=mean(ps_efs_code)))$ps_efs_code)/5)*100)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        ef_value <- floor((ceiling((subset(readiness, state==input$states_readiness) %>% summarise(ps_efs_code=mean(ps_efs_code)))$ps_efs_code)/5)*100)
      } else {
        ef_value <- floor((ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(ps_efs_code=mean(ps_efs_code)))$ps_efs_code)/5)*100)
      }
      
      progressBar(
        id = "ef",
        value = ef_value, 
        status = ifelse(ef_value < 33, "danger",
                        ifelse((ef_value > 33) & (ef_value < 66), "warning",
                               "success"))
      )
      
    }
  )
  
  output$score_policy_rf_tx_readiness <- renderText(
    {
      text <- c('')
      # if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      #   text <- paste0("Most ", (readiness %>% group_by(ps_efs) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$per[1], "% of states are ", (readiness %>% group_by(ps_efs) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_efs[1], " Agencies")
      # } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      #   text <- paste0("Most ", (subset(readiness, region==input$regions_readiness) %>% group_by(ps_efs) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$per[1], "% of states are ", (subset(readiness, region==input$regions_readiness) %>% group_by(ps_efs) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_efs[1], "Agencies")
      # } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      #   text <- paste0((subset(readiness, state==input$states_readiness) %>% group_by(ps_efs) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_efs[1], " Agencies")
      # } else {
      #   text <- paste0((subset(readiness, state==input$states_readiness) %>% group_by(ps_efs) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_efs[1], " Agencies")
      # }
      text
    }
  )
  
  output$score_policy_cs_readiness <- renderUI(
    {
      ef_value <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        ef_value <- floor((ceiling((readiness %>% summarise(ps_css_code=mean(ps_css_code)))$ps_css_code)/4)*100)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        ef_value <- floor((ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(ps_css_code=mean(ps_css_code)))$ps_css_code)/5)*100)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        ef_value <- floor((ceiling((subset(readiness, state==input$states_readiness) %>% summarise(ps_css_code=mean(ps_css_code)))$ps_css_code)/5)*100)
      } else {
        ef_value <- floor((ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(ps_css_code=mean(ps_css_code)))$ps_css_code)/5)*100)
      }
      
      progressBar(
        id = "ef",
        value = ef_value, 
        status = ifelse(ef_value < 33, "danger",
                        ifelse((ef_value > 33) & (ef_value < 66), "warning",
                               "success"))
      )
      
    }
  )
  
  output$score_policy_cs_tx_readiness <- renderText(
    {
      text <- c('')
      # if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      #   text <- paste0("Most ", (readiness %>% group_by(ps_css) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$per[1], "% of states have ", (readiness %>% group_by(ps_css) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0)) %>% arrange(desc(count)))$ps_css[1], " Operations")
      # } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      #   text <- paste0("Most ", (subset(readiness, region==input$regions_readiness) %>% group_by(ps_css) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$per[1], "% of states have ", (subset(readiness, region==input$regions_readiness) %>% group_by(ps_css) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_css[1], " Operations")
      # } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      #   text <- paste0((subset(readiness, state==input$states_readiness) %>% group_by(ps_css) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_css[1], " Operations")
      # } else {
      #   text <- paste0((subset(readiness, state==input$states_readiness) %>% group_by(ps_css) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 0))%>% arrange(desc(count)))$ps_css[1], " Operations")
      # }
      text
    }
  )
  
  output$score_policy_comms_readiness <- renderText(
    {
      text <- c('')
      # if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      #   text <- c("Select State to get comment")
      # } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      #   text <- c("Select State to get comment")
      # } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      #   text <- subset(readiness, state==input$states_readiness)$ps_comms
      # } else {
      #   text <- subset(readiness, state==input$states_readiness)$ps_comms
      # }
      text
    }
  )
  
  output$score_funding_bal_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- paste((readiness %>% summarise(average = ceiling(mean(fs_bal))))$average, "%", sep="")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- paste((subset(readiness, region==input$regions_readiness) %>% summarise(average = ceiling(mean(fs_bal))))$average, "%", sep="")
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- paste((subset(readiness, state==input$states_readiness) %>% summarise(average = ceiling(mean(fs_bal))))$average, "%", sep="")
      } else {
        text <- paste((subset(readiness, state==input$states_readiness) %>% summarise(average = ceiling(mean(fs_bal))))$average, "%", sep="")
      }
      text
    }
  )
  
  output$score_funding_pof_readiness <- renderUI(
    {
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        n <- (readiness %>% summarise(average = ceiling(mean(fs_pof_code))))$average
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        n <- (subset(readiness, region==input$regions_readiness) %>% summarise(average = ceiling(mean(fs_pof_code))))$average
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        n <- (subset(readiness, state==input$states_readiness) %>% summarise(average = ceiling(mean(fs_pof_code))))$average
      } else {
        n <- (subset(readiness, state==input$states_readiness) %>% summarise(average = ceiling(mean(fs_pof_code))))$average
      }
      lapply(1:n, function(a) {
        tags$i(class = "fa-solid fa-star fa-2x",
               style = 'color: #075D04')
      })
    }
  )
  
  output$score_funding_efg_pe_readiness <- renderUI(
    {
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        n <- (readiness %>% group_by(fs_efg_code) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_code[1]
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        n <- (subset(readiness, region==input$regions_readiness) %>% group_by(fs_efg_code) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_code[1]
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        n <- (subset(readiness, state==input$states_readiness) %>% group_by(fs_efg_code) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_code[1]
      } else {
        n <- (subset(readiness, state==input$states_readiness) %>% group_by(fs_efg_code) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_code[1]
      }
      if(n > 0){
        tags$i(class = "fa-solid fa-circle-check fa-1x",
               style = 'color: #6FD195; margin: 10px;')
      } else {
        tags$i(class = "fa-solid fa-circle-check fa-1x",
               style = 'color: #D16F6F; margin: 10px;')
      }
    }
  )
  
  output$score_funding_efg_dg_readiness <- renderUI(
    {
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        n <- (readiness %>% group_by(fs_efg_dg) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_dg[1]
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        n <- (subset(readiness, region==input$regions_readiness) %>% group_by(fs_efg_dg) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_dg[1]
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        n <- (subset(readiness, state==input$states_readiness) %>% group_by(fs_efg_dg) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_dg[1]
      } else {
        n <- (subset(readiness, state==input$states_readiness) %>% group_by(fs_efg_dg) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_dg[1]
      }
      if(n == 1){
        tags$i(class = "fa-solid fa-circle-check fa-1x",
               style = 'color: #6FD195; margin: 10px;')
      } else {
        tags$i(class = "fa-solid fa-circle-check fa-1x",
               style = 'color: #D16F6F; margin: 10px;')
      }
    }
  )
  
  output$score_funding_efg_bd_readiness <- renderUI(
    {
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        n <- (readiness %>% group_by(fs_efg_bd) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_bd[1]
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        n <- (subset(readiness, region==input$regions_readiness) %>% group_by(fs_efg_bd) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_bd[1]
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        n <- (subset(readiness, state==input$states_readiness) %>% group_by(fs_efg_bd) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_bd[1]
      } else {
        n <- (subset(readiness, state==input$states_readiness) %>% group_by(fs_efg_bd) %>% summarise(count = n()) %>% arrange(desc(count)))$fs_efg_bd[1]
      }
      if(n == 1){
        tags$i(class = "fa-solid fa-circle-check fa-1x",
               style = 'color: #6FD195; margin: 10px;')
      } else {
        tags$i(class = "fa-solid fa-circle-check fa-1x",
               style = 'color: #D16F6F; margin: 10px;')
      }
    }
  )
  
  output$score_funding_comms_readiness <- renderText(
    {
      text <- c('')
      # if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      #   text <- c("Select State to get comment")
      # } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      #   text <- c("Select State to get comment")
      # } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      #   text <- subset(readiness, state==input$states_readiness)$fs_comms
      # } else {
      #   text <- subset(readiness, state==input$states_readiness)$fs_comms
      # }
      text
    }
  )
  
  output$score_infrastructure_dmd_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- format(round(sum(readiness$demand, na.rm=T), 0), big.mark = ",")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- format(round(sum(subset(readiness, region==input$regions_readiness)$demand, na.rm=T), 0), big.mark = ",")
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- format(round(sum(subset(readiness, state==input$states_readiness)$demand, na.rm=T), 0), big.mark = ",")
      } else {
        text <- format(round(sum(subset(readiness, state==input$states_readiness)$demand, na.rm=T), 0), big.mark = ",")
      }
      
      text
    }
  )
  
  output$score_infrastructure_gc_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- format(round(sum(readiness$ins_gc, na.rm=T), 0), big.mark = ",")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- format(round(sum(subset(readiness, region==input$regions_readiness)$ins_gc, na.rm=T), 0), big.mark = ",")
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- format(round(sum(subset(readiness, state==input$states_readiness)$ins_gc, na.rm=T), 0), big.mark = ",")
      } else {
        text <- format(round(sum(subset(readiness, state==input$states_readiness)$ins_gc, na.rm=T), 0), big.mark = ",")
      }
      
      text
    }
  )
  
  output$score_infrastructure_mg_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- format(round(sum(readiness$ins_mg, na.rm=T), 0), big.mark = ",")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- format(round(sum(subset(readiness, region==input$regions_readiness)$ins_mg, na.rm=T), 0), big.mark = ",")
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- format(round(sum(subset(readiness, state==input$states_readiness)$ins_mg, na.rm=T), 0), big.mark = ",")
      } else {
        text <- format(round(sum(subset(readiness, state==input$states_readiness)$ins_mg, na.rm=T), 0), big.mark = ",")
      }
      
      text
    }
  )
  
  output$score_infrastructure_ipp_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- format(round(sum(readiness$ins_ipp, na.rm=T), 0), big.mark = ",")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- format(round(sum(subset(readiness, region==input$regions_readiness)$ins_ipp, na.rm=T), 0), big.mark = ",")
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- format(round(sum(subset(readiness, state==input$states_readiness)$ins_ipp, na.rm=T), 0), big.mark = ",")
      } else {
        text <- format(round(sum(subset(readiness, state==input$states_readiness)$ins_ipp, na.rm=T), 0), big.mark = ",")
      }
      
      text
    }
  )
  
  output$score_infrastructure_dsc_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- unique(readiness$ins_dsc)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- unique(subset(readiness, region==input$regions_readiness)$ins_dsc)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- unique(subset(readiness, state==input$states_readiness)$ins_dsc)
      } else {
        text <- unique(subset(readiness, state==input$states_readiness)$ins_dsc)
      }
      
      text
    }
  )
  
  output$score_infrastructure_cmc_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- format(round(mean(readiness$ins_cmc, na.rm=T), 2), big.mark = ",")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- format(round(sum(subset(readiness, region==input$regions_readiness)$ins_cmc, na.rm=T), 0), big.mark = ",")
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- format(round(sum(subset(readiness, state==input$states_readiness)$ins_cmc, na.rm=T), 0), big.mark = ",")
      } else {
        text <- format(round(sum(subset(readiness, state==input$states_readiness)$ins_cmc, na.rm=T), 0), big.mark = ",")
      }
      
      text
    }
  )
  
  output$score_infrastructure_comms_readiness <- renderText(
    {
      text <- c('')
      # if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      #   text <- c("Select State to get comment")
      # } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      #   text <- c("Select State to get comment")
      # } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      #   text <- subset(readiness, state==input$states_readiness)$ins_comms
      # } else {
      #   text <- subset(readiness, state==input$states_readiness)$ins_comms
      # }
      text
    }
  )
  
  output$score_capacity_tec_readiness <- renderUI(
    {
      tec_value <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        tec_value <- floor((ceiling((readiness %>% summarise(cs_tec=mean(cs_tec)))$cs_tec)/3)*100)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        tec_value <- floor((ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(cs_tec=mean(cs_tec)))$cs_tec)/5)*100)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        tec_value <- floor((ceiling((subset(readiness, state==input$states_readiness) %>% summarise(cs_tec=mean(cs_tec)))$cs_tec)/5)*100)
      } else {
        tec_value <- floor((ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(cs_tec=mean(cs_tec)))$cs_tec)/5)*100)
      }
      
      progressBar(
        id = "tec",
        value = tec_value, 
        status = ifelse(tec_value < 33, "danger",
                        ifelse((tec_value > 33) & (tec_value < 66), "warning",
                               "success"))
      )
      
    }
  )
  
  output$score_capacity_fin_readiness <- renderUI(
    {
      fin_value <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        fin_value <- floor((ceiling((readiness %>% summarise(cs_fin=mean(cs_fin)))$cs_fin)/3)*100)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        fin_value <- floor((ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(cs_fin=mean(cs_fin)))$cs_fin)/5)*100)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        fin_value <- floor((ceiling((subset(readiness, state==input$states_readiness) %>% summarise(cs_fin=mean(cs_fin)))$cs_fin)/5)*100)
      } else {
        fin_value <- floor((ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(cs_fin=mean(cs_fin)))$cs_fin)/5)*100)
      }
      
      progressBar(
        id = "fin",
        value = fin_value, 
        status = ifelse(fin_value < 33, "danger",
                        ifelse((fin_value > 33) & (fin_value < 66), "warning",
                               "success"))
      )
      
    }
  )
  
  output$score_capacity_prc_readiness <- renderUI(
    {
      prc_value <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        prc_value <- floor((ceiling((readiness %>% summarise(cs_prc=mean(cs_prc)))$cs_prc)/3)*100)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        prc_value <- floor((ceiling((subset(readiness, region==input$regions_readiness) %>% summarise(cs_prc=mean(cs_prc)))$cs_prc)/5)*100)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        prc_value <- floor((ceiling((subset(readiness, state==input$states_readiness) %>% summarise(cs_prc=mean(cs_prc)))$cs_prc)/5)*100)
      } else {
        prc_value <- floor((ceiling((subset(readiness, (region==input$regions_readiness) & (state==input$states_readiness)) %>% summarise(cs_prc=mean(cs_prc)))$cs_prc)/5)*100)
      }
      
      progressBar(
        id = "prc",
        value = prc_value, 
        status = ifelse(prc_value < 33, "danger",
                        ifelse((prc_value > 33) & (prc_value < 66), "warning",
                               "success"))
      )
      
    }
  )
  
  output$score_capacity_comms_readiness <- renderText(
    {
      text <- c('')
      # if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      #   text <- c("Select State to get comment")
      # } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      #   text <- c("Select State to get comment")
      # } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      #   text <- subset(readiness, state==input$states_readiness)$cs_comms
      # } else {
      #   text <- subset(readiness, state==input$states_readiness)$cs_comms
      # }
      text
    }
  )
  
  output$score_data_som_readiness <- renderText(
    {
      text <- c()
      n <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        n <- (readiness %>% group_by(ds_som) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_som[1]
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        n <- (subset(readiness, region==input$regions_readiness) %>% group_by(ds_som) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_som[1]
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        n <- (subset(readiness, state==input$states_readiness) %>% group_by(ds_som) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_som[1]
      } else {
        n <- (subset(readiness, state==input$states_readiness) %>% group_by(ds_som) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_som[1]
      }
      if(n == 1){
        text <- c("Yes")
        text
      } else {
        text <- c("No")
        text
      }
    }
  )
  
  output$score_data_mrd_readiness <- renderText(
    {
      text <- c()
      n <- c(1)
      # if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      #   n <- (readiness %>% group_by(ds_mrd) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_mrd[1]
      # } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      #   n <- (subset(readiness, region==input$regions_readiness) %>% group_by(ds_mrd) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_mrd[1]
      # } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      #   n <- (subset(readiness, state==input$states_readiness) %>% group_by(ds_mrd) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_mrd[1]
      # } else {
      #   n <- (subset(readiness, state==input$states_readiness) %>% group_by(ds_mrd) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_mrd[1]
      # }
      if(n == 1){
        text <- c("Yes")
        text
      } else {
        text <- c("No")
        text
      }
    }
  )
  
  output$score_data_oth_readiness <- renderText(
    {
      text <- c()
      n <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        n <- (readiness %>% group_by(ds_oth) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_oth[1]
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        n <- (subset(readiness, region==input$regions_readiness) %>% group_by(ds_oth) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_oth[1]
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        n <- (subset(readiness, state==input$states_readiness) %>% group_by(ds_oth) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_oth[1]
      } else {
        n <- (subset(readiness, state==input$states_readiness) %>% group_by(ds_oth) %>% summarise(count = n()) %>% arrange(desc(count)))$ds_oth[1]
      }
      if(n == 1){
        text <- c("Yes")
        text
      } else {
        text <- c("No")
        text
      }
    }
  )
  
  output$score_data_comms_readiness <- renderText(
    {
      text <- c('')
      # if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      #   text <- c("Select State to get comment")
      # } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
      #   text <- c("Select State to get comment")
      # } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
      #   text <- subset(readiness, state==input$states_readiness)$ds_comms
      # } else {
      #   text <- subset(readiness, state==input$states_readiness)$ds_comms
      # }
      text
    }
  )
  
  # output$map_readiness <- renderUI(
  #   {
  #     if((input$regions_readiness == "All") & (input$states_readiness == "All")){
  #       img(src="images/maps/nation.png", width="100%")
  #     } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
  #       img(src=paste0("images/maps/region/", input$regions_readiness, ".png", sep=""), width="100%")
  #     } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
  #       img(src=paste0("images/maps/state/", input$states_readiness, ".png", sep=""), width="100%")
  #     } else {
  #       img(src=paste0("images/maps/state/", input$states_readiness, ".png", sep=""), width="100%")
  #     }
  #   }
  # )
  # 
  # 
  
  # output$map_readiness <- renderUI({
  #   metric <- input$ranking_metric
  #   region <- input$regions_readiness
  #   state <- input$states_readiness
  #   
  #   # Construct metric-specific image folder
  #   metric_folder <- if (metric == "All") "default" else tolower(metric)
  #   
  #   if (region == "All" & state == "All") {
  #     img(src = paste0("images/maps/", metric_folder, "/nation.png"), width = "100%")
  #   } else if (region != "All" & state == "All") {
  #     img(src = paste0("images/maps/", metric_folder, "/region/", region, ".png"), width = "100%")
  #   } else {
  #     img(src = paste0("images/maps/", metric_folder, "/state/", state, ".png"), width = "100%")
  #   }
  # })
  # 
  
  
  # output$map_readiness <- renderUI({
  #   metric <- input$ranking_metric
  #   region <- input$regions_readiness
  #   state <- input$states_readiness
  #   
  #   if (region == "All" & state == "All") {
  #     # Metric-specific national map
  #     metric_folder <- if (metric == "All") "default" else tolower(metric)
  #     img(src = paste0("images/maps/", metric_folder, "/nation.png"), width = "100%")
  #     
  #   } else if (region != "All" & state == "All") {
  #     # Always use region map from default folder
  #     img(src = paste0("images/maps/region/", region, ".png"), width = "100%")
  #     
  #   } else {
  #     # Always use state map from default folder
  #     img(src = paste0("images/maps/state/", state, ".png"), width = "100%")
  #   }
  # })
  # 
  
  
  output$map_readiness <- renderUI({
    metric <- input$ranking_metric
    region <- input$regions_readiness
    state <- input$states_readiness
    
    if (region == "All" & state == "All") {
      # Use metric-specific national map
      metric_folder <- if (metric == "All") "default" else tolower(metric)
      img(src = paste0("images/maps/", metric_folder, "/nation.png"), width = "100%")
      
    } else if (region != "All" & state == "All") {
      # Region maps are always in the default folder
      img(src = paste0("images/maps/default/region/", region, ".png"), width = "100%")
      
    } else {
      # State maps are always in the default folder
      img(src = paste0("images/maps/default/state/", state, ".png"), width = "100%")
    }
  })
  
  
  output$map_info_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- c("National Info")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- c("Regional Info")
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- c("State Info")
      } else {
        text <- c("State Info")
      }
      
      text
    }
  )
  
  output$map_area_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- paste(format((region_state %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- paste(format((subset(region_state, region==input$regions_readiness) %>% group_by(region) %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- paste(format((subset(region_state, state==input$states_readiness) %>% group_by(state) %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      } else {
        text <- paste(format((subset(region_state, state==input$states_readiness) %>% group_by(state) %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      }
      
      text
    }
  )
  
  output$map_population_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- paste(round((region_state %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- paste(round((subset(region_state, region==input$regions_readiness) %>% group_by(region) %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- paste(round((subset(region_state, state==input$states_readiness) %>% group_by(state) %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      } else {
        text <- paste(round((subset(region_state, state==input$states_readiness) %>% group_by(state) %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      }
      
      text
    }
  )
  
  output$map_rank_readiness <- renderUI(
    {
      n <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        n <- ceiling(mean(mean(readiness$ps), mean(readiness$fs), mean(readiness$ins), mean(readiness$cs), mean(readiness$ds)))
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        n <- ceiling(mean(mean(subset(readiness, region==input$regions_readiness)$ps), mean(subset(readiness, region==input$regions_readiness)$fs), mean(subset(readiness, region==input$regions_readiness)$ins), mean(subset(readiness, region==input$regions_readiness)$cs), mean(subset(readiness, region==input$regions_readiness)$ds)))
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        n <- ceiling(mean(mean(subset(readiness, state==input$states_readiness)$ps), mean(subset(readiness, state==input$states_readiness)$fs), mean(subset(readiness, state==input$states_readiness)$ins), mean(subset(readiness, state==input$states_readiness)$cs), mean(subset(readiness, state==input$states_readiness)$ds)))
      } else {
        n <- ceiling(mean(mean(subset(readiness, state==input$states_readiness)$ps), mean(subset(readiness, state==input$states_readiness)$fs), mean(subset(readiness, state==input$states_readiness)$ins), mean(subset(readiness, state==input$states_readiness)$cs), mean(subset(readiness, state==input$states_readiness)$ds)))
      }
      
      # lapply(1:n, function(a) {
      #   tags$i(class = "fa-solid fa-star fa-1x",
      #          style = 'color: #075D04')
      # })
      n
    }
  )
  
  # output$map_register_readiness <- renderText(
  #   {
  #     text <- c()
  #     if((input$regions_readiness == "All") & (input$states_readiness == "All")){
  #       text <- nrow(register)
  #     } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
  #       text <- nrow(subset(register, region==input$regions_readiness))
  #     } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
  #       text <- nrow(subset(register, state==input$states_readiness))
  #     } else {
  #       text <-   nrow(subset(register, state==input$states_readiness))
  #     }
  #     
  #     text
  #   }
  # )
  # 
  
  output$map_register_readiness <- renderText({
    # Explode state column into individual rows per state
    expanded_register <- register %>%
      mutate(state = str_split(state, ",\\s*")) %>%
      tidyr::unnest(state) %>%
      mutate(state = str_trim(state))
    
    filtered_data <- expanded_register
    
    if((input$regions_readiness == "All") & (input$states_readiness == "All")){
      filtered_data <- register
    }
    # Apply filters
    else if ((input$regions_readiness != "All") & (input$states_readiness == "All")) {
      filtered_data <- filtered_data %>% filter(region == input$regions_readiness)
    }
    else if ((input$regions_readiness == "All") & (input$states_readiness != "All")) {
      filtered_data <- filtered_data %>% filter(state == input$states_readiness)
    }
    else{
      filtered_data <- filtered_data %>% filter(state == input$states_readiness)
    }
    
     nrow(filtered_data)
  })
  
  
  output$map_tier_readiness <- renderText(
    {
      
      # Convert Tier_avail_cap to numeric values
      access_tier$Tier_num <- as.numeric(gsub("Tier ", "", access_tier$Tier_avail_cap))
      
      
      
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- mean(access_tier$Tier_num, na.rm = TRUE)
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- mean(subset(access_tier, region==input$regions_readiness)$Tier_num, na.rm = TRUE)
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- mean(subset(access_tier, state==input$states_readiness)$Tier_num, na.rm = TRUE)
      } else {
        text <- mean(subset(access_tier, state==input$states_readiness)$Tier_num, na.rm = TRUE)
      }
      
      ea_state_readiness <- paste("Tier ", round(text, 0))
      
      ea_state_readiness
    }
  )
  
  output$map_region_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- paste(c("North central", "North east", "North west", "South west", "South east", "South south"), collapse=", ")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <-  c(levels(as.factor(as.character(subset(state_readiness, region == input$regions_readiness)$`region`)))) 
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- c(levels(as.factor(as.character(subset(state_readiness, state == input$states_readiness)$`region`))))
      } else {
        text <- c(levels(as.factor(as.character(subset(state_readiness, state == input$states_readiness)$`region`)))) 
       
      }
      
      text
    }
  )
  
  
  output$map_summary_readiness <- renderText(
    {
      text <- c()
      
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        
        text <- paste(c(""), collapse=", ")
        
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        
        text <-  c(levels(as.factor(as.character(subset(state_readiness, region == input$regions_readiness)$`reg_summary`)))) 
        
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        
        text <-  subset(state_readiness, state == input$states_readiness)$`State_summary`
        
      } else {
        
        text <- subset(state_readiness, state == input$states_readiness)$`State_summary`
        
      }
      
      text
    }
  )
  
  
  
  
  output$map_summary_readiness_2A <- renderText(
    {

      
      
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        
        text <- ""
        
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        
        text <-  c(levels(as.factor(as.character(subset(state_readiness, region == input$regions_readiness)$`reg_summary`)))) 
        
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        
        text <-  as.character(subset(state_readiness, state == input$states_readiness)$`State_summary`)
        
      } else {
        
        text <-  as.character(subset(state_readiness, state == input$states_readiness)$`State_summary`)
        
      }
      
      clean_text <- function(text) {
        text <- iconv(text, to = "UTF-8", sub = "byte")  # Ensure proper encoding
        text <- gsub("[^[:print:]]", "", text)           # Remove non-printable characters
        text <- gsub("\\s+", " ", text)                  # Normalize whitespace
        trimws(text)
      }
      
      clean_text
    }
  )
  
  
  output$map_state_readiness <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- c("36 & FCT/Abuja")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        text <- paste(subset(region_state, region == input$regions_readiness)$state, collapse = ", ")
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- input$states_readiness
      } else {
        text <- input$states_readiness
      }
      
      text
    }
  )
  
  output$map_summary_readiness___ <- renderText(
    {
      text <- c()
      if((input$regions_readiness == "All") & (input$states_readiness == "All")){
        text <- c("Nigeria has about 60% electricity access, with rural areas facing the greatest shortages. The country’s power generation capacity is 12,500 MW, but actual supply is often lower due to infrastructure and fuel issues. While the government and private sector focus on solar and off-grid solutions, challenges remain with grid reliability, infrastructure, and funding. Renewable energy potential, especially solar, is high, but rural electrification and consistent power supply are still major hurdles.")
      } else if((input$regions_readiness != "All") & (input$states_readiness == "All")){
        if(input$regions_readiness == "north_east"){
          text <- c("This region has the lowest electrification rates due to security challenges, weak grid infrastructure, and limited investments. Many rural communities lack access to the national grid and rely on expensive diesel generators. Solar mini-grids and standalone systems are gaining traction as alternative solutions. Government and donor-funded projects aim to expand energy access, but progress remains slow due to security risks.")
        } else if(input$regions_readiness == "north_west"){
          text <- c("Grid electricity access is expanding in major cities, but rural communities still face significant gaps. Many households depend on small solar home systems and mini-grids. Power infrastructure is improving, but vandalism and weak distribution networks limit reliability. The region has high solar potential, making off-grid electrification a promising alternative. Affordability remains a barrier for low-income communities.")
        } else if(input$regions_readiness == "north_west"){
          text <- c("Electrification is moderate, with urban centers enjoying better access than rural areas. The region hosts major hydropower plants like Kainji and Shiroro, but transmission bottlenecks limit efficiency. Rural communities still rely on diesel generators and standalone solar solutions. Distribution infrastructure is expanding, but affordability remains a challenge. Efforts to integrate renewable energy solutions are growing, but implementation is slow.")
        } else if(input$regions_readiness == "south_west"){
          text <- c("The region has the highest electrification rate in Nigeria, with a well-developed grid network. Lagos, the economic hub, has relatively better access, but demand still surpasses supply, leading to widespread generator use. Rural electrification is expanding, driven by private sector investments in off-grid solutions. Industrial and commercial sectors rely heavily on backup power due to grid instability. Renewable energy projects, particularly solar, are gaining traction to supplement existing capacity.")
        } else if(input$regions_readiness == "south_east"){
          text <- c("Urban areas have high grid access, but rural electrification lags behind. Many communities depend on diesel generators, while mini-grids and solar home systems are gradually emerging. Power supply is unreliable, with frequent blackouts affecting businesses and households. Economic activities, particularly in manufacturing and trade, are constrained by unstable electricity. Distributed renewable energy solutions are being explored to bridge the gap.")
        } else {
          text <- c("Despite strong grid infrastructure, electricity supply remains unreliable due to aging transmission lines and gas supply constraints. The region hosts several power plants but suffers from systemic inefficiencies. Rural electrification is improving, with growing adoption of solar mini-grids. Flood-prone areas face additional challenges in maintaining infrastructure. Efforts to strengthen grid reliability and integrate renewable energy sources are ongoing.")
        }
      } else if((input$regions_readiness == "All") & (input$states_readiness != "All")){
        text <- subset(readiness, state==input$states_readiness)$state_comms[1]
      } else {
        text <- subset(readiness, state==input$states_readiness)$state_comms[1]
      }
      
      text
    }
  )
  
  ###################### Energy Access ########
  
  output$regions_access <- renderUI({
    
    selectInput("regions_access", "Select a region : ", c('All', na.omit(regions)))
    
  })
  
  output$states_access <- renderUI({
    
    if (input$regions_access == 'All') {
      
      selectInput("states_access", "Select a state : ", c('All', na.omit(states)))
      
    } else {
      
      selectInput("states_access", "Select a state : ", c('All', unique(na.omit(subset(region_state_lga, region == input$regions_access)$state))))
      
    }
    
  })
  
  output$lgas_access <- renderUI({
    
    if (input$states_access == 'All') {
      
      selectInput("lgas_access", "Select a LGA : ", c('All', na.omit(lgas)))
      
    } else {
      
      selectInput("lgas_access", "Select a LGA : ", c('All', unique(na.omit(subset(region_state_lga, state == input$states_access)$lga))))
      
    }
    
  })
  
  #output$access_dist_access <- renderPlot( 
  #  { 
  #   ggplot(access_df, aes(category, value, fill=category)) + 
  #     geom_bar(stat = "identity") +
  #     scale_fill_manual(values = c("Tier 0" = "#C2251C",
  #                                  "Tier 1" = "#F47A1F",
  ##                                  "Tier 2" = "#FAA73B",
  ##                                  "Tier 3" = "#FFDB67",
  #                                 "Tier 4" = "#8AC366",
  #                                 "Tier 5" = "#01634F")) +
  #     geom_text(
  #      aes(label = per, y = value + 0.05),
  #      position = position_dodge(0.9),
  #      vjust = 0
  #     ) +
  #      scale_x_discrete(name="") +
  #    scale_y_continuous(name="") +
  #    theme_bw() +
  #    theme(legend.position = "none",
  #         axis.text.x = element_text(face="bold", colour="#075D04", size=16),
  #        panel.grid.major = element_blank(),
  ###         panel.grid.minor = element_blank(),
  #          panel.border = element_blank(),
  #          panel.background = element_blank(),
  #         axis.text.y = element_blank(),
  #          axis.ticks.y = element_blank()) 
  # } 
  #  )
  # 
  # output$access_dist_access_2 <- renderPlot(
  #   {
  #     if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
  #       
  #       access_tier_access <- access_tier %>% group_by(Tier_all_metrics) %>% summarise(count = n()) %>% mutate(value = (count / sum(count)) * 100, per = paste0(round(value, 1), "%"))
  #       
  #     } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
  #       access_tier_access <- subset(access_tier, region==input$regions_access) %>% group_by(Tier_all_metrics) %>% summarise(count = n()) %>% mutate(value = (count / sum(count)) * 100, per = paste0(round(value, 1), "%"))
  #       
  #       #access_tier_access <- subset(access_tier, region==input$regions_access) %>% group_by(tier) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
  #     } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
  #       access_tier_access <- subset(access_tier, state==input$states_access) %>% group_by(Tier_all_metrics) %>% summarise(count = n()) %>% mutate(value = (count / sum(count)) * 100, per = paste0(round(value, 1), "%"))
  #       
  #       #access_tier_access <- subset(access_tier, state==input$states_access) %>% group_by(tier) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
  #     } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
  #       access_tier_access <- subset(access_tier, state==input$states_access) %>% group_by(Tier_all_metrics) %>% summarise(count = n()) %>% mutate(value = (count / sum(count)) * 100, per = paste0(round(value, 1), "%"))
  #       
  #       #access_tier_access <- subset(access_tier, state==input$states_access) %>% group_by(tier) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
  #     } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
  #       access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(Tier_all_metrics) %>% summarise(count = n()) %>% mutate(value = (count / sum(count)) * 100, per = paste0(round(value, 1), "%"))
  #       
  #       #access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(tier) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
  #     } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
  #       access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(Tier_all_metrics) %>% summarise(count = n()) %>% mutate(value = (count / sum(count)) * 100, per = paste0(round(value, 1), "%"))
  #       
  #       #access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(tier) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
  #     } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
  #       access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(Tier_all_metrics) %>% summarise(count = n()) %>% mutate(value = (count / sum(count)) * 100, per = paste0(round(value, 1), "%"))
  #       
  #       #access_tier_access <- subset(access_tier, lga=="Ado") %>% group_by(tier) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
  #     } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
  #       access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(Tier_all_metrics) %>% summarise(count = n()) %>% mutate(value = (count / sum(count)) * 100, per = paste0(round(value, 1), "%"))
  #       
  #       #access_tier_access <- subset(access_tier, lga=="Ado") %>% group_by(tier) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
  #     } else {
  #       access_tier_access <- subset(access_tier, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(tier) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
  #     }
  #     
  #     ggplot(access_tier_access, aes(tier, value, fill=tier)) + 
  #       geom_bar(stat = "identity") +
  #       scale_fill_manual(values = c("Tier 0" = "#C2251C",
  #                                    "Tier 1" = "#F47A1F",
  #                                    "Tier 2" = "#FAA73B",
  #                                    "Tier 3" = "#FFDB67",
  #                                    "Tier 4" = "#8AC366",
  #                                    "Tier 5" = "#01634F")) +
  #       geom_text(
  #         aes(label = per, y = value + 0.05),
  #         position = position_dodge(0.9),
  #         vjust = 0
  #       ) +
  #       scale_x_discrete(name="") +
  #       scale_y_continuous(name="") +
  #       theme_bw() +
  #       theme(legend.position = "none",
  #             axis.text.x = element_text(face="bold", colour="#075D04", size=16),
  #             panel.grid.major = element_blank(),
  #             panel.grid.minor = element_blank(),
  #             panel.border = element_blank(),
  #             panel.background = element_blank(),
  #             axis.text.y = element_blank(),
  #             axis.ticks.y = element_blank()) 
  #   }
  # )
  # 
  output$access_dist_access <- renderPlot(
    {
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        access_tier_access <- access_tier %>%
          group_by(Tier_all_metrics) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        access_tier_access <- subset(access_tier, region==input$regions_access) %>%
          group_by(Tier_all_metrics) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, region==input$regions_access) %>% group_by(Tier_all_metrics) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        access_tier_access <- subset(access_tier, state==input$states_access) %>%
          group_by(Tier_all_metrics) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, state==input$states_access) %>% group_by(Tier_all_metrics) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        access_tier_access <- subset(access_tier, state==input$states_access) %>%
          group_by(Tier_all_metrics) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, state==input$states_access) %>% group_by(Tier_all_metrics) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        access_tier_access <- subset(access_tier, lga==input$lgas_access) %>%
          group_by(Tier_all_metrics) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(Tier_all_metrics) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        access_tier_access <- subset(access_tier, lga==input$lgas_access) %>%
          group_by(Tier_all_metrics) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(Tier_all_metrics) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        access_tier_access <- subset(access_tier, lga==input$lgas_access) %>%
          group_by(Tier_all_metrics) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, lga=="Ado") %>% group_by(Tier_all_metrics) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        access_tier_access <- subset(access_tier, lga==input$lgas_access) %>%
          group_by(Tier_all_metrics) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(Tier_all_metrics) %>% summarise(count = n()) %>% mutate(value = (count / sum(count)) * 100, per = paste0(round(value, 1), "%"))
      }else {
        access_tier_access <- access_tier %>%
          group_by(Tier_all_metrics) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(tier) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      }
      
      # Define all possible tiers (Tier 0 to Tier 5)
      required_tiers <- paste0("Tier ", 0:5)
      
      # Ensure all tiers (0-5) are present, filling missing ones with 0
      access_tier_access <- access_tier_access %>%
        complete(Tier_all_metrics = required_tiers, fill = list(count = 0, value = 0)) %>%
        mutate(per = paste0(round(value, 1), "%"))  # Format percentage column
      
      
      ggplot(access_tier_access, aes(Tier_all_metrics, value, fill=Tier_all_metrics)) + 
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("Tier 0" = "#C2251C",
                                     "Tier 1" = "#F47A1F",
                                     "Tier 2" = "#FAA73B",
                                     "Tier 3" = "#FFDB67",
                                     "Tier 4" = "#8AC366",
                                     "Tier 5" = "#01634F")) +
        geom_text(
          aes(label = per, y = value + 0.05),
          position = position_dodge(0.9),
          vjust = 0
        ) +
        scale_x_discrete(name="") +
        scale_y_continuous(name="") +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(face="bold", colour="#075D04", size=16),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) 
    }
  )
  
  output$access_dist_access_2 <- renderPlot(
    {
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        access_tier_access <- access_tier %>%
          group_by(Tier_avail_cap) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        access_tier_access <- subset(access_tier, region==input$regions_access) %>%
          group_by(Tier_avail_cap) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, region==input$regions_access) %>% group_by(Tier_avail_cap) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        access_tier_access <- subset(access_tier, state==input$states_access) %>%
          group_by(Tier_avail_cap) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, state==input$states_access) %>% group_by(Tier_avail_cap) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        access_tier_access <- subset(access_tier, state==input$states_access) %>%
          group_by(Tier_avail_cap) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, state==input$states_access) %>% group_by(Tier_avail_cap) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        access_tier_access <- subset(access_tier, lga==input$lgas_access) %>%
          group_by(Tier_avail_cap) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(Tier_avail_cap) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        access_tier_access <- subset(access_tier, lga==input$lgas_access) %>%
          group_by(Tier_avail_cap) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(Tier_avail_cap) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        access_tier_access <- subset(access_tier, lga==input$lgas_access) %>%
          group_by(Tier_avail_cap) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, lga=="Ado") %>% group_by(Tier_avail_cap) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        access_tier_access <- subset(access_tier, lga==input$lgas_access) %>%
          group_by(Tier_avail_cap) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, lga==input$lgas_access) %>% group_by(Tier_avail_cap) %>% summarise(count = n()) %>% mutate(value = (count / sum(count)) * 100, per = paste0(round(value, 1), "%"))
      }else {
        access_tier_access <- access_tier %>%
          group_by(Tier_avail_cap) %>%
          summarise(count = n(), .groups = "drop") %>%  # Count occurrences of each tier
          mutate(value = (count / sum(count)) * 100,
                 per = paste0(round(value, 1), "%"))
        
        #access_tier_access <- subset(access_tier, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(tier) %>% summarise(count = sum(count, na.rm=T)) %>% mutate(value = (count/sum(count))*100, per = paste(round(value, 1), "%", sep=""))
      }
      
      # Define all possible tiers (Tier 0 to Tier 5)
      required_tiers <- paste0("Tier ", 0:5)
      
      # Ensure all tiers (0-5) are present, filling missing ones with 0
      access_tier_access <- access_tier_access %>%
        complete(Tier_avail_cap = required_tiers, fill = list(count = 0, value = 0)) %>%
        mutate(per = paste0(round(value, 1), "%"))  # Format percentage column
      
      
      ggplot(access_tier_access, aes(Tier_avail_cap, value, fill=Tier_avail_cap)) + 
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("Tier 0" = "#C2251C",
                                     "Tier 1" = "#F47A1F",
                                     "Tier 2" = "#FAA73B",
                                     "Tier 3" = "#FFDB67",
                                     "Tier 4" = "#8AC366",
                                     "Tier 5" = "#01634F")) +
        geom_text(
          aes(label = per, y = value + 0.05),
          position = position_dodge(0.9),
          vjust = 0
        ) +
        scale_x_discrete(name="") +
        scale_y_continuous(name="") +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(face="bold", colour="#075D04", size=16),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) 
    }
  )
  
  output$access_esp_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(subset(access %>% group_by(esp) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), esp=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, region==input$regions_access) %>% group_by(esp) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), esp=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(esp) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), esp=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(esp) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), esp=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(esp) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), esp=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(esp) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), esp=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(esp) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), esp=="TRUE")$per, "%", sep="")
      } else {
        text <- paste(subset(subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(esp) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), esp=="TRUE")$per, "%", sep="")
      }
      text
    }
  )
  
  output$access_ecp_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(round((access %>% summarise(average = median(Daily_Capacity_in_Wh)))$average/1000, 2), "kWh", sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(round((subset(access, region==input$regions_access) %>% summarise(average = median(Daily_Capacity_in_Wh)))$average/1000, 2), "kWh", sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(round((subset(access, state==input$states_access) %>% summarise(average = median(Daily_Capacity_in_Wh)))$average/1000, 2), "kWh", sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(round((subset(access, state==input$states_access) %>% summarise(average = median(Daily_Capacity_in_Wh)))$average/1000, 2), "kWh", sep=" ")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(Daily_Capacity_in_Wh)))$average/1000, 2), "kWh", sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(Daily_Capacity_in_Wh)))$average/1000, 2), "kWh", sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(Daily_Capacity_in_Wh)))$average/1000, 2), "kWh", sep=" ")
      } else {
        text <- paste(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(Daily_Capacity_in_Wh)))$average/1000, 2), "kWh", sep=" ")
      }
      text
    }
  )
  
  output$access_tes_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((access %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else {
        text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      }
      text
    }
  )
  
  
  output$access_tes_access_1 <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((access %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else {
        text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(energy_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      }
      text
    }
  )
  
  output$access_wdr_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(format(round((access %>% summarise(average = median(weekly_disrupt, na.rm=T)))$average, 0), big.mark = ","), "Hrs", sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(format(round((subset(access, region==input$regions_access) %>% summarise(average = median(weekly_disrupt, na.rm=T)))$average, 0), big.mark = ","), "Hrs", sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(format(round((subset(access, state==input$states_access) %>% summarise(average = median(weekly_disrupt, na.rm=T)))$average, 0), big.mark = ","), "Hrs", sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(format(round((subset(access, state==input$states_access) %>% summarise(average = median(weekly_disrupt, na.rm=T)))$average, 0), big.mark = ","), "Hrs", sep=" ")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(weekly_disrupt, na.rm=T)))$average, 0), big.mark = ","), "Hrs", sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste(format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(weekly_disrupt, na.rm=T)))$average, 0), big.mark = ","), "Hrs", sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(weekly_disrupt, na.rm=T)))$average, 0), big.mark = ","), "Hrs", sep=" ")
      } else {
        text <- paste(format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(weekly_disrupt, na.rm=T)))$average, 0), big.mark = ","), "Hrs", sep=" ")
      }
      text
    }
  )
  
  output$access_hhs_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- (access %>% group_by(hhs) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$hhs[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- (subset(access, region==input$regions_access) %>% group_by(hhs) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$hhs[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- (subset(access, state==input$states_access) %>% group_by(hhs) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$hhs[1]
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- (subset(access, state==input$states_access) %>% group_by(hhs) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$hhs[1]
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(hhs) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$hhs[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(hhs) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$hhs[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(hhs) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$hhs[1]
      } else {
        text <- (subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(hhs) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$hhs[1]
      }
      text
    }
  )
  
  output$access_how_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(subset(access %>% group_by(how) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), how=="Owned")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, region==input$regions_access) %>% group_by(how) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), how=="Owned")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(how) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), how=="Owned")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(how) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), how=="Owned")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(how) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), how=="Owned")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(how) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), how=="Owned")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(how) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), how=="Owned")$per, "%", sep="")
      } else {
        text <- paste(subset(subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(how) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), how=="Owned")$per, "%", sep="")
      }
      text
    }
  )
  
  output$access_occup_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- (access %>% group_by(occupation) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$occupation[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- (subset(access, region==input$regions_access) %>% group_by(occupation) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$occupation[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- (subset(access, state==input$states_access) %>% group_by(occupation) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$occupation[1]
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- (subset(access, state==input$states_access) %>% group_by(occupation) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$occupation[1]
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(occupation) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$occupation[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(occupation) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$occupation[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(occupation) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$occupation[1]
      } else {
        text <- (subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(occupation) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$occupation[1]
      }
      text
    }
  )
  
  output$access_hbr_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(subset(access %>% group_by(hbr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), hbr=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, region==input$regions_access) %>% group_by(hbr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), hbr=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(hbr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), hbr=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(hbr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), hbr=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(hbr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), hbr=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(hbr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), hbr=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(hbr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), hbr=="TRUE")$per, "%", sep="")
      } else {
        text <- paste(subset(subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(hbr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), hbr=="TRUE")$per, "%", sep="")
      }
      text
    }
  )
  
  output$access_cfl_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- (access %>% group_by(cfl) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$cfl[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- (subset(access, region==input$regions_access) %>% group_by(cfl) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$cfl[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- (subset(access, state==input$states_access) %>% group_by(cfl) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$cfl[1]
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- (subset(access, state==input$states_access) %>% group_by(cfl) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$cfl[1]
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(cfl) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$cfl[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(cfl) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$cfl[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(cfl) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$cfl[1]
      } else {
        text <- (subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(cfl) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$cfl[1]
      }
      text
    }
  )
  
  output$access_mot_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- (access %>% group_by(mot) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$mot[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- (subset(access, region==input$regions_access) %>% group_by(mot) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$mot[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- (subset(access, state==input$states_access) %>% group_by(mot) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$mot[1]
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- (subset(access, state==input$states_access) %>% group_by(mot) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$mot[1]
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(mot) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$mot[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(mot) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$mot[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- (subset(access, lga==input$lgas_access) %>% group_by(mot) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$mot[1]
      } else {
        text <- (subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(mot) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$mot[1]
      }
      text
    }
  )
  
  output$inc_rev_access <- renderPlot( 
    {
      # access_rev <- access[access$emi != "bad" & access$brv != "bad", ]

      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        inc_rev <- na.omit(gather(subset(access, select = c(region, state, lga, emi, brv)), type, amount, emi:brv)) %>% group_by(type, amount) %>% summarise(count = n())
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        inc_rev <- na.omit(gather(subset(subset(access, region==input$regions_access), select = c(region, state, lga, emi, brv)), type, amount, emi:brv)) %>% group_by(type, amount) %>% summarise(count = n())
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        inc_rev <- na.omit(gather(subset(subset(access, state==input$states_access), select = c(region, state, lga, emi, brv)), type, amount, emi:brv)) %>% group_by(type, amount) %>% summarise(count = n())
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        inc_rev <- na.omit(gather(subset(subset(access, state==input$states_access), select = c(region, state, lga, emi, brv)), type, amount, emi:brv)) %>% group_by(type, amount) %>% summarise(count = n())
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        inc_rev <- na.omit(gather(subset(subset(access, lga==input$lgas_access), select = c(region, state, lga, emi, brv)), type, amount, emi:brv)) %>% group_by(type, amount) %>% summarise(count = n())
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        inc_rev <- na.omit(gather(subset(subset(access, lga==input$lgas_access), select = c(region, state, lga, emi, brv)), type, amount, emi:brv)) %>% group_by(type, amount) %>% summarise(count = n())
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        inc_rev <- na.omit(gather(subset(subset(access, lga==input$lgas_access), select = c(region, state, lga, emi, brv)), type, amount, emi:brv)) %>% group_by(type, amount) %>% summarise(count = n())
      } else {
        inc_rev <- na.omit(gather(subset(subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)), select = c(region, state, lga, emi, brv)), type, amount, emi:brv)) %>% group_by(type, amount) %>% summarise(count = n())
      }
      
      ggplot(inc_rev, aes(factor(amount, levels=c("< N50,000", "N50,001 - N100,000", 
                                                  "N100,001 - N250,000", "N250,001 - N500,000", 
                                                  "N500,001 - N1,000,000", "N1,000,001 - N5,000,000", 
                                                  "N5,000,001 - N10,000,000", "N10,000,001 - N50,000,000")), count, fill=type)) + 
        geom_bar(stat = "identity") +
        scale_x_discrete(name="") +
        scale_y_continuous(name="") +
        scale_fill_manual(values = c("emi" = "#6FD195", "brv" = "#FFAE4C")) +
        theme_bw() +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)  # Rotates x-axis labels
        )
      
    }
  ) 
  
  
  
  output$perc_inc_ener_spe <- renderPlot( 
    {

      if((input$regions_access == "All") & (input$states_access == "All")){
        income_data <- access_tier
      } else if((input$regions_access != "All") & (input$states_access == "All")){
        income_data <- subset(access_tier, region==input$regions_access)
      } else if((input$regions_access == "All") & (input$states_access != "All")){
        income_data <- subset(access_tier, state==input$states_access)
      } else if((input$regions_access != "All") & (input$states_access != "All")){
        income_data <- subset(access_tier, state==input$states_access)
      } else {
        income_data <- subset(access_tier, state==input$states_access)
      }
 
      
      
      #    # Step 2: Filter out NA or 0 percent spend
      #   filtered_data <- income_data %>%
      #   filter(!is.na(Percentage_income_spend_on_energy) & Percentage_income_spend_on_energy != 0)
      # 
      # # Step 3: Group by income distribution and calculate median % spend
      # grouped_data <- filtered_data %>%
      #   group_by(Household_income_dist) %>%
      #   summarise(median_percent_spend = median(Percentage_income_spend_on_energy)) %>%
      #   arrange(Household_income_dist)
      # 
      # # Step 4: Plot the bar chart
      # ggplot(grouped_data, aes(x = Household_income_dist, y = median_percent_spend)) +
      #   geom_bar(stat = "identity", fill = "#238b45") +
      #   labs(
      #     title = "",
      #     x = "Household Income Distribution",
      #     y = "Median % Income Spend"
      #   ) +
      #   theme_minimal() +
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
      # 
      
      
      # Filter out NA and 0
      filtered_data <- income_data %>%
        filter(!is.na(Percentage_income_spend_on_energy) & Percentage_income_spend_on_energy != 0)
      
      
      
      # Median % spend per income group
      grouped_data <- filtered_data %>%
        group_by(Household_income_dist) %>%
        summarise(median_percent_spend = median(Percentage_income_spend_on_energy)) %>%
        arrange(Household_income_dist)
      
      
      grouped_data <- grouped_data %>%
        arrange(desc(median_percent_spend)) %>%
        mutate(Household_income_dist = factor(Household_income_dist, levels = Household_income_dist))
      
      # Plot
      ggplot(grouped_data, aes(x = Household_income_dist, y = median_percent_spend)) +
        geom_bar(stat = "identity", fill = "#238b45", width = 0.6) +
        geom_text(
          aes(label = paste0(median_percent_spend, "%")),
          vjust = -0.5, size = 4.5
        ) +
        labs(
          title = "",
          x = "Household Income Distribution",
          y = NULL
        ) +
        theme_minimal() +
        # theme(
        #   panel.grid = element_blank(),
        #   axis.text.x = element_text(angle = 45, hjust = 1),
        #   axis.text.y = element_blank(),
        #   axis.ticks.y = element_blank()
        # )
        # 

      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    }
  ) 
  
  output$access_egc_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(subset(access %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, region==input$regions_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      } else {
        text <- paste(subset(subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), egc=="TRUE")$per, "%", sep="")
      }
      text
    }
  )
  
  output$access_wdrn_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- format(round((access %>% summarise(average = median(weekly_disruption, na.rm=T)))$average, 0), big.mark = ",")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- format(round((subset(access, region==input$regions_access) %>% summarise(average = median(weekly_disruption, na.rm=T)))$average, 0), big.mark = ",")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- format(round((subset(access, state==input$states_access) %>% summarise(average = median(weekly_disruption, na.rm=T)))$average, 0), big.mark = ",")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- format(round((subset(access, state==input$states_access) %>% summarise(average = median(weekly_disruption, na.rm=T)))$average, 0), big.mark = ",")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(weekly_disruption, na.rm=T)))$average, 0), big.mark = ",")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(weekly_disruption, na.rm=T)))$average, 0), big.mark = ",")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(weekly_disruption, na.rm=T)))$average, 0), big.mark = ",")
      } else {
        text <- format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(weekly_disruption, na.rm=T)))$average, 0), big.mark = ",")
      }
      text
    }
  )
  
  output$access_egs_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((access %>% summarise(average = median(grid_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(grid_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(grid_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(grid_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(grid_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(grid_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(grid_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else {
        text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(grid_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      }
      text
    }
  )
  
  output$access_ges_access <- renderUI(
    {
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- round(mean(access_cs_score$ges, na.rm = TRUE), 0)
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- round(mean(subset(access_cs_score, region==input$regions_access)$ges, na.rm = TRUE), 0)
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- round(mean(subset(access_cs_score, state==input$states_access)$ges, na.rm = TRUE), 0)
       
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- round(mean(subset(access_cs_score, state==input$states_access)$ges, na.rm = TRUE), 0)
        
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- round(mean(subset(access_cs_score, lga==input$lgas_access)$ges, na.rm = TRUE), 0)
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- round(mean(subset(access_cs_score, lga==input$lgas_access)$ges, na.rm = TRUE), 0)
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- round(mean(subset(access_cs_score, lga==input$lgas_access)$ges, na.rm = TRUE), 0)
       
      } else {
        
        text <- round(mean(subset(access_cs_score, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access))$ges, na.rm = TRUE), 0)
        
      }
      
      # Ensure text is numeric and within bounds
      text <- as.numeric(round(text, 0))
      text <- max(0, min(text, 5))  # Clamp value between 0 and 5
      
      # Generate stars based on value
      filled_stars <- paste(rep("★", text), collapse = "")  # Filled stars
      empty_stars <- paste(rep("☆", 5 - text), collapse = "")  # Empty stars
      
      paste0(filled_stars, empty_stars)  # Return star string
      
      
    }
  )
  
  output$access_geq_access <- renderUI(
    {
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        n <- round((access %>% summarise(average = mean(Quality, na.rm=T)))$average, 0)
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        n <- round((subset(access, region==input$regions_access) %>% summarise(average = mean(Quality, na.rm=T)))$average, 0)
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        n <- round((subset(access, state==input$states_access) %>% summarise(average = mean(Quality, na.rm=T)))$average, 0)
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        n <- round((subset(access, state==input$states_access) %>% summarise(average = mean(Quality, na.rm=T)))$average, 0)
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        n <- round((subset(access, lga==input$lgas_access) %>% summarise(average = mean(Quality, na.rm=T)))$average, 0)
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        n <- round((subset(access, lga==input$lgas_access) %>% summarise(average = mean(Quality, na.rm=T)))$average, 0)
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        n <- round((subset(access, lga==input$lgas_access) %>% summarise(average = mean(Quality, na.rm=T)))$average, 0)
      } else {
        n <- round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(Quality, na.rm=T)))$average, 0)
      }
      
      # Ensure text is numeric and within bounds
      text <- as.numeric(round(n, 0))
      text <- max(0, min(text, 5))  # Clamp value between 0 and 5
      
      # Generate stars based on value
      filled_stars <- paste(rep("★", text), collapse = "")  # Filled stars
      empty_stars <- paste(rep("☆", 5 - text), collapse = "")  # Empty stars
      
      paste0(filled_stars, empty_stars)  # Return star string
    }
  )
  
  output$access_aeo_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(subset(access %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), aeo=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, region==input$regions_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), aeo=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), aeo=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(subset(subset(access, state==input$states_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), aeo=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), aeo=="TRUE")$per, "%", sep="")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), aeo=="TRUE")$per, "%", sep="")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste(subset(subset(access, lga==input$lgas_access) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), aeo=="TRUE")$per, "%", sep="")
      } else {
        text <- paste(subset(subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)), aeo=="TRUE")$per, "%", sep="")
      }
      text
    }
  )
  
  output$access_agd_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- na.omit(access %>% group_by(agd) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$agd[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, region==input$regions_access) %>% group_by(agd) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$agd[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, state==input$states_access) %>% group_by(agd) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$agd[1]
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, state==input$states_access) %>% group_by(agd) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$agd[1]
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(agd) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$agd[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(agd) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$agd[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(agd) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$agd[1]
      } else {
        text <- na.omit(subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(agd) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$agd[1]
      }
      text
    }
  )
  
  output$access_ags_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((access %>% summarise(average = median(gen_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(gen_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(gen_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(gen_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(gen_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(gen_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(gen_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      } else {
        text <- paste("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(gen_spend, na.rm=T)))$average, 0), big.mark = ","), sep=" ")
      }
      text
    }
  )
  
  output$access_agc_access <- renderUI(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- access %>% summarise(per = round(mean(agc, na.rm = TRUE) * 100, 0)) %>% pull(per)
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        
        text <- subset(access, region==input$regions_access) %>% summarise(per = round(mean(agc, na.rm = TRUE) * 100, 0)) %>% pull(per)
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- subset(access, state==input$states_access) %>% summarise(per = round(mean(agc, na.rm = TRUE) * 100, 0)) %>% pull(per)
        
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        
        text <- subset(access, state==input$states_access) %>% summarise(per = round(mean(agc, na.rm = TRUE) * 100, 0)) %>% pull(per)
    
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- subset(access, lga==input$lgas_access) %>% summarise(per = round(mean(agc, na.rm = TRUE) * 100, 0)) %>% pull(per)
        
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        
        text <- subset(access, lga==input$lgas_access) %>% summarise(per = round(mean(agc, na.rm = TRUE) * 100, 0)) %>% pull(per)
        
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        
        text <- subset(access, lga==input$lgas_access) %>% summarise(per = round(mean(agc, na.rm = TRUE) * 100, 0)) %>% pull(per)
        
      } else {
        
        text <- subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(per = round(mean(agc, na.rm = TRUE) * 100, 0)) %>% pull(per)
  
      }
      
        quality = text/20
        
        # Ensure quality is numeric and within bounds
        quality <- as.numeric(round(quality, 0))
        quality <- max(0, min(quality, 5))  # Clamp value between 0 and 5
        
        # Generate stars based on value
        filled_stars <- paste(rep("★", quality), collapse = "")  # Filled stars
        empty_stars <- paste(rep("☆", 5 - quality), collapse = "")  # Empty stars
        
        paste0(filled_stars, empty_stars)  # Return star string
    }
  )
  
  ## NULL omit
  output$access_asc_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- na.omit(access %>% group_by(asc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$asc[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, region==input$regions_access) %>% group_by(asc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$asc[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, state==input$states_access) %>% group_by(asc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$asc[1]
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, state==input$states_access) %>% group_by(asc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$asc[1]
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(asc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$asc[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(asc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$asc[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(asc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$asc[1]
      } else {
        text <- na.omit(subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(asc) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$asc[1]
      }
      text
    }
  )
  
  ## format thousands K
  output$access_ass_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste0("₦", formatC(round((access %>% summarise(average = median(ass, na.rm=T)))$average, 0)/1000, big.mark = ","), "K", sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste0("₦", format(round((subset(access, region==input$regions_access) %>% summarise(average = median(ass, na.rm=T)))$average, 0)/1000, big.mark = ","), "K", sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste0("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(ass, na.rm=T)))$average, 0)/1000, big.mark = ","), "K", sep=" ")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste0("₦", format(round((subset(access, state==input$states_access) %>% summarise(average = median(ass, na.rm=T)))$average, 0)/1000, big.mark = ","), "K", sep=" ")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste0("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(ass, na.rm=T)))$average, 0)/1000, big.mark = ","), "K", sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- paste0("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(ass, na.rm=T)))$average, 0)/1000, big.mark = ","), "K", sep=" ")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- paste("₦", format(round((subset(access, lga==input$lgas_access) %>% summarise(average = median(ass, na.rm=T)))$average, 0)/1000, big.mark = ","), "K", sep=" ")
      } else {
        text <- paste0("₦", format(round((subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% summarise(average = median(ass, na.rm=T)))$average, 0)/1000, big.mark = ","), "K", sep=" ")
      }
      text
    }
  )
  
  ## NULL omit
  output$access_res_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- na.omit(access %>% group_by(res) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$res[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, region==input$regions_access) %>% group_by(res) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$res[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, state==input$states_access) %>% group_by(res) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$res[1]
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, state==input$states_access) %>% group_by(res) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$res[1]
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(res) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$res[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(res) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$res[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(res) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$res[1]
      } else {
        text <- na.omit(subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(res) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$res[1]
      }
      text
    }
  )
  
  ## NULL omit
  output$access_csr_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- na.omit(access %>% group_by(csr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$csr[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, region==input$regions_access) %>% group_by(csr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$csr[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, state==input$states_access) %>% group_by(csr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$csr[1]
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access, state==input$states_access) %>% group_by(csr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$csr[1]
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(csr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$csr[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(csr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$csr[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access, lga==input$lgas_access) %>% group_by(csr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$csr[1]
      } else {
        text <- na.omit(subset(access, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(csr) %>% summarise(count = n()) %>% mutate(per = round(count/sum(count)*100, 0)) %>% arrange(desc(per)))$csr[1]
      }
      
      if(text %in% c("TRUE")){
        text <- c("Yes")
      } else {
        text <- c("No")
      }
      
      text
    }
  )
  
  output$map_access <- renderUI(
    {
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        img(src="images/maps/nation.png", width="100%")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        img(src=paste0("images/maps/region/", input$regions_access, ".png", sep=""), width="100%")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        img(src=paste0("images/maps/state/", input$states_access, ".png", sep=""), width="100%")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        img(src=paste0("images/maps/state/", input$states_access, ".png", sep=""), width="100%")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        img(src="images/maps/nation.png", width="100%")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        img(src=paste0("images/maps/state/", input$states_access, ".png", sep=""), width="100%")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        img(src=paste0("images/maps/region/", input$regions_access, ".png", sep=""), width="100%")
      } else {
        img(src=paste0("images/maps/state/", input$states_access, ".png", sep=""), width="100%")
      }
    }
  )
  
  output$map_info_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- c("National Info")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- c("Regional Info")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- c("State Info")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- c("State Info")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- c("Local Government Info")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- c("Local Government Info")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- c("Local Government Info")
      } else {
        text <- c("Local Government Info")
      }
      
      text
    }
  )
  
  output$map_area_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(format((region_state %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(format((subset(region_state, region==input$regions_access) %>% group_by(region) %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(format((subset(region_state, state==input$states_access) %>% group_by(state) %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(format((subset(region_state, state==input$states_access) %>% group_by(state) %>% summarise(area = sum(area, na.rm=T)))$area, big.mark = ","), "Sq km")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- c("--")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- c("--")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- c("--")
      } else {
        text <- c("--")
      }
      
      text
    }
  )
  
  output$map_population_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(round((region_state %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- paste(round((subset(region_state, region==input$regions_access) %>% group_by(region) %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(round((subset(region_state, state==input$states_access) %>% group_by(state) %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- paste(round((subset(region_state, state==input$states_access) %>% group_by(state) %>% summarise(population = sum(population, na.rm=T)))$population/1e6, 0), "M")
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- c("--")
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- c("--")
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- c("--")
      } else {
        text <- c("--")
      }
      
      text
    }
  )
  
  output$map_rank_access <- renderUI(
    {
      n <- c()
      if((input$regions_access == "All") & (input$states_access == "All")){
        n <- ceiling(median(median(readiness$ps), median(readiness$fs), median(readiness$ins), median(readiness$cs), median(readiness$ds)))
      } else if((input$regions_access != "All") & (input$states_access == "All")){
        n <- ceiling(median(median(subset(readiness, region==input$regions_access)$ps), median(subset(readiness, region==input$regions_access)$fs), median(subset(readiness, region==input$regions_access)$ins), median(subset(readiness, region==input$regions_access)$cs), median(subset(readiness, region==input$regions_access)$ds)))
      } else if((input$regions_access == "All") & (input$states_access != "All")){
        n <- ceiling(median(median(subset(readiness, state==input$states_access)$ps), median(subset(readiness, state==input$states_access)$fs), median(subset(readiness, state==input$states_access)$ins), median(subset(readiness, state==input$states_access)$cs), median(subset(readiness, state==input$states_access)$ds)))
      } else {
        n <- ceiling(median(median(subset(readiness, state==input$states_access)$ps), median(subset(readiness, state==input$states_access)$fs), median(subset(readiness, state==input$states_access)$ins), median(subset(readiness, state==input$states_access)$cs), median(subset(readiness, state==input$states_access)$ds)))
      }
      
      lapply(1:n, function(a) {
        tags$i(class = "fa-solid fa-star fa-1x",
               style = 'color: #075D04')
      })
    }
  )
  
  output$map_register_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All")){
        text <- sum((register %>% group_by(level) %>% summarise(count = n()))$count)
      } else if((input$regions_access != "All") & (input$states_access == "All")){
        text <- sum((subset(register, region==input$regions_access) %>% group_by(level) %>% summarise(count = n()))$count)
      } else if((input$regions_access == "All") & (input$states_access != "All")){
        text <- sum((subset(register, state==input$states_access) %>% group_by(level) %>% summarise(count = n()))$count)
      } else {
        text <- sum((subset(register, state==input$states_access) %>% group_by(level) %>% summarise(count = n()))$count)
      }
      
      text
    }
  )
  
  output$map_tier_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- na.omit(access_tier %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access_tier, region==input$regions_access) %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access_tier, state==input$states_access) %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      } else if((input$regions_access != "All") & (input$states_access != "All") & (input$lgas_access == "All")){
        text <- na.omit(subset(access_tier, state==input$states_access) %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      } else if((input$regions_access == "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access_tier, lga==input$lgas_access) %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      } else if((input$regions_access == "All") & (input$states_access != "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access_tier, lga==input$lgas_access) %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      } else if((input$regions_access != "All") & (input$states_access == "All") & (input$lgas_access != "All")){
        text <- na.omit(subset(access_tier, lga==input$lgas_access) %>% group_by(tier) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      } else {
        text <- na.omit(subset(access_tier, (region==input$regions_access) & (state==input$states_access) & (lga==input$lgas_access)) %>% group_by(csr) %>% summarise(average = median(count)) %>% arrange(desc(average)))$tier[1]
      }
      
      text
    }
  )
  
  output$map_region_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All")){
        text <- paste(c("North central", "North east", "North west", "South west", "South east", "South south"), collapse=", ")
      } else if((input$regions_access != "All") & (input$states_access == "All")){
        text <- paste(str_to_title(str_split(input$regions_access, "_")[[1]][1]), str_split(input$regions_access, "_")[[1]][2], sep=" ")
      } else if((input$regions_access == "All") & (input$states_access != "All")){
        text <- paste(str_to_title(str_split(subset(region_state, state == input$states_access)$region, "_")[[1]][1]), str_split(subset(region_state, state == input$states_access)$region, "_")[[1]][2], sep=" ")
      } else {
        text <- paste(str_to_title(str_split(subset(region_state, state == input$states_access)$region, "_")[[1]][1]), str_split(subset(region_state, state == input$states_access)$region, "_")[[1]][2], sep=" ")
      }
      
      text
    }
  )
  
  output$map_state_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All")){
        text <- c("36 & FCT/Abuja")
      } else if((input$regions_access != "All") & (input$states_access == "All")){
        text <- paste(subset(region_state, region == input$regions_access)$state, collapse = ", ")
      } else if((input$regions_access == "All") & (input$states_access != "All")){
        text <- input$states_access
      } else {
        text <- input$states_access
      }
      
      text
    }
  )
  
  output$map_summary_access <- renderText(
    {
      text <- c()
      if((input$regions_access == "All") & (input$states_access == "All")){
        text <- c("Nigeria has about 60% electricity access, with rural areas facing the greatest shortages. The country’s power generation capacity is 12,500 MW, but actual supply is often lower due to infrastructure and fuel issues. While the government and private sector focus on solar and off-grid solutions, challenges remain with grid reliability, infrastructure, and funding. Renewable energy potential, especially solar, is high, but rural electrification and consistent power supply are still major hurdles.")
      } else if((input$regions_access != "All") & (input$states_access == "All")){
        if(input$regions_access == "north_east"){
          text <- c("This region has the lowest electrification rates due to security challenges, weak grid infrastructure, and limited investments. Many rural communities lack access to the national grid and rely on expensive diesel generators. Solar mini-grids and standalone systems are gaining traction as alternative solutions. Government and donor-funded projects aim to expand energy access, but progress remains slow due to security risks.")
        } else if(input$regions_access == "north_west"){
          text <- c("Grid electricity access is expanding in major cities, but rural communities still face significant gaps. Many households depend on small solar home systems and mini-grids. Power infrastructure is improving, but vandalism and weak distribution networks limit reliability. The region has high solar potential, making off-grid electrification a promising alternative. Affordability remains a barrier for low-income communities.")
        } else if(input$regions_access == "north_west"){
          text <- c("Electrification is moderate, with urban centers enjoying better access than rural areas. The region hosts major hydropower plants like Kainji and Shiroro, but transmission bottlenecks limit efficiency. Rural communities still rely on diesel generators and standalone solar solutions. Distribution infrastructure is expanding, but affordability remains a challenge. Efforts to integrate renewable energy solutions are growing, but implementation is slow.")
        } else if(input$regions_access == "south_west"){
          text <- c("The region has the highest electrification rate in Nigeria, with a well-developed grid network. Lagos, the economic hub, has relatively better access, but demand still surpasses supply, leading to widespread generator use. Rural electrification is expanding, driven by private sector investments in off-grid solutions. Industrial and commercial sectors rely heavily on backup power due to grid instability. Renewable energy projects, particularly solar, are gaining traction to supplement existing capacity.")
        } else if(input$regions_access == "south_east"){
          text <- c("Urban areas have high grid access, but rural electrification lags behind. Many communities depend on diesel generators, while mini-grids and solar home systems are gradually emerging. Power supply is unreliable, with frequent blackouts affecting businesses and households. Economic activities, particularly in manufacturing and trade, are constrained by unstable electricity. Distributed renewable energy solutions are being explored to bridge the gap.")
        } else {
          text <- c("Despite strong grid infrastructure, electricity supply remains unreliable due to aging transmission lines and gas supply constraints. The region hosts several power plants but suffers from systemic inefficiencies. Rural electrification is improving, with growing adoption of solar mini-grids. Flood-prone areas face additional challenges in maintaining infrastructure. Efforts to strengthen grid reliability and integrate renewable energy sources are ongoing.")
        }
      } else if((input$regions_access == "All") & (input$states_access != "All")){
        text <- subset(readiness, state==input$states_access)$state_comms[1]
      } else {
        text <- subset(readiness, state==input$states_access)$state_comms[1]
      }
      
      text
    }
  )
}
