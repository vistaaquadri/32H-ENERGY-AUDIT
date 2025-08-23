readiness <- read.csv("readiness.csv", stringsAsFactors = FALSE)
readiness_tag <- read.csv("readiness_tag.csv", stringsAsFactors = FALSE)


library(dplyr)


#policy

# Merge text columns for Electricity Law
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Electricity.Law, Electricity.Law.Text) %>%
              distinct(),
            by = "Electricity.Law")

# Merge text columns for Regulatory Framework
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Regulatory.Framework, Regulatory.Framework.Text) %>%
              distinct(),
            by = "Regulatory.Framework")

# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Electrification.Plan.Strategy, Electrification.Plan.Text) %>%
              distinct(),
            by = c("Electrification.Plan.Strategy"))



#### funding

# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Budget.Allocation, Budget.Allocation.Text) %>%
              distinct(),
            by = c("Budget.Allocation" = "Budget.Allocation"))



# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(PPP.Office, PPP.Office.Text) %>%
              distinct(),
            by = c("PPP.Office" = "PPP.Office"))



# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(External.Funding, External.Funding.Text) %>%
              distinct(),
            by = c("External.Funding"))




#infrastructure


# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(per.of.Transmission.Capacity.to.Household.Demand, per.of.Transmission.Capacity.to.Household.Demand.Text) %>%
              distinct(),
            by = c("per.of.Transmission.Capacity.to.Household.Demand"))



# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(per.of.Grid.Connection, per.of.Grid.Connection.Text) %>%
              distinct(),
            by = c("per.of.Grid.Connection"))



# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Metering.Rate, Metering.Rate.Text) %>%
              distinct(),
            by = c("Metering.Rate"))




#capacity

# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Stakeholder.Engagement, Stakeholder.Engagement.Text) %>%
              distinct(),
            by = c("Stakeholder.Engagement"))



# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Pre.Development.Capacity, Pre.Development.Capacity.Text) %>%
              distinct(),
            by = c("Pre.Development.Capacity"))



# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Development.Capacity, Development.Capacity.Text) %>%
              distinct(),
            by = c("Development.Capacity"))



# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Operational.Capacity, Operational.Capacity.Text) %>%
              distinct(),
            by = c("Operational.Capacity"))





#data



# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Dedicated.Agency, Dedicated.Agency.Text) %>%
              distinct(),
            by = c("Dedicated.Agency" = "Dedicated.Agency"))



# Merge text columns for Electrification Plan
readiness <- readiness %>%
  left_join(readiness_tag %>%
              select(Data.Accessibility, Data.Accessibility.Text) %>%
              distinct(),
            by = c("Data.Accessibility" = "Data.Accessibility"))





# Optionally rename for clarity
names(readiness)[names(readiness) == "Electricity.Law.Text"] <- "Electricity Law Text"
names(readiness)[names(readiness) == "Regulatory.Framework.Text"] <- "Regulatory Framework Text"
names(readiness)[names(readiness) == "Electrification.Plan.Text"] <- "Electrification Plan Text"




# Optionally rename for clarity
names(readiness)[names(readiness) == "Budget.Allocation.Text"] <- "Budget Allocation Text"
names(readiness)[names(readiness) == "PPP.Office.Text"] <- "PPP Office Text"
names(readiness)[names(readiness) == "External.Funding.Text"] <- "External Funding Text"




# Optionally rename for clarity
names(readiness)[names(readiness) == "per.of.Transmission.Capacity.to.Household.Demand.Text"] <- "per of Transmission Capacity to Household Demand Text"
names(readiness)[names(readiness) == "per.of.Grid.Connection.Text"] <- "per of Grid Connection Text"
names(readiness)[names(readiness) == "Metering.Rate.Text"] <- "Metering Rate Text"




# Optionally rename for clarity
names(readiness)[names(readiness) == "Stakeholder.Engagement.Text"] <- "Stakeholder Engagement Text"
names(readiness)[names(readiness) == "Pre.Development.Capacity.Text"] <- "Pre Development Capacity Text"
names(readiness)[names(readiness) == "Development.Capacity.Text"] <- "Development Capacity Text"
names(readiness)[names(readiness) == "Operational.Capacity.Text"] <- "Operational Capacity Text"



# Optionally rename for clarity
names(readiness)[names(readiness) == "Dedicated.Agency.Text"] <- "Dedicated Agency Text"
names(readiness)[names(readiness) == "Data.Accessibility.Text"] <- "Data Accessibility Text"


write.csv(readiness, "readiness_complete.csv")
