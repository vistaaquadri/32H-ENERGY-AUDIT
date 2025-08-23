# read all datamarts 

Pre_audit_df <- read_csv("../../data/pre_audit.csv")
network_analysis_df <- read_csv("../../data/network_analysis.csv")
applaince_audit_df <- read_csv("../../data/energy_audit.csv")
solar_feaibility_df <-  read_csv("../../data/solar_feasibility.csv")
power_analyzer_df <- read_csv("../../data/Power_analyzer.csv")
environmental_survey_df <- read_csv("../../data/environmental_impact.csv")
  




























# Stakeholder registry

# rg_url <- "https://docs.google.com/spreadsheets/d/1eXRrbBayH5VJmyJkTJbldZWq57yhM6uT/edit?usp=sharing&ouid=117696031907145889447&rtpof=true&sd=true"
# rg <- read_sheet(rg_url, sheet = "Registry") %>% 
#   mutate(across(everything(), as.character))
# 
# write_csv(rg, "data/register.csv")

rg <- read_csv("data/register.csv")

#################################################

# State Readiness

sr <- read_csv("data/readiness.csv")
policy <- read_csv("data/policy.csv")
funding <- read_csv("data/funding.csv")
infrastructure <- read_csv("data/infrastructure.csv")
comments <- read_csv("data/comments.csv")

#################################################

# Energy access
## sample
sample <- read_csv("data/sample.csv")

## south west
sw_url <- "https://docs.google.com/spreadsheets/d/1JOswyaO08cn11gUxMASLhneY_BYKBGLXvNLDcoejkyc/edit?gid=1424445530#gid=1424445530"
sw <- read_sheet(sw_url, sheet = "Copy of Field_Energy_Access_Residential") %>% 
  mutate(across(everything(), as.character))

write_csv(sw, "data/raw/south_west.csv")

## south south
ss_url <- "https://docs.google.com/spreadsheets/d/1gGx-XpB8gd09DIprU0m6lFocBRTT24i6D5D3zZWZbNE/edit?gid=641283500#gid=641283500"
ss <- read_sheet(ss_url, sheet = "Copy of Field_Energy_Access_Residential") %>% 
  mutate(across(everything(), as.character))

write_csv(ss, "data/raw/south_south.csv")

## south east
se_url <- "https://docs.google.com/spreadsheets/d/11DPbKipSpiW05lfgR9KXVFmr19F0pORX1HJdoYo-cqM/edit?gid=641283500#gid=641283500"
se <- read_sheet(se_url, sheet = "Copy of Field_Energy_Access_Residential") %>% 
  mutate(across(everything(), as.character))

write_csv(se, "data/raw/south_east.csv")

## north central
nc_url <- "https://docs.google.com/spreadsheets/d/1HzAWVRmBF7ueaIbHTUa3-WuQUdaycYroWPpR4In-_h8/edit?gid=641283500#gid=641283500"
nc <- read_sheet(nc_url, sheet = "Copy of Field_Energy_Access_Residential") %>% 
  mutate(across(everything(), as.character))

write_csv(nc, "data/raw/north_central.csv")

## north west
nw_url <- "https://docs.google.com/spreadsheets/d/1NrkTag_lXEpo1VwEHP8WN2Qv7aVYIJCs6y23cfk9M5w/edit?gid=1424445530#gid=1424445530"
nw <- read_sheet(nw_url, sheet = "Copy of Field_Energy_Access_Residential") %>% 
  mutate(across(everything(), as.character))

write_csv(nw, "data/raw/north_west.csv")

## north east
ne_url <- "https://docs.google.com/spreadsheets/d/1L7UIIpHL9LRN62Tooec8_1bw-8DKUpZvhGj9IfY40oI/edit?gid=1424445530#gid=1424445530"
ne <- read_sheet(ne_url, sheet = "Copy of Field_Energy_Access_Residential") %>% 
  mutate(across(everything(), as.character))

write_csv(ne, "data/raw/north_east.csv")


## passed
nesip_pass_url <- "https://docs.google.com/spreadsheets/d/1gCwYBWV_v0Ajuwfcive6yuqWcio_Ta0Bbad4IXzMz8w/edit?gid=674534119#gid=674534119"
nesip_pass <- read_sheet(nesip_pass_url, sheet = "2. Energy Access(Passed)") %>% 
  mutate(across(everything(), as.character))

ea <- read_csv("data/pass.csv")

write.csv(nesip_pass, "data/pass.csv")