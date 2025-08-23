
write_csv(register_clean, "dashboard/database/register.csv")

access_ref <- read_csv("data/access_ref.csv")

access_ref <- left_join(access_clean, access_ref)

write_csv(access_ref, "dashboard/database/access.csv")

inc_rev <- subset(access_ref, select = c(region, state, lga, emi, brv))
inc_rev <- gather(inc_rev, type, count, emi:brv)


access_tier <- read_csv("data/access_lga_tiers_count.csv")
region_state_lga <- read_csv("dashboard/database/region_state_lga.csv")

access_tier$region <- rep(NA, nrow(access_tier))
access_tier$state <- rep(NA, nrow(access_tier))
access_tier$lga <- rep(NA, nrow(access_tier))

for(i in 1:nrow(access_tier)) {
  for(j in 1:nrow(region_state_lga)) {
    if(access_tier$LGA[i] == region_state_lga$lga[j]) {
      access_tier$region[i] <- region_state_lga$region[j]
      access_tier$state[i] <- region_state_lga$state[j]
      access_tier$lga[i] <- region_state_lga$lga[j]
    }
  }
}

access_tier <- gather(access_tier, tier, count, `Tier 0`:`Tier 5`)
access_tier <- access_tier[,-1]


write_csv(access_tier, "dashboard/database/access_tier.csv")

region_state <- read_csv("dashboard/database/region_state.csv")

sample$state <- sample$State %>% str_replace(" State", "")
sample_stat <- subset(sample, select = c("Region", "State", "Area", "Population"))
names(sample_stat) <- c("region", "state", "area", "population")

region_state <- left_join(region_state, sample_stat)


readiness_clean$region <- rep(NA, nrow(readiness_clean))
for(i in 1:nrow(readiness_clean)) {
  for(j in 1:nrow(region_state)) {
    if(readiness_clean$state[i] == region_state$state[j]) {
      readiness_clean$region[i] <- region_state$region[j]
    }
  }
}

demand <- access_ref %>% group_by(State) %>% summarise(demand = mean(Daily_Capacity_in_Wh, na.rm=T)/24)
demand$State <- demand$State %>% str_replace("Akwa-Ibom", "Akwa Ibom")

sample$State <- sample$State %>% str_replace(" State", "")

demand <- left_join(demand, subset(sample, select=c(State, Population, `Household size`, `% household with electricity`)))

names(demand) <- c("state", "demand", "population", "household", "electrified")

demand <- demand %>% group_by(state) %>% mutate(demand = (demand*population*(household/100)*(electrified/100))/100000)

demand$state <- demand$state %>% str_replace("Akwa Ibom", "Akwa-Ibom")
demand$state <- demand$state %>% str_replace("Cross River", "Cross-River")
demand$state <- demand$state %>% str_replace("Federal Capital Territory", "FCT/Abuja")

demand <- subset(demand, select = c(state, demand))

readiness_clean <- left_join(readiness_clean, demand) %>% group_by(state) %>% mutate(demand = ifelse(state == "Lagos", demand,
                                                                                                     ifelse(demand < ins_gc, demand*2.5, demand*1.8)))

write_csv(readiness_clean, "dashboard/database/readiness.csv")
