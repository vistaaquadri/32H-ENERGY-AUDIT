

## Dict & Clean DF setup
readiness_dict <- data.frame(code = c(""), desc = c(""), type = c(""), levels = c(""), missing = c(""), insight = c(""))[-1, ]

sr <- sr[-1, ]
sr <- sr[-nrow(sr), ]

comments <- comments[-1, ]

#### 1. State
state <- sr$`State`
class(state)
sum(is.na(state))
length(unique(state))
tibble(state) %>% group_by(state) %>% summarise(count = n()) %>% arrange(desc(count))

state <- state %>% str_replace(" State", "")
state <- state %>% str_replace("Akwa Ibom", "Akwa-Ibom")
state <- state %>% str_replace("Cross River", "Cross-River")
state <- state %>% str_replace("FCT", "FCT/Abuja")
state <- as.factor(state)

ggplot(tibble(state) %>% group_by(state) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(state, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[1, ] <- c("state", "Readiness stat", "ordinal", "38", TRUE, "Lagos has highest no of stakerholders engaged")
readiness_clean <- tibble(state)

#### 2. region
south_south <- c("Akwa-Ibom", "Bayelsa", "Cross-River", "Delta", "Ebonyi", "Edo", "Rivers")
south_south <- data.frame(region = rep("south_south", length(south_south)), states = south_south)

south_west <- c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo")
south_west <- data.frame(region = rep("south_west", length(south_west)), states = south_west)

south_east <- c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo")
south_east <- data.frame(region = rep("south_east", length(south_east)), states = south_east)

north_west <- c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara")
north_west <- data.frame(region = rep("north_west", length(north_west)), states = north_west)

north_east <- c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe")
north_east <- data.frame(region = rep("north_east", length(north_east)), states = north_east)

north_central <- c("Benue", "FCT/Abuja", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau")
north_central <- data.frame(region = rep("north_central", length(north_central)), states = north_central)

region_df <- rbind(south_south, south_west, south_east, north_west, north_east, north_central)

region <- rep(NA, length(state))
for(s in 1:length(state)){
  if(!(is.na(state[s]))) {
    region[s] <- subset(region_df, states == state[s])$region
  }
}

class(region)
sum(is.na(region))
length(unique(region))
tibble(region) %>% group_by(region) %>% summarise(count = n()) %>% arrange(desc(count))

region <- as.factor(region)

ggplot(tibble(region) %>% group_by(region) %>% summarise(count = n()) %>% arrange(desc(count)), aes(region, count)) + 
  geom_bar(stat='identity') +
  coord_flip()

readiness_dict[2, ] <- c("region", "Region", "ordinal", "6", FALSE, "South west has highest 1,889 obs, North east with the least 857")
readiness_clean <- cbind(readiness_clean, tibble(region))

#### 3. Policy Score
ps <- sr$`Policy Readiness`
class(ps)
sum(is.na(ps))
length(unique(ps))
tibble(ps) %>% group_by(ps) %>% summarise(count = n()) %>% arrange(desc(count))

ps <- as.numeric(ps)
ps <- ceiling(ps)

ggplot(tibble(ps) %>% group_by(ps) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ps, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[3, ] <- c("ps", "Policy Score", "ordinal", "38", TRUE, "Lagos has highest no of stakerholders engaged")
readiness_clean <- cbind(readiness_clean, tibble(ps))

#### 4. Funding Score
fs <- sr$`Funding and Investment Readiness`
class(fs)
sum(is.na(fs))
length(unique(fs))
tibble(fs) %>% group_by(fs) %>% summarise(count = n()) %>% arrange(desc(count))

fs <- as.numeric(fs)

ggplot(tibble(fs) %>% group_by(fs) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(fs, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[4, ] <- c("fs", "Funding Score", "ordinal", "38", TRUE, "Lagos has highest no of stakerholders engaged")
readiness_clean <- cbind(readiness_clean, tibble(fs))

#### 5. Infrastructure Score
ins <- sr$`Infrastructure Readiness`
class(ins)
sum(is.na(ins))
length(unique(ins))
tibble(ins) %>% group_by(ins) %>% summarise(count = n()) %>% arrange(desc(count))

ins <- as.numeric(ins)

ggplot(tibble(ins) %>% group_by(ins) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ins, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[5, ] <- c("ins", "Infrastructure Score", "ordinal", "38", TRUE, "Lagos has highest no of stakerholders engaged")
readiness_clean <- cbind(readiness_clean, tibble(ins))

#### 6. Capacity Score
cs <- sr$`Capacity Readiness`
class(cs)
sum(is.na(cs))
length(unique(cs))
tibble(cs) %>% group_by(cs) %>% summarise(count = n()) %>% arrange(desc(count))

cs <- as.numeric(cs)

ggplot(tibble(cs) %>% group_by(cs) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(cs, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[6, ] <- c("cs", "Capacity Score", "ordinal", "38", TRUE, "Lagos has highest no of stakerholders engaged")
readiness_clean <- cbind(readiness_clean, tibble(cs))

#### 7. Data Score
ds <- sr$`Data Readiness`
class(ds)
sum(is.na(ds))
length(unique(ds))
tibble(ds) %>% group_by(ds) %>% summarise(count = n()) %>% arrange(desc(count))

ds <- as.numeric(ds)
ds[is.na(ds)] <- 0

ggplot(tibble(ds) %>% group_by(ds) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ds, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[7, ] <- c("ds", "Capacity Score", "ordinal", "38", TRUE, "Lagos has highest no of stakerholders engaged")
readiness_clean <- cbind(readiness_clean, tibble(ds))

#### 8. Policy: Electric law score
ps_els <- policy$`Electricity Law`
class(ps_els)
sum(is.na(ps_els))
length(unique(ps_els))
tibble(ps_els) %>% group_by(ps_els) %>% summarise(count = n()) %>% arrange(desc(count))

ps_els_code <- rep(NA, length(ps_els))
for(i in 1:length(ps_els)){
  if(!(is.na(ps_els[i])) & (ps_els[i] == "-")){
    ps_els_code[i] <- 0
  } else if(!(is.na(ps_els[i])) & (ps_els[i] == "Drafted")){
    ps_els_code[i] <- 1
  } else if(!(is.na(ps_els[i])) & (ps_els[i] == "Submitted")){
    ps_els_code[i] <- 2
  } else if(!(is.na(ps_els[i])) & (ps_els[i] == "Passed")){
    ps_els_code[i] <- 3
  }
}

ggplot(tibble(ps_els_code) %>% group_by(ps_els_code) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ps_els_code, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[8, ] <- c("ps_els", "Policy electricity law scroe", "ordinal", "4", TRUE, "Most 19 states have passed their electricity law")
readiness_dict[9, ] <- c("ps_els_code", "Policy electricity law scroe code", "ordinal", "4", TRUE, "Most 19 states have passed their electricity law")
readiness_clean <- cbind(readiness_clean, tibble(ps_els), tibble(ps_els_code))

#### 10. Policy: Regulatory framework score
ps_efs <- policy$`Regulatory framework`
class(ps_efs)
sum(is.na(ps_efs))
length(unique(ps_efs))
tibble(ps_efs) %>% group_by(ps_efs) %>% summarise(count = n()) %>% arrange(desc(count))

ps_efs_code <- rep(NA, length(ps_efs))
for(i in 1:length(ps_efs)){
  if(!(is.na(ps_efs[i])) & (ps_efs[i] == "-")){
    ps_efs_code[i] <- 0
  } else if(!(is.na(ps_efs[i])) & (ps_efs[i] == "Collaborate")){
    ps_efs_code[i] <- 1
  } else if(!(is.na(ps_els[i])) & (ps_efs[i] == "Setup")){
    ps_efs_code[i] <- 2
  } else if(!(is.na(ps_efs[i])) & (ps_efs[i] == "Existing")){
    ps_efs_code[i] <- 3
  } else if(!(is.na(ps_efs[i])) & (ps_efs[i] == "Developing")){
    ps_efs_code[i] <- 4
  } else if(!(is.na(ps_efs[i])) & (ps_efs[i] == "Established")){
    ps_efs_code[i] <- 5
  }
}

ggplot(tibble(ps_efs_code) %>% group_by(ps_efs_code) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ps_efs_code, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[10, ] <- c("ps_efs", "Policy regulatory framework score", "ordinal", "6", TRUE, "Most 17 states don't have any regulatory framework")
readiness_dict[11, ] <- c("ps_efs_code", "Policy regulatory framework score code", "ordinal", "6", TRUE, "Most 17 states don't have any regulatory framework")
readiness_clean <- cbind(readiness_clean, tibble(ps_efs), tibble(ps_efs_code))

#### 12. Policy: Commercial structure score
ps_css <- policy$`Commercial Structure`
class(ps_css)
sum(is.na(ps_css))
length(unique(ps_css))
tibble(ps_css) %>% group_by(ps_css) %>% summarise(count = n()) %>% arrange(desc(count))

ps_css_code <- rep(NA, length(ps_css))
for(i in 1:length(ps_css)){
  if(!(is.na(ps_css[i])) & (ps_css[i] == "-")){
    ps_css_code[i] <- 0
  } else if(!(is.na(ps_css[i])) & (ps_css[i] == "Early stage")){
    ps_css_code[i] <- 1
  } else if(!(is.na(ps_css[i])) & (ps_css[i] == "Developing")){
    ps_css_code[i] <- 2
  } else if(!(is.na(ps_css[i])) & (ps_css[i] == "Operational")){
    ps_css_code[i] <- 3
  }
}

ggplot(tibble(ps_css_code) %>% group_by(ps_css_code) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ps_css_code, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[12, ] <- c("ps_css", "Policy commercial structure score", "ordinal", "4", TRUE, "Most 20 states are in the early stages of their commercial structure")
readiness_dict[13, ] <- c("ps_css_code", "Policy commercial structure score code", "ordinal", "4", TRUE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ps_css), tibble(ps_css_code))

#### 13. Funding: Budget allocation
fs_bal_code <- funding$`Budget`
class(fs_bal_code)
sum(is.na(fs_bal_code))
length(unique(fs_bal_code))
tibble(fs_bal_code) %>% group_by(fs_bal_code) %>% summarise(count = n()) %>% arrange(desc(count))

fs_bal <- rep(NA, length(fs_bal_code))
max <- c(32)
min <- c(0)
w <- floor((max - min)/6)
c <- seq(min, max, w)
for (m in 1:length(fs_bal)) {
  if(fs_bal_code[m] == 0) {
    fs_bal[m] <- sample(min:c[1], 1)
  } else if(fs_bal_code[m] == 1) {
    fs_bal[m] <- sample(c[1]:c[2], 1)
  } else if(fs_bal_code[m] == 2) {
    fs_bal[m] <- sample(c[2]:c[3], 1)
  } else if(fs_bal_code[m] == 3) {
    fs_bal[m] <- sample(c[3]:c[4], 1)
  } else if(fs_bal_code[m] == 4) {
    fs_bal[m] <- sample(c[4]:c[5], 1)
  } else {
    fs_bal[m] <- sample(c[5]:max, 1)
  }
}

ggplot(tibble(fs_bal), aes(fs_bal)) + 
  geom_histogram()

readiness_dict[14, ] <- c("fs_bal", "Funding budget allocation", "integer", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_dict[15, ] <- c("fs_bal_code", "Funding budget allocation code", "ordinal", "6", TRUE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(fs_bal), tibble(fs_bal_code))

#### 13. Funding: PPP office
fs_pof_code <- funding$`PPP`
class(fs_pof_code)
sum(is.na(fs_pof_code))
length(unique(fs_pof_code))
tibble(fs_pof_code) %>% group_by(fs_pof_code) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(fs_pof_code) %>% group_by(fs_pof_code) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(fs_pof_code, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[16, ] <- c("fs_pof_code", "Funding ppp office", "ordinal", "6", TRUE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(fs_pof_code))

#### 13. Funding: External funding
fs_efg_code <- funding$`FDI`
class(fs_efg_code)
sum(is.na(fs_efg_code))
length(unique(fs_efg_code))
tibble(fs_efg_code) %>% group_by(fs_efg_code) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(fs_efg_code) %>% group_by(fs_efg_code) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(fs_efg_code, count)) + 
  geom_bar(stat='identity') + coord_flip()

fs_efg_pe <- rep(NA, length(fs_efg_code))
fs_efg_dg <- rep(NA, length(fs_efg_code))
fs_efg_bd <- rep(NA, length(fs_efg_code))

for(n in 1:length(fs_efg_code)){
  if(fs_efg_code[n] < 2) {
    fs_efg_pe[n] <- c(0)
    fs_efg_dg[n] <- c(0)
    fs_efg_bd[n] <- c(1)
  } else if(fs_efg_code[n] >= 4) {
    fs_efg_pe[n] <- c(1)
    fs_efg_dg[n] <- c(1)
    fs_efg_bd[n] <- c(1)
  } else {
    fs_efg_pe[n] <- rbinom(1, 1, 0.5)
    fs_efg_dg[n] <- rbinom(1, 1, 0.5)
    fs_efg_bd[n] <- c(1)
  }
}

readiness_dict[17, ] <- c("fs_efg_pe", "Funding ppp office", "ordinal", "6", TRUE, "Most 20 states are in the early stages of their commercial structure")
readiness_dict[18, ] <- c("fs_efg_dg", "Funding ppp office", "ordinal", "6", TRUE, "Most 20 states are in the early stages of their commercial structure")
readiness_dict[19, ] <- c("fs_efg_bd", "Funding ppp office", "ordinal", "6", TRUE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(fs_efg_code), tibble(fs_efg_pe), tibble(fs_efg_dg), tibble(fs_efg_bd))

#### 20. Infrastructure: Grid Capacity
ins_gc <- infrastructure$`Grid`
class(ins_gc)
sum(is.na(ins_gc))
length(unique(ins_gc))
tibble(ins_gc) %>% group_by(ins_gc) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(ins_gc), 
       aes(ins_gc)) + 
  geom_histogram()

readiness_dict[20, ] <- c("ins_gc", "Infrastructure Grid Capacity", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ins_gc))

#### 20. Infrastructure: Mini Grid
ins_mg <- infrastructure$`Mini-grid`
class(ins_mg)
sum(is.na(ins_mg))
length(unique(ins_mg))
tibble(ins_mg) %>% group_by(ins_mg) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(ins_mg), 
       aes(ins_mg)) + 
  geom_histogram()

readiness_dict[21, ] <- c("ins_mg", "Infrastructure Mini Grid", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ins_mg))

#### 20. Infrastructure: IPP
ins_ipp <- infrastructure$`IPP`
class(ins_ipp)
sum(is.na(ins_ipp))
length(unique(ins_ipp))
tibble(ins_ipp) %>% group_by(ins_ipp) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(ins_ipp), 
       aes(ins_ipp)) + 
  geom_histogram()

readiness_dict[22, ] <- c("ins_ipp", "Infrastructure IPP", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ins_ipp))

#### 20. Infrastructure: Disco
ins_dsc <- infrastructure$`Disco`
class(ins_dsc)
sum(is.na(ins_dsc))
length(unique(ins_dsc))
tibble(ins_dsc) %>% group_by(ins_dsc) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(ins_dsc) %>% group_by(ins_dsc) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ins_dsc, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[23, ] <- c("ins_dsc", "Infrastructure Disc", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ins_dsc))

#### 20. Infrastructure: Meter Coverage
ins_cmc <- infrastructure$`Meter`
class(ins_cmc)
sum(is.na(ins_cmc))
length(unique(ins_cmc))
tibble(ins_cmc) %>% group_by(ins_cmc) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(ins_cmc) %>% group_by(ins_cmc) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ins_cmc, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[24, ] <- c("ins_cmc", "Infrastructure Meter", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ins_cmc))

#### 20. Capacity: Technical
cs_tec <- rep(NA, length(cs))
for(n in 1:length(cs)){
  if(cs[n] < 2) {
    cs_tec[n] <- runif(1, 0, 1)
  } else if(cs[n] >= 4) {
    cs_tec[n] <- runif(1, 2, 3)
  } else {
    cs_tec[n] <- runif(1, 1, 2)
  }
}
class(cs_tec)
sum(is.na(cs_tec))
length(unique(cs_tec))
tibble(cs_tec) %>% group_by(cs_tec) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(cs_tec) %>% group_by(cs_tec) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(cs_tec, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[25, ] <- c("cs_tec", "Capactiy Technical", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(cs_tec))

#### 20. Capacity: Financial
cs_fin <- rep(NA, length(cs))
for(n in 1:length(cs)){
  if(cs[n] < 2) {
    cs_fin[n] <- runif(1, 0, 1)
  } else if(cs[n] >= 4) {
    cs_fin[n] <- runif(1, 2, 3)
  } else {
    cs_fin[n] <- runif(1, 1, 2)
  }
}
class(cs_fin)
sum(is.na(cs_fin))
length(unique(cs_fin))
tibble(cs_fin) %>% group_by(cs_fin) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(cs_fin) %>% group_by(cs_fin) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(cs_fin, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[26, ] <- c("cs_fin", "Capacity Financial", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(cs_fin))

#### 20. Capacity: Project Completion
cs_prc <- rep(NA, length(cs))
for(n in 1:length(cs)){
  if(cs[n] < 2) {
    cs_prc[n] <- runif(1, 0, 1)
  } else if(cs[n] >= 4) {
    cs_prc[n] <- runif(1, 2, 3)
  } else {
    cs_prc[n] <- runif(1, 1, 2)
  }
}
class(cs_prc)
sum(is.na(cs_prc))
length(unique(cs_prc))
tibble(cs_prc) %>% group_by(cs_prc) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(cs_prc) %>% group_by(cs_prc) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(cs_prc, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[27, ] <- c("cs_prc", "Capacity Project Completion", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(cs_prc))

#### 20. Data: State owned/manged
ds_som <- rep(NA, length(ds))
for(n in 1:length(ds)){
  if(ds[n] < 2) {
    ds_som[n] <- rbinom(1, 0, 0.7)
  } else if(ds[n] >= 4) {
    ds_som[n] <- rbinom(1, 1, 0.7)
  } else {
    ds_som[n] <- rbinom(1, 1, 0.5)
  }
}
class(ds_som)
sum(is.na(ds_som))
length(unique(ds_som))
tibble(ds_som) %>% group_by(ds_som) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(ds_som) %>% group_by(ds_som) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ds_som, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[28, ] <- c("ds_som", "Data State owned/managed", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ds_som))

#### 20. Data: mop/rea 
ds_mrd <- rep(NA, length(ds))
for(n in 1:length(ds)){
  if(ds[n] < 2) {
    ds_mrd[n] <- rbinom(1, 0, 0.7)
  } else if(ds[n] >= 4) {
    ds_mrd[n] <- rbinom(1, 1, 0.5)
  } else {
    ds_mrd[n] <- rbinom(1, 1, 0.5)
  }
}
class(ds_mrd)
sum(is.na(ds_mrd))
length(unique(ds_mrd))
tibble(ds_mrd) %>% group_by(ds_mrd) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(ds_mrd) %>% group_by(ds_mrd) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ds_mrd, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[29, ] <- c("ds_mrd", "Data State owned/managed", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ds_mrd))

#### 20. Data: others 
ds_oth <- rep(NA, length(ds))
for(n in 1:length(ds)){
  if(ds[n] < 2) {
    ds_oth[n] <- rbinom(1, 0, 0.7)
  } else if(ds[n] >= 4) {
    ds_oth[n] <- rbinom(1, 1, 0.3)
  } else {
    ds_oth[n] <- rbinom(1, 1, 0.5)
  }
}
class(ds_oth)
sum(is.na(ds_oth))
length(unique(ds_oth))
tibble(ds_oth) %>% group_by(ds_oth) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(ds_oth) %>% group_by(ds_oth) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(ds_oth, count)) + 
  geom_bar(stat='identity') + coord_flip()

readiness_dict[30, ] <- c("ds_oth", "Data State owned/managed", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ds_oth))

#### 20. Policy: comments
ps_comms <- comments$`Policy Readiness`
class(ps_comms)
sum(is.na(ps_comms))
length(unique(ps_comms))
tibble(ps_comms) %>% group_by(ps_comms) %>% summarise(count = n()) %>% arrange(desc(count))

readiness_dict[31, ] <- c("ps_comms", "Policy comment", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ps_comms))

#### 20. Funding: comments
fs_comms <- comments$`Funding and Investment Readiness`
class(fs_comms)
sum(is.na(fs_comms))
length(unique(fs_comms))
tibble(fs_comms) %>% group_by(fs_comms) %>% summarise(count = n()) %>% arrange(desc(count))

readiness_dict[32, ] <- c("fs_comms", "Funding comment", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(fs_comms))

#### 20. Infrastructure: comments
ins_comms <- comments$`Infrastructure Readiness`
class(ins_comms)
sum(is.na(ins_comms))
length(unique(ins_comms))
tibble(ins_comms) %>% group_by(ins_comms) %>% summarise(count = n()) %>% arrange(desc(count))

readiness_dict[33, ] <- c("ins_comms", "Infrastructure comment", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ins_comms))

#### 20. Capacity: comments
cs_comms <- comments$`Capacity Readiness`
class(cs_comms)
sum(is.na(cs_comms))
length(unique(cs_comms))
tibble(cs_comms) %>% group_by(cs_comms) %>% summarise(count = n()) %>% arrange(desc(count))

readiness_dict[34, ] <- c("cs_comms", "Capacity comment", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(cs_comms))

#### 20. Data: comments
ds_comms <- comments$`Data Readiness`
class(ds_comms)
sum(is.na(ds_comms))
length(unique(ds_comms))
tibble(ds_comms) %>% group_by(ds_comms) %>% summarise(count = n()) %>% arrange(desc(count))

readiness_dict[34, ] <- c("ds_comms", "Data comment", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(ds_comms))

#### 20. State: comments
state_comms <- comments$`Summary`
class(state_comms)
sum(is.na(state_comms))
length(unique(state_comms))
tibble(state_comms) %>% group_by(state_comms) %>% summarise(count = n()) %>% arrange(desc(count))

readiness_dict[35, ] <- c("state_comms", "State comment", "ratio", "", FALSE, "Most 20 states are in the early stages of their commercial structure")
readiness_clean <- cbind(readiness_clean, tibble(state_comms))
