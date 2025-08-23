


## Dict & Clean DF setup
access_dict <- data.frame(code = c(""), desc = c(""), type = c(""), levels = c(""), missing = c(""), insight = c(""))[-1, ]


#### 1. unique id
id <- ea$`unique_id`
class(id)
sum(is.na(id))
length(unique(id))
tibble(id) %>% group_by(id) %>% summarise(count = n()) %>% arrange(desc(count))

id_dups <- subset(tibble(id) %>% group_by(id) %>% summarise(count = n()) %>% arrange(desc(count)), count > 1)
drop_index <- c()
for(i in 1:nrow(id_dups)) {
  drop_index[i] <- sample(which(ea$unique_id == id_dups$id[i]), 1)
}

ea <- subset(ea, !(as.numeric(rownames(ea)) %in% drop_index))
id <- ea$`unique_id`

access_dict[1, ] <- c("id", "Unique identifier", "nominal", "", FALSE, "Original data had 7826 rows, cleaned out 64 duplicates")
access_clean <- tibble(id)

#### 2. state
state <- ea$`State`
class(state)
sum(is.na(state))
length(unique(state))
tibble(state) %>% group_by(state) %>% summarise(count = n()) %>% arrange(desc(count))

ea$`State` <- ea$`State` %>% str_replace("Cross River", "Cross-River")
ea$`State` <- ea$`State` %>% str_replace("Federal Capital Territory", "FCT/Abuja")
ea$`State` <- ea$`State` %>% str_replace("Katsina", "Kastina")
state <- ea$`State`
state <- as.factor(state)

ggplot(tibble(state) %>% group_by(state) %>% summarise(count = n()) %>% arrange(desc(count)), aes(state, count)) + 
  geom_bar(stat='identity') +
  coord_flip()

access_dict[3, ] <- c("state", "State", "ordinal", "37", FALSE, "Lagos has highest 671 obs, Taraba as least 72, average of 210/state")
access_clean <- cbind(access_clean, tibble(state))

#### 3. region
south_south <- c("Akwa-Ibom", "Bayelsa", "Cross-River", "Delta", "Ebonyi", "Edo", "Rivers")
south_south <- data.frame(region = rep("south_south", length(south_south)), states = south_south)

south_west <- c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo")
south_west <- data.frame(region = rep("south_west", length(south_west)), states = south_west)

south_east <- c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo")
south_east <- data.frame(region = rep("south_east", length(south_east)), states = south_east)

north_west <- c("Jigawa", "Kaduna", "Kano", "Kastina", "Kebbi", "Sokoto", "Zamfara")
north_west <- data.frame(region = rep("north_west", length(north_west)), states = north_west)

north_east <- c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe")
north_east <- data.frame(region = rep("north_east", length(north_east)), states = north_east)

north_central <- c("Benue", "FCT/Abuja", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau")
north_central <- data.frame(region = rep("north_central", length(north_central)), states = north_central)

region_df <- rbind(south_south, south_west, south_east, north_west, north_east, north_central)

region <- c()
for(s in 1:length(state)){
  region[s] <- subset(region_df, states == state[s])$region
}

class(region)
sum(is.na(region))
length(unique(region))
tibble(region) %>% group_by(region) %>% summarise(count = n()) %>% arrange(desc(count))

region <- as.factor(region)

ggplot(tibble(region) %>% group_by(region) %>% summarise(count = n()) %>% arrange(desc(count)), aes(region, count)) + 
  geom_bar(stat='identity') +
  coord_flip()

access_dict[2, ] <- c("region", "Region", "ordinal", "6", FALSE, "South west has highest 1,889 obs, North east with the least 857")
access_clean <- cbind(access_clean, tibble(region))
access_clean <- access_clean[, c(1,3,2)]

#### 4. lga
lga <- ea$`LGA`
class(lga)
sum(is.na(lga))
length(unique(lga))
tibble(lga) %>% group_by(lga) %>% summarise(count = n()) %>% arrange(desc(count))

access_dict[4, ] <- c("lga", "Local Government Area", "ordinal", "724", FALSE, "Expecting 774 but data contains 724, average of 10 obs/lga")
access_clean <- cbind(access_clean, tibble(lga))

### 5. area
area <- ea$`Area Description`
class(area)
sum(is.na(area))
length(unique(area))
tibble(area) %>% group_by(area) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(area) %>% group_by(area) %>% summarise(count = n()), 
       aes(area, count)) + geom_bar(stat="identity")

access_dict[5, ] <- c("area", "Description of area (Rural/Urban)", "ordinal", "2", FALSE, "Data contains more urban (50.3%) than rural")
access_clean <- cbind(access_clean, tibble(area))

#### 6. household latitude
latitude <- ea$`Geolocation`
for(i in 1:length(latitude)){
  latitude[i] <- str_split(latitude[i], ", ")[[1]][1]
}
class(latitude)
sum(is.na(latitude))
length(unique(latitude))
tibble(latitude) %>% group_by(latitude) %>% summarise(count = n())
latitude <- as.numeric(latitude)

access_dict[6, ] <- c("latitude", "Household Geolocation Latitude", "nominal", "", FALSE, "")
access_clean <- cbind(access_clean, tibble(latitude))

#### 8. household longitude
longitude <- ea$`Geolocation`
for(i in 1:length(longitude)){
  longitude[i] <- str_split(longitude[i], ", ")[[1]][1]
}
class(longitude)
sum(is.na(longitude))
length(unique(longitude))
tibble(longitude) %>% group_by(longitude) %>% summarise(count = n())
longitude <- as.numeric(longitude)

access_dict[7, ] <- c("longitude", "Household Geolocation Longitude", "nominal", "", FALSE, "")
access_clean <- cbind(access_clean, longitude = tibble(longitude))

#### 8. respondent name
name <- ea$`1A. Respondent's Name`
class(name)
sum(is.na(name))
length(unique(name))
tibble(name) %>% group_by(name) %>% summarise(count = n())

access_dict[8, ] <- c("name", "Respondent name", "nominal", "", FALSE, "Data contains numbers given as name")
access_clean <- cbind(access_clean, name = tibble(name))

#### 9. respondent contact
contact <- ea$`1B. Contact Information(Phone Number/email)`
class(contact)
sum(is.na(contact))
length(unique(contact))
tibble(contact) %>% group_by(contact) %>% summarise(count = n())

access_dict[9, ] <- c("contact", "Contact Information(Phone Number/email)", "nominal", "", FALSE, "Data contains incomplete no and email text")
access_clean <- cbind(access_clean, contact = tibble(contact))

#### 10. Household head
hhh <- ea$`1C. Are you the head of the Household?`
class(hhh)
sum(is.na(hhh))
length(unique(hhh))
tibble(hhh) %>% group_by(hhh) %>% summarise(count = n())

ggplot(tibble(hhh) %>% group_by(hhh) %>% summarise(count = n()), 
       aes(hhh, count)) + geom_bar(stat="identity")

access_dict[10, ] <- c("hhh", "1C. Are you the head of the Household?", "binary", "2", FALSE, "More head of households responded (59%)")
access_clean <- cbind(access_clean, hhh = tibble(hhh))


#### 11. Gender
gender <- ea$`1D. Gender`
class(gender)
sum(is.na(gender))
length(unique(gender))
tibble(gender) %>% group_by(gender) %>% summarise(count = n())

ggplot(tibble(gender) %>% group_by(gender) %>% summarise(count = n()), 
       aes(gender, count)) + geom_bar(stat="identity")

access_dict[11, ] <- c("gender", "Household Respondent gender", "ordinal", "2", FALSE, "More male gender responded (60%)")
access_clean <- cbind(access_clean, gender = tibble(gender))

#### 12. Age
age <- ea$`1E. Age Range`
class(age)
sum(is.na(age))
length(unique(age))
tibble(age) %>% group_by(age) %>% summarise(count = n())

ggplot(tibble(age) %>% group_by(age) %>% summarise(count = n()), 
       aes(age, count)) + geom_bar(stat="identity")

access_dict[12, ] <- c("age", "Household Respondent age", "ordinal", "5", FALSE, "Most respondent are between 30-49, uniform distribution of age representation")
access_clean <- cbind(access_clean, age = tibble(age))

#### 13. Education
education <- ea$`1F. Highest level of education attained`
class(education)
sum(is.na(education))
length(unique(education))
tibble(education) %>% group_by(education) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2))

ggplot(tibble(education) %>% group_by(education) %>% summarise(count = n()), 
       aes(education, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[13, ] <- c("education", "Household Respondent education", "ordinal", "5", FALSE, "Most respondents have completed secondary school (52%) and higher education (34%)")
access_clean <- cbind(access_clean, education = tibble(education))

#### 14. Occupation
occupation <- ea$`1G. What is the primary occupation of the head of household`
class(occupation)
sum(is.na(occupation))
length(unique(occupation))
tibble(occupation) %>% group_by(occupation) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2))

ggplot(tibble(occupation) %>% group_by(occupation) %>% summarise(count = n()), 
       aes(occupation, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[14, ] <- c("occupation", "Household Respondent occupation", "ordinal", "10", FALSE, "Top three occupations are in agric, service work & craft trades")
access_clean <- cbind(access_clean, occupation = tibble(occupation))

#### 15. House type
hht <- ea$`1H. What type of building is the household residing in?`
class(hht)
sum(is.na(hht))
length(unique(hht))
tibble(hht) %>% group_by(hht) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2))

ggplot(tibble(hht) %>% group_by(hht) %>% summarise(count = n()), 
       aes(hht, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[15, ] <- c("hht", "Household Respondent house type", "ordinal", "5", FALSE, "Most respondents live in Flats and apartment blocks (Face to face)")
access_clean <- cbind(access_clean, hht= tibble(hht))

#### 15. Household size
hhs <- ea$`1I. What is the size of your household (number of individuals permanently living in the household, including yourself)`
class(hhs)
sum(is.na(hhs))
length(unique(hhs))
tibble(hhs) %>% group_by(hhs) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2))

ggplot(tibble(hhs) %>% group_by(hhs) %>% summarise(count = n()), 
       aes(hhs, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[15, ] <- c("hhs", "Household size", "ordinal", "5", FALSE, "Most respondents household size is between 5-10")
access_clean <- cbind(access_clean, hhs= tibble(hhs))

#### 16. Home ownership
how <- ea$`1J. Is the house/housing unit owned or rented?`
class(how)
sum(is.na(how))
length(unique(how))
tibble(how) %>% group_by(how) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2))

ggplot(tibble(how) %>% group_by(how) %>% summarise(count = n()), 
       aes(how, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[16, ] <- c("how", "Home ownership", "ordinal", "2", FALSE, "Most respondents own (58%) the house they live in")
access_clean <- cbind(access_clean, how= tibble(how))

#### 17. Length of living in current house
llc <- ea$`1K. How long have you been residing in this house?`
class(llc)
sum(is.na(llc))
length(unique(llc))
tibble(llc) %>% group_by(llc) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2))

ggplot(tibble(llc) %>% group_by(llc) %>% summarise(count = n()), 
       aes(llc, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[17, ] <- c("llc", "Length of living in current house", "ordinal", "4", FALSE, "Most respondents have lived in their current home for more than 10 years (41%)")
access_clean <- cbind(access_clean, llc= tibble(llc))

#### 18. Primary source of income
psi <- ea$`1L. Primary source of income of the household?`
class(psi)
sum(is.na(psi))
length(unique(psi))
tibble(psi) %>% group_by(psi) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2))

ggplot(tibble(psi) %>% group_by(psi) %>% summarise(count = n()), 
       aes(psi, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[18, ] <- c("psi", "Primary source of income", "ordinal", "6", FALSE, "Most respondents are self employed (70%)")
access_clean <- cbind(access_clean, psi= tibble(psi))

#### 19. Estimated monthly income
emi <- ea$`1L(ii). What is your total monthly household income?, Range in ₦.`
class(emi)
sum(is.na(emi))
length(unique(emi))
tibble(emi) %>% group_by(emi) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2))

ggplot(tibble(emi) %>% group_by(emi) %>% summarise(count = n()), 
       aes(emi, count)) + geom_bar(stat="identity") + coord_flip()

emi <- emi %>% str_replace("N0 -  N50,000", "< N50,000")

access_dict[19, ] <- c("emi", "Estimate monthly income", "ordinal", "7", FALSE, "Most (74%) respondents earn between 50k - 250k monthly")
access_clean <- cbind(access_clean, emi= tibble(emi))

#### 20. Run business from home
hbr <- ea$`1N. Do you have a business you run in your household?`
class(hbr)
sum(is.na(hbr))
length(unique(hbr))
tibble(hbr) %>% group_by(hbr) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2))

ggplot(tibble(hbr) %>% group_by(hbr) %>% summarise(count = n()), 
       aes(hbr, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[20, ] <- c("hbr", "Run business from home", "binary", "2", FALSE, "Only aboyt 26% run businesses from home")
access_clean <- cbind(access_clean, hbr= tibble(hbr))

#### 21. Nature of business
nab <- ea$`1O. If yes, what is the nature of the business?`
class(nab)
sum(is.na(nab))
length(unique(nab))
tibble(nab) %>% group_by(nab) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(nab) %>% group_by(nab) %>% summarise(count = n()), 
       aes(nab, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[21, ] <- c("nab", "Nature of business", "ordinal", "27", TRUE, "Most people run retail store, farming and tailoring from their homes.")
access_clean <- cbind(access_clean, nab= tibble(nab))

#### 22. Employee size
emp <- ea$`1P. If yes, what is the size of the business (number of current employees who are not household members)`
class(emp)
sum(is.na(emp))
length(unique(emp))
tibble(emp) %>% group_by(emp) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(emp) %>% group_by(emp) %>% summarise(count = n()), 
       aes(emp, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[22, ] <- c("emp", "Employee size", "ordinal", "6", TRUE, "Most business have between 1 - 5 employees")
access_clean <- cbind(access_clean, emp= tibble(emp))

#### 23. Estimated monthly business revenue
brv <- ea$`1Q. If yes, what is the monthly revenue of the business`
class(brv)
sum(is.na(brv))
length(unique(brv))
tibble(brv) %>% group_by(brv) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(brv) %>% group_by(brv) %>% summarise(count = n()), 
       aes(brv, count)) + geom_bar(stat="identity") + coord_flip()

brv <- brv %>% str_replace("N0 -  N50,000", "< N50,000")

access_dict[23, ] <- c("brv", "Estimated monthly business revenue", "ordinal", "9", TRUE, "Most businesses generate between 0 - 100k monthly")
access_clean <- cbind(access_clean, brv= tibble(brv))

#### 24. Cooking fuel
cfl <- ea$`1R. What is your household’s primary cooking fuel?`
class(cfl)
sum(is.na(cfl))
length(unique(cfl))
tibble(cfl) %>% group_by(cfl) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(cfl) %>% group_by(cfl) %>% summarise(count = n()), 
       aes(cfl, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[24, ] <- c("cfl", "Primary cooking fuel", "ordinal", "7", FALSE, "Most housholds use either firewood (49%) of LP Gas (46%)")
access_clean <- cbind(access_clean, cfl= tibble(cfl))

#### 25. Mode of transport
mot <- ea$`1S. What is your household’s primary medium of transportation?`
class(mot)
sum(is.na(mot))
length(unique(mot))
tibble(mot) %>% group_by(mot) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(mot) %>% group_by(mot) %>% summarise(count = n()), 
       aes(mot, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[25, ] <- c("mot", "Mode of transport", "ordinal", "8", FALSE, "Most housholds use public transport (63%) & motorcycle (21%)")
access_clean <- cbind(access_clean, mot= tibble(mot))

#### 26. Electricity supply
esp <- ea$`2A. Do you get electricity supply from any source?`
class(esp)
sum(is.na(esp))
length(unique(esp))
tibble(esp) %>% group_by(esp) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(esp) %>% group_by(esp) %>% summarise(count = n()), 
       aes(esp, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[26, ] <- c("esp", "Access to electricity supply", "binary", "2", FALSE, "Most housholds (88%) have access to electricity supply.")
access_clean <- cbind(access_clean, esp= tibble(esp))

#### 27. Electric grid connection
egc <- ea$`2D(i). Are you connected to the grid?`
class(egc)
sum(is.na(egc))
length(unique(egc))
tibble(egc) %>% group_by(egc) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(egc) %>% group_by(egc) %>% summarise(count = n()), 
       aes(egc, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[27, ] <- c("egc", "Electric grid connection", "binary", "2", TRUE, "Most housholds that have electricity supply (86%) are connected to the grid.")
access_clean <- cbind(access_clean, egc= tibble(egc))

#### 28. Electric grid hours during day time (6am - 6pm)
eghd <- ea$`2G. During the day (6 am–6 pm), how many hours of electricity do you get from the primary sources?`
class(eghd)
sum(is.na(eghd))
length(unique(eghd))
tibble(eghd) %>% group_by(eghd) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(eghd) %>% group_by(eghd) %>% summarise(count = n()), 
       aes(eghd, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[28, ] <- c("eghd", "Electric grid hours during day time (6am - 6pm)", "ordinal", "6", TRUE, "Most housholds get more than 4 hours of grid supply during daytime.")
access_clean <- cbind(access_clean, eghd= tibble(eghd))

#### 29. Electric grid hours during night time (6pm - 6am)
eghn <- ea$`2H. At Night (6 pm–6 am), how many hours of electricity do you get from the primary sources?`
class(eghn)
sum(is.na(eghn))
length(unique(eghn))
tibble(eghn) %>% group_by(eghn) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(eghn) %>% group_by(eghn) %>% summarise(count = n()), 
       aes(eghn, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[29, ] <- c("eghn", "Electric grid hours during night time (6pm - 6am)", "ordinal", "6", TRUE, "Most housholds get more than 4 hours of grid supply during night time")
access_clean <- cbind(access_clean, eghn= tibble(eghn))

#### 30. Disruption for Electric grid
deg <- ea$`2I. Do you experience disruptions from your primary source of electricity supply?`
class(deg)
sum(is.na(deg))
length(unique(deg))
tibble(deg) %>% group_by(deg) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(deg) %>% group_by(deg) %>% summarise(count = n()), 
       aes(deg, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[30, ] <- c("deg", "Disruption from electric grid", "binary", "2", TRUE, "Most housholds (62%) experience disruption from grid supply")
access_clean <- cbind(access_clean, deg= tibble(deg))

#### 31. Disruption frequency
dfq <- ea$`2J. How often do you experience power outages?`
class(dfq)
sum(is.na(dfq))
length(unique(dfq))
tibble(dfq) %>% group_by(dfq) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(dfq) %>% group_by(dfq) %>% summarise(count = n()), 
       aes(dfq, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[31, ] <- c("dfq", "Disruption frequency from electric grid", "ordinal", "3", TRUE, "Most housholds (39%) experience daily disruption from grid supply")
access_clean <- cbind(access_clean, dfq= tibble(dfq))

#### 32. Restoration period
rpd <- ea$`2L. When the electricity is interrupted, how long does it typically take to be restored?`
class(rpd)
sum(is.na(rpd))
length(unique(rpd))
tibble(rpd) %>% group_by(rpd) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(rpd) %>% group_by(rpd) %>% summarise(count = n()), 
       aes(rpd, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[32, ] <- c("rpd", "Restoration period after disruption", "ordinal", "5", TRUE, "Most housholds (27%) don't get power restored for more than 4 hours")
access_clean <- cbind(access_clean, rpd= tibble(rpd))

#### 33. Payment frequency
pfq <- ea$`2N. How often do you pay for the primary source of energy?`
class(pfq)
sum(is.na(pfq))
length(unique(pfq))
tibble(pfq) %>% group_by(pfq) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(pfq) %>% group_by(pfq) %>% summarise(count = n()), 
       aes(pfq, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[33, ] <- c("pfq", "Electric payment frequency", "ordinal", "6", TRUE, "Most housholds (81%) pay their electric bill monthly")
access_clean <- cbind(access_clean, pfq= tibble(pfq))

#### 34. Electric spend
egs <- ea$`2P. how much do you pay for the primary source of electricity for the period mentioned earlier? (Exact Amount Specified n ₦)`
class(egs)
sum(is.na(egs))
length(unique(egs))
egs <- as.numeric(egs)
tibble(egs) %>% summarise(min = min(egs, na.rm=T), mean = mean(egs, na.rm=T), median = median(egs, na.rm=T), max = max(egs, na.rm=T))

ggplot(tibble(egs), aes(egs)) + geom_histogram()

access_dict[34, ] <- c("egs", "Estimated monthly electric spend", "ratio", "", TRUE, "Average monthly electric spend is 6,069 while maximum spend is about 1.2M")
access_clean <- cbind(access_clean, egs= tibble(egs))

#### 35. Grid electricity supply challenge
gec <- ea$`2R. If yes, what are the main challenges you face with your primary electricity source? (Outages, cost, low voltage, etc.)`
class(gec)
sum(is.na(gec))
length(unique(gec))
tibble(gec) %>% group_by(gec) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(gec) %>% group_by(gec) %>% summarise(count = n()), 
       aes(gec, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[35, ] <- c("gec", "Grid electricity supply challenge", "ordinal", "3", TRUE, "Most households (52%) have challenge with grid electric supply")
access_clean <- cbind(access_clean, gec= tibble(gec))

#### 36. Grid electricity supply quality
geq <- ea$`2U. How would you describe the quality of energy supply to your household`
class(geq)
sum(is.na(geq))
length(unique(geq))
tibble(geq) %>% group_by(geq) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(geq) %>% group_by(geq) %>% summarise(count = n()), 
       aes(geq, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[36, ] <- c("geq", "Grid electricity supply quality", "ordinal", "2", TRUE, "Most households (62%) have good quality of electricity supply")
access_clean <- cbind(access_clean, geq= tibble(geq))

#### 37. Grid electricity customer satisfaction
ges <- ea$`2M. On a scale of 1-5 how satisfied are you with the electricity supply from your primary source? (Rate on a scale of 1–5)`
class(ges)
sum(is.na(ges))
length(unique(ges))
tibble(ges) %>% group_by(ges) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(ges) %>% group_by(ges) %>% summarise(count = n()), 
       aes(ges, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[37, ] <- c("ges", "Grid electricity customer satisfaction", "ordinal", "5", TRUE, "Most households (27.5%) are satisfied with grid electricity supply")
access_clean <- cbind(access_clean, ges= tibble(ges))

#### 38. Grid electricity customer band
geb <- ea$`2S. if connected to grid, What is your electricity band?`
class(geb)
sum(is.na(geb))
length(unique(geb))
tibble(geb) %>% group_by(geb) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(geb) %>% group_by(geb) %>% summarise(count = n()), 
       aes(geb, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[38, ] <- c("geb", "Grid electricity customer band", "ordinal", "6", TRUE, "Most households (30%) are on Band E while only 4% are on Band A")
access_clean <- cbind(access_clean, geb= tibble(geb))

#### 39. Grid electricity connection timeline
get <- ea$`2V. When did you first get connected to electricity`
class(get)
sum(is.na(get))
length(unique(get))
tibble(get) %>% group_by(get) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(get) %>% group_by(get) %>% summarise(count = n()), 
       aes(get, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[39, ] <- c("get", "Grid electricity connection timeline", "ordinal", "5", TRUE, "Most households (30%) are on Band E while only 4% are on Band A")
access_clean <- cbind(access_clean, get= tibble(get))

#### 40. Household appliances
hap <- ea$`2X. Which appliances do you have now?`
class(hap)
sum(is.na(hap))
length(unique(hap))
tibble(hap) %>% group_by(hap) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(hap) %>% group_by(hap) %>% summarise(count = n()), 
       aes(hap, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[40, ] <- c("hap", "List of household appliances", "ordinal", "", TRUE, "")
access_clean <- cbind(access_clean, hap= tibble(hap))

#### 41. Alternative electric source ownership
aeo <- ea$`3A. Do you have any backup/alternate power sources (e.g., generator, solar)?`
class(aeo)
sum(is.na(aeo))
length(unique(aeo))
tibble(aeo) %>% group_by(aeo) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(aeo) %>% group_by(aeo) %>% summarise(count = n()), 
       aes(aeo, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[41, ] <- c("aeo", "Alternative electric source ownership", "binary", "", TRUE, "Only about 16% of households own alternative electric source")
access_clean <- cbind(access_clean, aeo= tibble(aeo))

#### 42. Alternative electric source type
aet <- ea$`3B. If yes, What alternate power supply do you use?`
class(aet)
sum(is.na(aet))
length(unique(aet))
tibble(aet) %>% group_by(aet) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(aet) %>% group_by(aet) %>% summarise(count = n()), 
       aes(aet, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[42, ] <- c("aet", "Alternative electric source type", "ordinal", "3", TRUE, "Most households (14%) use generator as alternative source.")
access_clean <- cbind(access_clean, aet= tibble(aet))

#### 43. Alternative generator daily hours
agd <- ea$`3C. how many hours per day do use the alternate source of electricity? (Single choice)`
class(agd)
sum(is.na(agd))
length(unique(agd))
tibble(agd) %>% group_by(agd) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(agd) %>% group_by(agd) %>% summarise(count = n()), 
       aes(agd, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[43, ] <- c("agd", "Alternative electric daily hours", "ordinal", "5", TRUE, "Most households (15%) use generator for between 0 - 8 hours daily")
access_clean <- cbind(access_clean, agd= tibble(agd))

#### 44. Alternative generator payment frequency
agf <- ea$`3D. If yes, How often do you pay for alternate source of energy? i.e petrol/diesel etc`
class(agf)
sum(is.na(agf))
length(unique(agf))
tibble(agf) %>% group_by(agf) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(agf) %>% group_by(agf) %>% summarise(count = n()), 
       aes(agf, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[44, ] <- c("agf", "Alternative electric payment frequency", "ordinal", "5", TRUE, "Most households (11%) buy generator fuel on daily & weekly basis")
access_clean <- cbind(access_clean, agf= tibble(agf))

#### 45. Alternative generator spend
ags <- ea$`3F how much do you pay for the alternate source of electricity for the period mentioned earlier? Exact Amount Specified in local currency.`
class(ags)
sum(is.na(ags))
length(unique(ags))
ags <- as.numeric(ags)
tibble(ags) %>% summarise(min = min(ags, na.rm=T), mean = mean(ags, na.rm=T), median = median(ags, na.rm=T), max = max(ags, na.rm=T))

ggplot(tibble(ags), aes(ags)) + geom_histogram()

access_dict[45, ] <- c("ags", "Alternative electric spend", "ratio", "", TRUE, "Average monthly generator spend is 6,375 while maximum spend is about 250k")
access_clean <- cbind(access_clean, ags= tibble(ags))

#### 46. Alternative generator supply challenge
agc <- ea$`3G. Do you face any challenges with your alternate electricity source?`
class(agc)
sum(is.na(agc))
length(unique(agc))
tibble(agc) %>% group_by(agc) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(agc) %>% group_by(agc) %>% summarise(count = n()), 
       aes(agc, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[46, ] <- c("agc", "Alternative electric supply challenge", "binary", "2", TRUE, "Most households (9%) don't have any challenge with their alternative generator supply")
access_clean <- cbind(access_clean, agc= tibble(agc))

#### 47. Alternative solar capacity
asc <- ea$`3B(ii) - What is the Capacity of the Solar System? Range in KVA`
class(asc)
sum(is.na(asc))
length(unique(asc))
tibble(asc) %>% group_by(asc) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(asc) %>% group_by(asc) %>% summarise(count = n()), 
       aes(asc, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[47, ] <- c("asc", "Alternative solar capacity", "ordinal", "9", TRUE, "Most households (2%) have solar system capacity of between 0 - 4 kVA")
access_clean <- cbind(access_clean, asc= tibble(asc))

#### 48. Alternative solar spend
ass <- ea$`3B(vi) - How much did you buy the solar system? Specify In Naira`
class(ass)
sum(is.na(ass))
length(unique(ass))
ass <- as.numeric(ass)
tibble(ass) %>% summarise(min = min(ass, na.rm=T), mean = mean(ass, na.rm=T), median = median(ass, na.rm=T), max = max(ass, na.rm=T))

ggplot(tibble(ass), aes(ass)) + geom_histogram()

access_dict[48, ] <- c("ass", "Alternative solar capacity", "ratio", "", TRUE, "Average solar system capex is 775,014 while maximum spend is about 70M")
access_clean <- cbind(access_clean, ass= tibble(ass))

#### 49. Reason for no electric supply
res <- ea$`2Y. If no access to power, Why?`
class(res)
sum(is.na(res))
length(unique(res))
tibble(res) %>% group_by(res) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(res) %>% group_by(res) %>% summarise(count = n()), 
       aes(res, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[49, ] <- c("res", "Reason for no electric supply", "ordinal", "3", TRUE, "")
access_clean <- cbind(access_clean, res= tibble(res))

#### 50. Metering
met <- ea$`5D. Are you metered?`
class(met)
sum(is.na(met))
length(unique(met))
tibble(met) %>% group_by(met) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(met) %>% group_by(met) %>% summarise(count = n()), 
       aes(met, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[50, ] <- c("met", "Household metered", "ordinal", "3", TRUE, "48% of households are metered either with prepaid or postpaid.")
access_clean <- cbind(access_clean, met= tibble(met))

#### 50. Consideration to switch to renewable
csr <- ea$`7B. Would you consider switching to renewable energy if it were affordable?`
class(csr)
sum(is.na(csr))
length(unique(csr))
tibble(csr) %>% group_by(csr) %>% summarise(count = n()) %>% mutate(per = round((count/sum(count))*100, 2)) %>% arrange(desc(count))

ggplot(tibble(csr) %>% group_by(csr) %>% summarise(count = n()), 
       aes(csr, count)) + geom_bar(stat="identity") + coord_flip()

access_dict[51, ] <- c("csr", "Consideration to switch to renewable", "binary", "2", TRUE, "64% of households are willing to switch if renewable energy is affordable to them")
access_clean <- cbind(access_clean, csr= tibble(csr))

write_csv(access_dict, "data/clean/access_dict.csv")
write_csv(access_clean, "data/clean/access.csv")

# ###### General clean up to align with sample method
# sample_urban <- subset(sample, select=c(`Region`, `State`, `% of population in urban areas`))
# sample_urban <- sample_urban[1:nrow(sample_urban)-1, ]
# names(sample_urban) <- c("region", "state", "urban_per")
# sample_urban$state <- sample_urban$state %>% str_replace(" State", "")
# sample_urban$state <- sample_urban$state %>% str_replace("Akwa Ibom", "Akwa-Ibom")
# sample_urban$state <- sample_urban$state %>% str_replace("Cross River", "Cross-River")
# sample_urban$state <- sample_urban$state %>% str_replace("Federal Capital Territory", "FCT/Abuja")
# 
# nesip_urban <- subset(access_clean %>% group_by(state, area) %>% summarise(count = n()) %>% mutate(per = (count/sum(count))*100), area == "Urban", select=c(state, count, per))
# 
# urban_fct <- left_join(sample_urban, nesip_urban) %>% mutate(urban_per = urban_per/100, per = per/100)
# 
# urban_fct <- urban_fct %>% group_by(state) %>% mutate(count_a = ceiling(((urban_per*count)-(urban_per*per*count))/(per*(1-urban_per))))
# 
# nesip_rural <- subset(access_clean %>% group_by(state, area) %>% summarise(count_r = n()) %>% mutate(per_r = (count_r/sum(count_r))*100), area == "Rural", select=c(state, count_r, per_r))
# 
# area_fct <- left_join(urban_fct, nesip_rural) %>% mutate(total = count_a + count_r, per_ax = count_a/total)
# 
# urban_fct <- urban_fct %>% group_by(state) %>% mutate(u_diff = count - count_a)

