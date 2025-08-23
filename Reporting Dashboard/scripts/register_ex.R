

## Dict & Clean DF setup
register_dict <- data.frame(code = c(""), desc = c(""), type = c(""), levels = c(""), missing = c(""), insight = c(""))[-1, ]

#### 1. level
level <- rg$`Level`
class(level)
sum(is.na(level))
length(unique(level))
tibble(level) %>% group_by(level) %>% summarise(count = n()) %>% arrange(desc(count))

level <- as.factor(level)

ggplot(tibble(level) %>% group_by(level) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(level, count)) + 
  geom_bar(stat='identity')

register_dict[1, ] <- c("level", "National level of governance", "ordinal", "4", FALSE, "State has highest engagement 100 stakeholders")
register_clean <- tibble(level)

#### 2. Category
category <- rg$`Category`
class(category)
sum(is.na(category))
length(unique(category))
tibble(category) %>% group_by(category) %>% summarise(count = n()) %>% arrange(desc(count))

category <- as.factor(category)

ggplot(tibble(category) %>% group_by(category) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(category, count)) + 
  geom_bar(stat='identity') + coord_flip()

register_dict[2, ] <- c("category", "Archetypes of stakeholder categories", "ordinal", "11", FALSE, "State is the highest archetype of stakeholders engaged so far")
register_clean <- cbind(register_clean, tibble(category))

#### 3. Name
name <- rg$`Stakeholder Name`
class(name)
sum(is.na(name))
length(unique(name))
tibble(name) %>% group_by(name) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(tibble(name) %>% group_by(name) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(name, count)) + 
  geom_bar(stat='identity') + coord_flip()

register_dict[3, ] <- c("name", "Stakeholder name", "norminal", "4", TRUE, "")
register_clean <- cbind(register_clean, tibble(name))

#### 4. State
state <- rg$`State`
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

register_dict[4, ] <- c("state", "Stakeholder state", "ordinal", "48", TRUE, "Lagos has highest no of stakerholders engaged")
register_clean <- cbind(register_clean, tibble(state))

#### 5. region *****
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
  if(!is.na(state[s])){
    region[s] <- subset(region_df, states == str_split(state[s], ",")[[1]][1])$region
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

register_dict[5, ] <- c("region", "Region", "ordinal", "6", FALSE, "South west has highest 1,889 obs, North east with the least 857")
register_clean <- cbind(register_clean, tibble(region))

#### 5. Organisation
organisation <- rg$`Organisation`
class(organisation)
sum(is.na(organisation))
length(unique(organisation))
tibble(organisation) %>% group_by(organisation) %>% summarise(count = n()) %>% arrange(desc(count))

organisation <- as.factor(organisation)

ggplot(tibble(organisation) %>% group_by(organisation) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(organisation, count)) + 
  geom_bar(stat='identity') + coord_flip()

register_dict[6, ] <- c("organisation", "Stakeholder organisation", "ordinal", "108", TRUE, "JEDC has highest no of stakeholders engaged")
register_clean <- cbind(register_clean, tibble(organisation))

#### 6. Influence
influence <- rg$`Influence`
class(influence)
sum(is.na(influence))
length(unique(influence))
tibble(influence) %>% group_by(influence) %>% summarise(count = n()) %>% arrange(desc(count))

influence <- as.factor(influence)

ggplot(tibble(influence) %>% group_by(influence) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(influence, count)) + 
  geom_bar(stat='identity') + coord_flip()

register_dict[7, ] <- c("influence", "Stakeholder influence on nesip", "ordinal", "108", TRUE, "JEDC has highest no of stakeholders engaged")
register_clean <- cbind(register_clean, tibble(influence))

#### 7. Importance
importance <- rg$`Importance`
class(importance)
sum(is.na(importance))
length(unique(importance))
tibble(importance) %>% group_by(importance) %>% summarise(count = n()) %>% arrange(desc(count))

importance <- as.factor(importance)

ggplot(tibble(importance) %>% group_by(importance) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(importance, count)) + 
  geom_bar(stat='identity') + coord_flip()

register_dict[8, ] <- c("importance", "Stakeholder importance on nesip", "ordinal", "108", TRUE, "JEDC has highest no of stakeholders engaged")
register_clean <- cbind(register_clean, tibble(importance))

#### 8. role
role <- rg$`Engagement Level`
class(role)
sum(is.na(role))
length(unique(role))
tibble(role) %>% group_by(role) %>% summarise(count = n()) %>% arrange(desc(count))

role <- as.factor(role)

ggplot(tibble(role) %>% group_by(role) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(role, count)) + 
  geom_bar(stat='identity') + coord_flip()

register_dict[9, ] <- c("role", "Stakeholder role on nesip", "ordinal", "5", TRUE, "JEDC has highest no of stakeholders engaged")
register_clean <- cbind(register_clean, tibble(role))

#### 9. Phone
phone <- rg$`Phone`
class(phone)
sum(is.na(phone))
length(unique(phone))
tibble(phone) %>% group_by(phone) %>% summarise(count = n()) %>% arrange(desc(count))

register_dict[10, ] <- c("phone", "Stakeholder contact phone no", "nominal", "", TRUE, "JEDC has highest no of stakeholders engaged")
register_clean <- cbind(register_clean, tibble(phone))

#### 10. Email
email <- rg$`Email`
class(email)
sum(is.na(email))
length(unique(email))
tibble(email) %>% group_by(email) %>% summarise(count = n()) %>% arrange(desc(count))

register_dict[11, ] <- c("email", "Stakeholder contact email", "nominal", "", TRUE, "JEDC has highest no of stakeholders engaged")
register_clean <- cbind(register_clean, tibble(email))

#### 11. status
status <- rg$`Status`
class(status)
sum(is.na(status))
length(unique(status))
tibble(status) %>% group_by(status) %>% summarise(count = n()) %>% arrange(desc(count))

status <- status %>% str_replace("Not Engaged", "Desk Research")
status <- status %>% str_replace("Engagement Request Rejected", "Desk Research")
status <- status %>% str_replace_all("No Response", "Desk Research")
status <- status %>% str_replace_all("Unavailable for Engagement", "Desk Research")
status <- status %>% str_replace("REA Letter Acknowledged", "Engagement Initiated")
status <- status %>% str_replace("Conversation Initiated", "Engagement Initiated")
status <- status %>% str_replace("Meeting Scheduled", "Engagement Initiated")
status <- status %>% str_replace("Not Interviewed. Questionnaire sent", "Engagement Ongoing")
status <- status %>% str_replace("Virtual Discusssion Invitation Sent", "Engagement Ongoing")
status <- status %>% str_replace("Questionnaire Follow Up", "Engagement Ongoing")
status <- status %>% str_replace(c("Engagement Completed"), "Engagement Completed")

status <- as.factor(status)

ggplot(tibble(status) %>% group_by(status) %>% summarise(count = n()) %>% arrange(desc(count)), 
       aes(status, count)) + 
  geom_bar(stat='identity') + coord_flip()

register_dict[12, ] <- c("status", "Stakeholder current engagement status", "ordinal", "12", TRUE, "JEDC has highest no of stakeholders engaged")
register_clean <- cbind(register_clean, tibble(status))



