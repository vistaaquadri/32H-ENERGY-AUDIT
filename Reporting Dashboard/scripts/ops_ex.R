
##### South South #####
ss_sample <- subset(sample, Region == "South south", select=c(`Region`, `State`, `Local government area`, `Optimised Sample_household`))
names(ss_sample) <- c('region', 'state', 'lga_ex', 'count_ex')
ss_sample$state <- ss_sample$state %>% str_replace(" State", "")
ss_sample$state <- ss_sample$state %>% str_replace("Akwa Ibom", "Akwa-Ibom")

ss_raw <- ss %>% group_by(`State`, `LGA`) %>% summarise(count = n())
ss_raw <- ss_raw %>% group_by(`State`) %>% summarise(lga_rw = n(), count_rw = sum(count))
names(ss_raw) <- c('state', 'lga_rw', 'count_rw')
ss_raw$state <- ss_raw$state %>% str_replace("River's", "Rivers")

ss_check <- left_join(ss_sample, ss_raw)

ss_pass <- subset(nesip_pass, `State` %in% c("Akwa-Ibom", "Bayelsa", "Cross River", "Delta", "Ebonyi", "Edo", "Rivers"))
ss_pass <- ss_pass %>% group_by(`State`, `LGA`) %>% summarise(count = n())
ss_pass <- ss_pass %>% group_by(`State`) %>% summarise(lga_ps = n(), count_ps = sum(count))
names(ss_pass) <- c('state', 'lga_ps', 'count_ps')

ss_check <- left_join(ss_check, ss_pass)

ss_check <- ss_check %>% group_by(state) %>% mutate(per = round((count_ps/count_ex),2)*100,
                                                    err = round(((count_rw - count_ps)/count_rw),2)*100)


##### South West #####
sw_sample <- subset(sample, Region == "South west", select=c(`Region`, `State`, `Local government area`, `Optimised Sample_household`))
names(sw_sample) <- c('region', 'state', 'lga_ex', 'count_ex')
sw_sample$state <- sw_sample$state %>% str_replace(" State", "")

sw_raw <- sw %>% group_by(`State`, `LGA`) %>% summarise(count = n())
sw_raw <- sw_raw %>% group_by(`State`) %>% summarise(lga_rw = n(), count_rw = sum(count))
names(sw_raw) <- c('state', 'lga_rw', 'count_rw')

sw_check <- left_join(sw_sample, sw_raw)

sw_pass <- subset(nesip_pass, `State` %in% c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo"))
sw_pass <- sw_pass %>% group_by(`State`, `LGA`) %>% summarise(count = n())
sw_pass <- sw_pass %>% group_by(`State`) %>% summarise(lga_ps = n(), count_ps = sum(count))
names(sw_pass) <- c('state', 'lga_ps', 'count_ps')

sw_check <- left_join(sw_check, sw_pass)

sw_check <- sw_check %>% group_by(state) %>% mutate(per = round((count_ps/count_ex),2)*100,
                                                    err = round(((count_rw - count_ps)/count_rw),2)*100)


##### South East #####
se_sample <- subset(sample, Region == "South east", select=c(`Region`, `State`, `Local government area`, `Optimised Sample_household`))
names(se_sample) <- c('region', 'state', 'lga_ex', 'count_ex')
se_sample$state <- se_sample$state %>% str_replace(" State", "")

se_raw <- se %>% group_by(`State`, `LGA`) %>% summarise(count = n())
se_raw <- se_raw %>% group_by(`State`) %>% summarise(lga_rw = n(), count_rw = sum(count))
names(se_raw) <- c('state', 'lga_rw', 'count_rw')

se_check <- left_join(se_sample, se_raw)

se_pass <- subset(nesip_pass, `State` %in% c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"))
se_pass <- se_pass %>% group_by(`State`, `LGA`) %>% summarise(count = n())
se_pass <- se_pass %>% group_by(`State`) %>% summarise(lga_ps = n(), count_ps = sum(count))
names(se_pass) <- c('state', 'lga_ps', 'count_ps')

se_check <- left_join(se_check, se_pass)

se_check <- se_check %>% group_by(state) %>% mutate(per = round((count_ps/count_ex),2)*100,
                                                    err = round(((count_rw - count_ps)/count_rw),2)*100)


##### North East #####
ne_sample <- subset(sample, Region == "North east", select=c(`Region`, `State`, `Local government area`, `Optimised Sample_household`))
names(ne_sample) <- c('region', 'state', 'lga_ex', 'count_ex')
ne_sample$state <- ne_sample$state %>% str_replace(" State", "")

ne_raw <- ne %>% group_by(`State`, `LGA`) %>% summarise(count = n())
ne_raw <- ne_raw %>% group_by(`State`) %>% summarise(lga_rw = n(), count_rw = sum(count))
names(ne_raw) <- c('state', 'lga_rw', 'count_rw')

ne_check <- left_join(ne_sample, ne_raw)

ne_pass <- subset(nesip_pass, `State` %in% c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe"))
ne_pass <- ne_pass %>% group_by(`State`, `LGA`) %>% summarise(count = n())
ne_pass <- ne_pass %>% group_by(`State`) %>% summarise(lga_ps = n(), count_ps = sum(count))
names(ne_pass) <- c('state', 'lga_ps', 'count_ps')

ne_check <- left_join(ne_check, ne_pass)

ne_check <- ne_check %>% group_by(state) %>% mutate(per = round((count_ps/count_ex),2)*100,
                                                    err = round(((count_rw - count_ps)/count_rw),2)*100)


##### North West #####
nw_sample <- subset(sample, Region == "North west", select=c(`Region`, `State`, `Local government area`, `Optimised Sample_household`))
names(nw_sample) <- c('region', 'state', 'lga_ex', 'count_ex')
nw_sample$state <- nw_sample$state %>% str_replace(" State", "")

nw_raw <- nw %>% group_by(`State`, `LGA`) %>% summarise(count = n())
nw_raw <- nw_raw %>% group_by(`State`) %>% summarise(lga_rw = n(), count_rw = sum(count))
names(nw_raw) <- c('state', 'lga_rw', 'count_rw')
nw_raw$state <- nw_raw$state %>% str_replace("Kastina", "Katsina")

nw_check <- left_join(nw_sample, nw_raw)

nw_pass <- subset(nesip_pass, `State` %in% c("Jigawa", "Kaduna", "Kano", "Kastina", "Kebbi", "Sokoto", "Zamfara"))
nw_pass <- nw_pass %>% group_by(`State`, `LGA`) %>% summarise(count = n())
nw_pass <- nw_pass %>% group_by(`State`) %>% summarise(lga_ps = n(), count_ps = sum(count))
names(nw_pass) <- c('state', 'lga_ps', 'count_ps')
nw_pass$state <- nw_pass$state %>% str_replace("Kastina", "Katsina")

nw_check <- left_join(nw_check, nw_pass)

nw_check <- nw_check %>% group_by(state) %>% mutate(per = round((count_ps/count_ex),2)*100,
                                                    err = round(((count_rw - count_ps)/count_rw),2)*100)


##### North Central #####
nc_sample <- subset(sample, Region == "North central", select=c(`Region`, `State`, `Local government area`, `Optimised Sample_household`))
names(nc_sample) <- c('region', 'state', 'lga_ex', 'count_ex')
nc_sample$state <- nc_sample$state %>% str_replace(" State", "")

nc_raw <- nc %>% group_by(`State`, `LGA`) %>% summarise(count = n())
nc_raw <- nc_raw %>% group_by(`State`) %>% summarise(lga_rw = n(), count_rw = sum(count))
names(nc_raw) <- c('state', 'lga_rw', 'count_rw')

nc_check <- left_join(nc_sample, nc_raw)

nc_pass <- subset(nesip_pass, `State` %in% c("Benue", "Federal Capital Territory", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau"))
nc_pass <- nc_pass %>% group_by(`State`, `LGA`) %>% summarise(count = n())
nc_pass <- nc_pass %>% group_by(`State`) %>% summarise(lga_ps = n(), count_ps = sum(count))
names(nc_pass) <- c('state', 'lga_ps', 'count_ps')
nc_pass$state <- nc_pass$state %>% str_replace("Kastina", "Katsina")

nc_check <- left_join(nc_check, nc_pass)

nc_check <- nc_check %>% group_by(state) %>% mutate(per = round((count_ps/count_ex),2)*100,
                                                    err = round(((count_rw - count_ps)/count_rw),2)*100)

nesip_check <- rbind(ss_check, sw_check, se_check, ne_check, nw_check, nc_check)

write_csv(nesip_check, "data/check.csv")