# library load and essentials ---------------------------------------------

pacman::p_load(foreign, stringr, ggplot2, dplyr, plyr,
               lubridate, MASS, scales)
rm(list = ls(all.names = TRUE))
gc(full = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
if (!is.null(dev.list()))
  dev.off()

# read data ---------------------------------------------------------------

deaths <- read.csv("../deaths/deaths08.csv.bz2", header = TRUE)
codes <- read.csv("../disease/icd-main.csv", header = TRUE)
locations <-
  read.csv("../locations/locations.csv.bz2", header = TRUE)
states <- read.csv("../maps/state-map.csv.bz2", header = TRUE)
daily <-
  read.csv("../weather-simat2/weather-daily.csv", header = TRUE)


# create cause data frame -------------------------------------------------

#count causes of death
cause = count(deaths, "cod")
names(cause)[1] = "code"
cause$disease = with(cause, codes$disease[match(code, codes$code)])


# top 20 diseases ---------------------------------------------------------

cause %>% na.omit() %>% head(., 20) %>%
  ggplot(., aes(freq / 1e4, reorder(disease, freq))) +
  geom_point(aes(color = freq / 1e4) , alpha = 0.6, size = 7) +
  scale_x_log10("Log (freq per 10,000 deaths)") +
  ylab("Disease") +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )
ggsave("../plots/top20diseases.png", 
       width = 344, height = 193, units = "mm",dpi = 320)


# Deaths by hour visualization --------------------------------------------

deaths$hod[deaths$hod == 99] = NA
deaths$hod[deaths$hod == 24] = NA

deaths %>% count("hod") %>% na.omit() %>%
  ggplot(., aes(x = hod, y = freq)) +
  geom_line() +
  scale_x_continuous("Hour of Death", breaks = seq(0, 24, 2)) +
  scale_y_continuous("Number of deaths",
                     labels = unit_format(scale = 1e-3, unit = "k")) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )

ggsave("../plots/Deaths_by_hour.png", 
       width = 344, height = 193, units = "mm",dpi = 320)

# Segregating deaths by hour and classify in disease ----------------------

#count unique combinations of cod and hod in deaths table
hod2 = count(deaths, c("cod", "hod"))

#calculate hourly proprtions of different "cod's"
#divide bu sum to standardize
hod2 = ddply(hod2, "cod", mutate, prop = freq / sum(freq))


#overall hourly trends
overall = ddply(hod2, "hod", summarise, freq_all = sum(freq))

#divide by sum of frequencies to standardize
overall = mutate(overall, prop_all = freq_all / sum(freq_all))


#join overall and hod2 to be able to compute MSE
hod2 = join(overall, hod2, by = "hod")
hod2 = merge(hod2, codes, by.x = "cod", by.y = "code")

#compute number of deaths by hour and their MSE
devi = ddply(hod2,
             "cod",
             summarise,
             n = sum(freq),
             dist = mean((prop - prop_all)^2))

#focusing on causes of deaths with more than 50 deaths in the whole
# deaths table

devi = subset(devi, n > 50)
ggplot(devi,aes(x= n,y=dist))+
  geom_point()
ggsave("../plots/death_vis.png", 
       width = 344, height = 193, units = "mm",dpi = 320)

ggplot(devi , aes(log(n), log(dist))) + 
  geom_point()+
  geom_smooth(method ='rlm')

ggsave("../plots/log_scale.png", 
       width = 344, height = 193, units = "mm",dpi = 320)

devi$resid = resid(rlm(log(dist) ~ log(n), data = devi))

unusual = subset(devi, resid >1.5)

hod_unusual_big = match_df(hod2,subset(unusual, n>350))
hod_unusual_sml = match_df(hod2,subset(unusual, n<=350))

ggplot(data = hod_unusual_sml, aes(x=hod, y=prop))+
  geom_line() + 
  geom_line(aes(y=prop_all), alpha=1/3) +
  scale_x_continuous("Hour of Death", breaks = seq(0, 24, 2)) +
  scale_y_continuous("Proportion of deaths") +
  facet_wrap(vars(disease))
ggsave("../plots/death_small.png", 
       width = 344, height = 193, units = "mm",dpi = 320)


ggplot(data = hod_unusual_big, aes(x=hod, y=prop))+
  geom_line() + 
  geom_line(aes(y=prop_all), alpha=1/3) +
  scale_x_continuous("Hour of Death", breaks = seq(0, 24, 2)) +
  scale_y_continuous("Proportion of deaths") +
  facet_wrap(vars(disease))

ggsave("../plots/death_big.png", 
       width = 344, height = 193, units = "mm",dpi = 320)


# Monthly trend visualization

deaths %>% 
  filter(yod == 2008 & mod <=11) %>% 
  mutate(dateod = ymd(str_c(yod,mod,dod, sep = "-"))) %>% 
  count("dateod") %>% 
  na.omit() %>% 
  ggplot(.,aes(dateod, freq)) +
  geom_line() +
  scale_x_date(name = NULL , date_breaks = "1 month",
                            date_labels = "%b %y")+
  ylab("Frequency of Deaths")+
  geom_smooth()

ggsave("../plots/monthly_trend.png", 
       width = 344, height = 193, units = "mm",dpi = 320)
# Prepare "deaths" dataframe for map visualization

deaths$loc = with(deaths, str_c(statD, countyD, locationD, sep = "-"))
deaths$long = with(locations, long[match(deaths$loc, id)])
deaths$lat = with(locations, lat[match(deaths$loc, id)])

deaths["loc"] = list(NULL)
deaths = na.omit(deaths)

#Map visualization

deaths %>% count(.,c("long", "lat")) %>% 
  ggplot(.,aes(x=long, y=lat)) +
  geom_polygon(aes(group = states$group),
               data = states,
               color = "white",
               fill = "grey90") +
  xlab("")+
  ylab("")+
  geom_point(aes(size = freq), alpha = 0.2) +
  scale_size_area(breaks = c(1,500,1000,5000,10000))+
  coord_map()

ggsave("../plots/map.png", 
       width = 344, height = 193, units = "mm",dpi = 320)

#weather visualization

#data preparation

deaths_2008 = deaths[which(deaths$yod == 2008 & deaths$mod!=0 &
                             deaths$dod !=0), ]
deaths_2008 = mutate(deaths_2008, 
                     dateod = ymd(str_c(yod, mod, dod, sep = "-")))

daily = mutate(daily , day =ymd(daily$day))
deaths_2008$disease = with(codes, disease[match(deaths_2008$cod, code)])
deaths_2008$temp_min = with(daily, temp_min[match(deaths_2008$dateod,day)])
deaths_2008$temp_max = with(daily, temp_max[match(deaths_2008$dateod,day)])
deaths_2008$wind = with(daily, wind[match(deaths_2008$dateod,day)])


# min temp. ---------------------------------------------------------------

deaths_2008 %>% 
  count("temp_min") %>% 
  na.omit() %>% 
  ggplot(.,aes(temp_min,freq)) +
  geom_point()+
  scale_x_continuous()+
  geom_smooth()
ggsave("../plots/min_temp1.png", 
       width = 344, height = 193, units = "mm",dpi = 320)


deaths_2008 %>% 
  group_by(dateod,temp_min) %>% 
  tally(name ="freq") %>% 
  filter(freq>=1200) %>% 
  na.omit() %>% 
  ggplot(.,aes(temp_min,freq)) + 
  geom_point()+
  scale_x_continuous()+
  geom_smooth()
ggsave("../plots/min_temp2.png", 
       width = 344, height = 193, units = "mm",dpi = 320)


# max temp. ---------------------------------------------------------------

deaths_2008 %>% 
  count("temp_max") %>% 
  na.omit() %>% 
  ggplot(.,aes(temp_max,freq)) +
  geom_point()+
  scale_x_continuous("Maximum Temp.")+
  scale_y_continuous("Frequency of deaths")+
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )+
  geom_smooth()
ggsave("../plots/max_temp1.png", 
       width = 344, height = 193, units = "mm",dpi = 320)



deaths_2008 %>% 
  group_by(dateod,temp_max) %>% 
  tally(name ="freq") %>% 
  filter(freq>=1200) %>% 
  na.omit() %>% 
  ggplot(.,aes(temp_max,freq)) + 
  geom_point()+
  scale_x_continuous("Maximum Temp.")+
  scale_y_continuous("Frequency of deaths")+
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )+
  geom_smooth()
ggsave("../plots/max_temp2.png", 
       width = 344, height = 193, units = "mm",dpi = 320)


# wind -------------------------------------------------------------------

deaths_2008 %>% 
  count("wind") %>% 
  na.omit() %>% 
  ggplot(.,aes(wind,freq)) +
  geom_point()+
  scale_x_continuous(name = "Wind Speed" ,breaks = seq(0,4,0.5))+
  scale_y_continuous("Frequency of deaths",
                     labels = unit_format(scale = 1e-3, unit = "k")) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )+
  geom_smooth()
ggsave("../plots/wind1.png", 
       width = 344, height = 193, units = "mm",dpi = 320)


deaths_2008 %>% 
  group_by(dateod,wind) %>% 
  tally(name ="freq") %>% 
  filter(freq>=1200) %>% 
  na.omit() %>% 
  ggplot(.,aes(wind,freq)) + 
  geom_point()+
  scale_x_continuous(name = "Wind Speed" ,breaks = seq(0,4,0.5))+
  scale_y_continuous("Frequency of deaths",
                     labels = unit_format(scale = 1e-3, unit = "k")) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )+
  geom_smooth()
ggsave("../plots/wind2.png", 
       width = 344, height = 193, units = "mm",dpi = 320)


# top disease weather visualization ---------------------------------------

deaths_2008 %>% 
  group_by(dateod, temp_min, disease) %>% 
  tally(name = "freq") %>% 
  na.omit() %>% 
  ddply(.,
        "disease", mutate,
        prop = freq / sum(freq),
        tot_freq = sum(freq)) %>% 
  filter(tot_freq %in% tail(sort(unique(tot_freq)) ,9)) %>% 
  filter (prop >=.001) %>% 
  ggplot(., aes(x=temp_min,y=prop)) +
    geom_point(alpha =0.5) +
    geom_smooth(se = TRUE, size =1)+
    facet_wrap(vars(disease), labeller = label_wrap_gen(multi_line = T)) +
  scale_x_continuous(name = "Min Temp." ,breaks = seq(0,20,2))+
  scale_y_continuous("Proportion of deaths")+
  theme(strip.text.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

ggsave("../plots/disease_w.png", 
       width = 344, height = 193, units = "mm",dpi = 320)
