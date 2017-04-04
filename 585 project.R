library(tidyverse)
library(rvest)
library(maptools)

## source: https://catalog.data.gov/dataset/county-level-data-sets

dat <- readxl::read_excel("Education.xls", skip = 4)
#tidy data
tidy = dat %>% gather(key = education, value = value, 8:ncol(dat)) %>%
  rename(FIPS = `FIPS Code`, Area = `Area name`) %>%
  separate(education, into = c("level", "year"), sep = "\\,") %>% select(-c(4:7)) %>%
  mutate(
    year = trimws(year),
    FIPS = parse_number(FIPS),
    type = "count",
    type = replace(type, grep("Percent.*", level), "percent")
  )
#check completeness for education levels
table(tidy$level)
tidy %>% group_by(FIPS, level) %>% tally() %>% arrange(n)  # should be 5 times
# make levels consistent
level = tidy$level
level = gsub("Percent of adults (completing |with [a ]?)", "", level)
level = gsub("[Ff]our years of college", "Bachelor's degree", level)
level = gsub("^[Ss](.*)or associate's degree", "S\\1(1-3 years)", level)
level = gsub("(^[a-z])", "\\U\\1", trimws(level), perl = T)
tidy$level = level
table(tidy$level)
#check correctness of percentage

wide = tidy %>% spread(key = type, value = value)

wide %>% group_by(FIPS, year) %>%
  mutate(new_percent = round(count / sum(count) * 100, 1)) %>%
  filter(percent != new_percent)

wide %>% filter(FIPS == 5055, year == "1970") %>% 
  mutate(new_p = count / sum(count) * 100)
wide %>% filter(FIPS == 26015, year == "1970") %>% 
  mutate(new_p = count / sum(count) * 100)
wide %>% filter(FIPS == 38083, year == "2000") %>% 
  mutate(new_p = count / sum(count) * 100) ## differences due to rounding

wide %>% filter(FIPS == 72000, year == "2000") %>% 
  mutate(new_c = sum(count) * percent / 100) ## percentage are wrong!!
# correct the percentage
wide[wide$FIPS == 72000 & wide$year == "2000", "percent"] = wide %>% 
  filter(FIPS == 72000, year == 2000) %>%
  mutate(new_p = round(count / sum(count) * 100, 1)) %>% select(new_p)

tidy = wide %>% gather(key = type, value = value, 6:7)

# visualize
pick = tidy %>% filter(State == "IA", Area == "Story County") 
pick %>%
  ggplot(aes(x = year, y = value)) +
  geom_point(aes(color = level)) + geom_path(aes(group = level, color = level)) +
  facet_wrap( ~ type, scale = "free") +
  theme_bw() + theme(legend.position = "bottom") + 
  ggtitle(paste(pick$Area[1], " ,", pick$State[1], sep ="")) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

## exclude PR state i.e. puerto rico
isstate = tidy[tidy$FIPS %% 1000 == 0 & tidy$FIPS < 72000, ]
isstate$state = tolower(isstate$Area)
map <- map_data("state") ## Alaska Hawaii and PR are not included in map
map2 <- map_data("county")
#check consistency
anti_join(isstate, map, by = c("state" = "region"))  ## misspell of Louisiana
#correct
tidy = tidy %>% mutate(Area = replace(Area, Area == "Lousiana", "Louisiana"))
save(tidy, file = "tidy.RData")
# separate states and counties
iscounty = tidy[tidy$FIPS %% 1000 != 0 & tidy$FIPS < 72000, ] ## exclude PR state i.e. puerto rico
isstate = tidy[tidy$FIPS %% 1000 == 0 & tidy$FIPS < 72000, ]
iscounty$county = gsub(" .*", "", tolower(iscounty$Area))
isstate$state = tolower(isstate$Area)
anti_join(isstate, map, by = c("state" = "region")) %>% group_by(Area) %>% tally()

# plot state data on map
select = isstate %>% filter(type == "percent", level == "Bachelor's degree or higher")
all = left_join(select, map, by = c("state" = "region"))

all %>% ggplot(aes(x = long, y = lat, group = state)) +
  geom_polygon(aes(fill = value))  +
  scale_fill_gradient2(name = "Percent", midpoint = min(value), mid = "white") +
  ggthemes::theme_map() + theme(legend.position = c(0.8, 0)) + facet_wrap(~ year) +
  ggtitle("Bachelor's degree or higher")

# plot county data for Iowa on map
iowa = iscounty %>% filter(State == "IA",
                           type == "percent",
                           level == "Bachelor's degree or higher")
iowa_all = left_join(iowa,
                     map2 %>% filter(region == "iowa"),
                     by = c("county" = "subregion"))
iowa_all %>%
  ggplot(aes(x = long, y = lat, group = county)) +
  geom_polygon(aes(fill = value))  +
  scale_fill_gradient2(name = "Percent", midpoint = min(value), mid = "white") +
  ggthemes::theme_map() + theme(legend.position = c(0.8, 0)) + facet_wrap(~ year) +
  ggtitle("Bachelor's degree or higher") +
  ggrepel::geom_label_repel(
    aes(label = county),
    point.padding = unit(0.5, "lines"),
    data = iowa_all %>% group_by(year) %>%
      filter(value >= 28) %>%  ##value %in% sort(unique(value),decreasing = T)[1:2]
      group_by(value) %>% slice(1)
  )
