#' Show list of counties of a given state.
#'
#' @param stateshort 2 character abbreviation of states in US. use "state.abb" to show abbreviation of all states
#' @importFrom dplyr filter mutate select
#' @export
#' @examples
#' showcounty("IA")
#'
showcounty <- function(stateshort){
  x=tidy %>% filter(State==stateshort) %>% select(Area) %>% unique() %>% unlist()
  as.vector(x[-1])
}


#' Plot the education level of a given county.
#'
#' @param stateshort 2 character abbreviation of states in US.
#' @param countyname County name.
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot
#' @export
#' @examples
#' lineEd("IA", "Story County")
#' mapstate(levelint=4)
#' mapcounty(levelint=4,stateshort="IA")
lineEd <- function(stateshort,countyname){
  pick = tidy %>% filter(State == stateshort, Area == countyname)
  pick %>%
    ggplot(aes(x = year, y = value)) +
    geom_point(aes(color = level)) + geom_path(aes(group = level, color = level)) +
    facet_wrap( ~ type, scale = "free") +
    theme_bw() + theme(legend.position = "bottom") +
    ggtitle(paste(pick$Area[1], ", ", pick$State[1], sep ="")) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
}


#' Map the education level of all states.
#'
#' @param vtype "count" or "percent".
#' @param levelint Education level: 1="Less than a high school diploma", 2="High school diploma only", 3="Some college (1-3 years)" or 4="Bachelor's degree or higher".
#' @importFrom dplyr left_join filter mutate
#' @importFrom ggplot2 ggplot
#' @export
#' @examples
#' lineEd("IA", "Story County")
#' mapstate(levelint = 4)
#' mapcounty(levelint = 4,stateshort="IA")
mapstate = function(vtype="percent", levelint){
  if (levelint==1) edlevel = "Less than a high school diploma"
  else if (levelint==2) edlevel = "High school diploma only"
  else if (levelint==3) edlevel = "Some college (1-3 years)"
  else if (levelint==4) edlevel = "Bachelor's degree or higher"
  else print("Wrong number for education level! Should be 1-4.")

  isstate = tidy[tidy$FIPS %% 1000 == 0 & tidy$FIPS < 72000, ]
  isstate$state = tolower(isstate$Area)
  map <- map_data("state")

  select = isstate %>% filter(type == vtype, level == edlevel)
  all = left_join(select, map, by = c("state" = "region"))

  all %>% ggplot(aes(x = long, y = lat, group = state)) +
    geom_polygon(aes(fill = value))  +
    scale_fill_gradient2(name = vtype, midpoint = min(all$value), mid = "white") +
    ggthemes::theme_map() + theme(legend.position = c(0.8, 0)) + facet_wrap(~ year) +
    ggtitle(edlevel)
}


#' Map the education level of a given state.
#'
#' @param stateshort 2 character abbreviation of states in US.
#' @param vtype "count" or "percent".
#' @param levelint Education level: 1="Less than a high school diploma", 2="High school diploma only", 3="Some college (1-3 years)" or 4="Bachelor's degree or higher".
#' @param limit Label county names whose percentage >= limit, defalut limit = 28
#' @importFrom dplyr left_join filter mutate
#' @importFrom ggplot2 ggplot
#' @export
#' @examples
#' lineEd("IA", "Story County")
#' mapstate(levelint = 4)
#' mapcounty(levelint = 4,stateshort="IA")
mapcounty =  function(vtype="percent", levelint, stateshort, limit = 28){
  if (levelint==1) edlevel = "Less than a high school diploma"
  else if (levelint==2) edlevel = "High school diploma only"
  else if (levelint==3) edlevel = "Some college (1-3 years)"
  else if (levelint==4) edlevel = "Bachelor's degree or higher"
  else print("Wrong number for education level! Should be 1-4.")

  if (stateshort=="AK"|stateshort=="HI") print("No map imformation for Alaska or Hawaii!")
  else{
    map2 <- map_data("county")
    st = data.frame(name =tolower(state.name), abb=state.abb)
    map2 =left_join(map2,st,by=c("region"="name"))

    iscounty = tidy[tidy$FIPS %% 1000 != 0 & tidy$FIPS < 72000, ] ## exclude PR state i.e. puerto rico
    iscounty$county = gsub(" .*", "", tolower(iscounty$Area))

    area = iscounty %>% filter(State == stateshort,
                               type == vtype,
                               level == edlevel)
    area_all = left_join(area,
                         map2 %>% filter(abb == stateshort),
                         by = c("county" = "subregion"))
    area_all %>%
      ggplot(aes(x = long, y = lat, group = county)) +
      geom_polygon(aes(fill = value))  +
      scale_fill_gradient2(name = vtype, midpoint = min(area_all$value), mid = "white") +
      ggthemes::theme_map() + theme(legend.position = c(0.8, 0)) + facet_wrap(~ year) +
      ggtitle(paste(edlevel,", ",stateshort,sep="")) +
      ggrepel::geom_label_repel(
        aes(label = county),
        point.padding = unit(0.5, "lines"),
        data = area_all %>% group_by(year) %>%
          filter(value >= limit) %>%  ##value %in% sort(unique(value),decreasing = T)[1:2]
          group_by(value) %>% slice(1)
      )
  }
}
