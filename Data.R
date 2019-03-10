#Read the data
games <- read_html("https://www.hockey-reference.com/leagues/NHL_2019_games.html#games::none")

dta <- html_table(html_nodes(games, "table")[[1]])
colnames(dta) <- c("date", "visitor", "vg", "home", "hg", "ot", "attendance", "log", "notes")
dta <- dta  %>% 
  dplyr::tbl_df() %>%
  mutate(vg = as.numeric(vg),
         hg = as.numeric(hg))

#Regular time scores
dta <- dta %>% mutate(date = as.Date(date),
               r_vg = case_when(vg>hg & ot == "OT" ~ vg-1,
                                vg>hg & ot == "SO" ~ vg-1,
                                TRUE ~ vg),
               r_hg = case_when(hg>vg & ot == "OT" ~ hg-1,
                                hg>vg & ot == "SO" ~ hg-1,
                                TRUE ~ hg),
               ot_vg = ifelse(vg>hg & (ot == "OT" | ot == "SO"), 1, 0),
               ot_hg = ifelse(hg>vg & (ot == "OT" | ot == "SO"), 1, 0),
               OT = as.numeric(ot=="OT"|ot=="SO"),
               OT_home_win = as.numeric(OT == 1 & hg > vg),
               visitor = word(visitor,-1),
               visitor = case_when(visitor == "Leafs" ~ "Maple Leafs",
                                   visitor == "Wings" ~ "Red Wings",
                                   visitor == "Knights" ~ "Golden Knights",
                                   visitor == "Jackets" ~ "Blue Jackets",
                                   TRUE ~ visitor),
               home = word(home,-1),
               home = case_when(home == "Leafs" ~ "Maple Leafs", 
                                home == "Wings" ~ "Red Wings",
                                home == "Knights" ~ "Golden Knights",
                                home == "Jackets" ~ "Blue Jackets",
                                TRUE ~ home))

#Data for overtime


#Create team list
teams <- unique(dta$visitor)

# Column for regression weight
dta <- dta %>% 
  mutate(weight = 0.05 + 0.95*(1-pbeta((as.numeric(max(date))-as.numeric(date))/(as.numeric(max(date))-as.numeric(min(date))), 5,5)))


# Data to explain home team goals

dta.h <- dta %>%
  dplyr::select(date, goals = r_hg, OT, OT_home_win, home, visitor, weight) %>%
  mutate(team = "home",
         h.team = home,
         v.team = visitor,
         home = paste("o", home, sep = "."),
         visitor = paste("d", visitor, sep = "."))

# Data to explain visitor team goals

dta.v <- dta %>%
  dplyr::select(date, goals = r_vg, OT, OT_home_win, home, visitor, weight) %>%
  mutate(team = "visitor",
         h.team = home,
         v.team = visitor,
         home = paste("d", home, sep = "."),
         visitor = paste("o", visitor, sep = "."))

# Stack data

dta <- rbind(dta.h, dta.v)

rm(dta.v, dta.h, games)

# Data for games not played
dta0 <- dta %>% 
  dplyr::filter(is.na(goals))

# Data for games played
dta <- dta[!is.na(dta$goals),]

last_game <- max(dta$date)


###########################################
### Create data frame for team rankings ###
###########################################

dta.h1 <- data.frame(visitor = rep(unique(dta0$v.team), n.sim)) %>%
  mutate(home = "NA",
         h.team = home,
         v.team = visitor,
         visitor = paste("d", visitor, sep = "."),
         team = "home",
         team.home = TRUE,
         team.away = FALSE)
dta.h2 <- data.frame(visitor = rep(unique(dta0$v.team), n.sim)) %>%
  mutate(home = "NA",
         h.team = home,
         v.team = visitor,
         visitor = paste("o", visitor, sep = "."),
         team = "visitor",
         team.home = TRUE,
         team.away = FALSE)

dta.v1 <-  data.frame(home = rep(unique(dta0$h.team), n.sim)) %>%
  mutate(visitor = "NA",
         h.team = home,
         v.team = visitor,
         home = paste("o", home, sep = "."),
         team = "home",
         team.home = FALSE,
         team.away = TRUE)
dta.v2 <-  data.frame(home = rep(unique(dta0$h.team), n.sim)) %>%
  mutate(visitor = "NA",
         h.team = home,
         v.team = visitor,
         home = paste("d", home, sep = "."),
         team = "visitor",
         team.home = FALSE,
         team.away = TRUE)

dta.sim <- bind_rows(dta.h1, dta.h2, dta.v1, dta.v2)
rm(dta.h1, dta.h2, dta.v1, dta.v2)


