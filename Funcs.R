##################################
### Function to decide of ties ###
##################################

func_ties <- function(team1, team2, data1, data2){
  
  dta_t <- rbind(data1, data2) %>%
    dplyr::filter(v.team %in% c(team1, team2),
                  h.team %in% c(team1, team2))
  
  dta_t_v <- dta_t %>%
    dplyr::select(v.team, point_v) %>%
    rename(team = v.team, points = point_v)
  
  dta_t_h <- dta_t %>%
    dplyr::select(h.team, point_h) %>%
    rename(team = h.team, points = point_h)
  
  dta_t_total <- rbind(dta_t_v, dta_t_h) %>%
    group_by(team) %>%
    summarize(points = sum(points)) %>%
    arrange(-points)
  
  if(dta_t_total$points[1] == dta_t_total$points[2]){
    winner <- dta_t_total$team[rbinom(1, 1, 0.5)+1]
  }
  
  if(dta_t_total$points[1] != dta_t_total$points[2]){
    winner <- dta_t_total$team[1]
  }
  
  return(winner)
  
}

####################################
### Function for playoff matchup ###
####################################

playoff_func <- function(team1, team2){

  dta1 <- data.frame(array(0, c(14, 4)))
  colnames(dta1) <- c("game", "goals", "home", "visitor")
  
  dta1$game <- c(1,1,2,2,3,3,4,4,5,5,6,6,7,7)
  
  dta1[c(1,3,9,13), "home"] <- paste("o", team1, sep = ".")
  dta1[c(1,3,9,13), "visitor"] <- paste("d", team2, sep = ".")
  dta1[c(2,4,10,14), "home"] <- paste("d", team1, sep = ".")
  dta1[c(2,4,10,14), "visitor"] <- paste("o", team2, sep = ".")
  
  dta1[c(5,7,11), "home"] <- paste("o", team2, sep = ".")
  dta1[c(5,7,11), "visitor"] <- paste("d", team1, sep = ".")
  dta1[c(6,8,12), "home"] <- paste("d", team2, sep = ".")
  dta1[c(6,8,12), "visitor"] <- paste("o", team1, sep = ".")
  
  dta2 <- data.frame(array(0, c(7, 2)))
  colnames(dta2) <- c("g1", "g2")
  
  while(nrow(dta1[dta2$g1 == dta2$g2,]) > 0){
    dta1$goals <- rpois(n = nrow(dta1), lambda = predict(eq.reg, newdata = dta1, type = "response"))
    dta2$g1 <- dta1$goal[c(1,3,6,8,9,12,13)]
    dta2$g2 <- dta1$goal[c(2,4,5,7,10,11,14)]
  }
  
  dta2$team1 <- ifelse(dta2$g1 > dta2$g2, 1, 0)
  dta2$team2 <- ifelse(dta2$g2 > dta2$g1, 1, 0)
  
  if(sum(dta2$team1) >= 4){
    winner <- team1
  }
  
  if(sum(dta2$team2) >= 4){
    winner <- team2
  }
  
  return(as.character(winner))
  
}


#########################################################################
### Function to simulate rest of season and generate number of points ###
#########################################################################

sim_season <- function(data){

  #Game outcomes
  data$goals <- rpois(n = nrow(data), lambda = predict(eq.reg, newdata = data, type = "response"))
  
  #Overtime outcome - to correct later
  data$OT_home_win <- 0
  data$OT_home_win[data$v.team != "Wild"] <- rbinom(n = nrow(data[data$v.team != "Wild",]), 1, predict(reg_ot, newdata = data[data$v.team != "Wild",], type = "response")) 
  data$OT_home_win[data$v.team == "Wild"] <- rbinom(n = nrow(data[data$v.team == "Wild",]), 1, 0.5) 
  
  ############################################
  ### Calculate predicted number of points ###
  ############################################
  
  dta_h <- data %>% 
    dplyr::select(date, hg = goals, OT_home_win, h.team, v.team, team) %>%
    dplyr::filter(team == "home")  %>%
    dplyr::select(-team)
  
  dta_v <- data %>% 
    dplyr::select(date, vg = goals, h.team, v.team, team) %>%
    dplyr::filter(team == "visitor") %>%
    dplyr::select(-team)
  
  dta0_p <- left_join(dta_h, dta_v) %>% 
    mutate(point_h = case_when(hg > vg ~ 2,
                               hg < vg  ~ 0,
                               hg == vg & OT_home_win==1 ~ 2,
                               hg == vg & OT_home_win==0 ~ 1),
           point_v = case_when(vg > hg ~ 2,
                               vg < hg ~ 0,
                               vg == hg & OT_home_win==1 ~ 1,
                               vg == hg & OT_home_win==0 ~ 2)) %>%
    dplyr::select(h.team, v.team, point_h, point_v) 
  
  dta0_v <- dta0_p %>%
    dplyr::select(v.team, point_v) %>%
    rename(team = v.team, points = point_v)
  
  dta0_h <- dta0_p %>%
    dplyr::select(h.team, point_h) %>%
    rename(team = h.team, points = point_h)
  
  dta0_p_total <- rbind(dta0_v, dta0_h) %>%
    group_by(team) %>%
    summarize(points = sum(points))
  
  dta_points <- rbind(dta_p_total, dta0_p_total) %>% 
    group_by(team) %>%
    summarize(points = sum(points))
  
  ########################
  ### Standings - East ###
  ########################
  
  # Atlantic
  east_atl <- dta_points %>%
    dplyr::filter(team %in% c("Lightning", "Maple Leafs", "Sabres", "Bruins", "Canadiens", "Red Wings", "Panthers", "Senators")) %>%
    arrange(-points)
  
  east_atl_p <- east_atl[east_atl$points >= east_atl$points[3],]
  
  if(nrow(east_atl_p)>3){
    third <- func_ties(east_atl_p$team[3], east_atl_p$team[4], dta_p, dta0_p)
    east_atl_p <- east_atl_p[east_atl_p$team %in% c(east_atl_p$team[1], east_atl_p$team[2], third),]
  }
  
  # Metropolitain
  east_metro <- dta_points %>%
    dplyr::filter(team %in% c("Capitals", "Blue Jackets", "Penguins", "Islanders", "Rangers", "Hurricanes", "Flyers", "Devils")) %>%
    arrange(-points)
  
  east_metro_p <- east_metro[east_metro$points >= east_metro$points[3],]
  
  if(nrow(east_metro_p)>3){
    third <- func_ties(east_metro_p$team[3], east_metro_p$team[4], dta_p, dta0_p)
    east_metro_p <- east_metro_p[east_metro_p$team %in% c(east_metro_p$team[1], east_metro_p$team[2], third),]
  }
  
  # East wildcards
  east_wild <- rbind(east_atl, east_metro) %>%
    dplyr::filter(!team %in% east_atl_p$team,
                  !team %in% east_metro_p$team) %>%
    arrange(-points)
  
  east_wild_p <- east_wild[east_wild$points >= east_wild$points[2],]
  
  if(nrow(east_wild_p)>2){
    second <- func_ties(east_wild_p$team[2], east_wild_p$team[3], dta_p, dta0_p)
    east_wild_p <- east_wild_p[east_wild_p$team %in% c(east_wild_p$team[1], second),]
  }
  
  # Brackets for playoffs
  
  east_atl_p$bracket <- ifelse(east_atl_p$points[1] > east_metro_p$points[1], 1, 2)
  east_metro_p$bracket <- ifelse(east_atl_p$points[1] < east_metro_p$points[1], 1, 2)
  
  if(east_atl_p$points[1] == east_metro_p$points[1]){
    first <- func_ties(east_atl_p$team[1], east_metro_p$team[1], dta_p, dta0_p)
    east_atl_p$bracket <- ifelse(first %in% east_atl_p$team, 1, 2)
    east_metro_p$bracket <- ifelse(first %in% east_metro_p$team, 1, 2)
  }
  
  east_wild_p$bracket <- c(2,1)
  
  east_p <- rbind(east_atl_p, east_metro_p, east_wild_p) %>%
    arrange(-points)
  
  
  ########################
  ### Standings - West ###
  ########################
  
  # Central
  west_ctl <- dta_points %>%
    dplyr::filter(team %in% c("Jets", "Predators", "Avalanche", "Stars", "Wild", "Blues", "Blackhawks")) %>%
    arrange(-points)
  
  west_ctl_p <- west_ctl[west_ctl$points >= west_ctl$points[3],]
  
  if(nrow(west_ctl_p)>3){
    third <- func_ties(west_ctl_p$team[3], west_ctl_p$team[4], dta_p, dta0_p)
    west_ctl_p <- west_ctl_p[west_ctl_p$team %in% c(west_ctl_p$team[1], west_ctl_p$team[2], third),]
  }
  
  # Pacific
  west_pac <- dta_points %>%
    dplyr::filter(team %in% c("Flames", "Sharks", "Golden Knights", "Ducks", "Oilers", "Canucks", "Coyotes", "Kings")) %>%
    arrange(-points)
  
  west_pac_p <- west_pac[west_pac$points >= west_pac$points[3],]
  
  if(nrow(west_pac_p)>3){
    third <- func_ties(west_pac_p$team[3], west_pac_p$team[4], dta_p, dta0_p)
    west_pac_p <- west_pac_p[west_pac_p$team %in% c(west_pac_p$team[1], west_pac_p$team[2], third),]
  }
  
  # west wildcards
  west_wild <- rbind(west_ctl, west_pac) %>%
    dplyr::filter(!team %in% west_ctl_p$team,
                  !team %in% west_pac_p$team) %>%
    arrange(-points)
  
  west_wild_p <- west_wild[west_wild$points >= west_wild$points[2],]
  
  if(nrow(west_wild_p)>2){
    second <- func_ties(west_wild_p$team[2], west_wild_p$team[3], dta_p, dta0_p)
    west_wild_p <- west_wild_p[west_wild_p$team %in% c(west_wild_p$team[1], second),]
  }
  
  # Brackets for playoffs
  
  west_ctl_p$bracket <- ifelse(west_ctl_p$points[1] > west_pac_p$points[1], 1, 2)
  west_pac_p$bracket <- ifelse(west_ctl_p$points[1] < west_pac_p$points[1], 1, 2)
  
  if(west_ctl_p$points[1] == west_pac_p$points[1]){
    first <- func_ties(west_ctl_p$team[1], west_pac_p$team[1], dta_p, dta0_p)
    west_ctl_p$bracket <- ifelse(first %in% west_ctl_p$team, 1, 2)
    west_pac_p$bracket <- ifelse(first %in% west_pac_p$team, 1, 2)
  }
  
  west_wild_p$bracket <- c(2,1)
  
  west_p <- rbind(west_ctl_p, west_pac_p, west_wild_p) %>%
    arrange(-points)
  
  #####################################
  ### Add dummy for making playoffs ###
  #####################################
  
  playoffs <- rbind(east_p, west_p) %>%
    arrange(-points)
  
  dta_points <- dta_points %>%
    mutate(playoffs = case_when(team %in% playoffs$team ~ 1,
                                TRUE ~ 0))
  
  
  #######################################################################
  ### Add dummy for winners of each conference and President's trophy ###
  #######################################################################
  
  dta_points$East <- as.numeric(dta_points$team == east_p$team[1])
  dta_points$West <- as.numeric(dta_points$team == west_p$team[1])
  dta_points$President <- as.numeric(dta_points$team == playoffs$team[1])
  
  ################
  ### Playoffs ###
  ################
  
  east_p$seed <- 0
  east_p[east_p$bracket==1,"seed"] <- 1:4
  east_p[east_p$bracket==2,"seed"] <- 1:4
  
  west_p$seed <- 0
  west_p[west_p$bracket==1,"seed"] <- 1:4
  west_p[west_p$bracket==2,"seed"] <- 1:4
  
  ### East ###
  
  EB1_1 <- playoff_func(east_p[east_p$bracket==1,][1, "team"], east_p[east_p$bracket==1,][4, "team"])
  EB1_2 <- playoff_func(east_p[east_p$bracket==1,][2, "team"], east_p[east_p$bracket==1,][3, "team"])
  EB2_1 <- playoff_func(east_p[east_p$bracket==2,][1, "team"], east_p[east_p$bracket==2,][4, "team"])
  EB2_2 <- playoff_func(east_p[east_p$bracket==2,][2, "team"], east_p[east_p$bracket==2,][3, "team"])  
  EB1_teams <- east_p[east_p$bracket==1 & east_p$team %in% c(EB1_1, EB1_2),] %>% arrange(seed)
  EB1 <- playoff_func(EB1_teams$team[1], EB1_teams$team[2])  
  
  EB2_teams <- east_p[east_p$bracket==2 & east_p$team %in% c(EB2_1, EB2_2),] %>% arrange(seed)
  EB2 <- playoff_func(EB2_teams$team[1], EB2_teams$team[2])  
  
  EF_teams <- east_p[east_p$team %in% c(EB1, EB2),] %>% arrange(-points)
  EF <- playoff_func(EF_teams$team[1], EF_teams$team[2])  
  
  ### West ###
  
  WB1_1 <- playoff_func(west_p[west_p$bracket==1,][1, "team"], west_p[west_p$bracket==1,][4, "team"])
  WB1_2 <- playoff_func(west_p[west_p$bracket==1,][2, "team"], west_p[west_p$bracket==1,][3, "team"])
  WB2_1 <- playoff_func(west_p[west_p$bracket==2,][1, "team"], west_p[west_p$bracket==2,][4, "team"])
  WB2_2 <- playoff_func(west_p[west_p$bracket==2,][2, "team"], west_p[west_p$bracket==2,][3, "team"])  
  
  WB1_teams <- west_p[west_p$bracket==1 & west_p$team %in% c(WB1_1, WB1_2),] %>% arrange(seed)
  WB1 <- playoff_func(WB1_teams$team[1], WB1_teams$team[2])  
  
  WB2_teams <- west_p[west_p$bracket==2 & west_p$team %in% c(WB2_1, WB2_2),] %>% arrange(seed)
  WB2 <- playoff_func(WB2_teams$team[1], WB2_teams$team[2])  
  
  WF_teams <- west_p[west_p$team %in% c(WB1, WB2),] %>% arrange(-points)
  WF <- playoff_func(WF_teams$team[1], WF_teams$team[2])  
  
  ### Finals ###
  
  final_teams <- playoffs[playoffs$team %in% c(EF, WF),] %>% arrange(-points)
  final <- playoff_func(final_teams$team[1], final_teams$team[2])  
  
  ################################
  ### Add dummies for playoffs ###
  ################################
  
  dta_points$second <- as.numeric(dta_points$team %in% c(EB1_1, EB1_2, EB2_1, EB2_2, WB1_1, WB1_2, WB2_1, WB2_2))
  dta_points$third <- as.numeric(dta_points$team %in% c(EB1, EB2, WB1, WB2))
  dta_points$final <- as.numeric(dta_points$team %in% c(EF, WF))
  dta_points$champion <- as.numeric(dta_points$team %in% final)
  
  return(dta_points)
  
}

##################################
### Function for team rankings ###
##################################

rank_func <- function(data, team.sim){
  
 #Prepare data
  
  dta.sim <- data %>%
    mutate(home = ifelse(home == "NA" & team == "home", paste("o", team.sim, sep = "."), as.character(home)),
           home = ifelse(home == "NA" & team == "visitor", paste("d", team.sim, sep = "."), as.character(home)),
           h.team = ifelse(h.team == "NA", team.sim, as.character(h.team)),
           visitor = ifelse(visitor == "NA" & team == "home", paste("d", team.sim, sep = "."), as.character(visitor)),
           visitor = ifelse(visitor == "NA" & team == "visitor", paste("o", team.sim, sep = "."), as.character(visitor)),
           v.team = ifelse(v.team  == "NA", team.sim, as.character(v.team)))
  
  # Simulated goals
  dta.sim$goals <- rpois(n = nrow(dta.sim), lambda = predict.glm(eq.reg, newdata = dta.sim, type = "response"))
  
  #Overtime outcome
  dta.sim$OT_home_win <- 0
  dta.sim$OT_home_win <- rbinom(n = nrow(dta.sim), 1, predict(reg_ot, newdata = dta.sim, type = "response")) 
  #dta.sim$OT_home_win[dta.sim$v.team != "Wild"] <- rbinom(n = nrow(dta.sim[dta.sim$v.team != "Wild",]), 1, predict(reg_ot, newdata = dta.sim[dta.sim$v.team != "Wild",], type = "response")) 
  #dta.sim$OT_home_win[dta.sim$v.team == "Wild"] <- rbinom(n = nrow(dta.sim[dta.sim$v.team == "Wild",]), 1, 0.5) 
  
  ############################################
  ### Calculate predicted number of points ###
  ############################################
  
  dta_h <- dta.sim %>% 
    dplyr::select(hg = goals, OT_home_win, h.team, v.team, team, team.home, team.away) %>%
    dplyr::filter(team == "home") 
  
  dta_v <- dta.sim %>% 
    dplyr::select(vg = goals, team) %>%
    dplyr::filter(team == "visitor") %>%
    dplyr::select(-team)
  
  dta0_p <- cbind(dta_h, dta_v) %>% 
    mutate(point_h = case_when(hg > vg ~ 2,
                               hg < vg  ~ 0,
                               hg == vg & OT_home_win==1 ~ 2,
                               hg == vg & OT_home_win==0 ~ 1),
           point_v = case_when(vg > hg ~ 2,
                               vg < hg ~ 0,
                               vg == hg & OT_home_win==1 ~ 1,
                               vg == hg & OT_home_win==0 ~ 2),
           points = case_when(team.home ~ point_h,
                              team.away ~ point_v),
           team_goal = case_when(team.home ~ hg,
                                 team.away ~ vg),
           opponent_goal = case_when(team.away ~ hg,
                                     team.home ~ vg))
  
  dta_sum <- dta0_p %>%
    summarize(team = team.sim,
              points = round(82*mean(points), 2),
              Team_goals = round(mean(team_goal, na.rm = TRUE), 2),
              Opp_goals = round(mean(opponent_goal, na.rm = TRUE), 2)) 
  
  return(dta_sum)
}

##################################################
### Function to calculate strength of schedule ###
##################################################

strength_func <- function(team_s, data){
  dta_1 <- data %>%
    dplyr::filter(h.team == team_s | v.team == team_s) %>%
    mutate(team = case_when(h.team == team_s ~ v.team,
                            v.team == team_s ~ h.team)) %>%
    dplyr::select(team)
  
  dta_2 <- dta_ranks %>%
    dplyr::select(team, points)
  
  
  left_join(dta_1, dta_2) %>% 
    summarise(team = team_s,
              strength = round(mean(points), 2)) %>%
    return
  
}

