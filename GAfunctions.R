### FITNESS FUNCTION ###
# calculates time for solution after applying penalties
fitness <- function(routes, min_stops, max_stops) {
  time = c()
  route_time_sum = c()
  route_string = c()
  for(i in 1:length(routes)) {
    no_of_stops_in_route = length(routes[[i]])
    solucounter = 0
    route = NULL
    route_time = c()
    for(j in 1:no_of_stops_in_route-1) {
      #putting all times in vector time
      time <- c(time,as.numeric(data3[routes[[i]][j],routes[[i]][j+1]]), data_stops$TIME_TO_COMPLETE_WORK[data_stops$STOP_ID == routes[[i]][j+1]])
      route_time <- c(route_time,as.numeric(data3[routes[[i]][j],routes[[i]][j+1]]), data_stops$TIME_TO_COMPLETE_WORK[data_stops$STOP_ID == routes[[i]][j+1]])
      
    }
    #creating values of dataframe with routes and time after penalty
    route_string = append(route_string, paste(unlist(routes[[i]]), collapse = ","))
    route_time_sum = append(route_time_sum, sum(route_time))
    #1 time is 1 $ and vehicle is 10$
  }
  
  #dataframe having routes and time taken for each route
  a <- unlist(route_string)
  b <- NULL
  for(i in 1:length(a)) {
    b <- paste(b,a[i], sep = "|")
  }
  solution_df = data.frame(Route = route_string, TravelTime = route_time_sum)
  solution_df$AfterPenalty = apply(solution_df, 1, FUN = get_penalty, min_stops = min_stops, max_stops = max_stops)
  solution_df$Solution <- substr(b, 2, nchar(b))
  solution_df$Time <- sum(solution_df$AfterPenalty)
  solution_df$Cost <- solution_df$Time + 10*length(routes)
  solution_df$Route <- NULL
  solution_df$TravelTime <- NULL
  solution_df$AfterPenalty <- NULL
  return(solution_df[1,])
}

#logic for penalty on route travel time
get_penalty = function(x, min_stops, max_stops) { 
  t = as.numeric((x['TravelTime']))
  #print(t)
  #1st condition
  if(t > 660) {
    extra_time = t - 660
    slab = ceiling(extra_time/60)
    penalty = 0
    for (i in slab:1) {
      if(i == slab) {
        penalty = penalty + (extra_time - 60*(i-1))*i + t*(i/10)
      } else {
        penalty = penalty + 60*i + t*(i/10)
      }
    }
  } else {
    penalty = 0
  }
  
  #2nd condition
  no_of_stops = length((strsplit(x['Route'], split = "[,]"))[[1]]) - 2
  if(no_of_stops < min_stops) {
    penalty = penalty + t*(min_stops - no_of_stops)/10
  } else if(no_of_stops > max_stops) {
    penalty = penalty + t*(min_stops - no_of_stops)
  }
  #3rd condition
  
  return(penalty+t)
}

#arranging travel times in matrix
stops_depot_matrix_creation <- function(data_travel_time) {
  data2=dcast(data_travel_time, FROM_STOP_ID ~ TO_STOP_ID, value="TRAVEL_TIME") #186 observations, there is some missing data
  left_out_stops = data_stops$STOP_ID[!data_stops$STOP_ID %in% data2$FROM_STOP_ID] #9 stops are missing from FROM_STOP_ID
  from_stop_ids = NULL
  to_stop_ids = NULL
  travel_times = NULL
  for(j in 1:length(left_out_stops)) {
    reverse_relation_rows = which(data_travel_time$TO_STOP_ID == left_out_stops[j])
    for(i in 1:length(reverse_relation_rows)) {
      existing_travel_time = data_travel_time$TRAVEL_TIME[reverse_relation_rows[i]]
      from_stop_ids = append(from_stop_ids, left_out_stops[j])
      to_stop_ids = append(to_stop_ids, data_travel_time$FROM_STOP_ID[reverse_relation_rows[i]])
      travel_times = append(travel_times, existing_travel_time)
    }
  }
  
  d = data.frame(from_stop_ids, to_stop_ids, travel_times)
  names(d) <- names(data_travel_time)
  data_travel_time = rbind(data_travel_time, d)
  rm(data2)
  rm(d)
  
  data3=dcast(data_travel_time, FROM_STOP_ID ~ TO_STOP_ID, value.var="TRAVEL_TIME")
  
  #making first column of matrix data3 as rownames
  df <- data.frame(data3[,1])
  rownames(data3) <- df[,1]
  data3[,1] <- NULL
  #checking na values
  sum(is.na(data3)) #81 NA values, it seems 9 stops have NA values with other 9 stops
  index_of_na <- unique(which(is.na(data3), arr.ind = T))
  for(i in 1:(length(index_of_na)/2)) {
    data3[index_of_na[i,1],index_of_na[i,2]] <- round(mean(data3[, index_of_na[i,2]], na.rm = T))
  }
  rm(df)
  return(data3)
}

#Crossover function which takes 2 random routes from solution
#Takes 1 random position from each route
#swaps first half of first random route with first half of second random route
#swaps second half of first random route with second half of second random route
#calculates fitness of new solution and returns
#this solution is added to new population
fnCrossover <- function(x, min_stops, max_stops) {
  routes = as.list(strsplit(paste(x['Solution'], collapse = ","), split = "[|]"))
  routes = lapply(routes, function(y) { return(strsplit(y, split = "[,]")) })
  #get random route from routes
  random_route_positions = sample(1:length(routes[[1]]), 2)
  random_routes = routes[[1]][random_route_positions]
  no_of_stops_route_1 = length(random_routes[[1]]) - 3 # 2 depots at beginning and end plus crossover cannot happen if last stop is chosen
  no_of_stops_route_2 = length(random_routes[[2]]) - 3 # 2 depots at beginning and end
  if(no_of_stops_route_1 > 1 || no_of_stops_route_2 > 1) {
    random_position1 = sample(1:no_of_stops_route_1, 1, replace = FALSE)
    random_position2 = sample(1:no_of_stops_route_2, 1, replace = FALSE)
    #swapping parts of route from 1 position in each route
    r1 = c(random_routes[[1]][1:(random_position1+1)],random_routes[[2]][(random_position2+2):(length(random_routes[[2]]) - 1)], random_routes[[1]][1])
    r2 = c(random_routes[[2]][1:(random_position2+1)],random_routes[[1]][(random_position1+2):(length(random_routes[[1]]) - 1)], random_routes[[2]][1])
    routes[[1]][[random_route_positions[[1]]]] =  r1
    routes[[1]][[random_route_positions[[2]]]] =  r2
    fit <- fitness(routes[[1]], min_stops, max_stops)
    return(fit[1,])
  } else {
    return(eliteSolution)
  }
}

#Mutation function which takes 1 random routes from solution
#Takes 2 random points from random route
#swaps 1st random point with 2nd random point
#fitness is calculated for new solution
#this solution is added to new population
fnMutate <- function(x, min_stops, max_stops) {
  routes = as.list(strsplit(paste(x['Solution'], collapse = ","), split = "[|]"))
  routes = lapply(routes, function(y) { return(strsplit(y, split = "[,]")) })
  #get random route from routes
  random_route_position = sample(1:length(routes[[1]]), 1)
  random_route = routes[[1]][random_route_position]
  no_of_stops = length(random_route[[1]]) - 2 # 2 depots at beginning and end
  if(no_of_stops > 1) {
    random_position = sample(1:no_of_stops, 2, replace = FALSE)
    #swapping the stops on these positions with each other
    temp = random_route[[1]][random_position[1]+1]
    random_route[[1]][random_position[1]+1] = random_route[[1]][random_position[2]+1]
    random_route[[1]][random_position[2]+1] = temp
    routes[[1]][random_route_position] = random_route
    fit <- fitness(routes[[1]], min_stops, max_stops)
    return(fit[1,])
  } else {
    return(x)
  }
}

#generating initial population
#size of population is 100 which contains randomly selected stops and depots exactly once
genInitialPopulation <- function(min_stops, max_stops, max_route_time, max_early_time_window, max_late_time_window) {
  initPopSize = 100
  all_solutions = NULL
  all_solutions_time = NULL
  all_solutions_cost = NULL
  for(i in 1:initPopSize) {
    routes = list()
    counter = 0
    stops_length_check = 0
    temp_stops = stops
    set.seed(i)
    while(length(temp_stops) > 0) {
      if(length(temp_stops) >= 8) {
        random_stops = sample(temp_stops,sample(min_stops:max_stops, 1),replace=FALSE)
      } else {
        random_stops = sample(temp_stops,sample(1:length(temp_stops), 1),replace=FALSE)
      }
      stops_length_check = stops_length_check + length(random_stops)
      random_depot = sample(depots,1,replace = FALSE)
      route = c(random_depot[1],random_stops,random_depot[1])
      counter = counter + 1
      routes[[counter]] = route
      temp_stops = temp_stops[!temp_stops %in% random_stops]
    }
    fit <- fitness(routes, min_stops, max_stops)
    all_solutions = append(all_solutions, fit[,1])
    all_solutions_time = append(all_solutions_time, fit[,2])
    all_solutions_cost = append(all_solutions_cost, fit[,3])
  }
  
  population_df = data.frame(Solution = all_solutions, Time = all_solutions_time, Cost = all_solutions_cost, stringsAsFactors = FALSE)
  population_df <- population_df[order(population_df$Cost),]
  return(population_df)
}
