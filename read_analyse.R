rm(list=ls(all=T))
library("RMySQL")
old = Sys.time()

source("GAfunctions.R")
library("reshape2")
library("xlsx")
library("ggplot2")
#connection with mysql
mydb = dbConnect(MySQL(), user = 'root', password= 'admin', dbname = 'spring_clean',
                 host = '127.0.0.1')
dbListTables(mydb)

#reading tables, creating data frames, removing tables after filling data frames
table_parameters = dbSendQuery(mydb, "select * from parameters_info_db")
data_parameters = fetch(table_parameters, n = -1)

table_stops = dbSendQuery(mydb, "select * from stops_info_db")
data_stops = fetch(table_stops, n = -1)

table_travel_time = dbSendQuery(mydb, "select * from travel_time_matrix_db")
data_travel_time = fetch(table_travel_time, n = -1)

#clearing result of each table and closing connection
dbClearResult(table_parameters)
dbClearResult(table_stops)
dbClearResult(table_travel_time)
dbDisconnect(mydb)
rm(table_parameters)
rm(table_stops)
rm(table_travel_time)
rm(mydb)

stops = data_stops$STOP_ID[7:195]
depots = data_stops$STOP_ID[1:6]
#matrix for easier retrieval of time between 2 stops/depots
data3 = stops_depot_matrix_creation(data_travel_time)

travel_times = c()
cost = c()
parameters = c()
unique_parameters = c()
optimum_cost_per_solution = c()
optimum_travel_time_per_solution = c()
iterations = c()

#parameters for running complete process, convergence will happen after 25 same values
whole_process_run = 5
convergence = 25

for(j in 1:whole_process_run) {
  for (k in 1:nrow(data_parameters)) {
    min_stops = data_parameters[2][[1]][k]
    max_stops = data_parameters[1][[1]][k]
    max_route_time = data_parameters[3][[1]][k]
    max_early_time_window = data_parameters[4][[1]][k]
    max_late_time_window = data_parameters[5][[1]][k]
    
    population_df = genInitialPopulation(min_stops, max_stops, max_route_time, max_early_time_window, max_late_time_window)
    
    mutstartProb=0.5
    origPopSize = nrow(population_df)
    NewPop = population_df
    count = 0
    i = 1
    
    while(count < (convergence+1)) {
      iterations = append(iterations, i)
      i = i + 1
      eliteSolutions = NewPop[1:10,]
      set.seed(j+k)
      
      eliteSolution = eliteSolutions[sample(1:10, 1),]
      NewPop = population_df[-(1:nrow(population_df)),]
      mut = mutstartProb/i
      a = runif(1,0,1)
      NewPop = eliteSolutions
      while (nrow(NewPop)<origPopSize) {
        if (a<mut) {
          #mutate
          NewPop[nrow(NewPop)+1,]=fnMutate(eliteSolution, min_stops, max_stops)
          if (nrow(NewPop)==origPopSize){break()}
        } else {
          #crossover
          NewPop[nrow(NewPop)+1,] = fnCrossover(eliteSolution, min_stops, max_stops)
          if (nrow(NewPop)==origPopSize){break()}
        }
        
      }
      NewPop = NewPop[order(NewPop$Time),]
      travel_times = append(travel_times, as.numeric(NewPop[1,2]))
      parameters = append(parameters, paste(min_stops,"-",max_stops,"-",max_early_time_window,"-",max_late_time_window))
      cost = append(cost, as.numeric(NewPop[1,3]))
      if(eliteSolutions[1,3] == NewPop[1,3]) {
        count = count + 1
      } else {
        count = 0
      }
      if(count == convergence) {
        optimum_cost_per_solution = append(optimum_cost_per_solution, NewPop[1,3])
        optimum_travel_time_per_solution = append(optimum_travel_time_per_solution, NewPop[1,2])
        unique_parameters = append(unique_parameters, paste(min_stops,"-",max_stops,"-",max_early_time_window,"-",max_late_time_window))
        break()
      }
    }
  }
  
}

#time taken in whole process
new = Sys.time() - old
results_df = data.frame(Iterations = iterations,
                        Parameters = parameters,
                        Time = travel_times,
                        Cost = cost)

optimum_results = data.frame(Parameters = unique_parameters,
                             Final_Time = optimum_travel_time_per_solution,
                             Cost = optimum_cost_per_solution)

write.xlsx(results_df, "results.xlsx")

#Visualisation
#Graph to show which parameter has produced least cost over 5 trials
#finding average cost per Parameters
levels(factor(optimum_results$Parameters))
optimal_solution_group_by_parameter = aggregate(optimum_results$Cost, by=list(Parameters = optimum_results$Parameters), FUN = mean)
#plot graph of optimal_solution_group_by_parameter
average_costs <- optimal_solution_group_by_parameter$x

barplot(average_costs, main = "Parameter comparison", xlab = "Parameters", ylab = "Average cost over 5 trials",
        names.arg = optimal_solution_group_by_parameter$Parameters,
        col = barAllMethodsDurationRainbowColors)

basic_plot <- ggplot(optimal_solution_group_by_parameter, aes(x=Parameters, y=x)) + 
                      geom_bar(stat = "identity", width = 0.6) +
                      theme_classic()+
                      xlab("Parameters") + 
                      ylab("Average cost across 5 trials") + 
                      ggtitle("Parameter comparison") + 
                      theme(plot.title = element_text(hjust = 0.5, size = 24, margin = margin(20,0,20,0)))

basic_plot

#Parameter 3-5-30-30, 3-5-60-60 are best in terms of cost, Optimum route should be found using these parameters
  
