rm(list=ls(all=T))

#Loading data from MYSQL.
#install.packages('RMySQL')
library(RMySQL)
library(sqldf)
mydb = dbConnect(MySQL(), user='root', password='admin', dbname='spring_clean', host='127.0.0.1')
db_parameters <- dbSendQuery(mydb, "select * from parameters_info_db")
data_para = fetch(db_parameters, n=-1)
rm(db_parameters)

db_stops_info <- dbSendQuery(mydb, "select * from stops_info_db")
data_stops_info <- fetch(db_stops_info, n=-1)
rm(db_stops_info)

db_travel_time <- dbSendQuery(mydb, "select * from travel_time_matrix_db")
data_travel_time <- fetch(db_travel_time, n=-1)
rm(db_travel_time)
rm(mydb)

str(data_para)
str(data_stops_info)
str(data_travel_time)

###################### Data Processing & Analysis #################################
head(data_travel_time)
Depo_dist <- sqldf('select * from data_travel_time where FROM_STOP_ID like "DEP%" and TO_STOP_ID like "DEP%"')

#  FROM_STOP_ID TO_STOP_ID TRAVEL_TIME
#     DEP10001   DEP45024           0
#     DEP35024   DEP50002           0

#NOTE --- Distance between "DEP10001 and DEP45024" & "DEP35024 and DEP50002" is 0, so we can remove one of them each
#Removing DEP45024, DEP50002  from "data_stops_info" data frame
#Removing all records starting from "DEP45024 and DEP50002" depot in "data_travel_time" data frame
#install.packages("sqldf")

data_stops_info <- sqldf('select * from data_stops_info where STOP_ID not in ("DEP45024","DEP50002")')
data_travel_time <- sqldf('select * from data_travel_time where FROM_STOP_ID not in ("DEP45024","DEP50002")')
data_travel_time <- sqldf('select * from data_travel_time where TO_STOP_ID not in ("DEP45024","DEP50002")')

#Checking for any missing values in the matrix and stops data frame
#sapply(data_travel_time_matrix, function(x) sum(is.na(x)))
sapply(data_stops_info, function(x) sum(is.na(x)))
sapply(data_travel_time, function(x) sum(is.na(x)))
#No missing values in data frames

library(rlist)
library(base)
library(gdata)
library(Rmalschains)
library(rgp)
library(rgpui)
library(mcga)
library(DBI)
library(RMySQL)


############ Method 1: Using GA and clustering stops to nearest depots #########

matrix_travel_time <- data.frame(matrix(NA, nrow = 193, ncol = 193))

stopnames <- data_stops_info$STOP_ID
colnames(matrix_travel_time) <- stopnames
row.names(matrix_travel_time) <- stopnames

for (i in 1:nrow(data_travel_time))
{
  matrix_travel_time[data_travel_time$FROM_STOP_ID[i], data_travel_time$TO_STOP_ID[i]] <-data_travel_time$TRAVEL_TIME[i]
}

#checking for null values and then imputing them with max col values
sapply(matrix_travel_time, function(x) sum(is.na(x)))

Max_Col <- function(x) 
{
  apply(x,2,FUN=max,na.rm=TRUE )
}

Max_Val <- Max_Col(matrix_travel_time)

#Imputing distance values with Max values

for (i in c("STP68664","STP69424","STP69799","STP69927","STP70024","STP70151","STP70255","STP70398","STP70434"))
{
  for (j in 1:length(stopnames))
  {
    matrix_travel_time[i, j] <- Max_Val[j]
  }
}

# Rechecking null values.
sapply(matrix_travel_time, function(x) sum(is.na(x)))

#clustering stops to its nearest depots
depot_to_stop <-as.matrix(matrix_travel_time[1:4,1:193])
stop_to_depot <-t(matrix_travel_time[1:193,1:4])
avg_distance<-as.data.frame((depot_to_stop+stop_to_depot)/2)

#Depo1, Depo2, Depo3 and Depo4 are numbers as list
cluster<-list()

for(j in 1:4)
{
  cluster[[j]]<-j
}

#Based on the travel time matrix, clustering stops to depots

for(i in 5:193)
{
  
  temp <-which(avg_distance[,i] == min(avg_distance[,i]))
  
  if(length(temp)==1)
  {  
    cluster[[temp]] <-c(cluster[[temp]],i)
  }
  else
  {
    
    min<-length(stopnames)
    depot<-0
    for(ii in 1:length(temp))
    {
      if(length(cluster[[temp[ii]]])<min)
      {
        min<-length(cluster[[temp[ii]]])
        depot<-temp[ii]
      }
    }
    cluster[[depot]] <-c(cluster[[depot]],i)
  }
  
}
#Checking the clusters now
cluster
#This created 4 clusters with first element as it depot name and others as Service stops

########### Creating appropriate functions to run GA algorithm on clusters #####
#List of functions
#To calculate total time - Travel Time + Execution time
#To generate initial population
#Function to add 1st penality constraint of crossing 660 minutes
#Function to add second penality constraint of minimum no of stops
#Function add third penality constraint of reaching customers delayed than parameters
#Ojective function2 for this problem
#Final Score Per Solution
#Route Count Per Solution
#Cost Per Solution
#Function to pick elite population
#FUnction to generate new Population in GA which includes
# Crossover
# MUtation
#Function to calculate cost of the best solution
#Genetic Algorithm on interations based on above functions
#User input parameters
#VIsualizations

#Calculate total time - Travel time + Execution time
Total_time_calc <-function(inp)
{
  stime <-0
  ttime <-0
  for(kk in 1:(length(inp)-1))
  {
    ttime=as.numeric(matrix_travel_time[inp[kk],inp[kk+1]])+ttime
    stime=data_stops_info[inp[kk],2]+stime
  }
  
  tTot <-stime+ttime
  
  return(tTot)  
}

#Ojective function2 for this problem
#Final Score Per Solution
final_score <-function(inp)
{
  sum(unlist(inp))
}

#Route Count Per Solution
no_routes <-function(inp)
{
  length(inp)
}

#Cost Per Solution
route_cost <-function(inp1,inp2)
{
  # inp1--Route count, inp2 score value  
  return((CPRoute*inp1)+(CPMin*inp2))
}

#Function to pick elite population
Filter_ElitePop <-function(inp1,inp2)
{
  inp1[c(inp2)] 
}

#Function to generate initial population of CHROMOSOMES
InitPop <-function(inp,Minstop,Maxstop)
{
  set.seed(1234*Maxstop)
  mlist<-list()
  depot<-inp[1]
  inp<-setdiff(inp, depot)
  for (ii in 1:InitPopSize)
  {
    
    clist <-inp
    templist<-list()
    for(jj in 1: length(clist))
    {
      if (length(clist) >=MaxStops)
      {
        a <-resample(clist,size = 3,replace = FALSE)
        clist<-setdiff(clist, a)
        b<-Total_time_calc(c(depot,a,depot))
        zz<-Total_time_calc(a)
        if(b>=MaxRTime)
        {
          templist[[jj]] <-c(depot,a,depot)
        }
        else
        {
          c<-NULL
          while((length(a)<MaxStops) & (length(clist)> 0) & zz<180)
          {
            c <-resample(clist,size = 1)
            b<-Total_time_calc(c(depot,a,c,depot))
            zz<-Total_time_calc(c(a,c))
            if(b<=MaxRTime & zz<=180)
            {
              a<-c(a,c)
              clist<-setdiff(clist, a)
            }
            else
            {
              break
            }
          }
          templist[[jj]] <-c(depot,a,depot)
        }
      }
      else if ((length(clist)<MaxStops) & (length(clist) > 0))
      {
        a <- resample(clist, size =length(clist),replace = FALSE)
        templist[[jj]] <-c(depot,a,depot)
        clist<-setdiff(clist, a)
      }
      if (length(clist) == 0)
        break
    }
    mlist[[ii]]<-templist
  }
  return(mlist)
}

#Function to add 1st penality constraingt of crossing 660 minutes
PenConst1 <-function(inp)
{
  
  if(inp>MaxRTime)
  {
    c <-trunc((inp-MaxRTime)/60)+1
    c<-(c*10*inp)/100
    return(inp+c)
  }
  else  
  {
    return(inp)
  }
}

#Function to add second penality constraint of minimum no of stops
PenConst2 <-function(inp1,inp2)
{
  d <-length(inp1)-2
  if(d<MinStops)
  {
    sc<-(MinStops-d)*10
    sc<-((sc)/100)*inp2
    sc<-inp2+sc
    return(sc)
  }
  else
    return(inp2)
}

#Function add third penality constraint of reaching customers delayed than parameters

PenConst3 <-function(inp1,inp2)
{
  # x is route, inp2 is score of the route 
  if(inp2<=180)
  {
    return(inp2)
  }
  else
  {
    inp1<-setdiff(inp1,inp1[1])
    if(length(inp1)==1)
    {
      return(inp2)
    }
    else
    {
      stime <-0
      ttime <-0
      for(kk in 1:(length(inp1)-1))
      {
        ttime=as.numeric(matrix_travel_time[inp1[kk],inp1[kk+1]])+ttime
        stime=data_stops_info[inp1[kk],2]+stime
      }
      ptime <-stime+ttime
      zz<-180+unlist(ETWindow)+unlist(LTWindow)
      
      if(ptime<=zz)
      {
        z<-inp2+0.1*inp2
        return(z)
      }
      else
      {
        diff<-(ptime-(180+ETWindow+LTWindow))/60
        diff<-trunc(diff)+1
        diff<-(diff*10*inp2)/100
        z<-inp2+diff
        return(z)
      }
    }
  }
}

#FUnction to mutate the population
mutate <-function(inp)
{
  if(length(inp)==3)
    return(inp)
  else
  {
    temp<-inp[length(inp)-1]
    inp[length(inp)-1]<-inp[2]
    inp[2]<-temp
    return(inp)
  }
}


#Function to calculate cost of the best solution
Cost_Top_sol <-function(cost,best)
{
  bcost<-mapply(function(x,y) x[[unlist(y)]] ,cost,best)
  return(sum(bcost))
}

#FUnction to generate new Population in GA which includes
# Crossover
# MUtation

NewGen_Pop <- function(inp1,inp2,z)
{
  
  while (length(inp1) < InitPopSize)
  {
    # Mutation
    if (runif(1,0,1)< z) 
    {
      parent<-resample(inp1,size =1)
      temp<-parent[[1]]
      t<-lapply(temp, function(z) mutate(z))
      inp1<-list.append(inp1,t)
      
    }
    # Crossover
    else 
    {
      inp2<-setdiff(inp2,inp2[1])
      # length of elements to be swapped 
      elements_to_be_swapped<-resample(inp2,size = round((resample(0.90:0.95, size = 1, replace = F)*length(inp2)/10),0),replace = FALSE)
      #Pick 2 parents
      parents<-resample(inp1,size =2,replace = FALSE)
      
      while(length(elements_to_be_swapped)>1)
      {
        d <-resample(elements_to_be_swapped,size = 2, replace = FALSE)
        elements_to_be_swapped<-setdiff(elements_to_be_swapped,d)
        parents<-rapply(parents, function(z) ifelse(z == d[1],d[2],ifelse(z==d[2],d[1], z)), how = "list")
      }    
      inp1<-list.append(inp1,parents[[1]])
      inp1<-list.append(inp1,parents[[2]])
      
    }
  }
  
  return(inp1)
}



#Genetic Algorithm on interations based on above functions
GA_routing_opt<- function(Pop_inp,Iterations,Prob_Mutate,pick_elite_pop_perc)
{
  
  cat("Running Genetic Algorithm for ", Iterations, " iterations now \n\n\n")
  
  #Applying Penalty on Inital Population
  
  rt<-rapply(Pop_inp, function(x) Total_time_calc(x),how = "list")
  p1<-rapply(rt, function(x) PenConst1(x),how = "list")
  p2 <-mapply(function(x,y) mapply(function(x,y) mapply(function(x,y) PenConst2(x,y),x,y,SIMPLIFY=FALSE),x,y,SIMPLIFY=FALSE),Pop_inp,p1,SIMPLIFY=FALSE)
  p3<-mapply(function(x,y) mapply(function(x,y) mapply(function(x,y) PenConst3(x,y),x,y,SIMPLIFY=FALSE),x,y,SIMPLIFY=FALSE),Pop_inp,p2,SIMPLIFY=FALSE)
  
  #Applying Cost function on Inital Population
  
  Route_count<-lapply(Pop_inp,function(x) lapply(x,function(x) no_routes(x) ))
  f_score <-lapply(p3,function(x) lapply(x,function(x) final_score(x)))
  c<-mapply(function(x,y) mapply(function(x,y) route_cost(x,y),x,y,SIMPLIFY = FALSE),Route_count,f_score,SIMPLIFY=FALSE)
  sorted <-lapply(c,function(x) order(unlist(x),decreasing = FALSE))
  
  #Calculate Elite Population Percentage 
  
  elite=round(pick_elite_pop_perc*InitPopSize,0)
  sorted <-lapply(sorted,function(x) x[1:elite])
  Routes <- list()
  
  # Main Iteration Loop
  best_cost<-c()
  best_min<-c()
  best_Route_count<-c()
  
  
  for (i in 1:Iterations) 
  {
    
    ##Pick Elite Population
    Elitepop <-mapply(function(x,y) Filter_ElitePop(x,y),Pop_inp,sorted,SIMPLIFY = FALSE)
    Routes <<- list.append(Routes, Elitepop)
    
    #Mutation Probability
    mut =Prob_Mutate/i
    
    #Generate New Gen Population
    Pop_inp<-mapply(function(x,y,z) NewGen_Pop(x,y,z),Elitepop,cluster,Prob_Mutate,SIMPLIFY = FALSE)
    
    #Penalty functins on new gen
    rt<- rapply(Pop_inp, function(x) Total_time_calc(x),how = "list")
    p1<-rapply(rt, function(x) PenConst1(x),how = "list")
    
    p2 <-mapply(function(x,y) mapply(function(x,y) mapply(function(x,y) PenConst2(x,y),x,y,SIMPLIFY=FALSE),x,y,SIMPLIFY=FALSE),Pop_inp,p1,SIMPLIFY=FALSE)
    p3<-mapply(function(x,y) mapply(function(x,y) mapply(function(x,y) PenConst3(x,y),x,y,SIMPLIFY=FALSE),x,y,SIMPLIFY=FALSE),Pop_inp,p2,SIMPLIFY=FALSE)
    
    # Cost functions on new gen
    Route_count<-lapply(Pop_inp,function(x) lapply(x,function(x) length(x) ))
    f_score <-lapply(p3,function(x) lapply(x,function(x) final_score(x)))
    c<-mapply(function(x,y) mapply(function(x,y) route_cost(x,y),x,y,SIMPLIFY = FALSE),Route_count,f_score,SIMPLIFY=FALSE)
    sorted <-lapply(c,function(x) order(unlist(x),decreasing = FALSE))
    
    #Pick Elite  
    sorted <-lapply(sorted,function(x) x[1:elite])
    
    cat("Iteration", i, "\n")
    
    ##Print Best solution of the iterarion
    best<-sapply(sorted, `[[`, 1,simplify = F)
    
    best_cost[i]<- Cost_Top_sol(c,best)
    best_min[i]<-Cost_Top_sol(f_score,best)
    best_Route_count[i]<-Cost_Top_sol(Route_count,best)
    
    print(paste0("Cost =",best_cost[i],", Total Minutes =",best_min[i],", Routes =",best_Route_count[i] ))
  }
  
  plot(c(1:length(best_cost)),best_cost,xlab="Number of Iterations",ylab="Total Cost",xlim=c(1,Iterations),ylim=c(19000,30000),type='s',col="blue",main="Fitness over time")
  Costs_iterations <<- best_cost
  Minutes_iterations <<- best_min
  Routes_Iterations <<- best_Route_count
  
  
  return("GA Completed running. Verify results")
}

#Creating function to read input parameters from the user.
input_param <- function()
{ 
  cat("Enter Min stops per vehicle")
  m <- as.integer(readline(prompt = ""))
  cat("Enter Max stops per vehicle")
  n <- as.integer(readline(prompt = ""))
  cat("Enter Max Route Time \n")
  o<-as.integer(readline(prompt = ""))
  cat("Enter Max_Early Time Windows\n")
  p<-as.integer(readline(prompt = ""))
  cat("Enter Max_Late Time Windows \n")
  q<-as.integer(readline(prompt = ""))
  cat("Enter Fixed Cost Per Vehicle ")
  r<-as.integer(readline(prompt = ""))
  cat("Enter Fixed Cost for Per Min Travel ")
  s<-as.integer(readline(prompt = ""))
  return(c(m,n,o,p,q,r,s))
}

######################Program Execution Begins here ###################

param<-input_param()
MinStops<-param[1]
MaxStops<-param[2]
MaxRTime<-param[3]
ETWindow<-param[4]
LTWindow<-param[5]
CPRoute<-param[6]
CPMin<-param[7]

InitPopSize=200

#Generate Initial population
Pop_List_initial<-mapply(function(x,y,z) InitPop(x,y,z),cluster,MinStops,MaxStops,SIMPLIFY = FALSE)

#Execute Genetic Algorithm+ 
GA_routing_opt(Pop_List_initial,200,0.3,0.35)


#Fetching up the latest routes from Depo1 at the end of Genetic Algorithm
#Routes
#Costs_iterations
#Minutes_iterations
#Routes_Iterations

Depo1_Routes <- Routes[[1]][[1]]
length1<- length(Depo1_Routes)
Depo1_final_Routes <- Depo1_Routes[[length1]]
Routes_in_Depo1 <- length(Depo1_final_Routes)

Depo2_Routes <- Routes[[1]][[2]]
length2<- length(Depo2_Routes)
Depo2_final_Routes <- Depo2_Routes[[length2]]
Routes_in_Depo2 <- length(Depo2_final_Routes)

Depo3_Routes <- Routes[[1]][[3]]
length3<- length(Depo3_Routes)
Depo3_final_Routes <- Depo3_Routes[[length3]]
Routes_in_Depo3 <- length(Depo3_final_Routes)


Depo4_Routes <- Routes[[1]][[4]]
length4<- length(Depo4_Routes)
Depo4_final_Routes <- Depo4_Routes[[length4]]
Routes_in_Depo4 <- length(Depo4_final_Routes)


######## VIsualizations #################

#Plots on Total costs and Total time savings
plot(c(1:length(Costs_iterations)),Costs_iterations,xlab="Number of Iterations",ylab="Total Cost",xlim=c(1,length(Costs_iterations)),
     ylim=c(min(Costs_iterations) - 200 ,max(Costs_iterations) + 200),type='s',col="blue",main="Cost over iterations")

plot(c(1:length(Minutes_iterations)),Minutes_iterations,xlab="Number of Iterations",ylab="Total time",xlim=c(1,length(Minutes_iterations)),
     ylim=c(min(Minutes_iterations) - 200 ,max(Minutes_iterations) + 200),type='s',col="blue",main="Total time(Minutes) over iterations")

#Plot on Cost Savings to the client
max_val <- max(Costs_iterations)
min_val <- min(Costs_iterations)
norm_diff <- (max_val - min_val)/max_val*100
act_cost <- 100 - norm_diff 

slices <- c(norm_diff, act_cost) 
lbls <- c("Savings","New_Costs")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Cost Savings percentage")

#Plot on Time Savings to the client
max_val <- max(Minutes_iterations)
min_val <- min(Minutes_iterations)
norm_diff <- (max_val - min_val)/max_val*100
act_cost <- 100 - norm_diff 

slices <- c(norm_diff, act_cost) 
lbls <- c("Savings","New_Costs")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Time Savings in Percentages")

##Describe the savings in terms of Costs every day and in a month and in a year
##Showcase the final output of routes from each Depo
Depo_num <- 1
for(i in c("Depo1_final_Routes","Depo2_final_Routes","Depo3_final_Routes","Depo4_final_Routes")){
  Depo_name <- i
  cat("Routes from Depo No:", Depo_num ,"\n\n")
  print.table(get(Depo_name))
  cat("\n\n")
  Depo_num <- Depo_num + 1
}
