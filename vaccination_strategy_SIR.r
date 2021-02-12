#keep track of all movements
mov<-c()

#VACCINATION STRATEGY
strategy<-c('mobility','mortality')

#INITIAL AMOUNT OF PEOPLE IN EACH SUBGROUP

#age group 0-20
I_1<- 26
S_1<- 44403 - I_1
#age group 21-40
I_2<- 179
S_2<- 40992 - I_2
#age group 41-60
I_3<- 148
S_3<- 29839 - I_3
#age group 60+
I_4<- 56
S_4<- 15029 - I_4

#Put them in vectors for easier access
S<-c(S_1, S_2, S_3, S_4)
I<-c(I_1, I_2, I_3, I_4)

#total dead
D<- 0
#total recuperated
R<- 4018

#average daily vaccination capacity (consider efficacy)
V<- 10

#SUBGROUP parameters

#death rate
k<-c(0.01,0.02,0.09,0.13)
#recuperation rate
b<-(1-k)
#mobility
a<-c(2.25,3,2.75,2)

#vaccines per group per day, set to 0 initially
lambda<-c(0,0,0,0)

#population in a vector
pop<-as.vector(c(rbind(S,I),D,R))
names(pop)<-c('S1','I1','S2','I2','S3','I3','S4','I4','D','R')

#this way we can simply add the vector of the event to our pop counter
poss<-data.frame(inf1=c(-1,1,rep(0,8)),
                 inf2=c(0,0,-1,1,rep(0,6)),
                 inf3=c(rep(0,4),-1,1,rep(0,4)),
                 inf4=c(rep(0,6),-1,1,0,0),
                 die1=c(0,-1,rep(0,6),1,0),
                 die2=c(rep(0,3),-1,rep(0,4),1,0),
                 die3=c(rep(0,5),-1,0,0,1,0),
                 die4=c(rep(0,7),-1,1,0),
                 rec1=c(0,-1,rep(0,7),1),
                 rec2=c(rep(0,3),-1,rep(0,5),1),
                 rec3=c(rep(0,5),-1,rep(0,3),1),
                 rec4=c(rep(0,7),-1,0,1),
                 vac1=c(-1,rep(0,8),1),
                 vac2=c(0,0,-1,rep(0,6),1),
                 vac3=c(rep(0,4),-1,rep(0,4),1),
                 vac4=c(rep(0,6),-1,0,0,1))

#put subgroup parameters into a data frame too, for easier handling
params<-data.frame(a, b, k, lambda)

#we will call this function every time we iterate inside the simulation
get_times<-function(pop,params){
  times<-c(rep(0,16))
  #population
  N<-sum(pop)
  #all infected
  infected<-sum(pop[c(2,4,6,8)])
  #iterate over each group
  for (i in 1:4){
    #infection
    times[i]<-rexp(n=1,rate=(0.06)*params$a[i]*pop[(2*i-1)]*infected/N)
    #death
    times[i+4]<-rexp(n=1,rate=params$k[i]*pop[(2*i)]*(1/17))
    #vaccination
    times[i+12]<-rexp(n=1,rate=params$lambda[i]*pop[2*i-1]/pop[2*i-1])
    #recuperation
    times[i+8]<-rexp(n=1,rate=params$b[i]*pop[(2*i)]*(1/14))
  }
  return(times)
}

get_lambda <- function(V, N, P){
  #distribute and normalize
  lambda<-c(0,0,0,0)
  lambda [ which(P*N==max(P*N))] <- V
  return(lambda)
}


set_lambda<-function(S, params, strategy, V){
  #id parameter of interest from strategy
  if (strategy=='mobility'){
    interest<-params$a
  }else if(strategy=='mortality'){
    interest<-params$k
  }
  #get lambda values according to priority parameter
  lambda<-get_lambda(P=interest,V=V,N=S)
  #return new lambda vector
  return(lambda)
}

#MAIN SIMULATION ITERATORS
#We will fluctuate between strategies with different probability values (limit)
#Each probability combination will be run e simulations
#each simulation will run until herd immunity is achieved or no active cases remain
#At the end of each simulation, a report will be printed


#report titles
print(c('mobility','pandemic_time','death_toll','vaccines','total_cases'))

#number of iterations on strategy probability
limit<-5
#number of repetitions on with each strategy parameters
reps<-10
#amount of rec people needed for pandemic to end
herd_immunity<-sum(pop[c(1:8,10)])*0.7

#we will run the simulations with a range of probabilities
for (j in 0:limit){
  #new probability parameters for iteration
  strategyR<-c(j/limit,1-j/limit)
  
  #repetitions per value
  for (e in 1:reps){
    
    #time accumulator 
    t<-0
    
    #run simulation
    while( sum(pop[c(2,4,6,8,10)]) < herd_immunity ){
      
      #select strategy according to probabilities
      strat<-sample(strategy, size=1, prob=strategyR)
      #update lambda values according to strategy
      params$lambda<-set_lambda(S=pop[c(1,3,5,7)],params=params,strategy=strat, V=V)
      #obtain times
      times<-get_times(pop,params)
      #identify next event
      chosen<-which(times==min(times[which(times>0)]))[1]
      #execute next event
      pop<-pop+(poss[,chosen])
      #accumulate time
      t<-t+times[chosen]
      #update amount of susceptible people needed for pandemic to end
      herd_immunity<-sum(pop[c(1:8,10)])*0.7
      #register event
      mov<-append(mov,chosen)
      
    }#end of simulation
    
    #save important output info for analysis
    tot_vacc<-sum(mov>=13)
    tot_cases<-sum(mov<=4)
    output<-c(strategyR[1],t,pop[[9]],tot_vacc,tot_cases)
    
    #print and save output
    print(output)
    
    #reset population values for new simulation repetitions
    pop<-as.vector(c(rbind(S,I),D,R))
    mov<-c()
    
  }#end of value repetitions
  
}#end of whole enchilada

print('DONE')




