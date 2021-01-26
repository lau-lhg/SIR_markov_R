#debugger
mov<-c()

#VACCINATION STRATEGY
strategy<-c('mobility','mortality')

#INITIAL AMOUNT OF PEOPLE IN EACH SUBGROUP

#age group 0-20
S_1<- 8000
I_1<- 0
#age group 21-40
S_2<- 8000
I_2<- 1
#age group 41-60
S_3<- 8000
I_3<- 0
#age group 60+
S_4<- 8000
I_4<- 0

#Put them in vectors for easier access
S<-c(S_1, S_2, S_3, S_4)
I<-c(I_1, I_2, I_3, I_4)

#total dead
D<- 0
#total recuperated
R<- 0

#average daily vaccination capacity (consider efficacy)
V<- 100

#SUBGROUP parameters

#death rate
k<-c(0.01,0.02,0.09,0.13)
#recuperation rate
b<-(1-k)*(1/14)
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
    times[i]<-rexp(n=1,rate=params$a[i]*pop[(2*i-1)]*infected/N)
    #death
    times[2*i]<-rexp(n=1,rate=params$k[i]*pop[(2*i)])
    #vaccination
    times[3*i]<-rexp(n=1,rate=params$lambda[i])
    #recuperation
    times[4*i]<-rexp(n=1,rate=params$b[i]*pop[(2*i)])
  }
  return(times)
}

get_lambda <- function(V, N, P){
  #distribute and normalize
  lambda<-V*N*P/sum(N*P)
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


#we are interested in two values
out_time<-vector(mode='numeric')
out_dead<-vector(mode='numeric')

#number of iterations
limit<-1
#herd immunity at 75 percent
herd_immunity<-sum(pop)*0.75

#we will run the simulations with a range of probabilities
for (j in 1:limit){
  #time accumulator 
  t<-0
  #new probability parameters for iteration
  strategyR<-c(j/limit,1-j/limit)
  
  #run simulation
  while( sum(pop[9])<=herd_immunity & sum(pop[c(2,4,6,8)])>=0 ){
    #select strategy according to probabilities
    strat<-sample(c('mobility','mortality'), size=1, prob=strategyR)
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
    
    #DEBUGGER
    mov<-append(mov,chosen)
  }
  #save time and number of deaths for iteration
  out_time<-append(out_time,t,after=j)
  out_dead<-append(out_dead,pop[10],after=j)
  #reset population values for new iterations
  #pop<-as.vector(c(rbind(S,I),D,R))
}



