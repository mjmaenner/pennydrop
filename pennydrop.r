# penny drop simulator

roll <- function(){
  roller <- sample(1:6,1,replace=F)
  return(roller)
}

strategy <- function(gamestatus){
  up.pennies <- sum(gamestatus[4:8] )
  strat<- if(gamestatus[1]==1){
            gamestatus[10]
          }else{
            gamestatus[11]
          }
  decision <- ifelse(up.pennies< strat, 1,0)  
  return(decision)
}

pennydrop <- function(nplayers=length(strategies), npennies=10, strategies=c(3,3), printgame=FALSE){
  # gamestatus is a vector describing the status of the game
  # it is defined as c(pturn, p1penny, p2penny, h1, h2, h3, h4, h5, bank,strategy1, strategy2) 
  gamestatus <- c(1,npennies,npennies,0,0,0,0,0,0,stratp1,stratp2)
  turns <- 1
  playermove <- 1

  while(gamestatus[2]> 0 & gamestatus[3] > 0){
  
  
  #if(playermove ==1){
    rollresult <- roll()
    
    if(gamestatus[rollresult+3]==0 & rollresult<6 ){
      gamestatus[rollresult+3]<-1
    
      #deduct a penny from active player
      if(gamestatus[1]==1){
        gamestatus[2] <- gamestatus[2]-1
      }else{
        gamestatus[3] <- gamestatus[3]-1
      }  
      playermove <- strategy(gamestatus)  
      
      
    }else if(rollresult==6){
      gamestatus[9] <- gamestatus[9] +1
      
      #deduct a penny from active player
      if(gamestatus[1]==1){
        gamestatus[2] <- gamestatus[2]-1
      }else{
        gamestatus[3] <- gamestatus[3]-1
      }
      playermove <- strategy(gamestatus)  
      
        
    }else{
      #bad roll
      num.pennies <- sum(gamestatus[4:8])
      gamestatus[4:8] <- 0
      
      #add pennies to active player
      if(gamestatus[1]==1){
        gamestatus[2] <- gamestatus[2] + num.pennies
      }else{
        gamestatus[3] <- gamestatus[3] + num.pennies
      }
     playermove = 0 
      
    }
  #}
    #if they don't choose to roll, it defaults to pass, and they switch turns
    
  if(printgame==TRUE){
    print(gamestatus)
  }
    
    #switch turns
    if(playermove==0){
      gamestatus[1] <- ifelse(gamestatus[1]==1,2,1)
      turns <- turns +1
      playermove <-1
    }
     
      }


  winner <- ifelse(gamestatus[2]==0,1,2)
  return(c(winner,turns))
}  
  
#games <- as.data.frame(t(replicate(20000,pennydrop(npennies=50,stratp1=3, stratp2=3),simplify = T)))

#games <- as.data.frame(t(replicate(20000,pennydrop(npennies=10,stratp1=3, stratp2=3),simplify = T)))


#table(games$V1)

#pennydrop(stratp1=2, stratp2=3, printgame=TRUE)




# questions to ask
# game length vs # starting pennies and # players - both individually and together
# p1 advantage for pennies and players and strategies
# do p1 and p2 wins have different game lengths?

# average number of pennies up when rolling - vary by strategy
# average outcome of successful rolls - vary by strategy

# create a game summary that runs when a game is completed talking about win percents, game length, etc.
# table(games$V1)
