# penny drop simulator


roll <- function(){
  roller <- sample(1:6,1,replace=F)
  return(roller)
}

strategy5 <- function(gamestatus){
  up.pennies <- sum(gamestatus[4:8] )
  decision <- ifelse(up.pennies< 5, 1,0)  
  return(decision)
}

strategy4 <- function(gamestatus){
  up.pennies <- sum(gamestatus[4:8] )
  decision <- ifelse(up.pennies< 4, 1,0)  
  return(decision)
}


strategy3 <- function(gamestatus){
  up.pennies <- sum(gamestatus[4:8] )
  decision <- ifelse(up.pennies< 3, 1,0)  
  return(decision)
}

strategy2 <- function(gamestatus){
  up.pennies <- sum(gamestatus[4:8] )
  decision <- ifelse(up.pennies< 2, 1,0)  
  return(decision)
}

strategy1 <- function(gamestatus){
  up.pennies <- sum(gamestatus[4:8] )
  decision <- ifelse(up.pennies< 1, 1,0)  
  return(decision)
}

decider <- function(gamestatus)
  if(gamestatus[1]==1){
    plan <- strategy2(gamestatus)
  }else{
    plan <- strategy3(gamestatus)
  }

pennydrop <- function(nplayers=2, npennies=10){
  #gamestatus is a vector describing the status of the game
  # it is defined as c(pturn, p1penny, p2penny, h1, h2, h3, h4, h5, bank) 
  gamestatus <- c(1,10,10,0,0,0,0,0,0)
  turns <- 1

  while(gamestatus[2]> 0 & gamestatus[3] > 0){
  playermove <- 1
  if(playermove ==1){
    rollresult <- roll()
    
    if(gamestatus[rollresult+3]==0 & rollresult<6 ){
      gamestatus[rollresult+3]<-1
    
      #deduct a penny from active player
      if(gamestatus[1]==1){
        gamestatus[2] <- gamestatus[2]-1
      }else{
        gamestatus[3] <- gamestatus[3]-1
      }  
    playermove <- decider(gamestatus)  
      
      
    }else if(rollresult==6){
      gamestatus[9] <- gamestatus[9] +1
      
      #deduct a penny from active player
      if(gamestatus[1]==1){
        gamestatus[2] <- gamestatus[2]-1
      }else{
        gamestatus[3] <- gamestatus[3]-1
      
      playermove <- decider(gamestatus)  
      }
        
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
  }
    #if they don't choose to roll, it defaults to pass, and they switch turns

    # print(gamestatus)
  
    #switch turns
    if(playermove==0){
      gamestatus[1] <- ifelse(gamestatus[1]==1,2,1)
      turns <- turns +1
    }
  
      }


  
  winner <- ifelse(gamestatus[2]==0,1,2)
  return(c(winner,turns))
}  
  
games <- as.data.frame(t(replicate(20000,pennydrop(),simplify = T)))

gamesummary <- function(gameresult){
  P1winner <- 
  P2winner <- 
}
