#ISYE6644
#Mini Project 1: Let's Gamble
#Yurkofsky

#Things you can change in this simulation!
B = 300  #number of games played for each strategy
set.seed(6) #seed

#initialize lists to hold outcomes
player_wins <- NULL
final_dealer <- NULL
final_player <- NULL
hand_sum <- NULL
#strategies to try (ie., player hits on the following hand values)
strat <- c(14,15,16,17,18,19)

#run 10 games

for (b in 1:B) {
  
  for (var in strat) {
    #create one deck
    deck = c(rep("A",4), rep.int(10,16), rep.int(9,4), rep.int(8,4), rep.int(7,4), rep.int(6,4), rep.int(5,4), rep.int(4,4), rep.int(3,4), rep.int(2,4))
    #given casinos typically use multiple deck, we'll use 4 decks
    cards = c(deck, deck)
    decks <- sum(cards=="A") / 4
  
  
    #####PLAYER#####
  
  
    #create the players hand by randomly sampling two cards
    #from the 4 decks without replacement
    hand_player = sample(cards, 2, replace = FALSE)
  
    #remove the players hand from the deck
    for(i in hand_player){
      if(is.element(i,cards)) 
        cards <-cards[-match(i,cards)]}
  
    #check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }

    #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                     ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                     ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                            ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                   ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                          ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                 ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                        ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                               ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
  
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")
  
    #if hit, take a take a new card from the remaining cards in the deck and add it to players hand          
    newcard <- ifelse(hit_stay=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_player <-  append(hand_player, newcard)
  
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
  
    #again, check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }
  
    #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                     ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                            ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                   ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                          ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                 ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                        ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                               ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                      ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
  
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")
  
    #if hit, take a take a new card from the remaining cards in the deck and add it to players hand          
    newcard <- ifelse(hit_stay=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_player <-  append(hand_player, newcard)
  
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
  
    #again, check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }
  
    #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                     ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                            ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                   ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                          ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                 ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                        ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                               ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + var, 
                                                                      ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
  
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")#if hit, take a take a new card from the remaining cards in the deck and add it to players hand          
    newcard <- ifelse(hit_stay=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_player <-  append(hand_player, newcard)
  
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
  
    #again, check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }
  
    #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                     ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                            ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                   ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                          ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                 ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                        ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                               ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                      ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
  
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")#if hit, take a take a new card from the remaining cards in the deck and add it to players hand          
    newcard <- ifelse(hit_stay=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_player <-  append(hand_player, newcard)
  
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
  
    #again, check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }
  
  #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                     ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                            ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                   ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                          ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                 ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                        ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                               ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                      ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
  
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")#if hit, take a take a new card from the remaining cards in the deck and add it to players hand          
    newcard <- ifelse(hit_stay=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_player <-  append(hand_player, newcard)
  
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
  
    #again, check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }
  
    #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                     ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                            ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                   ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                          ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                 ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                        ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                               ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                      ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
  
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")#if hit, take a take a new card from the remaining cards in the deck and add it to players hand          
    newcard <- ifelse(hit_stay=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_player <-  append(hand_player, newcard)
  
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
  
    #again, check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }
  
    #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
    
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")#if hit, take a take a new card from the remaining cards in the deck and add it to players hand          
    newcard <- ifelse(hit_stay=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_player <-  append(hand_player, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #again, check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }
    
    #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
    
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")#if hit, take a take a new card from the remaining cards in the deck and add it to players hand          
    newcard <- ifelse(hit_stay=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_player <-  append(hand_player, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #again, check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }
    
    #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
    
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")#if hit, take a take a new card from the remaining cards in the deck and add it to players hand          
    newcard <- ifelse(hit_stay=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_player <-  append(hand_player, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #again, check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }
    
    #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
    
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")#if hit, take a take a new card from the remaining cards in the deck and add it to players hand          
    newcard <- ifelse(hit_stay=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_player <-  append(hand_player, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #again, check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_player
    for(i in hand_player){
      if(is.element(i,"A"))
        hand_noaces <- (hand_player[-match(i, hand_player)])
    }
    
    #count how many aces
    num_aces <- length(hand_player) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_player), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_noaces), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_player), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_player), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_player), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_player), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_player), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_player, na.rm=TRUE)))))))))))
    
    #player decides to stay or hit based on the total value
    hit_stay <- ifelse(sum(hand_sum) > var, "stay", "hit")
    
    #get final sum of players hand
    final_player <- append(final_player, sum(as.numeric(hand_sum), na.rm = TRUE))
    
    
    ####DEALER#####
    
    
    #create dealers hand by randomly sampling two cards
    #from the 4 decks without replacement
    hand_dealer = sample(cards, 2, replace = FALSE)
    
    #remove the dealers hand from the deck
    for(i in hand_dealer){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_dealer
    for(i in hand_dealer){
      if(is.element(i,"A"))
        hand_noaces <- (hand_dealer[-match(i, hand_dealer)])
    }
    
    #count how many aces
    num_aces <- length(hand_dealer) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_dealer), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_dealer), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_dealer, na.rm=TRUE)))))))))))
    
    #dealer wins, loses or hits based on sum of their hand and players hand (tie goes to dealer)
    hit_win_lose <- ifelse(sum(as.numeric(hand_player), na.rm = TRUE) >21, "win", 
                           ifelse(sum(as.numeric(hand_sum), na.rm = TRUE) > 21, "lose",
                                  ifelse((sum(as.numeric(hand_sum), na.rm = TRUE) >= sum(as.numeric(hand_player), na.rm = TRUE)) & sum(as.numeric(hand_sum), na.rm = TRUE) <= 21, "win", "hit")))
    
    #if hit, take a take a new card from the remaining cards in the deck and add it to dealers hand    
    newcard <- ifelse(hit_win_lose=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_dealer <-  append(hand_dealer, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_dealer
    for(i in hand_dealer){
      if(is.element(i,"A"))
        hand_noaces <- (hand_dealer[-match(i, hand_dealer)])
    }
    
    #count how many aces
    num_aces <- length(hand_dealer) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_dealer), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_dealer), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_dealer, na.rm=TRUE)))))))))))
    
    #dealer wins, loses or hits based on sum of their hand and players hand (tie goes to dealer)
    hit_win_lose <- ifelse(sum(as.numeric(hand_player), na.rm = TRUE) >21, "win", 
                           ifelse(sum(as.numeric(hand_sum), na.rm = TRUE) > 21, "lose",
                                  ifelse((sum(as.numeric(hand_sum), na.rm = TRUE) >= sum(as.numeric(hand_player), na.rm = TRUE)) & sum(as.numeric(hand_sum), na.rm = TRUE) <= 21, "win", "hit")))
    
    #if hit, take a take a new card from the remaining cards in the deck and add it to dealers hand    
    newcard <- ifelse(hit_win_lose=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_dealer <-  append(hand_dealer, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_dealer
    for(i in hand_dealer){
      if(is.element(i,"A"))
        hand_noaces <- (hand_dealer[-match(i, hand_dealer)])
    }
    
    #count how many aces
    num_aces <- length(hand_dealer) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_dealer), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_dealer), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_dealer, na.rm=TRUE)))))))))))
    
    #dealer wins, loses or hits based on sum of their hand and players hand (tie goes to dealer)
    hit_win_lose <- ifelse(sum(as.numeric(hand_player), na.rm = TRUE) >21, "win", 
                           ifelse(sum(as.numeric(hand_sum), na.rm = TRUE) > 21, "lose",
                                  ifelse((sum(as.numeric(hand_sum), na.rm = TRUE) >= sum(as.numeric(hand_player), na.rm = TRUE)) & sum(as.numeric(hand_sum), na.rm = TRUE) <= 21, "win", "hit")))
    
    #if hit, take a take a new card from the remaining cards in the deck and add it to dealers hand    
    newcard <- ifelse(hit_win_lose=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_dealer <-  append(hand_dealer, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_dealer
    for(i in hand_dealer){
      if(is.element(i,"A"))
        hand_noaces <- (hand_dealer[-match(i, hand_dealer)])
    }
    
    #count how many aces
    num_aces <- length(hand_dealer) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_dealer), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_dealer), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_dealer, na.rm=TRUE)))))))))))
    
    #dealer wins, loses or hits based on sum of their hand and players hand (tie goes to dealer)
    hit_win_lose <- ifelse(sum(as.numeric(hand_player), na.rm = TRUE) >21, "win", 
                           ifelse(sum(as.numeric(hand_sum), na.rm = TRUE) > 21, "lose",
                                  ifelse((sum(as.numeric(hand_sum), na.rm = TRUE) >= sum(as.numeric(hand_player), na.rm = TRUE)) & sum(as.numeric(hand_sum), na.rm = TRUE) <= 21, "win", "hit")))
    
    #if hit, take a take a new card from the remaining cards in the deck and add it to dealers hand    
    newcard <- ifelse(hit_win_lose=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_dealer <-  append(hand_dealer, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_dealer
    for(i in hand_dealer){
      if(is.element(i,"A"))
        hand_noaces <- (hand_dealer[-match(i, hand_dealer)])
    }
    
    #count how many aces
    num_aces <- length(hand_dealer) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_dealer), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_dealer), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_dealer, na.rm=TRUE)))))))))))
    
    #dealer wins, loses or hits based on sum of their hand and players hand (tie goes to dealer)
    hit_win_lose <- ifelse(sum(as.numeric(hand_player), na.rm = TRUE) >21, "win", 
                           ifelse(sum(as.numeric(hand_sum), na.rm = TRUE) > 21, "lose",
                                  ifelse((sum(as.numeric(hand_sum), na.rm = TRUE) >= sum(as.numeric(hand_player), na.rm = TRUE)) & sum(as.numeric(hand_sum), na.rm = TRUE) <= 21, "win", "hit")))
    
    #if hit, take a take a new card from the remaining cards in the deck and add it to dealers hand    
    newcard <- ifelse(hit_win_lose=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_dealer <-  append(hand_dealer, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_dealer
    for(i in hand_dealer){
      if(is.element(i,"A"))
        hand_noaces <- (hand_dealer[-match(i, hand_dealer)])
    }
    
    #count how many aces
    num_aces <- length(hand_dealer) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_dealer), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_dealer), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_dealer, na.rm=TRUE)))))))))))
    
    #dealer wins, loses or hits based on sum of their hand and players hand (tie goes to dealer)
    hit_win_lose <- ifelse(sum(as.numeric(hand_player), na.rm = TRUE) >21, "win", 
                           ifelse(sum(as.numeric(hand_sum), na.rm = TRUE) > 21, "lose",
                                  ifelse((sum(as.numeric(hand_sum), na.rm = TRUE) >= sum(as.numeric(hand_player), na.rm = TRUE)) & sum(as.numeric(hand_sum), na.rm = TRUE) <= 21, "win", "hit")))
    
    #if hit, take a take a new card from the remaining cards in the deck and add it to dealers hand    
    newcard <- ifelse(hit_win_lose=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_dealer <-  append(hand_dealer, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_dealer
    for(i in hand_dealer){
      if(is.element(i,"A"))
        hand_noaces <- (hand_dealer[-match(i, hand_dealer)])
    }
    
    #count how many aces
    num_aces <- length(hand_dealer) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_dealer), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_dealer), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_dealer, na.rm=TRUE)))))))))))
    
    #dealer wins, loses or hits based on sum of their hand and players hand (tie goes to dealer)
    hit_win_lose <- ifelse(sum(as.numeric(hand_player), na.rm = TRUE) >21, "win", 
                           ifelse(sum(as.numeric(hand_sum), na.rm = TRUE) > 21, "lose",
                                  ifelse((sum(as.numeric(hand_sum), na.rm = TRUE) >= sum(as.numeric(hand_player), na.rm = TRUE)) & sum(as.numeric(hand_sum), na.rm = TRUE) <= 21, "win", "hit")))
    
    #if hit, take a take a new card from the remaining cards in the deck and add it to dealers hand    
    newcard <- ifelse(hit_win_lose=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_dealer <-  append(hand_dealer, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #check if hand has aces and create alternate hand without aces
    hand_noaces <- hand_dealer
    for(i in hand_dealer){
      if(is.element(i,"A"))
        hand_noaces <- (hand_dealer[-match(i, hand_dealer)])
    }
    
    #count how many aces
    num_aces <- length(hand_dealer) - length(hand_noaces)
    #get final hand sum
    hand_sum <- ifelse(num_aces==0, sum(as.numeric(hand_dealer), na.rm=TRUE), 
                       ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 11, 
                              ifelse(num_aces ==1 & sum(as.numeric(hand_dealer), na.rm = TRUE) >10, sum(as.numeric(hand_noaces), na.rm=TRUE) + 1,
                                     ifelse(num_aces ==2 & sum(as.numeric(hand_dealer), na.rm = TRUE) <= 9, sum(as.numeric(hand_noaces), na.rm=TRUE) + 12,
                                            ifelse(num_aces==2 & sum(as.numeric(hand_dealer), na.rm=TRUE) >9, sum(as.numeric(hand_noaces), na.rm = TRUE) + 2, 
                                                   ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 13, 
                                                          ifelse(num_aces==3 & sum(as.numeric(hand_dealer), na.rm = TRUE) >8, sum(as.numeric(hand_noaces), na.rm = TRUE) + 3, 
                                                                 ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) <=7, sum(as.numeric(hand_noaces), na.rm = TRUE) + 14, 
                                                                        ifelse(num_aces==4 & sum(as.numeric(hand_dealer), na.rm = TRUE) >7, sum(as.numeric(hand_noaces), na.rm=TRUE) +4, sum(as.numeric(hand_dealer, na.rm=TRUE)))))))))))
    
    #dealer wins, loses or hits based on sum of their hand and players hand (tie goes to dealer)
    hit_win_lose <- ifelse(sum(as.numeric(hand_player), na.rm = TRUE) >21, "win", 
                           ifelse(sum(as.numeric(hand_sum), na.rm = TRUE) > 21, "lose",
                                  ifelse((sum(as.numeric(hand_sum), na.rm = TRUE) >= sum(as.numeric(hand_player), na.rm = TRUE)) & sum(as.numeric(hand_sum), na.rm = TRUE) <= 21, "win", "hit")))
    
    #if hit, take a take a new card from the remaining cards in the deck and add it to dealers hand    
    newcard <- ifelse(hit_win_lose=="hit", sample(cards, 1, replace = FALSE), NA)
    hand_dealer <-  append(hand_dealer, newcard)
    
    #update cards to remove new card
    for(i in newcard){
      if(is.element(i,cards)) 
        cards<-cards[-match(i,cards)]}
    
    #get final sum of dealers hand
    final_dealer <- append(final_dealer, sum(as.numeric(hand_sum), na.rm = TRUE))
    
    
    ####KEEPING SCORE####
    
    
    #keeping score
    game_result_player <- ifelse(hit_win_lose=="win", 0, 1)
    player_wins <- append(player_wins, game_result_player)
  }

}

strat_hit14 <- player_wins[1:B]
strat_hit15 <- player_wins[(B+1):(2*B)]
strat_hit16 <- player_wins[((2*B)+1):(3*B)]
strat_hit17 <- player_wins[((3*B)+1):(4*B)]
strat_hit18 <- player_wins[((4*B)+1):(5*B)]
strat_hit19 <- player_wins[((5*B)+1):(6*B)]

#strategy 14
par(mfrow=c(1,2))
  
simulation_outcome <- table(strat_hit14)
prob_player_win <- round((sum(strat_hit14 / B) * 100),2)
prob_dealer_win <- 100 - prob_player_win
labels <- c(paste0("Dealer Wins-",prob_dealer_win,"%"), paste0("Player Wins-", prob_player_win,"%"))
pie(simulation_outcome, labels = labels,
    main="Simulation 1: Hit on 14")
  
counts <- table(final_player[1:B])
plot <- barplot(counts, main="Player Hand Totals",
                xlab="Sum of Player Hand")


#strategy 15
par(mfrow=c(1,2))

simulation_outcome <- table(strat_hit15)
prob_player_win <- round((sum(strat_hit15 / B) * 100),2)
prob_dealer_win <- 100 - prob_player_win
labels <- c(paste0("Dealer Wins-",prob_dealer_win,"%"), paste0("Player Wins-", prob_player_win,"%"))
pie(simulation_outcome, labels = labels,
    main="Simulation 1: Hit on 15")

counts <- table(final_player[((1*B)+1):(2*B)])
plot <- barplot(counts, main="Player Hand Totals",
                xlab="Sum of Player Hand")


#strategy 16
par(mfrow=c(1,2))

simulation_outcome <- table(strat_hit16)
prob_player_win <- round((sum(strat_hit16 / B) * 100),2)
prob_dealer_win <- 100 - prob_player_win
labels <- c(paste0("Dealer Wins-",prob_dealer_win,"%"), paste0("Player Wins-", prob_player_win,"%"))
pie(simulation_outcome, labels = labels,
    main="Simulation 1: Hit on 16")

counts <- table(final_player[((2*B)+1):(3*B)])
plot <- barplot(counts, main="Player Hand Totals",
                xlab="Sum of Player Hand")


#strategy 17
par(mfrow=c(1,2))

simulation_outcome <- table(strat_hit17)
prob_player_win <- round((sum(strat_hit17 / B) * 100),2)
prob_dealer_win <- 100 - prob_player_win
labels <- c(paste0("Dealer Wins-",prob_dealer_win,"%"), paste0("Player Wins-", prob_player_win,"%"))
pie(simulation_outcome, labels = labels,
    main="Simulation 1: Hit on 17")

counts <- table(final_player[((3*B)+1):(4*B)])
plot <- barplot(counts, main="Player Hand Totals",
                xlab="Sum of Player Hand")


#strategy 18
par(mfrow=c(1,2))

simulation_outcome <- table(strat_hit18)
prob_player_win <- round((sum(strat_hit18 / B) * 100),2)
prob_dealer_win <- 100 - prob_player_win
labels <- c(paste0("Dealer Wins-",prob_dealer_win,"%"), paste0("Player Wins-", prob_player_win,"%"))
pie(simulation_outcome, labels = labels,
    main="Simulation 1: Hit on 18")

counts <- table(final_player[((4*B)+1):(5*B)])
plot <- barplot(counts, main="Player Hand Totals",
                xlab="Sum of Player Hand")


#strategy 19
par(mfrow=c(1,2))

simulation_outcome <- table(strat_hit19)
prob_player_win <- round((sum(strat_hit19 / B) * 100),2)
prob_dealer_win <- 100 - prob_player_win
labels <- c(paste0("Dealer Wins-",prob_dealer_win,"%"), paste0("Player Wins-", prob_player_win,"%"))
pie(simulation_outcome, labels = labels,
    main="Simulation 1: Hit on 19")

counts <- table(final_player[((5*B)+1):(6*B)])
plot <- barplot(counts, main="Player Hand Totals",
                xlab="Sum of Player Hand")

#means
mean(final_player[1:B])
mean(final_player[((1*B)+1):(2*B)])
mean(final_player[((2*B)+1):(3*B)])
mean(final_player[((3*B)+1):(4*B)])
mean(final_player[((4*B)+1):(5*B)])
mean(final_player[((5*B)+1):(6*B)])
