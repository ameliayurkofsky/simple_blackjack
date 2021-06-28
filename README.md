# simple_blackjack
Simulating a few strategies for playing a simple game of blackjack. 

This project tests the efficacy of six different strategies a player could use when playing the card game blackjack. The strategies tested focused on the optimal threshold for determining whether to ‘hit’, i.e.., take another card, or ‘stay’, i.e., not take another card. To do this I created a script in R that simulated 2,000 hands of blackjack where players were assigned a threshold of 14, 15, 16, 17, 18, or 19. The average probability of winning was taken for each of the strategies and the strategy with the highest average probability of winning was deemed the most successful. Under the assumptions that there was one dealer, one player, one deck of cards, and, in the case of a tie, that the win went to the dealer, I found that a player choosing a strategy of ‘hitting’ when their hand totaled less than or equal to 17 was the most successful (a win rate of 27.20%), followed very closely by ‘hitting’ when their hand totaled less than or equal to 16 (a win rate of 27.15%).

Given that there are many different versions of blackjack rules, to simplify the analysis, I chose relatively standard house rules, with a few modifications:
•	One 52-card deck is used (I chose to use one deck to simplify the simulation, but most casinos use between 4 to 8 decks. Different results should be expected with different numbers of decks in play). 
•	There is one player and one dealer (Again, different results should be expected with additional players).   
•	On their turn, players choose to "hit" (take a card) or "stay" (end their turn and stop without taking a card). Other play options such as "doubling" (the player doubles their wager, takes a single card, and finishes), "splitting" (if the two cards have the same value, the player can choose to separate them to make two hands), or "surrender" (the player can choose to give up a half-bet and retire from the game) can also sometimes be used. Again, to simplify the analysis I focused only on the hit or stay decision.
•	If a player’s total hand exceeds 21, they “bust” and immediately lose
•	Given there is only one player, the dealer draws card they either “bust”, i.e., their hand sums to more than 21, or their hand is more than the player’s. 
•	If the dealer does not bust, the player wins if their hand is higher than the dealer's. 
•	If the dealer’s hand and the player’s hand sum to the same amount, the tie goes to the dealer. 

