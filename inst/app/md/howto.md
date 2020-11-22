### how to play
_slotR is a simple slot machine game of chance inspired by R:_
> __make your bet and spin the reels!__

- R symbols make you win, Python ones make you loose,

- two or three equal hexagons make you win,

- NAs hexagons are bad,

- you can bet all of your money but no less than 100 credits,

- payout is computed multiplying the bet by a factor resulting from adding the values reported on the payout table (see payout tab) depending on which and how many symbols appear in the reels,
          
- if you run out of money you can ask for credit but no more than twice,

- you win if you reach a credit of _30,000_.

### how to configure the reels
The tab panel _config_ allows to customize the reels (__slotR__ has 3 equal reels) inserting how many stops by selecting the sliders in the sidebar panel. 

At every change in the stops quantity the probabilities are recomputed and displayed in the main panel.

The customized reels can be used in actual play only after clicking the _change reel_ button.

The _configured reel_ button shows the current reel configuration.

### how to analyze the current game
the tab panel _analysis_ allows to review the current game both:

- visualizing the trend of credit along the game rounds in the credit trend tab,

- displaying every round outcome (resulting credit, bet done, symbols in each reel) in the game data tab.