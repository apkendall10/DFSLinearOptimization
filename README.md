# DFSLinearOptimization
Linear Optimizer for Daily Fanstasy Sports to generate the best 5 lineups according to rotogrinders.com projections
GetData.r reads data from the rotogrinders website
Optimize.r uses a linear optimizer to maximize projected fantasy value given budget and positional constraints. Iterates through 5 times with some tuning to diversify and select the 5 best lineups.
