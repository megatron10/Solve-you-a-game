# Solve-You-A-Game
[![CircleCI](https://circleci.com/gh/IITH-SBJoshi/haskell-7-1.svg?style=svg&circle-token=708be149818b6e98d35428b7a7e4bfa84feafb35)](https://circleci.com/gh/IITH-SBJoshi/haskell-7-1)

A framework for Solving Zero Sum Games with Perfect Information

To use the framework user has to define a instance of class Game as
defined in Solver.hs and implementation has to be provided to the functions
defined there. Also to play the geme the game should be an instance of solvable 
game defined in GamePlay.hs to support functions like showGame and printMoves. <br>

some games are provided like TicTacToe, Chomp and Two other.

## Instructions to install 
clone the repository:	<pre>
git clone "https://github.com/IITH-SBJoshi/haskell-7-1/"
</pre>

Install Stack for haskell from here <pre>
"https://docs.haskellstack.org/en/stable/README/#how-to-install"
</pre>

To Build the project <pre> 
stack build
</pre>

To Run the executable created <pre> 
stack exec Solve-You-A-Game-exe
</pre>

Install using <pre> stack install
</pre>

## The Documentation 
To generate the documentation run following command	<pre> 
stack haddock 
The documentation will be generated at ./.stack-work/install/X86-64*/lts-*/8.*/doc </pre> 

