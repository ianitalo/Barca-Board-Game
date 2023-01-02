# Barca-Board-Game
>## Identification of the work (game)
>Barca Game developed in Prolog for the PFL Class in Feup
>
>Group :
>| Name         | Contribution | Up           |
>| ------------ | ------------ | ------------ |
>| Ian Italo Martins Gomes | 33,3%        | up202000707  |
>| Matilde Pinho Borges Sequeira   | 33,3%        | up202007623  |
>| Igor Liberato de Castro   | 33,3%        | up202000161 |

>## Install and execution
>
>Apart from the standard SICStus Prolog installation, you should run the consult command on the `logic.pl` file to load the game predicates.
After that simply execute the `play` predicate to start the game.
>
>Load the predicates:
>
>```prolog
>consult('main.pl').
>```
>
>Run the game:
>
>```prolog
>play.
>```

>## Game description
>Played on a squared board, each player controls 6 animals of 3 different types. The Mouse can move orthogonally (horizontal or vertical) and is 'scared' of the Lion. The Lion can move diagonally (it's color locked) and is 'scared' of the Elephant. The Elephant can move both orthogonally and diagonally and is 'scared' of the Mouse; each piece can move as far as it can on its intended line of travel as long as it's not within the square perimeter (orthogonal and diagonal) of the animal its afraid of.
>
>The goal of the game is to control 3 of the 4 'watering holes' at the end of your turn. However, if your piece is next to a piece (orthogonal AND diagonal) that it is 'scared' of it MUST move on your next turn. You cannot move a piece next to a piece (orthogonal AND diagonal) that it is 'scared' of.
>
> More information about the game can be found in https://boardgamegeek.com/boardgame/69347/barca
> 
> Here is also a brief video describing the game: https://boardgamegeek.com/video/4589/barca/brief-demo-video

>## Game Logic
>>### Internal representation of the state of the game
>> The internal representation of the game state for prolog is like that: [Player, Board, Size]
>> The empty spaces are represented with the letter 'o'
>> The pieces are represented with (Animal, Player) so we can know which animal is there and for what player it belongs
>> The water(w,(Animal,Player)) is represented like that so we know what player and animal owns that water, (o,0) if no one owns it yet

>> Initial State
>>>
>>>```
>>>  [1,
>>>
>>>     [[o,o,o,o,piece(e,1),piece(e,1),o,o,o,o],
>>>
>>>     [o,o,o,piece(l,1),piece(r,1),piece(r,1),piece(l,1),o,o,o],
>>>
>>>     [o,o,o,o,o,o,o,o,o,o],
>>>
>>>     [o,o,o,water(w,(o,0)),o,o,water(w,(o,0)),o,o,o],
>>>
>>>     [o,o,o,o,o,o,o,o,o,o],
>>>
>>>     [o,o,o,o,o,o,o,o,o,o],
>>>
>>>     [o,o,o,water(w,(o,0)),o,o,water(w,(o,0)),o,o,o],
>>>
>>>     [o,o,o,o,o,o,o,o,o,o],
>>>
>>>     [o,o,o,piece(l,2),piece(r,2),piece(r,2),piece(l,2),o,o,o],
>>>
>>>     [o,o,o,o,piece(e,2),piece(e,2),o,o,o,o]],
>>>
>>>10]
>>>```

>> Mid State (2 of the for water holes taken, player 2 turn)
>>>
>>>```
>>>[2,
>>>     [[o,o,o,o,o,o,o,o,o,o],
>>>     [o,o,o,o,piece(r,1),piece(r,1),piece(e,1),o,o,o],
>>>     [o,o,o,o,o,o,o,o,o,o],
>>>     [o,o,o,water(w,piece(e,1)),piece(l,1),piece(l,1),water(w,(o,0)),o,o,o],
>>>     [o,o,o,o,o,o,o,o,o,o],
>>>     [o,o,o,piece(l,2),o,o,piece(l,2),o,o,o],
>>>     [o,o,o,water(w,piece(r,2)),piece(e,2),o,water(w,(o,0)),o,o,o],
>>>     [o,o,o,o,o,o,o,o,o,o],
>>>     [o,o,o,o,o,piece(r,2),o,o,o,o],
>>>     [o,o,o,o,o,piece(e,2),o,o,o,o]],
>>>10]
>>>```

>> Final State (Player 1 own 3 water holes)
>>>
>>>```
>>>[1,
>>>      [[o,o,o,o,o,o,o,o,o,o],
>>>      [o,o,o,o,o,piece(r,1),o,o,o,o],
>>>      [o,o,o,o,o,o,o,o,o,o],
>>>      [o,o,piece(e,2),water(w,piece(e,1)),piece(r,1),piece(l,1),water(w,piece(e,1)),o,o,o],
>>>      [o,o,o,o,o,o,o,o,o,o],
>>>      [o,o,o,piece(l,2),o,o,piece(l,2),o,o,o],
>>>      [o,o,o,water(w,piece(l,1)),piece(r,2),o,water(w,piece(e,2)),o,o,o],
>>>      [o,o,o,o,o,o,o,o,o,o],
>>>      [o,o,o,o,o,piece(r,2),o,o,o,o],
>>>      [o,o,o,o,o,o,o,o,o,o]],
>>>10]
>>>```

>>### Game state view
>> We implemented the initial_state(+Size, -GameState) predicate so the user can choose on initial screen the size of the board
>> 
>> Also in the initial menu you have the option to see the instructions and start the game
>> 
>> If you choose to start the game, you are given 4 options:
>> - Player x Player
>> - Player x Easy Bot
>> - Player x Hard Bot
>> - Hard Bot x Hard Bot
>> 
>> And after you choose the game mode, the game begins<br>
>> 
>> If a bot is in game, before every move there is a message saying what's the bots move, for example :<br>
>> 
>> Bot is going to move piece from [6-3]<br>
>> To [7-4]<br>
>> 
>> For players before each move there is a message saying the current player to make the move and the correct format of the input:
>> 
>> Player:  1<br>
>> Please write the position of the piece to move in the format: <br> 
>> Row-Col<br>
>> 
>> After the player inputs for example 2-5.<br>
>> The following is displayed:
>> 
>> The list of possible moves for this piece is: <br>
>> [[3,5],[4,5],[5,5],[6,5],[7,5]]<br>
>> Please choose your movement in the format:  <br>
>> Row-Col <br>
>> 
>> That way the player know which are the possible moves for that piece. <br>
>> The player's input are validated so that if it inputs a movement in the wrong format, the game will display again the same message asking for the input
>> Players are also warned when a forced move needs to be made (for him or for the bot) so that it cannot move other piece different them the forced one

>>### Moves Execution
>> We splited the move(+GameState, +Move, -NewGameState) into 3 predicates for better coding:
>> - turn(+GameState, -NewGameState), for human players
>> - easybot(+GameState, -NewGameState), for easy bot
>> - hardbot(+GameState, -NewGameState), for hard bot
>> We did that because the moves of each one are processed differently
>> - Human (turn) Has validations for input, choices of different pieces and choice of move
>> - Easy Bot (easybot) Has a generator of random moves and a validation of them, choosing all at random
>> - Hard Bot (hardbot) Has functions that for each possible move the bot can do, chooses the one that gives the most advantage in the game (greedy)

>>### List of Valid Moves
>> We needed to change the valid_moves predicate because we needed more information so its know is called possible_moves(+GameState,+Row,+Col,+Piece,-Moves)
>> It receives the GameState, row and col of a piece, the piece, and returns all the possible moves for that piece as showed in the examples in game state view point

>>### End of Game
>> The game_over(+GameState, -Winner) predicate verify if the Game is over (if any player has 3 water hole), and in positive case, calls the end game view with the Winner number, else the game continues

>>### Board Evaluation
>> We dont have a single predicate to evaluate all state of the game, it would be too complex, so besides all the evaluation already spoken, the board is evaluated for validation (no animal can be close to another its afraid of) and evaluated in case the hard bot is playing with the hardbot_move predicate, which tests all its pieces in the game and gives a score to each move based on the greedy algorithm (move pieces to the waters or as close as possible)

>>### Computer move
>> The computers move predicates are divided into two:
>> - easybot(+GameState, -UpdatedBoard), that performs a random move 
>> - hardbot(+GameState, -UpdatedBoard), that uses a greedy algorithm evaluating the board with the hardbot_move predicate

>## Conclusions
>>For imperative minded programmers, Prolog proves to be an healthy challenge, forcing us to focus on the problem instead of in the steps to solve it. This isn't always easy, and we found ourselves battling many times for thinking imperatively, gasping for cycles, when declarative programming offered us a compact and logical solution. More often than not, long hours translated in few but powerful lines of code.
>>
>>Known issues: The lack of answers in the internet to help with some challenges, (what is a programmer without stack overflow xD)
