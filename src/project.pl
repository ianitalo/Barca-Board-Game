:- use_module(library(lists)).
% Define the dimensions of the board
board_size(10, 10).

% Define the three types of animals and their movement rules
animal(mouse).
animal(lion).
animal(elephant).
movement(mouse, orthogonal).
movement(lion, diagonal).
movement(elephant, both).
fear(mouse, lion).
fear(lion, elephant).
fear(elephant, mouse).

% Define the positions of the four watering holes
watering_hole(1, 1).
watering_hole(1, 10).
watering_hole(10, 1).
watering_hole(10, 10).

% Define the initial state of the board
% Each player has three mice, two lions, and one elephant
% The first player's pieces are placed in the top left corner of the board
% The second player's pieces are placed in the bottom right corner of the board
initial_board([[mouse,1,1],[mouse,1,2],[mouse,2,1],[lion,1,3],[lion,2,2],[elephant,1,4],
               [mouse,10,10],[mouse,9,10],[mouse,10,9],[lion,10,8],[lion,9,9],[elephant,10,7]]).

% Define the rules for making a move
% A piece can move as far as it can on its intended line of travel as long as it's not within the square perimeter (orthogonal and diagonal) of the animal it's afraid of
move(Animal, FromX, FromY, ToX, ToY) :-
  animal(Animal),
  movement(Animal, Direction),
  ((Direction = orthogonal, FromX = ToX, FromY \= ToY) ;
   (Direction = diagonal, abs(FromX - ToX) =:= abs(FromY - ToY)) ;
   (Direction = both, (FromX = ToX, FromY \= ToY) ; (abs(FromX - ToX) =:= abs(FromY - ToY)))),
  \+ afraid(Animal, ToX, ToY).

% Define the rules for determining if a piece is afraid
afraid(Animal, X, Y) :-
  fear(Animal, AfraidOf),
  adjacent(X, Y, AfraidOf).

% Define the rules for determining if a piece is adjacent to another piece
adjacent(X, Y, AfraidOf) :-
  board_size(MaxX, MaxY),
  between(1, MaxX, X),
  between(1, MaxY, Y),
  ((AfraidOf = orthogonal, (X1 is X-1 ; X1 is X+1), Y = Y1) ;
   (AfraidOf = diagonal, (X1 is X-1, Y1 is Y-1) ; (X1 is X-1, Y1 is Y+1) ; (X1 is X+1, Y1 is Y-1) ; (X1 is X+1, Y1 is Y+1)) ;
   (AfraidOf = both, ((X1 is X-1 ; X1 is X+1), Y = Y1) ;
    (X = X1, (Y1 is Y-1 ; Y1 is Y+1)))).

% Define the rules for determining if a player has won the game
% A player wins if they control three of the four watering holes
win(Player, Board) :-
  findall(X-Y, watering_hole(X, Y), WateringHoles),
  findall(X-Y, (member([Animal, X, Y], Board), Animal \= elephant), Controlled),
  intersection(WateringHoles, Controlled, Intersection),
  length(Intersection, 3).

% Define the rules for making a legal move
% A player must move a piece that is next to a piece (orthogonal AND diagonal) that it is afraid of on their next turn
% A player cannot move a piece next to a piece (orthogonal AND diagonal) that it is afraid of
legal_move(Player, FromX, FromY, ToX, ToY, Board) :-
  member([Animal, FromX, FromY], Board),
  move(Animal, FromX, FromY, ToX, ToY),
  \+ ((fear(Animal, AfraidOf), adjacent(ToX, ToY, AfraidOf)), member([AfraidOf, ToX, ToY], Board)).

% Define the rules for switching players
% The first player is player 1 and the second player is player 2
next_player(1, 2).
next_player(2, 1).

% Define the rules for updating the board after a move is made
% Remove the piece from its original position and add it to its new position
update_board(Player, FromX, FromY, ToX, ToY, Board, NewBoard) :-
  delete(Board, [_, FromX, FromY], TempBoard),
  append(TempBoard, [[Player, ToX, ToY]], NewBoard).

% Define the main play/2 predicate that represents a game of Barca
% The first argument is the player who is making a move
% The second argument is the current state of the board
play(Player, Board) :-
  win(Player, Board),
  write('Player '), write(Player), write(' wins!'), nl.
play(Player, Board) :-
  write('Player '), write(Player), write(', enter your move (FromX FromY ToX ToY): '),
  read([FromX, FromY, ToX, ToY]),
  legal_move(Player, FromX, FromY, ToX, ToY, Board),
  update_board(Player, FromX, FromY, ToX, ToY, Board, NewBoard),
  next_player(Player, NextPlayer),
  play(NextPlayer, NewBoard).

% Define the main predicate that starts a new game of Barca
barca :-
  initial_board(Board),
  play(1, Board).


