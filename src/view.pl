% Define the main menu
menu(Size,P1,P2) :-
    write('1. Play'), nl,
    write('2. Instructions'), nl,
    write('3. Set Size'), nl,
    write('4. Exit'), nl,
    % Read the user's choice
    read(Choice),
    % Process the choice
    (
        Choice = 1 -> play_menu(Size,P1,P2); % Show the play menu
        Choice = 2 -> game_rules, nl, menu(Size,P1,P2); % Show the instructions and go back to the main menu
        Choice = 3 -> set_size(Size), menu(Size,P1,P2); % Set the size of the game and go back to the main menu
        Choice = 4 -> true % Exit the program
    ).

% Display the game rules
game_rules :-
    write('Each player controls 6 animals of 3 different types'), nl, 
    write('The Mouse can move orthogonally (horizontal or vertical)'), nl, 
    write('and is scared of the Lion. The Lion can move diagonally (it is color locked)'), nl, 
    write('and is scared of the Elephant. The Elephant can move both orthogonally'), nl, 
    write('and diagonally and is scared of the Mouse;'), nl, 
    write('each piece can move as far as it can on its intended line of travel as long as'), nl, 
    write('it is not within the square perimeter (orthogonal and diagonal) of the animal its afraid of.'), nl, 
    write('The goal of the game is to control 3 of the 4 watering holes at the end of your turn.'), nl, 
    write('However, if your piece is next to a piece (orthogonal AND diagonal) that it is'), nl, 
    write('scared of it MUST move on your next turn. You cannot move a piece next to a piece (orthogonal AND diagonal) that it is scared of.').


% Define the play menu, default value is 10 for the size
play_menu(Size,P1,P2) :-
    % Check if Value has a value
        ( var(Size) -> Size = 10; true ),
    write('1. Player vs Player'), nl,
    write('2. Player vs Easy Robot'), nl,
    write('3. Player vs Hard Robot'), nl,
    write('4. Hard Robot vs Hard Robot'), nl,
    % Read the user's choice
    read(Choice),nl,
    % Process the choice
    (
        Choice = 1 -> write('Starting Player vs Player game'), P1 = 1, P2 = 2, nl; % Start a Player vs Player game
        Choice = 2 -> write('Starting Player vs Easy Robot game'),P1 = 1, P2 = 'e', nl; % Start a Player vs Easy Robot game
        Choice = 3 -> write('Starting Player vs Hard Robot game'),P1 = 1, P2 = 'h', nl; % Start a Player vs Hard Robot game
        Choice = 4 -> write('Hard Robot vs Hard Robot game'),P1 = 'h', P2 = 'h', nl
    ).

% Define the set_size predicate to set the size of the game
set_size(Size) :-
    write('Enter the size of the game: '),
    read(Size),
    % Set the size of the game
    number(Size).

% Display the Winner of the game
winner(Winner) :-
    nl, write('Game is over! Player '), write(Winner), write(' Wins!'), nl,
    nl,
    write('Thanks for playing! hope you enjoyed!').
