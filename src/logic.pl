:- use_module(library(lists)).
:-ensure_loaded('board.pl').

animal(l).
animal(e).
animal(r).

water(w, (Animal, Player)):-
        (animal(Animal) ; Animal = o),
        (Player = 0; Player = 1; Player = 2).

scared_of(l,e).
scared_of(e,r).
scared_of(r,l).

movement(l, diagonal).
movement(r, orthogonal).
movement(e, both).

valid_move(piece(Animal,Player), FromX, ToX, FromY, ToY,GameState,CurrentPlayer):-                         
        Player = CurrentPlayer,        
        movement(Animal, Direction),
        (element_at_position(GameState, ToX, ToY, o) ; element_at_position(GameState, ToX, ToY, water(w,(_,0)))), %check if the player is moving to an empty square
        ((Direction = orthogonal, (FromX = ToX, FromY \= ToY) ; (FromX \= ToX, FromY = ToY)) ;
        (Direction = diagonal, abs(FromX - ToX) =:= abs(FromY - ToY)) ;
        (Direction = both, (FromX = ToX, FromY \= ToY) ; (FromX \= ToX, FromY = ToY) ; (abs(FromX - ToX) =:= abs(FromY - ToY)))),
        write('valid direction'), nl, !,
        \+ adjacent_afraid(Animal, ToX, ToY, GameState, Player),
        write('not afraid'),
        (FromX \= ToX; FromY \= ToY). % cant stay in place

valid_move(water(w,piece(Animal,Player)), FromX, ToX, FromY, ToY,GameState,CurrentPlayer):-                         
        Player = CurrentPlayer,        
        movement(Animal, Direction),
        (element_at_position(GameState, ToX, ToY, o) ; element_at_position(GameState, ToX, ToY, water(w,(_,0)))), %check if the player is moving to an empty square
        ((Direction = orthogonal, (FromX = ToX, FromY \= ToY) ; (FromX \= ToX, FromY = ToY)) ;
        (Direction = diagonal, abs(FromX - ToX) =:= abs(FromY - ToY)) ;
        (Direction = both, (FromX = ToX, FromY \= ToY) ; (FromX \= ToX, FromY = ToY) ; (abs(FromX - ToX) =:= abs(FromY - ToY)))),
        write('valid direction'), nl, !,
        \+ adjacent_afraid(Animal, ToX, ToY, GameState, Player),
        write('not afraid'),
        (FromX \= ToX; FromY \= ToY). % cant stay in place

adjacent_afraid(Animal, X, Y, GameState,Player):-
        scared_of(Animal,Scared),
        NextPlayer is (Player mod  2) + 1,
        (X1 is X+1, element_at_position(GameState,X1,Y,piece(Scared,NextPlayer));
         X1 is X-1, element_at_position(GameState,X1,Y,piece(Scared,NextPlayer));
         Y1 is Y+1, element_at_position(GameState,X,Y1,piece(Scared,NextPlayer));
         Y1 is Y-1, element_at_position(GameState,X,Y1,piece(Scared,NextPlayer));
         X1 is X+1, Y1 is Y+1, element_at_position(GameState,X1,Y1,piece(Scared,NextPlayer));
         X1 is X-1, Y1 is Y+1, element_at_position(GameState,X1,Y1,piece(Scared,NextPlayer));
         X1 is X-1, Y1 is Y-1, element_at_position(GameState,X1,Y1,piece(Scared,NextPlayer));
         X1 is X+1, Y1 is Y-1, element_at_position(GameState,X1,Y1,piece(Scared,NextPlayer))).
         
read_move(Row, Col, ToRow, ToCol, [Size,Board],Player) :-
        write('Please write your movement in the format:  '), nl,
        write('Row-Col'), nl,         
        read(Input),
        Row1-Col1 = Input,
        Row is Row1-1,
        Col is Col1-1,
        element_at_position([Size,Board], Row, Col, piece(Animal,OwnPlayer)),
        OwnPlayer = Player,
        possible_moves([Size,Board], Row, Col, Moves, piece(Animal,OwnPlayer)),
        write('The list of possible moves for this piece is: '), nl,
        write(Moves), nl,
        
        write('Please choose your movement in the format:  '), nl,
        write('Row-Col'), nl,         
        read(Input2),
        ToRow1-ToCol1 = Input2,
        ToRow is ToRow1-1,
        ToCol is ToCol1-1,
        number(Row), number(Col), number(ToRow), number(ToCol).

turn(Player, [Size,Board], UpdatedBoard):-
        write('Player:  '), write(Player), nl,
        read_move(FromX, FromY, ToX, ToY,[Size,Board],Player),
        write('valid read'),nl,
        element_at_position([Size,Board], FromX, FromY, Piece), %get Piece
        write('valid element at position'), nl,
        (\+valid_move(Piece, FromX, ToX, FromY, ToY, [Size,Board],Player) -> write('Invalid Move!, try again'), nl, turn(Player, [Size,Board],UpdatedBoard);
         write('valid move'),nl,
         write(Piece),nl,
        (Piece = water(w, _) -> replace_in_board(Board,FromX, FromY, Piece, water(w,(o,0)), NewBoard);
                        replace_in_board(Board,FromX, FromY, Piece, o, NewBoard)),
        write('valid New Board'),nl,
        element_at_position([Size,Board], ToX, ToY, Destination),
        (Destination = water(w, _) -> replace_in_board(NewBoard,ToX, ToY, Piece, water(w,Piece),UpdatedBoard);
                (Piece = water(w, piece(AnimalInWater,PlayerInWater)) -> replace_in_board(NewBoard,ToX, ToY, o, piece(AnimalInWater,PlayerInWater), UpdatedBoard);
                        replace_in_board(NewBoard,ToX, ToY, o, Piece, UpdatedBoard))),
        write('valid New UpBoard'),nl).

game_loop(Player,[Size,Board]) :-
        turn(Player,[Size,Board],NewBoard),
        NextPlayer is (Player mod  2) + 1,
        %(game_over(GameState,Winner) -> break; true),
        displayBoard(NewBoard),
        game_loop(NextPlayer,[Size,NewBoard]).
        

play :-
        Player is 1,
        Size is 10,
        initial_state(Size,[Player,Board]),
        game_loop(Player, [Size,Board]).

game_over([_,Board], Winner) :- 
        count_occurrences(Board, 1, Count1),
        count_occurrences(Board, 2, Count2),
        (Count1 >= 3 -> Winner = 1;
         (Count2 >= 3 -> Winner = 2;
          fail)).

possible_moves(GameState,Row,Col,Moves,piece(Animal,Player)):-
        movement(Animal, Direction),
        ((Direction = orthogonal, possible_moves_orthogonally(GameState,Row,Col,Moves,piece(Animal,Player))) ;
        (Direction = diagonal, possible_moves_diagonally(GameState,Row,Col,Moves,piece(Animal,Player)));
        (Direction = both,
         possible_moves_orthogonally(GameState,Row,Col,MovesOrt,piece(Animal,Player)),
         possible_moves_diagonally(GameState,Row,Col,MovesDiag,piece(Animal,Player)),
         append([], MovesOrt, Moves1),
         append(Moves1, MovesDiag, Moves))
        ).
        
possible_moves_diagonally(GameState,Row,Col,Moves,piece(Animal,Player)):-
        possible_moves_upright(GameState,Row,Col,MovesUpRight,Animal,Player),
        possible_moves_upleft(GameState,Row,Col,MovesUpLeft,Animal,Player),
        possible_moves_downright(GameState,Row,Col,MovesDownRight,Animal,Player),
        possible_moves_downleft(GameState,Row,Col,MovesDownLeft,Animal,Player),
        append([], MovesUpRight, Moves1),
        append(Moves1, MovesUpLeft, Moves2),
        append(Moves2, MovesDownRight, Moves3),
        append(Moves3, MovesDownLeft, Moves).

possible_moves_upright(GameState,Row,Col,Moves,Animal,Player):-
        NewRow is Row +1,
        NewCol is Col +1,
        (element_at_position(GameState,NewRow,NewCol,Element),
         (Element = 'o'; Element = water(w,(o,0))) ->

         possible_moves_upright(GameState,NewRow,NewCol,MovesUpRight,Animal,Player),
         OutputRow is NewRow + 1,
         OutputCol is NewCol + 1,
         (\+ adjacent_afraid(Animal, NewRow, NewCol, GameState, Player) ->
         append([[OutputRow, OutputCol]], MovesUpRight, Moves) ; append([], MovesUpRight, Moves))
        ;
         Moves = []
        ).

possible_moves_upleft(GameState,Row,Col,Moves,Animal,Player):-
        NewRow is Row +1,
        NewCol is Col -1,
        (element_at_position(GameState,NewRow,NewCol,Element),
         (Element = 'o'; Element = water(w,(o,0))) ->

         possible_moves_upleft(GameState,NewRow,NewCol,MovesUpLeft,Animal,Player),
         OutputRow is NewRow + 1,
         OutputCol is NewCol + 1,
         (\+ adjacent_afraid(Animal, NewRow, NewCol, GameState, Player) ->
         append([[OutputRow, OutputCol]], MovesUpLeft, Moves) ;
          append([], MovesUpLeft, Moves))
        ;
         Moves = []
        ).

possible_moves_downright(GameState,Row,Col,Moves,Animal,Player):-
        NewRow is Row -1,
        NewCol is Col +1,
        (element_at_position(GameState,NewRow,NewCol,Element),
         (Element = 'o'; Element = water(w,(o,0))) ->

         possible_moves_downright(GameState,NewRow,NewCol,MovesDownRight,Animal,Player),
         OutputRow is NewRow + 1,
         OutputCol is NewCol + 1,
         (\+ adjacent_afraid(Animal, NewRow, NewCol, GameState, Player) ->
         append([[OutputRow, OutputCol]], MovesDownRight, Moves) ; append([], MovesDownRight, Moves))
        ;
         Moves = []
        ).

possible_moves_downleft(GameState,Row,Col,Moves,Animal,Player):-
        NewRow is Row -1,
        NewCol is Col -1,
        (element_at_position(GameState,NewRow,NewCol,Element),
         (Element = 'o'; Element = water(w,(o,0))) ->

         possible_moves_downleft(GameState,NewRow,NewCol,MovesDownLeft,Animal,Player),
         OutputRow is NewRow + 1,
         OutputCol is NewCol + 1,
         (\+ adjacent_afraid(Animal, NewRow, NewCol, GameState, Player) ->
         append([[OutputRow, OutputCol]], MovesDownLeft, Moves) ; append([], MovesDownLeft, Moves))
        ;
         Moves = []
        ).

possible_moves_orthogonally(GameState,Row,Col,Moves,piece(Animal,Player)):-
        possible_moves_up(GameState,Row,Col,MovesUp,Animal,Player),
        possible_moves_down(GameState,Row,Col,MovesDown,Animal,Player),
        possible_moves_left(GameState,Row,Col,MovesLeft,Animal,Player),
        possible_moves_right(GameState,Row,Col,MovesRight,Animal,Player),
        append([], MovesUp, Moves1),
        append(Moves1, MovesDown, Moves2),
        append(Moves2, MovesLeft, Moves3),
        append(Moves3, MovesRight, Moves).

possible_moves_up(GameState,Row,Col,Moves,Animal,Player):-
        NewRow is Row +1,
        (element_at_position(GameState,NewRow,Col,Element),
         (Element = 'o'; Element = water(w,(o,0))) ->

         possible_moves_up(GameState,NewRow,Col,MovesUp,Animal,Player),
         OutputRow is NewRow + 1,
         OutputCol is Col + 1,
         (\+ adjacent_afraid(Animal, NewRow, Col, GameState, Player) ->
         append([[OutputRow, OutputCol]], MovesUp, Moves) ; append([], MovesUp, Moves))
        ;
         Moves = []
        ).

possible_moves_down(GameState,Row,Col,Moves,Animal,Player):-
        NewRow is Row -1,
        (element_at_position(GameState,NewRow,Col,Element),
         (Element = 'o'; Element = water(w,(o,0))) ->

         possible_moves_down(GameState,NewRow,Col,MovesDown,Animal,Player),
         OutputRow is NewRow + 1,
         OutputCol is Col + 1,
         (\+ adjacent_afraid(Animal, NewRow, Col, GameState, Player) ->
         append([[OutputRow, OutputCol]], MovesDown, Moves) ; append([], MovesDown, Moves))
        ;
         Moves = []
        ).

possible_moves_left(GameState,Row,Col,Moves,Animal,Player):-
        NewCol is Col -1,
        (element_at_position(GameState,Row,NewCol,Element),
         (Element = 'o'; Element = water(w,(o,0))) ->

         possible_moves_left(GameState,Row,NewCol,MovesLeft,Animal,Player),
         OutputRow is Row + 1,
         OutputCol is NewCol + 1,
         (\+ adjacent_afraid(Animal, Row, NewCol, GameState, Player) ->
         append([[OutputRow, OutputCol]], MovesLeft, Moves) ; append([], MovesLeft, Moves))
        ;
         Moves = []
        ).

possible_moves_right(GameState,Row,Col,Moves,Animal,Player):-
        NewCol is Col +1,
        (element_at_position(GameState,Row,NewCol,Element),
         (Element = 'o'; Element = water(w,(o,0))) ->

         possible_moves_right(GameState,Row,NewCol,MovesRight,Animal,Player),
         OutputRow is Row + 1,
         OutputCol is NewCol + 1,
         (\+ adjacent_afraid(Animal, Row, NewCol, GameState, Player) ->
         append([[OutputRow, OutputCol]], MovesRight, Moves) ; append([], MovesRight, Moves))
        ;
         Moves = []
        ).

count_occurrences(Array, Player, Count) :-
        findall(X, (member(Row, Array), member(X, Row), X = water(w,_,Player)), Occurrences),
        length(Occurrences, Count).
 
display_adjacency_error(X,Y,Animal):-
        OutputX is X + 1,
        OutputY is Y + 1,
        animal(Animal),
        write('Not a valid move! The animal you tryied to move is afraid of the '), write(Animal), write(' in position '), write(OutputX), write(','), write(OutputY),!.

                
        
        
        
        
        

        
        
        