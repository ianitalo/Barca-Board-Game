:- use_module(library(lists)).
:- use_module(library(random)).
:-ensure_loaded('board.pl').
:-ensure_loaded('view.pl').

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

valid_move(piece(Animal,Player), FromX, ToX, FromY, ToY,GameState,CurrentPlayer,Moves):-                         
        Player = CurrentPlayer,
        OutputX is ToX + 1,
        OutputY is ToY + 1,
        member([OutputX,OutputY],Moves),   
        movement(Animal, Direction),
        (element_at_position(GameState, ToX, ToY, o) ; element_at_position(GameState, ToX, ToY, water(w,(_,0)))), %check if the player is moving to an empty square
        ((Direction = orthogonal, (FromX = ToX, FromY \= ToY) ; (FromX \= ToX, FromY = ToY)) ;
        (Direction = diagonal, abs(FromX - ToX) =:= abs(FromY - ToY)) ;
        (Direction = both, (FromX = ToX, FromY \= ToY) ; (FromX \= ToX, FromY = ToY) ; (abs(FromX - ToX) =:= abs(FromY - ToY)))), !,
        \+ adjacent_afraid(Animal, ToX, ToY, GameState, Player),
        (FromX \= ToX; FromY \= ToY). % cant stay in place

valid_move(water(w,piece(Animal,Player)), FromX, ToX, FromY, ToY,GameState,CurrentPlayer,Moves):-                         
        Player = CurrentPlayer,
        OutputX is ToX + 1,
        OutputY is ToY + 1,
        member([OutputX,OutputY],Moves),
        movement(Animal, Direction),
        (element_at_position(GameState, ToX, ToY, o) ; element_at_position(GameState, ToX, ToY, water(w,(_,0)))), %check if the player is moving to an empty square
        ((Direction = orthogonal, (FromX = ToX, FromY \= ToY) ; (FromX \= ToX, FromY = ToY)) ;
        (Direction = diagonal, abs(FromX - ToX) =:= abs(FromY - ToY)) ;
        (Direction = both, (FromX = ToX, FromY \= ToY) ; (FromX \= ToX, FromY = ToY) ; (abs(FromX - ToX) =:= abs(FromY - ToY)))),
        write('valid direction'), nl, !,
        \+ adjacent_afraid(Animal, ToX, ToY, GameState, Player),
        (FromX \= ToX; FromY \= ToY). % cant stay in place

adjacent_afraid(Animal, X, Y, GameState,Player):-
        scared_of(Animal,Scared),
        NextPlayer is (Player mod  2) + 1,
        (X1 is X+1, element_at_position(GameState,X1,Y,piece(Scared,NextPlayer));
         X1 is X+1, element_at_position(GameState,X1,Y,water(w,piece(Scared,NextPlayer)));
         X1 is X-1, element_at_position(GameState,X1,Y,piece(Scared,NextPlayer));
         X1 is X-1, element_at_position(GameState,X1,Y,water(w,piece(Scared,NextPlayer)));
         Y1 is Y+1, element_at_position(GameState,X,Y1,piece(Scared,NextPlayer));
         Y1 is Y+1, element_at_position(GameState,X,Y1,water(w,piece(Scared,NextPlayer)));
         Y1 is Y-1, element_at_position(GameState,X,Y1,piece(Scared,NextPlayer));
         Y1 is Y-1, element_at_position(GameState,X,Y1,water(w,piece(Scared,NextPlayer)));
         X1 is X+1, Y1 is Y+1, element_at_position(GameState,X1,Y1,piece(Scared,NextPlayer));
         X1 is X+1, Y1 is Y+1, element_at_position(GameState,X1,Y1,water(w,piece(Scared,NextPlayer)));
         X1 is X-1, Y1 is Y+1, element_at_position(GameState,X1,Y1,piece(Scared,NextPlayer));
         X1 is X-1, Y1 is Y+1, element_at_position(GameState,X1,Y1,water(w,piece(Scared,NextPlayer)));
         X1 is X-1, Y1 is Y-1, element_at_position(GameState,X1,Y1,piece(Scared,NextPlayer));
         X1 is X-1, Y1 is Y-1, element_at_position(GameState,X1,Y1,water(w,piece(Scared,NextPlayer)));
         X1 is X+1, Y1 is Y-1, element_at_position(GameState,X1,Y1,piece(Scared,NextPlayer));
         X1 is X+1, Y1 is Y-1, element_at_position(GameState,X1,Y1,water(w,piece(Scared,NextPlayer)))).
         
process_move(Row, Col, ToRow, ToCol, [Size,Board],Player,Moves) :-
        (forced_move(Player,[Size,Board],Row,Col) -> write('You need to move the piece in position '),
        OutputRow is Row +1, OutputCol is Col+1,
        write(OutputRow), write('-'), write(OutputCol), write(' Because it is afraid of a nearby animal'),nl ;        
        write('Please write your movement in the format:  '), nl,
        write('Row-Col'), nl,         
        read(Input),
        Row1-Col1 = Input,
        Row is Row1-1,
        Col is Col1-1
        ),
        element_at_position([Size,Board], Row, Col, Piece),
        possible_moves([Size,Board], Row, Col, Moves, Piece, Player),
        write('The list of possible moves for this piece is: '), nl,
        write(Moves), nl,
        
        write('Please choose your movement in the format:  '), nl,
        write('Row-Col'), nl,         
        read(Input2),
        ToRow1-ToCol1 = Input2,
        ToRow is ToRow1-1,
        ToCol is ToCol1-1,
        number(Row), number(Col), number(ToRow), number(ToCol).

forced_move(Player,[Size,Board],Row,Col):-
        nth0(Row, Board, RowElements),
        (nth0(Col, RowElements, piece(Animal,Player)); nth0(Col, RowElements, water(w,piece(Animal,Player)))),
        adjacent_afraid(Animal,Row,Col,[Size,Board],Player),!.

turn(Player, [Size,Board], UpdatedBoard):-
        write('Player:  '), write(Player), nl,
        process_move(FromX, FromY, ToX, ToY,[Size,Board],Player,Moves),
        element_at_position([Size,Board], FromX, FromY, Piece), %get Piece
        (\+valid_move(Piece, FromX, ToX, FromY, ToY, [Size,Board],Player,Moves) -> write('Invalid Move!, try again'), nl, turn(Player, [Size,Board],UpdatedBoard);
         write('valid move'),nl,
        (Piece = water(w, _) -> replace_in_board(Board,FromX, FromY, Piece, water(w,(o,0)), NewBoard);
                        replace_in_board(Board,FromX, FromY, Piece, o, NewBoard)),
        element_at_position([Size,Board], ToX, ToY, Destination),
        (Destination = water(w, _) -> replace_in_board(NewBoard,ToX, ToY, Piece, water(w,Piece),UpdatedBoard);
                (Piece = water(w, piece(AnimalInWater,PlayerInWater)) -> replace_in_board(NewBoard,ToX, ToY, o, piece(AnimalInWater,PlayerInWater), UpdatedBoard);
                        replace_in_board(NewBoard,ToX, ToY, o, Piece, UpdatedBoard)))).

easybot(Player,[Size,Board], UpdatedBoard) :-
        (forced_move(Player,[Size,Board],Row,Col) ->
         findall((Row, Col, Piece), (nth0(Row, Board, RowList), nth0(Col, RowList, Piece), (Piece = piece(_, Player) ; Piece = water(w,piece(_,Player)))), Pieces),
         write('Forced!'), nl
         ;
        findall((R, C, Piece), (nth0(R, Board, RowList), nth0(C, RowList, Piece), (Piece = piece(_, Player) ; Piece = water(w,piece(_,Player)))), Pieces)
        ),
        length(Pieces, Choices),
        random(0, Choices, Choice),
        nth0(Choice, Pieces, (Row,Col,Element)),
    
        % If the element is a piece, get all the possible moves for that piece
        possible_moves([Size,Board], Row, Col, PossibleMoves, Element,Player), !,
        % Choose a random move from the list of possible moves
        length(PossibleMoves, NumMoves),
        random(0, NumMoves, MoveIndex),
        nth0(MoveIndex, PossibleMoves, [X1,Y1]),
        OutputRow is Row +1, OutputCol is Col+1,
        write('Bot is going to move piece from '),write('['),write(OutputRow),write('-'), write(OutputCol),write(']'), nl,
        write('To '),write('['),write(X1),write('-'), write(Y1),write(']'), nl,
        X is X1 - 1,
        Y is Y1 - 1,
        % Update the board with the chosen move
        (Element = water(w, _) -> replace_in_board(Board,Row, Col, Element, water(w,(o,0)), NewBoard);
                        replace_in_board(Board,Row, Col, Element, o, NewBoard)),
        element_at_position([Size,Board], X, Y, Destination),
        (Destination = water(w, _) -> replace_in_board(NewBoard,X, Y, Element, water(w,Element),UpdatedBoard);
                (Element = water(w, piece(AnimalInWater,PlayerInWater)) -> replace_in_board(NewBoard,X, Y, o, piece(AnimalInWater,PlayerInWater), UpdatedBoard);
                        replace_in_board(NewBoard,X, Y, o, Element, UpdatedBoard))).


hardbot(Player,[Size,Board], UpdatedBoard) :-
        forced_move(Player,[Size,Board],Row,Col),
        write('Forced!'), nl,
        OutputRow is Row +1, OutputCol is Col+1,
        hardbot_move([Size,Board], Player, Row,Col,Element, [R,C]),
        write('Bot is going to move piece from '),write('['),write(OutputRow),write('-'), write(OutputCol),write(']'), nl,
        write('To '),write('['),write(R),write('-'), write(C),write(']'), nl,
        (Element = water(w, _) -> replace_in_board(Board,Row, Col, Element, water(w,(o,0)), NewBoard);
                        replace_in_board(Board,Row, Col, Element, o, NewBoard)),
        element_at_position([Size,Board], R, C, Destination),
        (Destination = water(w, _) -> replace_in_board(NewBoard,R, C, Element, water(w,Element),UpdatedBoard);
                (Element = water(w, piece(AnimalInWater,PlayerInWater)) -> replace_in_board(NewBoard,R, C, o, piece(AnimalInWater,PlayerInWater), UpdatedBoard);
                        replace_in_board(NewBoard,R, C, o, Element, UpdatedBoard))).

hardbot(Player,[Size,Board], UpdatedBoard) :-
        
        hardbot_move([Size,Board], Player, (Row,Col,Element), [R,C]),
        OutputRow is Row +1, OutputCol is Col+1,
        write('Bot is going to move piece from '),write('['),write(OutputRow),write('-'), write(OutputCol),write(']'), nl,
        write('To '),write('['),write(R),write('-'), write(C),write(']'), nl,
        
        (Element = water(w, _) -> replace_in_board(Board,Row, Col, Element, water(w,(o,0)), NewBoard);
                        replace_in_board(Board,Row, Col, Element, o, NewBoard)),
        
        element_at_position([Size,Board], R, C, Destination),
        (Destination = water(w, _) -> replace_in_board(NewBoard,R, C, Element, water(w,Element),UpdatedBoard);
                (Element = water(w, piece(AnimalInWater,PlayerInWater)) -> replace_in_board(NewBoard,R, C, o, piece(AnimalInWater,PlayerInWater), UpdatedBoard);
                        replace_in_board(NewBoard,R, C, o, Element, UpdatedBoard))).

game_loop(Player,[Size,Board],P1,P2) :-      
        ((Player = P1 ; Player = P2) ->turn(Player,[Size,Board],NewBoard) ; true),
        ((Player = 2, P2 = 'e') -> easybot(Player,[Size,Board],NewBoard) ; true),       
        (((Player = 1, P1 = 'h') ; (Player = 2, P2 = 'h')) -> hardbot(Player,[Size,Board],NewBoard) ; true),
        NextPlayer is (Player mod  2) + 1,
        displayBoard(NewBoard),
        (game_over([Size,NewBoard],Winner) -> winner(Winner), true;    
        game_loop(NextPlayer,[Size,NewBoard],P1,P2)).
        
play :-
        menu(Size,P1,P2),
        Player is 1,
        initial_state(Size,[Player,Board]), !,
        game_loop(Player, [Size,Board],P1,P2).

game_over([_,Board], Winner) :- 
        count_occurrences(Board, 1, Count1),
        count_occurrences(Board, 2, Count2),
        (Count1 >= 3 -> Winner = 1;
         (Count2 >= 3 -> Winner = 2;
          fail)).

possible_moves(GameState,Row,Col,Moves,water(w,piece(Animal,Player)), ActualPlayer):-
        ActualPlayer = Player,
        movement(Animal, Direction),
        ((Direction = orthogonal, possible_moves_orthogonally(GameState,Row,Col,Moves,piece(Animal,Player))) ;
        (Direction = diagonal, possible_moves_diagonally(GameState,Row,Col,Moves,piece(Animal,Player)));
        (Direction = both,
         possible_moves_orthogonally(GameState,Row,Col,MovesOrt,piece(Animal,Player)),
         possible_moves_diagonally(GameState,Row,Col,MovesDiag,piece(Animal,Player)),
         append([], MovesOrt, Moves1),
         append(Moves1, MovesDiag, Moves))
        ).
              
possible_moves(GameState,Row,Col,Moves,piece(Animal,Player),ActualPlayer):-
        ActualPlayer = Player,
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
        findall(X, (member(Row, Array), member(X, Row), X = water(w,piece(_,Player))), Occurrences),       
        length(Occurrences, Count).
                

hardbot_move([Size,Board], Player, Row, Col,Element, BestMove) :-
    % Get all the pieces of the given player
    element_at_position([Size,Board], Row, Col, PII),  
    findall((D, ROOO, COOO), (possible_moves([Size,Board], Row, Col, Moves, PII, Player),
                                member([ROOO,COOO],Moves),distance_forced(ROOO,COOO,PII,Di,Board,Player), min_member(D, Di) ),Choice),

    min_member((_-Piec,ToRowOutput,ToColOutput),Choice),
    Element = Piec,
    BestRow is ToRowOutput -1,
    BestCol is ToColOutput -1,
    BestMove = [BestRow,BestCol].     

hardbot_move([Size,Board], Player, ChoosenPiece, BestMove) :-
    % Get all the pieces of the given player       
    findall((R, C, P), (nth0(R, Board, RowList), nth0(C, RowList, P), (P = piece(_, Player) ; P = water(w,piece(_,Player)))), Pieces),
    findall((D,ROO, COO, ROOO, COOO), (member((ROO,COO,PII), Pieces), PII \= water(w,_), possible_moves([Size,Board], ROO, COO, Moves, PII, Player),
                                member([ROOO,COOO],Moves),distance(ROOO,COOO,ROO, COO,PII,Di,Board,Player), min_member(D, Di) ),Choice),
    
    min_member((_-Piec,FromRow,FromCol,ToRowOutput,ToColOutput),Choice),
    ChoosenPiece = (FromRow,FromCol,Piec),
    BestRow is ToRowOutput -1,
    BestCol is ToColOutput -1,
    BestMove = [BestRow,BestCol].

    

        
distance(Ro,Co,OldRo, OldCo,PI,D,Board,Player):-
        Ro1 is Ro -1,
        Co1 is Co -1,
        findall((Distance-PI), (nth0(R, Board, RowList), nth0(C, RowList, W), (W = water(w,_), (W \= water(w,(_,Player)))),
                                Distance is (abs(Ro1 - R) + abs(Co1 - C)),
                               Distance2 is (abs(OldRo - R) + abs(OldCo - C)),
                               Distance < Distance2), D).

distance_forced(Ro,Co,PI,D,Board,Player):-
        Ro1 is Ro -1,
        Co1 is Co -1,
        findall((Distance-PI), (nth0(R, Board, RowList), nth0(C, RowList, W), (W = water(w,_), (W \= water(w,(_,Player)))),
                                Distance is (abs(Ro1 - R) + abs(Co1 - C))), D).
        