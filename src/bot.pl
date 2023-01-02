%The easy bot turn, chooses a random piece from the bot pieces (not random if forced move)
%After that chooses a random move from the choosen piece and updates the board with the move
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

%The hard bot turn, chooses the best piece (if not forced) and movement, updates the board after that
hardbot(Player,[Size,Board], UpdatedBoard) :-
        (forced_move(Player,[Size,Board],Row,Col) ->
        write('Forced!'), nl,
        hardbot_move([Size,Board], Player, Row,Col,Element, [R,C])
        ;
        hardbot_move([Size,Board], Player, (Row,Col,Element), [R,C])
         ),
        
        OutputRow is Row +1, OutputCol is Col+1,
        OutputR is R +1, OutputC is C+1,
        write('Bot is going to move piece from '),write('['),write(OutputRow),write('-'), write(OutputCol),write(']'), nl,
        write('To '),write('['),write(OutputR),write('-'), write(OutputC),write(']'), nl,
        (Element = water(w, _) -> replace_in_board(Board,Row, Col, Element, water(w,(o,0)), NewBoard);
                        replace_in_board(Board,Row, Col, Element, o, NewBoard)),
        element_at_position([Size,Board], R, C, Destination),
        (Destination = water(w, _) -> replace_in_board(NewBoard,R, C, Element, water(w,Element),UpdatedBoard);
                (Element = water(w, piece(AnimalInWater,PlayerInWater)) -> replace_in_board(NewBoard,R, C, o, piece(AnimalInWater,PlayerInWater), UpdatedBoard);
                        replace_in_board(NewBoard,R, C, o, Element, UpdatedBoard))).

%Version not forced of the bot move, gets the best move for the piece in position row,col and returns the piece and its move
hardbot_move([Size,Board], Player, Row, Col,Element, BestMove) :-
    element_at_position([Size,Board], Row, Col, PII),
    %finds all the tuples (Distance, row, col) of the pieces to a water  
    findall((D, Ro, Co), (possible_moves([Size,Board], Row, Col, Moves, PII, Player),
                                member([Ro,Co],Moves),distance_forced(Ro,Co,PII,Di,Board,Player), min_member(D, Di) ),Choice),

    min_member((_-Piec,ToRowOutput,ToColOutput),Choice),
    Element = Piec,
    BestRow is ToRowOutput -1,
    BestCol is ToColOutput -1,
    BestMove = [BestRow,BestCol].     

%Version not forced of the hard bot move, tests all the possible moves with all possible pieces and returns the best one
hardbot_move([Size,Board], Player, ChoosenPiece, BestMove) :-
    % Get all the pieces of the given player       
    findall((R, C, P), (nth0(R, Board, RowList), nth0(C, RowList, P), (P = piece(_, Player) ; P = water(w,piece(_,Player)))), Pieces),
    %finds all the tuples (Distance, FromRow, FromCol,ToRow,ToCol) of the pieces to a water  
    findall((D,ROO, COO, ROOO, COOO), (member((ROO,COO,PII), Pieces), PII \= water(w,_), possible_moves([Size,Board], ROO, COO, Moves, PII, Player),
                                member([ROOO,COOO],Moves),distance(ROOO,COOO,ROO, COO,PII,Di,Board,Player), min_member(D, Di) ),Choice),
    
    min_member((_-Piec,FromRow,FromCol,ToRowOutput,ToColOutput),Choice),
    ChoosenPiece = (FromRow,FromCol,Piec),
    BestRow is ToRowOutput -1,
    BestCol is ToColOutput -1,
    BestMove = [BestRow,BestCol].

    

% Gets all the tuples (distance-piece) with their distances to a water with the given move, returns it on D        
distance(Ro,Co,OldRo, OldCo,PI,D,Board,Player):-
        Ro1 is Ro -1,
        Co1 is Co -1,
        findall((Distance-PI), (nth0(R, Board, RowList), nth0(C, RowList, W), (W = water(w,_), (W \= water(w,(_,Player)))),
                                Distance is (abs(Ro1 - R) + abs(Co1 - C)),
                               Distance2 is (abs(OldRo - R) + abs(OldCo - C)),
                               Distance < Distance2), D).
% Forced version, get the best move for the piece at Ro,Co (minimize its distance to a water)
distance_forced(Ro,Co,PI,D,Board,Player):-
        Ro1 is Ro -1,
        Co1 is Co -1,
        findall((Distance-PI), (nth0(R, Board, RowList), nth0(C, RowList, W), (W = water(w,_), (W \= water(w,(_,Player)))),
                                Distance is (abs(Ro1 - R) + abs(Co1 - C))), D).