% This file contains all functions that manipulates the game board

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    Creates & Initializes a board   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

% creates and returns the initial gameboard with a given <Size>

%initial_state(+Size, -GameState):-
:- use_module(library(lists)).

piece(Animal,Player):-
    animal(Animal),
    (Player = 1; Player = 2).

% creates and returns the empty side of the board[
initial_state(Size, [1,InitialBoard]):-
    Row is Size,
    Column is Size,
    create_rows(Size,Row,Column, [],[], Board),
    insert_Elephants(Board,BoardWithElephants,Size),
    insert_Lions(BoardWithElephants, BoardWithLions, Size),
    insert_Rats(BoardWithLions, BoardWithRats, Size),
    insert_Gates(BoardWithRats, InitialBoard, Size),
    displayBoard(InitialBoard), !.

create_rows(_,1,0,CurrentRow,CurrentBoard,Result):-   
    append([CurrentRow],CurrentBoard,Result).
    
create_rows(Size,Row,Column,CurrentRow,CurrentBoard,Result):-
    Row > 0,
    Column > 0,
    append(CurrentRow,[o],UpdatedRow),
    NextCol is Column -1,
    create_rows(Size,Row,NextCol,UpdatedRow,CurrentBoard,Result).

create_rows(Size,Row,Column,FinishedRow,CurrentBoard,Result):-
    Row > 0,
    Column =:= 0,
    append(CurrentBoard,[FinishedRow],NewBoard),
    NextRow is Row -1,
    BeginColumn is Size,
    create_rows(Size,NextRow,BeginColumn,[],NewBoard,Result).

displayBoard(Board):-
    write('  ---'),
    displayNumbers(Board,1),
    displayAllBoard(Board,1),
    write('  '),
    displayHorizontalRow(Board),nl.

displayNumbers([_|[]],Num):-
   write(Num), write('---'), nl.

displayNumbers([_|XS],Num):-
    Num >= 9,
    write(Num), write('--'),write('-'),
    Num1 is Num + 1,
    displayNumbers(XS,Num1).

displayNumbers([_|XS],Num):-
    write(Num), write('--'),write('--'),
    Num1 is Num + 1,
    displayNumbers(XS,Num1).

displayHorizontalRow([_|[]]):-
    write('-------').
                           
displayHorizontalRow([_|XS]):-
    write('-----'),
    displayHorizontalRow(XS).

displayAllBoard([X|[]],Num):-
    Num > 9,
    write(Num),write('|'),
    displayRow(X).

displayAllBoard([X|[]],Num):-   
    write(Num),write(' '),write('|'),
    displayRow(X).

displayAllBoard([X|XS],Num):-
    Num > 9,
    write(Num),
    write('|'),
    displayRow(X),
    Num1 is Num +1,
    displayAllBoard(XS,Num1).

displayAllBoard([X|XS],Num):-
    write(Num),
    write(' '),
    write('|'),
    displayRow(X),
    Num1 is Num +1,
    displayAllBoard(XS,Num1).

displayRow([piece(Animal,Player)]):-
    write(' '),
    write(Animal),write(Player),write(' '),write(' '),write('|'),nl.

displayRow([water(w,(o,0))]):-
    write(' '),
    write('w'),write(o),write(0),write(' '),write('|'),nl.

displayRow([water(w,piece(Animal,Player))]):-
    write(' '),
    write('w'),write(Animal),write(Player),write(' '),write('|'),nl.

displayRow([_]):-
    write(' '),
    write(' '),
    write(-),write(' '),write(' '), write('|') , nl.

displayRow([piece(Animal,Player)|XS]):-
    write(' '),
    write(Animal),write(Player) ,write(' '),
    write(' '),
    displayRow(XS).

displayRow([water(w,(o,0))|XS]):-
    write(' '),
    write('w'),write(o),write(0),
    write(' '),
    displayRow(XS).

displayRow([water(w,piece(Animal,Player))|XS]):-
    write(' '),
    write('w'),write(Animal),write(Player),
    write(' '),
    displayRow(XS).

displayRow([_|XS]):-
    write(' '),
    write(' '),
    write(-),
    write(' '),
    write(' '),
    displayRow(XS).

insert_Elephants(Board,BoardWithElephants,Size):-

    LastRole is Size - 1,
    MiddleColumn is round(Size/2),
    FirstElephantColumn is MiddleColumn -1,
    SecondElephantColumn is MiddleColumn, %just for better reading
    replace_in_board(Board,0,FirstElephantColumn,o,piece(e,1),BoardWith1Elephants),
    replace_in_board(BoardWith1Elephants,0,SecondElephantColumn,o,piece(e,1),BoardWith2Elephants),
    replace_in_board(BoardWith2Elephants,LastRole,FirstElephantColumn,o,piece(e,2),BoardWith3Elephants),
    replace_in_board(BoardWith3Elephants,LastRole,SecondElephantColumn,o,piece(e,2),BoardWithElephants).
    
insert_Lions(Board, BoardWithLions, Size):-
    
    LastLionRole is Size - 2,
    MiddleColumn is round(Size/2),
    FirstLionColumn is MiddleColumn - 2,
    SecondLionColumn is MiddleColumn + 1,
    replace_in_board(Board,1,FirstLionColumn,o,piece(l,1),BoardWith1Lion),
    replace_in_board(BoardWith1Lion,1,SecondLionColumn,o,piece(l,1),BoardWith2Lions),
    replace_in_board(BoardWith2Lions,LastLionRole,FirstLionColumn,o,piece(l,2),BoardWith3Lions),
    replace_in_board(BoardWith3Lions,LastLionRole,SecondLionColumn,o,piece(l,2),BoardWithLions).
    
insert_Rats(Board, BoardWithRats, Size):-
    
    LastRatRole is Size - 2,
    MiddleColumn is round(Size/2),
    FirstRatColumn is MiddleColumn - 1,
    SecondRatColumn is MiddleColumn,
    replace_in_board(Board,1,FirstRatColumn,o,piece(r,1),BoardWith1Rat),
    replace_in_board(BoardWith1Rat,1,SecondRatColumn,o,piece(r,1),BoardWith2Rat),
    replace_in_board(BoardWith2Rat,LastRatRole,FirstRatColumn,o,piece(r,2),BoardWith3Rat),
    replace_in_board(BoardWith3Rat,LastRatRole,SecondRatColumn,o,piece(r,2),BoardWithRats).    

insert_Gates(Board, BoardWithGates, Size):-

    LastGateRole is Size - 4,
    MiddleColumn is round(Size/2),
    FirstGateColumn is MiddleColumn - 2,
    SecondGateColumn is MiddleColumn + 1,
    replace_in_board(Board,3,FirstGateColumn,o,water(w,(o,0)),BoardWith1Gate),
    replace_in_board(BoardWith1Gate,3,SecondGateColumn,o,water(w,(o,0)),BoardWith2Gate),
    replace_in_board(BoardWith2Gate,LastGateRole,FirstGateColumn,o,water(w,(o,0)),BoardWith3Gate),
    replace_in_board(BoardWith3Gate,LastGateRole,SecondGateColumn,o,water(w,(o,0)),BoardWithGates).

element_at_position([Size,Board], Row, Col, Element) :-
    within_Board(Row,Col,Size),
    nth0(Row, Board, SelectedRow),
    nth0(Col, SelectedRow, Element).

% Check if a position is within the board
within_Board(X, Y, Size) :-
    X >= 0, X < Size,
    Y >= 0, Y < Size.

%nth0(?Index, ?List, ?Elem)
%nth0(?N, ?List, ?Elem, ?Rest)
replace_in_board(Board, Row, Col,OldElem, NewElem, NewBoard) :-
   % predicate works forward: Index,List -> OldElem, Transfer
   nth0(Row,Board,EspecificRow),
   nth0(Col,EspecificRow,_,Transfer),
   % predicate works backwards: Index,NewElem,Transfer -> NewList
   nth0(Col,NewRow,NewElem,Transfer),
   nth0(Row,Board,EspecificRow,BoardRest),
   nth0(Row,NewBoard,NewRow,BoardRest).
   
