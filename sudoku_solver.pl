:- use_module(library(clpfd)).

sumsudoku(PuzzleRows) :-
  transpose(PuzzleRows, PuzzleColumns),
  length(PuzzleRows, 10),
  length(PuzzleColumns, 10),
  reverse(PuzzleRows, [_|ValueRows]),
  reverse(PuzzleColumns, [_|ValueColumns]),
  transpose(ValueColumns, RowsAndColumnSums),
  reverse(RowsAndColumnSums, [_|Rows]),
  transpose(Rows, Columns),
  Rows ins 1..9,
  maplist(sumbetween1and9, ValueRows),
  maplist(sumbetween1and9, ValueColumns),
  transpose(Rows, Columns),
  blocks(Rows, Blocks),
  maplist(all_distinct, Rows),
  maplist(all_distinct, Columns),
  maplist(all_distinct, Blocks),
  maplist(label, Rows).

sumbetween1and9(SumAndValues) :-
  reverse(SumAndValues, [Sum|Values]),
  between1and9(Values, SumValues),
  sum(SumValues, #=, Sum).

between1and9(List, Between) :-
  ( append(_, [1|Rest], List), append(Between, [9|_], Rest) );
  ( append(_, [9|Rest], List), append(Between, [1|_], Rest) ).

blocks([A,B,C,D,E,F,G,H,I], Blocks) :-
  blocks(A,B,C,Block1),
  blocks(D,E,F,Block2),
  blocks(G,H,I,Block3),
  append([Block1, Block2, Block3], Blocks).

blocks([], [], [], []).
blocks([A,B,C|Bs1],[D,E,F|Bs2],[G,H,I|Bs3], [Block|Blocks]) :-
  Block = [A,B,C,D,E,F,G,H,I],
  blocks(Bs1, Bs2, Bs3, Blocks).

main :-
  Puz =
    [ [_,_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_,0] ],
    /*    [ [_,_,_,_,_,_,_,_,_,4]
    , [_,_,_,_,_,_,_,_,_,33]
    , [_,_,1,_,_,_,_,_,_,20]
    , [_,_,_,_,_,_,_,_,_,17]
    , [_,_,_,_,5,_,_,_,_,26]
    , [_,_,_,_,_,_,_,_,_,10]
    , [_,_,_,_,_,_,9,_,_,16]
    , [_,_,_,_,_,_,_,_,_,24]
    , [_,_,_,_,_,_,_,_,_,0]
    , [8,4,17,35,14,13,3,10,25,0] ],*/
/*    Puz =
    [ [_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_]
    , [_,_,_,_,_,_,_,_,_] ],*/
    sumsudoku(Puz),
    write(Puz),
    halt.

main :- halt(1).

