:- use_module(library(clpfd)).

/* There may be a way to achieve this with `call/N` and meta predicates but
 * for now that's still a bit beyond me. */

onlyone(List, Num) :-
  member(Num, List) ->
    ( append(Y, [Num|Z], List),
      append(Y, Z, Rest),
      maplist(dif(Num), Rest) ).

onethroughnine(List) :-
  Vals = [1,2,3,4,5,6,7,8,9],
  maplist(onlyone(List), Vals).

blocks([A,B,C,D,E,F,G,H,I], Blocks) :-
  blocks(A,B,C,Block1),
  blocks(D,E,F,Block2),
  blocks(G,H,I,Block3),
  append([Block1,Block2,Block3],Blocks).

blocks([], [], [], []).
blocks([A,B,C|Bs1],[D,E,F|Bs2],[G,H,I|Bs3],[Block|Blocks]) :-
  Block = [A,B,C,D,E,F,G,H,I],
  blocks(Bs1, Bs2, Bs3, Blocks).

sudoku(Rows) :-
  flatten(Rows, Values),
  Values ins 1..9,
  transpose(Rows, Columns),
  length(Rows, 9),
  length(Columns, 9),
  blocks(Rows, Blocks),
  trace,
  maplist(onethroughnine, Rows),
  trace,
  maplist(onethroughnine, Columns),
  maplist(onethroughnine, Blocks),
  label(Values).:- use_module(library(clpfd)).
  
