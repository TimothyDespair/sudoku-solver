:- use_module(library(clpfd)).

nodeArcsOfList([FirstStep|Rest], [FirstN,FinalN], [Arc|Arcs]) :-
  Arc = arc(FirstN,FirstStep,NextN),
  nodeArcsOfList(Rest, NextN, FinalN, Arcs).
nodeArcsOfList([LastStep|[]], PrevN, FinalN, [Arc]) :-
  Arc = arc(PrevN,LastStep,FinalN).
nodeArcsOfList([Step|Rest], PrevN, FinalN, [Arc|Arcs]) :-
  Arc = arc(PrevN,Step,NextN),
  nodeArcsOfList(Rest, NextN, FinalN, Arcs).

rangeCapArcs([],_,[]).
rangeCapArcs([Head|Rest], CapNode, [Arc|Arcs]) :-
  Arc = arc(CapNode, Head, CapNode),
  rangeCapArcs(Rest, CapNode, Arcs).

contains(Range, SubList, List) :-
  nodeArcsOfList(SubList, [FirstN, FinalN], Arcs1),
  rangeCapArcs(Range, FirstN, Arcs2),
  rangeCapArcs(Range, FinalN, Arcs3),
  flatten([Arcs1,Arcs2,Arcs3], Arcs),
  automaton(List, [source(FirstN), sink(FinalN)], Arcs).

contains_either(Range, A, B, List) :-
  nodeArcsOfList(A, [FirstN,FinalN], Arcs1),
  nodeArcsOfList(B, [FirstN,FinalN], Arcs2),
  rangeCapArcs(Range, FirstN, Arcs3),
  rangeCapArcs(Range, FinalN, Arcs4),
  flatten([Arcs1, Arcs2, Arcs3, Arcs4], Arcs),
  automaton(List, [source(FirstN), sink(FinalN)], Arcs).

/* Turn this into an in-set/out-set/boundary deal for sum-sudoku:
 * a-> outset <-b-> boundary <-c-> inset */

sharedNodeArcs(List, Node, Arcs) :-
   nodeArcs(List, Node, Node, Arcs).

nodeArcs([],_,_,[]).
nodeArcs([Step|Rest], Node1, Node2, [Arc|Arcs]) :-
  Arc = arc(Node1, Step, Node2),
  nodeArcs(Rest, Node1, Node2, Arcs).

listDom([], Domain) :-
  Domain = 0..0.
listDom([Head|Rest], Domain) :-
  listDom(Rest, RestDom),
  Domain = Head \/ RestDom.

notIns(Dom, Item) :-
  #\ Item in Dom.

blocks([A,B,C,D,E,F,G,H,I], Blocks) :-
    blocks(A,B,C,Block1), blocks(D,E,F,Block2), blocks(G,H,I,Block3),
    append([Block1, Block2, Block3], Blocks).

blocks([], [], [], []).
blocks([A,B,C|Bs1],[D,E,F|Bs2],[G,H,I|Bs3], [Block|Blocks]) :-
    Block = [A,B,C,D,E,F,G,H,I],
    blocks(Bs1, Bs2, Bs3, Blocks).

sumBetween1And9([Sum|List]) :-
  length(List, 9),
  List ins 1..9,
  all_distinct(List),
  length(Boundary, 2),
  all_distinct(Boundary),
  Boundary ins 1\/9,
  label(Boundary),
  length(InSet,InSetLength),
  InSetLength in 0..7,
  InSet ins 2..8,
  sum(InSet, #=, Sum),
  label(InSet),
  OutSetLength is 7 - InSetLength,
  length(OutSet,OutSetLength),
  all_distinct(OutSet),
  OutSet ins 2..8,
  listDom(InSet, InSetDomain),
  maplist(notIns(InSetDomain), OutSet),
  label(OutSet),
  sharedNodeArcs(OutSet,A,OA1),
  nodeArcs(Boundary,A,B,BA1),
  sharedNodeArcs(InSet,B,IA),
  nodeArcs(Boundary,B,C,BA2),
  sharedNodeArcs(OutSet,C,OA2),
  flatten([OA1,BA1,IA,BA2,OA2], Arcs),
  trace, /* Current error point... says graph is impossible.
  Need to test whether it's a constraint conflict or if my
  graph logic is flawed...

  This works: So it's not my graph logic.
  length(List, 9),
    List ins 1..9,
    all_distinct(List),
    Outs=[2,3,5,6,7,8],
	Bnds=[1,9],
	Ins=[4],
	sharedNodeArcs(Outs,A,ArsO1),
	nodeArcs(Bnds,A,B,ArsB1),
    sharedNodeArcs(Ins,B,ArsI),
    nodeArcs(Bnds,B,C,ArsB2),
    sharedNodeArcs(Outs,C,ArsO2),
    flatten([ArsO1,ArsB1,ArsI,ArsB2,ArsO2], Ars),
    automaton(List, [source(A), sink(C)], Ars),
    label(List).*/
  automaton(List, [source(A), sink(C)], Arcs).

sudoku(Rows) :-
  length(Rows, 9),
  maplist(same_length(Rows), Rows),
  flatten(Rows, Cells), Cells ins 1..9,
  transpose(Rows, Columns),
  blocks(Rows, Blocks),
  maplist(all_distinct, Rows),
  maplist(all_distinct, Columns),
  maplist(all_distinct, Blocks).

sumSudoku(Rows) :-
  length(Rows, 10),
  maplist(same_length(Rows), Rows),
  transpose(Rows, Cols),
  reverse(Rows, [__CSums|RJustRows]),
  reverse(RJustRows, JustRows),
  reverse(Cols, [__RSums|RJustCols]),
  reverse(RJustCols, JustCols),
  transpose(JustCols, SudokuRowsVals),
  reverse(SudokuRowsVals, [__Sum|RSudokuRows]),
  reverse(RSudokuRows, SudokuRows),
  sudoku(SudokuRows),
  trace,
  maplist(sumBetween1And9, JustRows),
  maplist(sumBetween1And9, JustCols).
  
    
