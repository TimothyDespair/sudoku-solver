:- use_module(library(clpfd)).

loop(Node, List, Arcs) :-
   arcs(Node, List, Node, Arcs).

arcs(_,[],_,[]).
arcs( Node1,[Step|Rest], Node2, [Arc|Arcs]) :-
  Arc = arc(Node1, Step, Node2),
  arcs(Node1, Rest, Node2, Arcs).

% Not useful for what I'm doing right now, but you should be able to
% adapt this to grow multiple "cultures" by removing every step after
% TTT and using WallL as both source and sink.
agar_plate(Substrate, Membranes, Tissue, [WallL, WallR], AgarPlate) :-
  arcs(WallL, Membranes, TissueLoop, WMT),
  arcs(WallL, Substrate, SubstrateLoopL, WSS),
  loop(SubstrateLoopL, Substrate, SSL),
  arcs(SubstrateLoopL, Membranes, TissueLoop, SMT),
  loop(TissueLoop, Tissue, TTT),
  arcs(TissueLoop, Membranes, SubstrateLoopR, TMS),
  loop(SubstrateLoopR, Substrate, LSS),
  arcs(SubstrateLoopR, Substrate, WallR, SSW),
  arcs(TissueLoop, Membranes, WallR, TMW),
  flatten([WMT,WSS,SSL,SMT,TTT,TMS,LSS,SSW,TMW], AgarPlate).

agar_growth(Dish, Plate, Result) :-
  Dish = [WallL, WallR],
  automaton(Result, [source(WallL), sink(WallR)], Plate).

agar_super_plate([Substrate1,Substrate2], Membrane, [Tissue1,Tissue2], Dish, SuperPlate) :-
  agar_plate(Substrate1, Membrane, Tissue1, Dish, Plate1),
  agar_plate(Substrate2, Membrane, Tissue2, Dish, Plate2),
  flatten([Plate1, Plate2], SuperPlate).

agar_hyper_plate(Substrates, Membrane, Tissues, Dish, HyperPlate) :-
  agar_hyper_plate_(Substrates, Membrane, Tissues, Dish, Plates),
  flatten(Plates, HyperPlate).

agar_hyper_plate_([],_,[],_,[]).
agar_hyper_plate_([Substrate|Substrates], Membrane, [Tissue|Tissues], Dish, [Plate|Plates]) :-
  agar_plate(Substrate, Membrane, Tissue, Dish, Plate),
  agar_hyper_plate_(Substrates, Membrane, Tissues, Dish, Plates).

sum_set(Sum, Set) :-
  between(0,7,Length),
  length(Set, Length),
  Set ins 2..8,
  all_distinct(Set),
  sum(Set, #=, Sum),
  chain(Set, #>),
  label(Set).

sum_sets(Sum, Sets) :-
  setof(Set, sum_set(Sum, Set), Sets).

list_dom([], Domain) :-
  Domain = 0..0.
list_dom([Head|Rest], Domain) :-
  list_dom(Rest, RestDom),
  Domain = Head \/ RestDom.

not_ins(Dom, Item) :-
  #\ Item in Dom.

ins_outs([Ins, Outs]) :-
  length(Ins, InsLength),
  length(Outs, OutsLength),
  OutsLength is 7 - InsLength,
  all_distinct(Outs),
  Outs ins 2..8,
  list_dom(Ins, InsDom),
  maplist(not_ins(InsDom), Outs),
  chain(Outs, #>),
  label(Outs).

all_ins_outs([Ins], [Outs]) :-
  ins_outs([Ins, Outs]).
all_ins_outs([Ins|InsRest], [Outs|OutsRest]) :-
  ins_outs([Ins, Outs]),
  all_ins_outs(InsRest,OutsRest).

membranes(Membranes) :-
  length(Membranes, 2),
  all_distinct(Membranes),
  Membranes ins 1\/9,
  chain(Membranes, #>),
  label(Membranes).

sum_between_1_and_9(ListSum) :-
  reverse(ListSum,[Sum|List]),
  length(List, 9),
  List ins 1..9,
  all_distinct(List),
  % Membranes/Ins/Outs
  once(membranes(Membranes)),
  once(sum_sets(Sum, Tissue)), % Once Because these are deteminative.
  once(all_ins_outs(Tissue, Substrate)),
  % HyperState Automaton to Check Ever Permutation at Once.
  agar_hyper_plate(Substrate, Membranes, Tissue, Dish, HyperPlate),
  agar_growth(Dish, HyperPlate, List).

blocks([A,B,C,D,E,F,G,H,I], Blocks) :-
    blocks(A,B,C,Block1), blocks(D,E,F,Block2), blocks(G,H,I,Block3),
    append([Block1, Block2, Block3], Blocks).

blocks([], [], [], []).
blocks([A,B,C|Bs1],[D,E,F|Bs2],[G,H,I|Bs3], [Block|Blocks]) :-
    Block = [A,B,C,D,E,F,G,H,I],
    blocks(Bs1, Bs2, Bs3, Blocks).

sudoku(Rows) :-
  length(Rows, 9),
  maplist(same_length(Rows), Rows),
  flatten(Rows, Cells), Cells ins 1..9,
  transpose(Rows, Columns),
  blocks(Rows, Blocks),
  maplist(all_distinct, Rows),
  maplist(all_distinct, Columns),
  maplist(all_distinct, Blocks).

sum_sudoku(Rows) :-
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
  maplist(sum_between_1_and_9, JustRows),
  maplist(sum_between_1_and_9, JustCols).   
