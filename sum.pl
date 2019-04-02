nodeArcsOfList([FirstStep|Rest], [Node|Nodes], [Arc|Arcs]) :-
  Ark = arc(FirstRel,FirstStep,FirstRel),
  source(FirstRel),
  nodeArcsOfList(Rest, FirstRel, Nodes, Arcs).
nodeArcsOfList([Step|Rest], LastRel, Nodes, [Arc|Arcs]) :-
  Ark = arc(LastRel,Step,NextRel),
  nodeArcsOfList(Rest, NextRel, Nodes, Arc).
nodeArcsOfList([LastStep], LastRel, [Node|Nodes], [Arc|Arcs]) :-
  Ark = arc(LastRel,LastStep,LastRel),
  sink(LastRel).

contains(SubList, List) :-
  nodeArcsOfList(Sublist, Nodes, Arcs),
  automaton(List, Nodes, Arcs).

