between1and9(List,Between) :-
  ( append(_, [1|Rest], List), append(Between, [9|_], Rest) );
  ( append(_, [9|Rest], List), append(Between, [1|_], Rest) ).


