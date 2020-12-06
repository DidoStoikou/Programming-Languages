binary(0, [0], 0).
binary(1, [1], 1).
binary(N, Bin, Sum1):-
  N > 1,
  A is N mod 2,
  B is N // 2,
  binary(B, Subbin, Sum2),
  Bin = [A|Subbin],
  Sum1 is Sum2 + A.

breaking([A], [C], 0):-
  C is A.
breaking([A,B], [C], 0):-
  B = 0,
  C is A.
breaking([A,B], [C], 1):-
  B = 1,
  C is A + 2.
breaking([A,B], [C,D], 1):-
  B > 0,
  C is A + 2,
  D is B - 1.
breaking([A,B|X], [C,D|Y], 1):-
  B > 0,
  C is A + 2,
  D is B - 1,
  Y = X.
breaking([A,B|X], [C,D|Y], Added):-
  B = 0,
  C is A,
  breaking([B|X], [D|Y], Added).

product(Assoi, K, _, TelikhLista):-
  Assoi > K,
  TelikhLista = [].
product(Assoi, K, ArxikhLista, TelikhLista):-
  Assoi = K,
  TelikhLista = ArxikhLista.
product(Assoi, K, ArxikhLista, UpdatedTelikhLista):-
  Assoi < K,
  breaking(ArxikhLista, TelikhLista, Added),
  Moreassoi is Assoi + Added,
  product(Moreassoi, K, TelikhLista, UpdatedTelikhLista).


alg(N, K, TelikhLista):-
  binary(N, Dyadikh, Sum1),
  once(product(Sum1, K, Dyadikh, TelikhLista)).

traverse(0, _Stream, Answer, Final):-
  Final = Answer.
traverse(T, Stream, Now, Answer):-
  read_line(Stream, [N, K]),
  alg(N, K, Output),
  append(Now, [Output], UpdatedNow),
  UpdatedT is T - 1,
  traverse(UpdatedT, Stream, UpdatedNow, Answer).

powers2(File, Answer):-
  open(File, read, Stream),
  read_line(Stream, T),
  [Head|_] = T,
  traverse(Head, Stream, [], Answer),!.

read_line(Stream, L):-
  not(at_end_of_stream(Stream)),
  read_line_to_codes(Stream, Line),
  atom_codes(Atom, Line),
  atomic_list_concat(Atoms, ' ', Atom),
  maplist(atom_number,Atoms, L).
