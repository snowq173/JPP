% ks418388 - Kacper Sołtysiak
%
% Przyjęta przeze mnie reprezentacja automatów to termy postaci
% dfaRepr(STATES, START, ENDINGSTATES, ALPHABET),
% gdzie STATES - dwupoziomowe drzewo BST stanów,
% START - stan początkowy,
% ENDINGSTATES - drzewo BST stanów końcowych,
% ALPHABET - drzewo BST zawierające alfabet automatu (do sprawdzania,
% czy dwa automaty są określone nad tym samym językiem)
% przykładowy term - wygenerowany dla termu dfa([fp(1,a,1)], 1, [1]):
% dfaRepr(n(nil,(1,n(nil,(a, 1),nil)),nil), 1, n(nil,1,nil), n(nil, a, nil)).


% znajduje atrybut dla wartości w wzbogaconym drzewie BST
% predykat wymaga, aby wartośc (2 argument) była ustalona

findInEnrichedBST(n(_, (VALUE, ATTR), _), VALUE, ATTR) :- !.
findInEnrichedBST(n(LEFT, (VALUE, _), _), W, ATTR) :-
   nonvar(LEFT),
   W @< VALUE,
   !,
   findInEnrichedBST(LEFT, W, ATTR).
findInEnrichedBST(n(_, (VALUE, _), RIGHT), W, ATTR) :-
   nonvar(RIGHT),
   VALUE @< W,
   !,
   findInEnrichedBST(RIGHT, W, ATTR).

% sprawdza czy wzbogacone drzewo BST zawiera daną wartość
% gdy W jest ustalone, wtedy sprawdza, czy istnieje węzeł
% o danej wartości; gdy W jest zmienną - jest generatorem
% wartości

containsEnrichedBST(n(LEFT, (VALUE, _), _), W) :-
  nonvar(LEFT),
  (nonvar(W) -> W @< VALUE, !; true),
  containsEnrichedBST(LEFT, W).
containsEnrichedBST(n(_, (VALUE, _), RIGHT), W) :-
  nonvar(RIGHT),
  (nonvar(W) -> VALUE @< W, ! ; true),
  containsEnrichedBST(RIGHT, W).
containsEnrichedBST(n(_, (VALUE, _), _), V) :-
  (nonvar(V) -> ! ; true),
  VALUE = V.

% dodaje element do wzbogaconego drzewa BST

insertToEnrichedBST((L, S), n(_, (L, S), _)) :- !.
insertToEnrichedBST((L1, S1), n(LEFT, (L2, _), _)) :-
   L1 @< L2,
   insertToEnrichedBST((L1, S1), LEFT).
insertToEnrichedBST((L1, S1), n(_, (L2, _), RIGHT)) :-
   L2 @< L1,
   insertToEnrichedBST((L1, S1), RIGHT).

% zamyka drzewo BST

closeBST(nil) :- !.
closeBST(n(L, _, R)) :-
    (var(L) -> L = nil ; closeBST(L)),
    (var(R) -> R = nil ; closeBST(R)).

% zamyka wzbogacone drzewo BST, w którym atrybuty
% są drzewami BST - w praktyce takie drzewo jest
% drzewem stanów automatu

closeEnrichedBST(n(L, (_, T), R)) :- 
   (var(L) -> L = nil ; closeEnrichedBST(L)),
   (var(R) -> R = nil ; closeEnrichedBST(R)),
   closeBST(T), 
   !.

% sprawdza czy drzewo BST zawiera daną wartość

containsBST(n(LEFT, VALUE, _), W) :-
   (nonvar(W) -> W @< VALUE, ! ; true),
   nonvar(LEFT), 
   containsBST(LEFT, W).
containsBST(n(_, VALUE, RIGHT), W) :-
   (nonvar(W) -> VALUE @< W, ! ; true),
   nonvar(RIGHT),
   containsBST(RIGHT, W).
containsBST(n(_, VALUE, _), V) :-
   (nonvar(V) -> ! ; true),
   VALUE = V.

% dodaje wartośc do drzewa BST

insertToBST(n(_, VALUE, _), VALUE) :- !.
insertToBST(n(LEFT, VALUE, _), W) :-
   W @< VALUE,
   insertToBST(LEFT, W),
   !.
insertToBST(n(_, VALUE, RIGHT), W) :-
   VALUE @< W,
   insertToBST(RIGHT, W).

% sprawdza czy wzbogacone drzewo BST zawiera wszystkie elementy z listy

containsEnrichedAllFromList(_, []) :- !.
containsEnrichedAllFromList(TREE, [ELEM|ELEMS]) :-
   containsEnrichedBST(TREE, ELEM),
   containsEnrichedAllFromList(TREE, ELEMS).

% predykat wyznaczający rozmiar drzewa BST

treeSize(nil, 0).
treeSize(n(L, _, R), TREESIZE) :-
  (nonvar(L) -> treeSize(L, LEFTSIZE) ; LEFTSIZE = 0),
  (nonvar(R) -> treeSize(R, RIGHTSIZE) ; RIGHTSIZE = 0),
  OVERALL is LEFTSIZE+RIGHTSIZE+1,
  TREESIZE = OVERALL.

% wewnętrzna funkcja sprawdzająca akceptowanie słowa

accept0(dfaRepr(STATES, START, ENDINGS, AL), X) :-
   var(X),                      % gdy X jest zmienną - generujemy słowa
   !,
   treeSize(STATES, TREESIZE),
   (is_infinite(dfaRepr(STATES, START, ENDINGS, AL), TREESIZE) -> 
       accept2(dfaRepr(STATES, START, ENDINGS, AL), X, 0) ;
       accept1(dfaRepr(STATES, START, ENDINGS, AL), TREESIZE, X, 0)).
accept0(dfaRepr(STATES, CURRENT, ENDINGS, AL), [X|Xs]) :- 
   findInEnrichedBST(STATES, CURRENT, STATETREE),
   (var(X) -> true ; !), 
   containsEnrichedBST(STATETREE, X),
   findInEnrichedBST(STATETREE, X, TARGET), 
   accept0(dfaRepr(STATES, TARGET, ENDINGS, AL), Xs).
accept0(dfaRepr(_, CURRENT, ENDINGSTATES, _), []) :- 
   containsBST(ENDINGSTATES, CURRENT).

% generowanie nieskończonego języka
% dfaRepr - automat, X - słowo, N - długość słowa
accept2(DFA, X, N) :-
   length(X, N),
   accept0(DFA, X).
accept2(DFA, X, N) :- 
   NN is N+1, 
   accept2(DFA, X, NN).

% generowanie skończonego języka
% dfaRepr - automat, X - słowo, N - długość słowa

accept1(DFA, _, X, N) :- 
   length(X, N), 
   accept0(DFA, X).
accept1(DFA, CNT, X, N) :- 
   NN is N+1, NN =< CNT,
   !,
   accept1(DFA, CNT, X, NN).

% sprawdza czy język DFA jest nieskończony
% dfaRepr - automat, STATESCNT - liczba stanów automatu
% w celu ustalenia skończoności języka wystarczy sprawdzić,
% czy automat akceptuje słowo o długości >= STATESCNT
% i mniejszej niż 2*STATESCNT (żeby nie sprawdzać w nieskończoność)

is_infinite(DFA, STATESCNT) :-
   fin(STATESCNT, VAL), 
   SHIFTEDVAL is VAL+STATESCNT, % w przedziale [STATESCNT, 2*STATESCNT)
   length(Y, SHIFTEDVAL),
   accept0(DFA, Y).

% predykat z ćwiczeń - generuje X jako
% liczby całkowite nieujemne mniejsze niż N

fin(N, M, X) :-
   M < N,
   M = X.
fin(N, M, X) :-
   M < N,
   M2 is N+1,
   fin(N, M2, X).
fin(N, X) :-
   fin(N, 0, X).

% sprawdza czy język generowany przez DFA jest niepusty

non_empty([_, ESTATES], CURRENT, _) :- 
   containsBST(ESTATES, CURRENT),  % stan końcowy - język niepusty
   !.
non_empty([STATES, ENDINGSTATES], CURRENTSTATE, VISITED) :- 
   findInEnrichedBST(STATES, CURRENTSTATE, STATETREE),
   containsEnrichedBST(STATETREE, SYM),
   findInEnrichedBST(STATETREE, SYM, STATE),
   \+containsBST(VISITED, STATE),
   insertToBST(VISITED, STATE),
   non_empty([STATES, ENDINGSTATES], STATE, VISITED).

is_empty(dfaRepr(STATES, START, ENDINGSTATES, _)) :-
   insertToBST(VISITED, START), 
   (non_empty([STATES, ENDINGSTATES], START, VISITED) -> fail ; true).

% buduje drzewo alfabetu

buildAlphabetTree([], TREE) :- closeBST(TREE).
buildAlphabetTree([fp(_, LABEL, _)|FPS], TREE) :-
   insertToBST(TREE, LABEL),
   buildAlphabetTree(FPS, TREE).

% sprawdza, czy przejście może zostać dodane do drzewa stanów

validTransition(fp(S1, LAB, _), TREE) :-
   (findInEnrichedBST(TREE, S1, T), % T - drzewo przejść stanu S
      containsEnrichedBST(T, LAB) ->      % już jest krawędź z LAB - błąd
         fail ; 
         true
   ).

statesTreeHelperSingle(fp(S1, LAB, S2), TREE) :-
   validTransition(fp(S1, LAB, S2), TREE),
   (findInEnrichedBST(TREE, S1, T) -> 
      insertToEnrichedBST((LAB, S2), T) ; 
      insertToEnrichedBST((S1, n(_, (LAB, S2), _)), TREE)).

statesTreeHelper([], _) :- !.
statesTreeHelper([FP|FPS], STATESTREE) :-
   statesTreeHelperSingle(FP, STATESTREE),
   statesTreeHelper(FPS, STATESTREE).

% buduje drzewo stanów na podstawie definicji funkcji przejścia

statesTree([], _) :- fail.  % gdy brak przejść - błąd
statesTree([fp(S1, LAB, S2)], n(_, (S1, n(_, (LAB, S2), _)), _)) :- !.
statesTree([FP|FPS], STATESTREE) :-
   statesTree([FP], STATESTREE),        % początkowe drzewo
   statesTreeHelper(FPS, STATESTREE).   % przetwarzanie reszty

% predykat tworzący drzewo stanów końcowych na podstawie listy,
% nie udaje się, gdy lista zawiera zduplikowany stan końcowy

endingStatesTree([], nil).              % brak stanów końcowych - drzewo nil
endingStatesTree([STATE], ESTATESTREE) :-
   insertToBST(ESTATESTREE, STATE).
endingStatesTree([STATE|STATES], ESTATESTREE) :-
   endingStatesTree(STATES, ESTATESTREE),  % dodajemy stan
    (containsBST(ESTATESTREE, STATE) ->
     fail ;                                % powtórzony stan końcowy - błąd
     insertToBST(ESTATESTREE, STATE)).

% sprawdza drzewo stanów

validStatesTree(STATESTREE, ALPHABETSIZE) :-
   containsEnrichedBST(STATESTREE, STATE),     % wybieramy dowolny stan
   findInEnrichedBST(STATESTREE, STATE, TRANSITIONSTREE),
   treeSize(TRANSITIONSTREE, ALPHABETSIZE).    % z kazdego stanu musi
                                               % wychodzić tyle krawędzi ile
                                               % wynosi rozmiar alfabetu

% wyznacza liczbę stanów na podstawie funkcji przejścia

helpStatesTree([fp(S1, _, S2)|FPS], T) :-
   insertToBST(T, S1),
   insertToBST(T, S2),
   helpStatesTree(FPS, T).
helpStatesTree([], T) :-
   closeBST(T).

statesCount(FPS, STATESCOUNT) :-
   helpStatesTree(FPS, T),
   treeSize(T, STATESCOUNT).


alphabetSize(FPS, ALPHABETSIZE) :-
   buildAlphabetTree(FPS, ALPTREE),
   treeSize(ALPTREE, ALPHABETSIZE).

% sprawdza poprawność reprezentacji automatu

correct(dfa(FPS, START, ENDINGSTATES), 
        dfaRepr(STATESTREE, START, ESTATESTREE, ALPTREE)) :-
   statesTree(FPS, STATESTREE),              
   closeEnrichedBST(STATESTREE),
   statesCount(FPS, STATESCOUNT),
   treeSize(STATESTREE, STATESCOUNT),
   containsEnrichedBST(STATESTREE, START),
   containsEnrichedAllFromList(STATESTREE, ENDINGSTATES),
   buildAlphabetTree(FPS, ALPTREE),
   treeSize(ALPTREE, ALPHABETSIZE),
   validStatesTree(STATESTREE, ALPHABETSIZE),
   endingStatesTree(ENDINGSTATES, ESTATESTREE),
   closeBST(ESTATESTREE),
   !.

% sprawdza, czy język akceptowany przez automat jest pusty

empty(A) :- correct(A, R), is_empty(R).

% sprawdza, czy automat akceptuje dane słowo

accept(A, X) :- correct(A, R), accept0(R, X).

% wyznacza drzewo stanów końcowych dla dopełnienia automatu

complementEndingStatesTree(_, nil, nil) :- !.
complementEndingStatesTree(nil, _, _).
complementEndingStatesTree(n(L, (S, _), R), E, T) :-
   (containsBST(E, S) -> true ; insertToBST(T, S)),
   complementEndingStatesTree(L, E, T),
   complementEndingStatesTree(R, E, T).

complement(dfaRepr(STATES, START, ESTATES, AL), dfaRepr(STATES, START, TREE, AL)) :-
   complementEndingStatesTree(STATES, ESTATES, TREE),
   !,
   closeBST(TREE).

% wyznacza automat będący produktem dwóch automatów

productDfa(ALPHABET,
           dfaRepr(ST1, S1, E1, A1), 
           dfaRepr(ST2, S2, E2, _),
           dfaRepr(STP, pstate(S1, S2), EP, A1)) :-
   statesTreeProd(ST1, ST2, ALPHABET, STP),   % drzewo stanów
   !,
   bstProd(E1, E2, EP).                       % drzewo stanów końcowych

% przekształca drzewo BST do listy (używane do uzyskania listy alfabetu)

dappend(X-Y, Y-Z, X-Z).

bstToList(nil, X-X).
bstToList(n(LEFT, VALUE, RIGHT), LIST-D) :- 
   bstToList(LEFT, A-B), 
   dappend(A-B, [VALUE|C]-C, L-C), 
   bstToList(RIGHT, C-D),
   dappend(L-C, C-D, LIST-D).

closeList(L-X, L) :- 
   X = [].

bstToClosedList(T, L) :-
   bstToList(T, LL),
   closeList(LL, L).

% sprawdza czy drzewa BST są równe co do wartości i jeśli tak -
% zapisuje (identyczną reprezentację w postaci listy w porządku inorder)
% do listy L na potrzeby późniejszych operacji - przy tworzeniu
% automatu produktowego

equalBst(A1, A2, L) :-
   bstToClosedList(A1, L1),
   bstToClosedList(A2, L2),
   L1 = L2,
   L = L1.

% sprawdza czy automaty mają ten sam alfabet i jeśli tak,
% zapisuje go w liście L.

equalAlphabets(dfaRepr(_, _, _, A1), dfaRepr(_, _, _, A2), L) :-
   equalBst(A1, A2, L).

% sprawdza czy język automatu R1 zawiera się w języku R2,
% AL - alfabet w postaci listy (ten sam) automatów R1, R2

subsetEqInternal(R1, R2, AL) :-
   complement(R2, R2C),         % R2C - dopełnienie automatu R2
   productDfa(AL, R1, R2C, PROD),   % PROD - automat produktowy R1 i R2C
   is_empty(PROD).              % PROD musi mieć pusty język

% predykat z treści zadania - sprawdza, czy język automatu
% A1 zawiera się w języku automatu A2

subsetEq(A1, A2) :- 
   correct(A1, R1),
   correct(A2, R2),
   equalAlphabets(R1, R2, L),
   subsetEqInternal(R1, R2, L).

% predykat z treści zadania - sprawdza, czy jęyki automatów
% A1 i A2 są równe

equal(A1, A2) :-
   correct(A1, R1),
   correct(A2, R2),
   equalAlphabets(R1, R2, L),
   subsetEqInternal(R1, R2, L),    % język R1 zawiera się w języku R2
   subsetEqInternal(R2, R1, L).    % język R2 zawiera się w języku R1

% predykaty budujące drzewo BST będące produktem drzew BST -
% używane do budowania drzewa stanów końcowych przy konstrukcji
% automatu produktowego

bstProdTwo(_, nil, _).
bstProdTwo(V, n(LEFT, W, RIGHT), TREEPROD) :-
   insertToBST(TREEPROD, pstate(V, W)),
   bstProdTwo(V, LEFT, TREEPROD),
   bstProdTwo(V, RIGHT, TREEPROD).

bstProdOne(nil, _, _).
bstProdOne(n(LEFT, V, RIGHT), T2, TREEPROD) :-
   bstProdTwo(V, T2, TREEPROD),
   bstProdTwo(V, T2, TREEPROD),
   bstProdOne(LEFT, T2, TREEPROD),
   bstProdOne(RIGHT, T2, TREEPROD).

bstProd(nil, _, nil) :- !.
bstProd(_, nil, nil) :- !.
bstProd(T1, T2, TREEPROD) :-
   bstProdOne(T1, T2, TREEPROD),
   !,
   closeBST(TREEPROD).

% predykaty budujące drzewo stanów dla automatu produktowego

buildProdTreeFour([S|SS], (S1, T1), (S2, T2), TREEPROD) :-
   findInEnrichedBST(T1, S, TARGET1),
   findInEnrichedBST(T2, S, TARGET2),
   findInEnrichedBST(TREEPROD, pstate(S1, S2), STREE),
   insertToEnrichedBST((S, pstate(TARGET1, TARGET2)), STREE),
   buildProdTreeFour(SS, (S1, T1), (S2, T2), TREEPROD).
buildProdTreeFour([], _, _, _) :- !.

buildProdTreeThree((S1, T1), (S2, T2), AL, TREEPROD) :-
   insertToEnrichedBST((pstate(S1, S2), _), TREEPROD),
   buildProdTreeFour(AL, (S1, T1), (S2, T2), TREEPROD).

buildProdTreeTwo((ST1, TREE1), n(LEFT, (ST2, TREE2), RIGHT), AL, TREEPROD) :-
   buildProdTreeThree((ST1, TREE1), (ST2, TREE2), AL, TREEPROD),
   buildProdTreeTwo((ST1, TREE1), LEFT, AL, TREEPROD),
   buildProdTreeTwo((ST1, TREE1), RIGHT, AL, TREEPROD).
buildProdTreeTwo(_, nil, _, _).

buildProdTree(n(LEFT, (STATE, TREE), RIGHT), ST2, AL, TREEPROD) :-
   buildProdTreeTwo((STATE, TREE), ST2, AL, TREEPROD),
   buildProdTree(LEFT, ST2, AL, TREEPROD),
   buildProdTree(RIGHT, ST2, AL, TREEPROD).
buildProdTree(nil, _, _, _).

statesTreeProd(ST1, ST2, AL, STPROD) :-
   buildProdTree(ST1, ST2, AL, STPROD),
   closeEnrichedBST(STPROD).
