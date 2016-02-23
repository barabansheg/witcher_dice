%  Комбинации по старшинству
% 
%  Пара — только на двух кубиках выпали грани одинакового значения
%  Две пары — на двух кубиках грани одного значения и ещё на двух — другого
%  Сет(Тройка) — на трёх кубиках грани одного значения
%  Стрейт — пять значений по порядку. То есть на всех пяти кубиках выпали пять разных граней со значениями либо от одного до пяти (малый стрейт), либо от двух до шести (большой стрейт)
%  Фулл-хаус — на трёх кубиках грани одного значения и на двух другого
%  Каре — одно значение выпало на четырёх кубиках
%  Покер — одно значение выпало на всех пяти кубиках
% 
%  Если у вас одинаковые комбинации, то выигрывает та, у которой значение выше. 
%  В случае со стрейтом большой бьёт малый, в случае с фулл-хаузом и двумя парами считается максимальное из двух значений (а если оно одинаковое, то тогда уже считается минимальное).
% 
%  Если у комбинаций совпадает и значение, то выигрывает тот, у кого выпало максимальное значение на кубиках, не входящих в комбинацию. 
%  И лишь если и это значение одинаковое, результат считается ничьей.

-module(dice).
-export([turn/0, turn/2, get_result/1]).

-record(state, {d1, d2, d3, d4, d5}).
-record(result, {pair, two_pairs, set, small_straight, big_straight, fullhouse, foak, poker}).


turn() ->
    #state{d1=rand:uniform(6),
	   d2=rand:uniform(6),
	   d3=rand:uniform(6),
	   d4=rand:uniform(6),
	   d5=rand:uniform(6)}.

turn([H|List], State = #state{}) ->
    turn(List, change(H, State));

turn([], State = #state{}) ->
    State.

change(d1,State = #state{}) ->
    State#state{d1=rand:uniform(6)};
change(d2,State = #state{}) ->
    State#state{d2=rand:uniform(6)};
change(d3,State = #state{}) ->
    State#state{d3=rand:uniform(6)};
change(d4,State = #state{}) ->
    State#state{d4=rand:uniform(6)};
change(d5,State = #state{}) ->
    State#state{d5=rand:uniform(6)}.

get_result(State = #state{}) ->
    List = lists:sort([State#state.d1, State#state.d2, State#state.d3, State#state.d4, State#state.d5]),
    #result{pair = check_pair(List), two_pairs = check_two_pairs(List), set = check_set(List),
	    small_straight = check_small_straight(List), big_straight = check_big_straight(List), 
	    fullhouse = check_fullhouse(List), foak = check_foak(List),  poker = check_poker(List)}.
		 
%compare_results(R1 = #result{}, R2 = #result{})

    
check_pair([X, X, _, _, _]) ->
    {true, X*2};
check_pair([_, X, X, _, _]) ->
    {true, X*2};
check_pair([_, _, X, X, _]) ->
    {true, X*2};
check_pair([_, _, _, X, X]) ->
    {true, X*2};
check_pair(_) ->
    false.

check_two_pairs([X, X, Y, Y, _]) ->
    {true, max(X*2, Y*2), min(X*2, Y*2)};
check_two_pairs([X, X, _, Y, Y]) ->
    {true, max(X*2, Y*2), min(X*2, Y*2)};
check_two_pairs([_, X, X, Y, Y]) ->
    {true, max(X*2, Y*2), min(X*2, Y*2)};
check_two_pairs(_) ->
    false.

check_set([X, X, X, _, _]) ->
    {true, X*3};
check_set([_, X, X, X, _]) ->
    {true, X*3};
check_set([_, _, X, X, X]) ->
    {true, X*3};
check_set(_) ->
    false.

check_small_straight([1, 2, 3, 4, 5]) ->
    true;
check_small_straight(_) ->
    false.

check_big_straight([2, 3, 4, 5, 6]) ->
    true;
check_big_straight(_) ->
    false.

check_fullhouse([X, X, Y, Y, Y]) ->
    {true, max(X*2, Y*3), min(X*2, Y*3)};
check_fullhouse([Y, Y, Y, X, X]) ->
    {true, max(X*2, Y*3), min(X*2, Y*3)};
check_fullhouse(_) ->
    false.
    
check_foak([X, X, X, X, _]) ->
    {true, X*4};
check_foak([_, X, X, X, X]) ->
    {true, X*4};
check_foak(_) ->
    false.

check_poker([X, X, X, X, X]) ->
    {true, (X*5)};
check_poker(_) ->
    false.
