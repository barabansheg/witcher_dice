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

-module(logic).
-export([turn/0, turn/2, get_result/1]).

-record(state, {d1, d2, d3, d4, d5}).



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
    FuncsArray = [fun check_poker/1, fun check_foak/1, fun check_fullhouse/1, fun check_big_straight/1, 
		  fun check_small_straight/1, fun check_set/1,  fun check_two_pairs/1, fun check_pair/1 ],
    List = lists:sort([State#state.d1, State#state.d2, State#state.d3, State#state.d4, State#state.d5]),
    get_result(List, FuncsArray).

get_result(List, [])->
    {0, false};

get_result(List, [F|Tail]) ->
    Res = F(List),
    if Res == false ->
	    get_result(List, Tail);
    true ->
	    Res
    end.

   

		 
%compare_results(R1 = #result{}, R2 = #result{})

% {weight, name, max_value, min_value(optional), [values not in combinations]}
    
check_pair([X, X, V1, V2, V3]) ->
    {1, pair, X*2, [V1, V2, V3]};

check_pair([V1, X, X, V2, V3]) ->
    {1, pair, X*2, [V1, V2, V3]};

check_pair([V1, V2, X, X, V3]) ->
    {1, pair, X*2, [V1, V2, V3]};

check_pair([V1,V2, V3, X, X]) ->
    {1, pair, X*2, [V1, V2, V3]};

check_pair(_) ->
    false.

check_two_pairs([X, X, Y, Y, V1]) ->
    {2, two_pairs, max(X*2, Y*2), min(X*2, Y*2), [V1]};

check_two_pairs([X, X, V1, Y, Y]) ->
    {2, two_pairs, max(X*2, Y*2), min(X*2, Y*2), [V1]};

check_two_pairs([V1, X, X, Y, Y]) ->
    {2, two_pairs, max(X*2, Y*2), min(X*2, Y*2), [V1]};

check_two_pairs(_) ->
    false.

check_set([X, X, X, V1, V2]) ->
    {3, set, true, X*3, [V1, V2]};

check_set([V1, X, X, X, V2]) ->
    {3, set, true, X*3, [V1, V2]};

check_set([V1, V2, X, X, X]) ->
    {3, set, true, X*3, [V1, V2]};
check_set(_) ->
    false.

check_small_straight([1, 2, 3, 4, 5]) ->
    {4, small_straight};
check_small_straight(_) ->
    false.

check_big_straight([2, 3, 4, 5, 6]) ->
    {5, big_straight};
check_big_straight(_) ->
    false.

check_fullhouse([X, X, Y, Y, Y]) ->
    {6, fullhouse, true, max(X*2, Y*3), min(X*2, Y*3)};
check_fullhouse([Y, Y, Y, X, X]) ->
    {6, fullhouse, true, max(X*2, Y*3), min(X*2, Y*3)};
check_fullhouse(_) ->
    false.
    
check_foak([X, X, X, X, V1]) ->
    {7, foak, true, X*4, [V1]};

check_foak([V1, X, X, X, X]) ->
    {7, foak, true, X*4, [V1]};
check_foak(_) ->
    false.

check_poker([X, X, X, X, X]) ->
    {8, poker, (X*5)};
check_poker(_) ->
    false.
