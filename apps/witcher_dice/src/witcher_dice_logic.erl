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

-module(witcher_dice_logic).
-export([throw/0, rethrow/2, get_result/1, determine_winner/2]).

-record(throw_result, {dice1, dice2, dice3, dice4, dice5}).
-record(throw_result_info, {weight, name, max_sum, min_sum, not_in_combination}).

throw() ->
    #throw_result{dice1=crypto:rand_uniform(1, 6),
	   dice2=crypto:rand_uniform(1, 6),
	   dice3=crypto:rand_uniform(1, 6),
	   dice4=crypto:rand_uniform(1, 6),
	   dice5=crypto:rand_uniform(1, 6)}.

% First argument is list of numbers of player's dices on table
rethrow([H|List], Throw_Result = #throw_result{}) ->
    rethrow(List, change(H, Throw_Result));

rethrow([], Throw_Result = #throw_result{}) ->
    Throw_Result.

change(dice1, Throw_Result = #throw_result{}) ->
    Throw_Result#throw_result{dice1 = crypto:rand_uniform(1, 6)};
change(dice2, Throw_Result = #throw_result{}) ->
    Throw_Result#throw_result{dice2 = crypto:rand_uniform(1, 6)};
change(dice3, Throw_Result = #throw_result{}) ->
    Throw_Result#throw_result{dice3 = crypto:rand_uniform(1, 6)};
change(dice4, Throw_Result = #throw_result{}) ->
    Throw_Result#throw_result{dice4 = crypto:rand_uniform(1, 6)};
change(dice5, Throw_Result = #throw_result{}) ->
    Throw_Result#throw_result{dice5 = crypto:rand_uniform(1, 6)}.

get_result(Throw_Result = #throw_result{}) ->
    FuncsArray = [fun check_poker/1, fun check_foak/1, fun check_fullhouse/1, fun check_big_straight/1,
		  fun check_small_straight/1, fun check_set/1,  fun check_two_pairs/1, fun check_pair/1 ],
    List = lists:sort([Throw_Result#throw_result.dice1, Throw_Result#throw_result.dice2, Throw_Result#throw_result.dice3, Throw_Result#throw_result.dice4, Throw_Result#throw_result.dice5]),
    get_result(List, FuncsArray).

get_result(List, [])->
    #throw_result_info{weight = 0};

get_result(List, [F|Tail]) ->
    Res = F(List),

    case Res#throw_result_info.weight of
	      0 -> get_result(List, Tail);
	      _ -> Res
    end.

% Ничья
determine_winner(Info1 = #throw_result_info{weight = 0}, Info2 = #throw_result_info{weight = 0}) ->
    draw;

determine_winner(Info1 = #throw_result_info{name = big_straight}, Info2 = #throw_result_info{name = big_straight}) ->
    draw;

determine_winner(Info1 = #throw_result_info{name = small_straight}, Info2 = #throw_result_info{name = small_straight}) ->
    draw;

% сравниваем по весу
determine_winner(Info1 = #throw_result_info{}, Info2 = #throw_result_info{})
	when Info1#throw_result_info.weight > Info2#throw_result_info.weight ->
    win1;

determine_winner(Info1 = #throw_result_info{}, Info2 = #throw_result_info{})
  	when Info1#throw_result_info.weight < Info2#throw_result_info.weight ->
  	win2;

determine_winner(Info1 = #throw_result_info{}, Info2 = #throw_result_info{})
  	when Info1#throw_result_info.weight == Info2#throw_result_info.weight ->
  	determine_winner(Info1, Info2);

determine_winner(Info1 = #throw_result_info{max_sum = X}, Info2 = #throw_result_info{max_sum = Y}) when X > Y ->
    win1;

determine_winner(Info1 = #throw_result_info{max_sum = X}, Info2 = #throw_result_info{max_sum = Y}) when X < Y ->
    win2;

determine_winner(Info1 = #throw_result_info{max_sum = X}, Info2 = #throw_result_info{max_sum = Y}) when X == Y ->
    determine_winner(Info1, Info2),

determine_winner(Info1 = #throw_result_info{min_sum = X}, Info2 = #throw_result_info{min_sum = Y}) when X > Y ->
    win1;

determine_winner(#throw_result_info{min_sum = X}, #throw_result_info{min_sum = Y}) when X < Y ->
    win2;

determine_winner(Info1 = #throw_result_info{min_sum = X}, Info2 = #throw_result_info{min_sum = Y}) when X == Y ->
    determine_winner(Info1, Info2);

determine_winner(#throw_result_info{not_in_combination = X}, #throw_result_info{not_in_combination = Y}) when X > Y ->
    win1;

determine_winner(#throw_result_info{not_in_combination = X}, #throw_result_info{not_in_combination = Y}) when X < Y ->
    win2;

determine_winner(Info1 = #throw_result_info{not_in_combination = X}, Info2 = #throw_result_info{not_in_combination = Y}) when X == Y ->
    draw.

check_pair([X, X, V1, V2, V3]) ->
    #throw_result_info{
	    weight = 1,
	    name = pair,
	    max_sum = X*2,
	    not_in_combination = V1 + V2 + V3
	   };

check_pair([V1, X, X, V2, V3]) ->
    #throw_result_info{
	    weight = 1,
	    name = pair,
	    max_sum = X*2,
	    not_in_combination = V1 + V2 + V3
	   };

check_pair([V1, V2, X, X, V3]) ->
    #throw_result_info{
	    weight = 1,
	    name = pair,
	    max_sum = X*2,
	    not_in_combination = V1 + V2 + V3
	   };

check_pair([V1,V2, V3, X, X]) ->
    #throw_result_info{
	    weight = 1,
	    name = pair,
	    max_sum = X*2,
	    not_in_combination = V1 + V2 + V3
	   };

check_pair(_) ->
    #throw_result_info{weight = 0}.

check_two_pairs([X, X, Y, Y, V1]) ->
    #throw_result_info{
	    weight = 2,
        name = two_pairs,
   	 	max_sum = max(X*2, Y*2),
		min_sum = max(X*2, Y*2),
		not_in_combination = V1
	   };

check_two_pairs([X, X, V1, Y, Y]) ->
    #throw_result_info{
		 weight = 2,
		 name = two_pairs,
		 max_sum = max(X*2, Y*2),
		 min_sum = max(X*2, Y*2),
		 not_in_combination = V1
	   };

check_two_pairs([V1, X, X, Y, Y]) ->
    #throw_result_info{
		 weight = 2,
		 name = two_pairs,
		 max_sum = max(X*2, Y*2),
		 min_sum = max(X*2, Y*2),
		 not_in_combination = V1
		};

check_two_pairs(_) ->
    #throw_result_info{weight = 0}.

check_set([X, X, X, V1, V2]) ->
    #throw_result_info{
	   weight = 3,
	   name = set,
	   max_sum = X*3,
	   not_in_combination = V1+V2
	  };

check_set([V1, X, X, X, V2]) ->
    #throw_result_info{
	   weight = 3,
	   name = set,
	   max_sum = X*3,
	   not_in_combination = V1+V2
	  };

check_set([V1, V2, X, X, X]) ->
    #throw_result_info{
	   weight = 3,
	   name = set,
	   max_sum = X*3,
	   not_in_combination = V1+V2
	  };

check_set(_) ->
    #throw_result_info{weight = 0}.

check_small_straight([1, 2, 3, 4, 5]) ->
    #throw_result_info{
		      weight = 4,
		      name = small_straight
		     };

check_small_straight(_) ->
    #throw_result_info{weight = 0}.

check_big_straight([2, 3, 4, 5, 6]) ->
    #throw_result_info{
		    weight = 5,
		    name = big_straight
		   };

check_big_straight(_) ->
    #throw_result_info{weight = 0}.

check_fullhouse([X, X, Y, Y, Y]) ->
    #throw_result_info{
		 weight = 6,
		 name = fullhouse,
		 max_sum = max(X*2, Y*3),
		 min_sum = min(X*2, Y*3)
		};

check_fullhouse([Y, Y, Y, X, X]) ->
    #throw_result_info{
		 weight = 6,
		 name = fullhouse,
		 max_sum = max(X*2, Y*3),
		 min_sum = min(X*2, Y*3)
		};

check_fullhouse(_) ->
    #throw_result_info{weight = 0}.

check_foak([X, X, X, X, V1]) ->
    #throw_result_info{
	    weight = 7,
	    name = foak,
	    max_sum = X*4,
	    not_in_combination = V1
	   };

check_foak([V1, X, X, X, X]) ->
    #throw_result_info{
	    weight = 7,
	    name = foak,
	    max_sum = X*4,
	    not_in_combination = V1
	   };

check_foak(_) ->
    #throw_result_info{weight = 0}.

check_poker([X, X, X, X, X]) ->
    #throw_result_info{
	     weight = 8,
	     name = poker,
	     max_sum = X*5
	    };

check_poker(_) ->
    #throw_result_info{weight = 0}.
