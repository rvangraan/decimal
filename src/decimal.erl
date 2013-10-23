%%% File        : decimal.erl

%%% Copyright   : Rudolph van Graan
%%% Author      : Rudolph van Graan <>
%%% Description : 
%%% Created     : 12 Oct 2008 by Rudolph van Graan <>

-module(decimal).


-include("../include/decimal.hrl").
-compile({no_auto_import,[max/2]}).
-export([from_float/2,
	 from_int/1,
	 from_minor_int/2,
	 to_minor_int/1,
	 to_fixed/1,
	 adjust/2,
	 pow/2,
	 add/2,
	 sub/2,
	 shr/2,
	 round/1,
	 multiply/2,
	 abs/1,
	 divide/2,
	 format/1,
	 format/2]).
%%==========================================================================
-type decimal() :: #decimal{}.
-type scale()   :: pos_integer().
-type num()  :: decimal() | float() | integer().
%%==========================================================================
-spec from_float(float(), scale()) -> decimal().
from_float(Float,Scale) when is_float(Float) ->
  Shift = pow(10,Scale),
  BigInt = erlang:round(Float * Shift),
  #decimal{scale = Scale, value = (BigInt div Shift), fraction = (BigInt rem Shift)}.
%%==========================================================================
-spec from_int(integer()) -> decimal().
from_int(Int) when is_integer(Int) ->
    #decimal{scale  = 0,
	     value = Int}.

%%==========================================================================
from_minor_int(MinorInt, Scale) when is_integer(MinorInt),
				     is_integer(Scale),
				     Scale >= 0 ->
    Shift = pow(10,Scale),
    #decimal{scale = Scale, value = (MinorInt div Shift), fraction = (MinorInt rem Shift)}.
%%==========================================================================
to_minor_int(Fp) when is_record(Fp,decimal) ->
    Shift = pow(10,Fp#decimal.scale),
    Fp#decimal.value * Shift + Fp#decimal.fraction.

%%==========================================================================
to_fixed(Int)   when is_integer(Int)          -> from_int(Int);
to_fixed(Float) when is_float(Float)          -> from_float(Float,4);
to_fixed(FP)    when is_record(FP,decimal) -> FP.

%%==========================================================================
adjust(FP,D) when is_record(FP,decimal), 
                  is_integer(D),
                  D >= 0 ->
    D1  = FP#decimal.scale,
    SH  = D - D1,
    Shift = pow(10,erlang:abs(SH)),
    MI1 = to_minor_int(FP),
    case SH >= 0 of
	true  -> from_minor_int(MI1 * Shift,D);
	false -> from_minor_int(erlang:round(MI1 / Shift),D)
    end.

%%==========================================================================
-spec multiply(num(),num()) -> decimal().
multiply(Fp,Float) when is_record(Fp,decimal),
			is_float(Float) ->
    Scale = Fp#decimal.scale,
    MinorInt = to_minor_int(Fp),
    Value    = erlang:round(MinorInt * Float),
    from_minor_int(Value,Scale);
multiply(Float,Decimal) when is_record(Decimal,decimal),
			     is_float(Float) ->
    multiply(Decimal,Float);

multiply(Fp,Int) when is_record(Fp,decimal),
		      is_integer(Int) ->
    Scale = Fp#decimal.scale,
    MinorInt = to_minor_int(Fp),
    Value    = MinorInt * Int,
    from_minor_int(Value,Scale);
multiply(Int,Decimal) when is_record(Decimal,decimal),
			   is_integer(Int) ->
    multiply(Decimal,Int);

multiply(FP1,FP2) when is_record(FP1,decimal),
		       is_record(FP2,decimal) ->
    D1  = FP1#decimal.scale,
    D2  = FP2#decimal.scale,
    D   = max(D1,D2),
    F1I = to_minor_int(adjust(FP1,D)),
    F2I = to_minor_int(adjust(FP2,D)),
    V1  = from_minor_int(F1I*F2I,D*2),
    adjust(V1,D).

%%==========================================================================
-spec divide(num(),num()) -> decimal().
divide(FP1,FP2)  when is_record(FP1,decimal),
		      is_record(FP2,decimal) ->
  D1  = FP1#decimal.scale,
  D2  = FP2#decimal.scale,
  D   = max(D1,D2),
  F1I = to_minor_int(adjust(FP1,D)),
  F2I = to_minor_int(adjust(FP2,D)),
  
  V1 = F1I / F2I,
  Shift = pow(10,D),
  V2 = erlang:round(V1 * Shift),
  from_minor_int(V2,D);

divide(Fp,Int)  when is_record(Fp,decimal),
		     is_integer(Int) ->
    Scale = Fp#decimal.scale,
    MinorInt = to_minor_int(Fp),
    Value    = erlang:round(MinorInt / Int),
    from_minor_int(Value,Scale);

divide(Fp,Float)  when is_record(Fp,decimal),
		       is_float(Float) ->
    Scale = Fp#decimal.scale,
    MinorInt = to_minor_int(Fp),
    Value    = erlang:round(MinorInt / Float),
    from_minor_int(Value,Scale).


-spec add(num(),num()) -> decimal().
add(FP1,FP2) when is_record(FP1,decimal),
		  is_record(FP2,decimal) ->
    D1  = FP1#decimal.scale,
    D2  = FP2#decimal.scale,
    D   = max(D1,D2),
    F1I = to_minor_int(adjust(FP1,D)),
    F2I = to_minor_int(adjust(FP2,D)),
    from_minor_int(F1I+F2I,D);
add(V1,V2) ->
    add(to_fixed(V1),to_fixed(V2)).

%%==========================================================================
-spec sub(num(),num()) -> decimal().
sub(FP1,FP2) when is_record(FP1,decimal),
		  is_record(FP2,decimal) ->
    D1  = FP1#decimal.scale,
    D2  = FP2#decimal.scale,
    D   = max(D1,D2),
    F1I = to_minor_int(adjust(FP1,D)),
    F2I = to_minor_int(adjust(FP2,D)),
    from_minor_int(F1I-F2I,D);
sub(V1,V2) ->
    sub(to_fixed(V1),to_fixed(V2)).

%%==========================================================================
round(FP) when is_record(FP,decimal) ->
    adjust(FP,0).

%%==========================================================================
shr(FP,D) when is_record(FP,decimal),
	       is_integer(D),
	       D >= 0 ->
    F1I = to_minor_int(FP) div pow(10,D),
    from_minor_int(F1I,FP#decimal.scale).

%%==========================================================================
abs(FP) when is_record(FP,decimal) ->
    FP#decimal{value = erlang:abs(FP#decimal.value),
	       fraction  = erlang:abs(FP#decimal.fraction)}.


%%==========================================================================
format(FP) when is_record(FP,decimal),
                FP#decimal.scale > 0,
                FP#decimal.value >= 0,
                FP#decimal.fraction  >= 0 ->
    lists:flatten(io_lib:format("~p.~*..0w",[FP#decimal.value,FP#decimal.scale,FP#decimal.fraction]));
format(FP) when is_record(FP,decimal),
                FP#decimal.scale > 0,
                FP#decimal.value =< 0,
                FP#decimal.fraction  =< 0 ->
    lists:flatten(io_lib:format("-~p.~*..0w",[erlang:abs(FP#decimal.value),FP#decimal.scale,erlang:abs(FP#decimal.fraction)]));
format(FP) when is_record(FP,decimal),
                FP#decimal.scale =:= 0 ->
    lists:flatten(io_lib:format("~p",[FP#decimal.value])).
%%==========================================================================
format(FP,Dec) ->
    format(adjust(FP,Dec)).
%%==========================================================================
pow(_Base,0) ->  1;
pow(Base, N) when N > 0 ->
    Base * pow(Base,N-1).

%%==========================================================================
max(A,B) when A >= B -> A;
max(A,B) when A =< B -> B.
%%==========================================================================
