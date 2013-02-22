%%% File        : decimal.erl

%%% Copyright   : Rudolph van Graan
%%% Author      : Rudolph van Graan <>
%%% Description : 
%%% Created     : 12 Oct 2008 by Rudolph van Graan <>

-module(decimal).


-include("../include/decimal.hrl").

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
	 mult/2,
	 abs/1,
	 divide/2,
	 format/1,
	 format/2]).

from_float(Float,Decimals) when is_float(Float) ->
  Shift = pow(10,Decimals),
  BigInt = erlang:round(Float * Shift),
  #fp{decimals = Decimals, magnitude = (BigInt div Shift), fraction = (BigInt rem Shift)}.

from_int(Int) ->
  #fp{decimals  = 0,
      magnitude = Int}.

from_minor_int(MinorInt, Decimals) when is_integer(MinorInt),
					is_integer(Decimals),
					Decimals >= 0 ->
  Shift = pow(10,Decimals),
  #fp{decimals = Decimals, magnitude = (MinorInt div Shift), fraction = (MinorInt rem Shift)}.

to_minor_int(Fp) when is_record(Fp,fp) ->
  Shift = pow(10,Fp#fp.decimals),
  Fp#fp.magnitude * Shift + Fp#fp.fraction.

to_fixed(Int)   when is_integer(Int)          -> from_int(Int);
to_fixed(Float) when is_float(Float)          -> from_float(Float,4);
to_fixed(FP)    when is_record(FP,fp) -> FP.

adjust(FP,D) when is_record(FP,fp), 
                  is_integer(D),
                  D >= 0 ->
  D1  = FP#fp.decimals,
  SH  = D - D1,
  Shift = pow(10,erlang:abs(SH)),
  MI1 = to_minor_int(FP),
  case SH >= 0 of
    true  -> from_minor_int(MI1 * Shift,D);
    false -> from_minor_int(erlang:round(MI1 / Shift),D)
  end.
  

mult(Fp,Float) when is_record(Fp,fp),
		    is_float(Float) ->
  Decimals = Fp#fp.decimals,
  MinorInt = to_minor_int(Fp),
  Value    = erlang:round(MinorInt * Float),
  from_minor_int(Value,Decimals);
mult(Float,Fp) when is_record(Fp,fp),
		    is_float(Float) ->
  mult(Fp,Float);

mult(Fp,Int) when is_record(Fp,fp),
		  is_integer(Int) ->
  Decimals = Fp#fp.decimals,
  MinorInt = to_minor_int(Fp),
  Value    = MinorInt * Int,
  from_minor_int(Value,Decimals);
mult(Int,Fp) when is_record(Fp,fp),
		  is_integer(Int) ->
  mult(Fp,Int);

mult(FP1,FP2) when is_record(FP1,fp),
		   is_record(FP2,fp) ->
  D1  = FP1#fp.decimals,
  D2  = FP2#fp.decimals,
  D   = max(D1,D2),
  F1I = to_minor_int(adjust(FP1,D)),
  F2I = to_minor_int(adjust(FP2,D)),
  V1  = from_minor_int(F1I*F2I,D*2),
  adjust(V1,D).



divide(FP1,FP2)  when is_record(FP1,fp),
		      is_record(FP2,fp) ->
  D1  = FP1#fp.decimals,
  D2  = FP2#fp.decimals,
  D   = max(D1,D2),
  F1I = to_minor_int(adjust(FP1,D)),
  F2I = to_minor_int(adjust(FP2,D)),
  
  V1 = F1I / F2I,
  Shift = pow(10,D),
  V2 = erlang:round(V1 * Shift),
  from_minor_int(V2,D);

divide(Fp,Int)  when is_record(Fp,fp),
		     is_integer(Int) ->
  Decimals = Fp#fp.decimals,
  MinorInt = to_minor_int(Fp),
  Value    = erlang:round(MinorInt / Int),
  from_minor_int(Value,Decimals);

divide(Fp,Float)  when is_record(Fp,fp),
		       is_float(Float) ->
  Decimals = Fp#fp.decimals,
  MinorInt = to_minor_int(Fp),
  Value    = erlang:round(MinorInt / Float),
  from_minor_int(Value,Decimals).



add(FP1,FP2) when is_record(FP1,fp),
		  is_record(FP2,fp) ->
  D1  = FP1#fp.decimals,
  D2  = FP2#fp.decimals,
  D   = max(D1,D2),
  F1I = to_minor_int(adjust(FP1,D)),
  F2I = to_minor_int(adjust(FP2,D)),
  from_minor_int(F1I+F2I,D);
add(V1,V2) ->
  add(to_fixed(V1),to_fixed(V2)).


sub(FP1,FP2) when is_record(FP1,fp),
		  is_record(FP2,fp) ->
  D1  = FP1#fp.decimals,
  D2  = FP2#fp.decimals,
  D   = max(D1,D2),
  F1I = to_minor_int(adjust(FP1,D)),
  F2I = to_minor_int(adjust(FP2,D)),
  from_minor_int(F1I-F2I,D);
sub(V1,V2) ->
  sub(to_fixed(V1),to_fixed(V2)).


round(FP) when is_record(FP,fp) ->
  adjust(FP,0).


shr(FP,D) when is_record(FP,fp),
	       is_integer(D),
	       D >= 0 ->
  F1I = to_minor_int(FP) div pow(10,D),
  from_minor_int(F1I,FP#fp.decimals).
  

abs(FP) when is_record(FP,fp) ->
  FP#fp{magnitude = erlang:abs(FP#fp.magnitude),
	fraction  = erlang:abs(FP#fp.fraction)}.
  
  

format(FP) when is_record(FP,fp),
                FP#fp.decimals > 0,
                FP#fp.magnitude >= 0,
                FP#fp.fraction  >= 0 ->
  lists:flatten(io_lib:format("~p.~*..0w",[FP#fp.magnitude,FP#fp.decimals,FP#fp.fraction]));
format(FP) when is_record(FP,fp),
                FP#fp.decimals > 0,
                FP#fp.magnitude =< 0,
                FP#fp.fraction  =< 0 ->
  lists:flatten(io_lib:format("-~p.~*..0w",[erlang:abs(FP#fp.magnitude),FP#fp.decimals,erlang:abs(FP#fp.fraction)]));
format(FP) when is_record(FP,fp),
                FP#fp.decimals =:= 0 ->
  lists:flatten(io_lib:format("~p",[FP#fp.magnitude])).

format(FP,Dec) ->
  format(adjust(FP,Dec)).

pow(_Base,0) ->  1;
pow(Base, N) when N > 0 ->
  Base * pow(Base,N-1).
max(A,B) when A >= B -> A;
max(A,B) when A =< B -> B.
   
