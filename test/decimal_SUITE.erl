%%%-------------------------------------------------------------------
%%% Copyright   : (C) 2003-2008 Rudolph van Graan
%%% File        : decimal_SUITE.erl
%%% Author      : Rudolph van Graan <>
%%% Description : 
%%%
%%% Created     : 12 Oct 2008 by Rudolph van Graan <>
%%%-------------------------------------------------------------------
-module(decimal_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("test_server/include/test_server.hrl").
-include("../include/decimal.hrl").

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

all(doc) -> 
  [""];

all(suite) -> 
  [float_conversion,
   int_conversion,
   minor_int_conversion,
   adjust_precision,
   multiply,
   divide,
   add,
   format,
   round,
   shr,
   abs].


%%--------------------------------------------------------------------
float_conversion(doc) -> 
  [""];

float_conversion(suite) -> 
  [];

float_conversion(Config) when is_list(Config) -> 
  {fp,4,13,1234} = fixedpoint:from_float(13.1234,4),
  {fp,4,-13,-1234} = fixedpoint:from_float(-13.1234,4),
  ok.

minor_int_conversion(doc) ->
  [];
minor_int_conversion(suite) ->
  [];
minor_int_conversion(Config) when is_list(Config) ->
  {fp,4,0,13}   = fixedpoint:from_minor_int(13,4),
  {fp,4,0,123}  = fixedpoint:from_minor_int(123,4),
  {fp,4,1,2345} = fixedpoint:from_minor_int(12345,4),
  {fp,2,0,13}   = fixedpoint:from_minor_int(13,2),
  {fp,2,0,-13}  = fixedpoint:from_minor_int(-13,2),
  {fp,2,-1,-13} = fixedpoint:from_minor_int(-113,2).
  
int_conversion(doc) ->
  [];
int_conversion(suite) ->
  [];
int_conversion(Config) when is_list(Config) ->
  {fp,0,13,0}   = fixedpoint:from_int(13),
  {fp,0,-13,0}   = fixedpoint:from_int(-13).

adjust_precision(doc) ->
  [];
adjust_precision(suite) ->
  [];
adjust_precision(_Config) ->
  ?line {fp,2,6,58}   = fixedpoint:adjust({fp,4,6,5840},2),
  ?line {fp,4,6,5800} = fixedpoint:adjust({fp,2,6,58},4),
  ?line {fp,0,7,0}    = fixedpoint:adjust({fp,2,6,58},0),
  ?line {fp,4,6,5840} = fixedpoint:adjust({fp,4,6,5840},4).


multiply(doc) ->
  [];
multiply(suite) ->
  [];
multiply(_Config) ->
  {fp,4,6,1725} = fixedpoint:mult({fp,4,1,2345},5),
  {fp,4,6,5840} = fixedpoint:mult({fp,4,1,2345},5.333333),
  
  %%Inverted params
  {fp,4,6,1725} = fixedpoint:mult(5,{fp,4,1,2345}),
  {fp,4,6,5840} = fixedpoint:mult(5.333333,{fp,4,1,2345}),
  
  {fp,0,50,0}   = fixedpoint:mult({fp,0,10,0}, {fp,0,5,0}),
  {fp,0,50,0}   = fixedpoint:mult({fp,0,5,0},  {fp,0,10,0}),
  
  {fp,2,51,30}  = fixedpoint:mult({fp,2,10,26},{fp,0,5,0}),
  {fp,2,51,30}  = fixedpoint:mult({fp,0,5,0},  {fp,2,10,26}),
  
  {fp,3,51,300} = fixedpoint:mult({fp,2,10,26},{fp,3,5,0}),
  {fp,3,51,300} = fixedpoint:mult({fp,3,5,0},  {fp,2,10,26}).
  
divide(doc)  ->
  [];
divide(suite) ->
  [];
divide(_Config) ->
  {fp,2,0,25} = fixedpoint:divide({fp,2,1,23},5),
  {fp,2,1,00} = fixedpoint:divide({fp,2,10,00},{fp,2,10,00}),
  {fp,2,4,07} = fixedpoint:divide({fp,0,5,0},{fp,2,1,23}),
  {fp,0,5,00} = fixedpoint:divide({fp,0,5,0},1.0),
  {fp,2,3,68} = fixedpoint:divide({fp,2,4,20},1.14).

add(doc) ->  
  [];
add(suite) ->
  [];
add(_Config) ->
  ?line {fp,0,15,0}   = fixedpoint:add({fp,0,10,0},{fp,0,5,0}),
  ?line {fp,2,2,23}   = fixedpoint:add({fp,2,0,23},2),
  ?line {fp,4,2,3300} = fixedpoint:add({fp,2,0,23},2.1),
  ?line {fp,4,2,3411} = fixedpoint:add({fp,2,0,23},2.111111).

round(doc) ->
  [];
round(suite) ->
  [];
round(_Config) ->
  ?line {fp,0,51,0} = fixedpoint:round({fp,3,51,300}),
  ?line {fp,0,52,0} = fixedpoint:round({fp,3,51,665}).

format(doc) ->
  [];
format(suite) ->
  [];
format(_Config) ->
  ?line "51.345"  = fixedpoint:format({fp,3,51,345}),
  ?line "51.003"  = fixedpoint:format({fp,3,51,3}),
  ?line "-51.003" = fixedpoint:format({fp,3,-51,-3}),
  ?line "-0.003"  = fixedpoint:format({fp,3,0,-3}),
  ?line "51"      = fixedpoint:format({fp,0,51,0}).

shr(doc) ->
  [];
shr(suite) ->
  [];
shr(_Config) ->
  ?line {fp,4,0,1400} = fixedpoint:shr({fp,4,14,0},2),
  ?line {fp,4,14,0}   = fixedpoint:shr({fp,4,14,0},0),
  ?line {fp,4,0,1414} = fixedpoint:shr({fp,4,14,1400},2).

abs(doc) ->
  [];
abs(suite) ->
  [];
abs(_Config) ->
  ?line {fp,4,14,00}     = fixedpoint:abs({fp,4,14,0}),
  ?line {fp,4,14,1400} = fixedpoint:abs({fp,4,-14,-1400}).
  
