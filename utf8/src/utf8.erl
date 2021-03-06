% Copyright (C) 2016 Eddie Antonio Santos <easantos@ualberta.ca>
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation, either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Affero General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module('utf8').

%% API exports
-export([encode/1, decode/1]).

%%====================================================================
%% API functions
%%====================================================================

-type code_point() :: 0..16#10FFFF.

-spec encode([code_point()]) -> binary().
encode(List) ->
    iolist_to_binary([encode_char(CP) || CP <- List]).

-spec decode(binary()) -> [code_point()].
decode(Binary) ->
    decode(Binary, []).

%%====================================================================
%% Internal functions
%%====================================================================

% Encode ASCII
encode_char(CodePoint) when 16#0000 =< CodePoint, CodePoint =< 16#007F ->
    <<CodePoint>>;
% Encode two byte forms.
encode_char(CodePoint) when 16#0080 =< CodePoint, CodePoint =< 16#07FF ->
    <<0:5, Upper:5, Lower:6>> = <<CodePoint:16/big>>,
    <<
      2#110:3, Upper:5,
      2#10:2,  Lower:6
    >>;
% Reject surrogate values.
encode_char(CodePoint) when 16#D800 =< CodePoint, CodePoint =< 16#DFFF ->
    throw(surrogate_value_disallowed);
% Encode three byte forms.
encode_char(CodePoint) when 16#0800 =< CodePoint, CodePoint =< 16#FFFF ->
    <<Upper:4, Middle:6, Lower:6>> = <<CodePoint:16/big>>,
    <<
      2#1110:4, Upper:4,
      2#10:2,   Middle:6,
      2#10:2,   Lower:6
    >>;
% Encode four byte forms.
encode_char(CodePoint) when 16#010000 =< CodePoint, CodePoint =< 16#10FFFF ->
    <<0:11, Upper:3, Two:6, Three:6, Four:6>> = <<CodePoint:32/big>>,
    <<
      2#11110:5, Upper:3,
      2#10:2,    Two:6,
      2#10:2,    Three:6,
      2#10:2,    Four:6
    >>.


% Decoding an empty sequence:
decode(<<>>, ReversedCodePoints) ->
    lists:reverse(ReversedCodePoints);
% Decode ASCII.
decode(<<0:1, ASCII:7, Rest/binary>>, List) ->
    decode(Rest, [ASCII|List]);
% Decode two byte form.
decode(<<2#110:3, Upper:5, 2#10:2, Lower:6, Rest/binary>>, List) ->
    <<CodePoint:11/big>> = <<Upper:5, Lower:6>>,
    if
        CodePoint < 16#0080 ->
            throw(overlong_form);
        true ->
            decode(Rest, [CodePoint|List])
    end;
% Decode three byte form.
decode(<<2#1110:4, Upper:4,
         2#10:2, Middle:6,
         2#10:2, Lower:6, Rest/binary>>, List) ->
    <<CodePoint:16/big>> = <<Upper:4, Middle:6, Lower:6>>,
    if
        CodePoint < 16#0800 ->
            throw(overlong_form);
        16#D800 =< CodePoint, CodePoint =< 16#DFFF ->
            throw(surrogate_value_disallowed);
        true ->
            decode(Rest, [CodePoint|List])
    end;
% Decode four byte form.
decode(<<2#11110:5, First:3,
         2#10:2,    Second:6,
         2#10:2,    Third:6,
         2#10:2,    Fourth:6, Rest/binary>>, List) ->
    <<CodePoint:21/big>> = <<First:3, Second:6, Third:6, Fourth:6>>,
    if
        CodePoint < 16#010000 -> throw(overlong_form);
        CodePoint > 16#10FFFF -> throw(invalid_code_point);
        true -> decode(Rest, [CodePoint|List])
    end;
% Any other bit pattern not covered is malformed.
decode(_, _) ->
    throw(malformed_utf8).
