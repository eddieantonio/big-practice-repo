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

-module(utf8_tests).
-include_lib("eunit/include/eunit.hrl").

encode_ascii_test() ->
    % Newline
    ?assertEqual(<<"\n">>, utf8:encode([16#0A])),
    % Control character
    ?assertEqual(<<16#07>>, utf8:encode([16#07])),
    % Space
    ?assertEqual(<<" ">>, utf8:encode([16#20])),
    % Digit
    ?assertEqual(<<"1">>, utf8:encode([$1])),
    % Uppercase
    ?assertEqual(<<"A">>, utf8:encode([$A])),
    % Lowercase
    ?assertEqual(<<"a">>, utf8:encode([$a])).

encode_latin1_test() ->
    % ß U+00DF LATIN SMALL LETTER SHARP S
    Eszet = 16#00DF,
    ?assertEqual(16, bit_size(utf8:encode([Eszet]))),
    ?assertMatch(<<
                   2#110:3, _:5,  % Byte 0: Should have leader.
                   2#10:2,  _:6   % Byte 1: Should have continuation
                 >>,
                 utf8:encode([Eszet])),
    ?assertEqual(<<16#C3, 16#9F>>, utf8:encode([Eszet])).

encode_devanagari_test() ->
    % अ U+0905 DEVANAGARI LETTER A
    AbugidaA = 16#0905,
    ?assertEqual(24, bit_size(utf8:encode([AbugidaA]))),
    ?assertMatch(<<
                   2#1110:4, _:4,  % Byte 0: Should have leader.
                   2#10:2,   _:6,  % Byte 1: Should have continuation
                   2#10:2,   _:6   % Byte 2: Should have continuation
                 >>,
                 utf8:encode([AbugidaA])),
    ?assertEqual(<<16#E0, 16#A4, 16#85>>, utf8:encode([AbugidaA])).

encode_emoji_test() ->
    % 💩  U+01F4A9 PILE OF POO
    Poop = 16#01F4A9,
    ?assertEqual(32, bit_size(utf8:encode([Poop]))),
    ?assertMatch(<<
                   2#11110:5, _:3,  % Byte 0: Should have leader.
                   2#10:2,    _:6,  % Byte 1: Should have continuation
                   2#10:2,    _:6,  % Byte 2: Should have continuation
                   2#10:2,    _:6   % Byte 3: Should have continuation
                 >>,
                 utf8:encode([Poop])),
    ?assertEqual(<<16#F0, 16#9F, 16#92, 16#A9>>, utf8:encode([Poop])).

encode_ascii_string_test() ->
    ?assertEqual(<<"Hello, World!">>, utf8:encode("Hello, World!")).

encode_unicode_string_test() ->
    % 😄  U+1F604 SMILING FACE WITH OPEN MOUTH AND SMILING EYES
    Emoji = 16#01F604,
    ?assertEqual(<<
                   "Cze",
                   16#C5, 16#9B,
                   16#C4, 16#87,
                   "! ",
                   16#F0, 16#9F, 16#98, 16#84
                 >>,
                utf8:encode("Cześć! " ++ [Emoji])).

error_on_encode_surrogate_code_points_test() ->
    ?assertThrow(surrogate_value_disallowed, utf8:encode([16#D87f])).

%%%

decode_ascii_test() ->
    ?assertEqual("Hello, world!", utf8:decode(<<"Hello, world!">>)).

decode_latin1_test() ->
    % "Cześć"
    Greeting = <<"Cze", 16#C5, 16#9B, 16#C4, 16#87>>,
    ?assertEqual("Cze" ++ [$ś, $ć], utf8:decode(Greeting)).

decode_hangul_test() ->
    % "한국어" -- the Korean language
    Greeting = <<237,149,156,234,181,173,236,150,180>>,
    ?assertEqual([$한, $국, $어], utf8:decode(Greeting)).

decode_emoji_test() ->
    % "🆒🔞"
    CoolNoUnder18 = <<240,159,134,146, 240,159,148,158>>,
    ?assertEqual([$🆒, $🔞], utf8:decode(CoolNoUnder18)).

decode_rejects_overlong_form_test() ->
    <<Zero:8>> = <<2#10:2, 0:6>>,
    TwoBytes = <<"hello", 2#110:3, 0:5, Zero>>,
    ?assertThrow(overlong_form, utf8:decode(TwoBytes)),
    ThreeBytes = <<"hello", 2#1110:4, 0:4, Zero, Zero>>,
    ?assertThrow(overlong_form, utf8:decode(ThreeBytes)),
    FourBytes = <<"hello", 2#11110:5, 0:3, Zero, Zero, Zero>>,
    ?assertThrow(overlong_form, utf8:decode(FourBytes)).

c(X) when 0 =< X, X =< 2#111111 ->
    <<2#10:2, X:6>>.

decode_rejects_non_unicode_test() ->
    <<First:3, Second:6, Third:6, Fourth:6>> = <<16#11FFFF:21/big>>,
    Invalid = <<2#11110:5, First:3,
                (c(Second))/binary,
                (c(Third))/binary,
                (c(Fourth))/binary>>,
    ?assertEqual(32, bit_size(Invalid)),
    ?assertThrow(invalid_code_point, utf8:decode(Invalid)).

decode_rejects_invalid_utf8_test() ->
    OneByte = <<1:1, 0:7>>,
    TwoByte = <<2#110:3, 1:5, $A:8>>,
    ThreeByte = <<2#1110:4, 1:4, (c(0))/binary, $A:8>>,
    FourByte = <<2#11110:5, 1:3, (c(0))/binary, (c(0))/binary, $A:8>>,
    ?assertThrow(malformed_utf8, utf8:decode(OneByte)),
    ?assertThrow(malformed_utf8, utf8:decode(TwoByte)),
    ?assertThrow(malformed_utf8, utf8:decode(ThreeByte)),
    ?assertThrow(malformed_utf8, utf8:decode(FourByte)).
