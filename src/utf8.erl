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
    lists:reverse(decode(Binary, [])).

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

decode(<<>>, ReversedCodePoints) ->
    ReversedCodePoints;
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
    case CodePoint < 16#010000 of
        true -> throw(overlong_form);
        _ -> decode(Rest, [CodePoint|List])
    end.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
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

-endif.
