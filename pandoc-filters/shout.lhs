Shout
=====

This makes all documents more exciting!

[Except for links urls and titles](http://pandoc.org/scripting.html#removing-links "This title is boring")
(links are not exciting).

> import Text.Pandoc.JSON
> import Data.Char (toUpper)

This one is trivial:

> main :: IO ()
> main = toJSONFilter shout
>   where shout :: Inline -> Inline
>         shout (Str text)       = Str (uppercase text)
>         shout tree             = tree

> uppercase :: String -> String
> uppercase = map toUpper
