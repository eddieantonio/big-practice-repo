Romulus
=======

This file changes all ordered lists into lists numbered by roman numerals.

50. Here we go.
50. This is another.

---

1999. Celebrate good times?
2000. Come 'on!

---

> import Text.Pandoc.JSON

> main :: IO ()
> main = toJSONFilter romulus

> romulus :: Block -> Block
> romulus (OrderedList (start, _, delim) blocks) = 
>          OrderedList (start, UpperRoman, delim) blocks
> romulus x = x
