Shout
=====

This makes all documents more exciting!

[Except for links](http://pandoc.org/scripting.html#removing-links) (links are
not exciting).

---

Note: We can't use the JSON thing because it doesn't allow us to control the
walk. Therefore,

> import Text.Pandoc
> import Text.Pandoc.Walk
> import Data.Char (toUpper)


We can control the walk

> shout :: Inline -> Inline
> shout link@(Link _  _) = link
> shout (Str text)       = Str (uppercase text)
> shout tree             = walk shout tree

> uppercase :: String -> String
> uppercase = map toUpper

---

Then there's some boring boilerplate stuff.

> main :: IO ()
> main = interact (writeDoc . walk shout . readDoc)

> readDoc = case readMarkdown def of
>                Right doc -> doc
>                Left err  -> error (show err)

> writeDoc :: Pandoc -> String
> writeDoc = writeMarkdown def
