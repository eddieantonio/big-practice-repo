Gee Whiz
========

This filter renders graphviz documents.

[example](http://pandoc.org/scripting.html).

~~~ dot
digraph {
    h -> he  [title="Who Am the Only One"]
}
~~~

---

> import Text.Pandoc.JSON
> import System.Process (readProcess)
> import Data.Hashable (hash)

> geewhiz :: Block -> IO Block
> geewhiz (CodeBlock attr text)
>   | isDot attr = do
>       name <- renderDot text
>       return $ image "graph" name
> geewhiz x = return x

It's dot when the only class is `dot`.

> isDot :: Attr -> Bool
> isDot (_, ["dot"], _) = True
> isDot _ = False

To run a process, `readProcess`, passing the graph. Its return will be the
filename.

> renderDot :: String -> IO String
> renderDot graph = let
>     name = (++".png") $ show $ hash graph
>   in readProcess "dot" ["-Tpng", "-o" ++ name] graph >> return name

Create an inline image. This will work both in HTML, LaTeX, and (probably)
more!

> image :: String -> String -> Block
> image alt link = Plain [Image [Str alt] (link, alt)]

> main :: IO ()
> main = toJSONFilter geewhiz
