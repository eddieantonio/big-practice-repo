Gee Whiz
========

This filter renders graphviz documents, like so:


~~~ dot
digraph {
    h -> he  [label="Who Am the Only One"]
}
~~~

This is an exercise proposed in the [Pandoc Scripting
Tutorial](http://pandoc.org/scripting.html).

Da Code
=======

This is a simple JSON tree-based filter. Basically, every time we see a code
block, replace its contents with an block containing the rendered image.

> import Text.Pandoc.JSON
> import System.Process (readProcess)
> import Data.Hashable (hash)

> geewhiz :: Block -> IO Block
> geewhiz (CodeBlock attr text)
>   | isDot attr = do
>       name <- renderDot text
>       return $ image "graph" name
> geewhiz x = return x

We determine a codeblock is in the DOT language when it is declared as such;
its only classname should be `"dot"`:

> isDot :: Attr -> Bool
> isDot (_, ["dot"], _) = True
> isDot _ = False

`readProcess`, runs `dot`, passing the graph through standard input.  I tried
to do nifty things with the process's standard output, but without a way of
opening it in binary mode, things go awry. Instead, I opted for save the file
with a name based on the hash of the graph. Consequently, the return
(burritoed up in an IO monad) of this function is the saved filename.

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
