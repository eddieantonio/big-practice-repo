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
> import Data.ByteString.Char8 (pack, unpack)
> import Data.ByteString.Base64 (encode)

> geewhiz :: Block -> IO Block
> geewhiz (CodeBlock attr text)
>   | isDot attr = do
>       dot text
>       return $ image "graph" "noname.gv.png"
> geewhiz x = return x

It's dot when the only class is `dot`.

> isDot :: Attr -> Bool
> isDot (_, ["dot"], _) = True
> isDot _ = False

To load a process, `readProcess`, passing the graph. Its return will be the
rendered graph as binary.

> dot :: String -> IO ()
> dot graph =
>   readProcess "dot" ["-O", "-Tpng"] graph >> return ()

Now we create a data url from this:

> dataurl :: String -> String -> String
> dataurl mediatype binary = "data:" ++ mediatype ++ ";base64," ++ media
>   where media = unpack $ encode $ pack binary

> image :: String -> String -> Block
> image alt link = Plain [Image [Str alt] (link, alt)]

> main :: IO ()
> main = toJSONFilter geewhiz
