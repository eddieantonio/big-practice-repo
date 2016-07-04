Runpy
=====

This filter runs embedded Python!

```python
import random

print("Hello, {}!".format(random.choice([
    "world", "buttercups", "sweetcheeks", "mellonballer",
    "deez nutz", "Martha", "hella tight", "bananahead"
])))
```

This is an exercise proposed in the [Pandoc Scripting
Tutorial](http://pandoc.org/scripting.html).

Da Code
=======

A lot of imports, but nothing too intense. Mostly, we want to create files,
and write to `stderr`. The latter `System.IO`; the former `System.Director`.

> import Data.Hashable (hash)
> import System.IO
> import System.Directory (removeFile)
> import System.Process (readProcess)
> import Text.Pandoc.JSON

This is very simple. Basically, call `runPython` for its side-effect only,
doing absolutely nothing to the AST. We can't use `walk` because its
type-signature strangely forbids `IO a`.

> runpy :: Block -> IO Block
> runpy cb@(CodeBlock attr text)
>   | isPython attr = runPython text >> return cb
> runpy x = return x

Python code has the class... `python`. Unsurprisingly.

> isPython :: Attr -> Bool
> isPython (_, ["python"], _) = True
> isPython _ = False

This creates a file with the hashed name.

> runPython :: String -> IO ()
> runPython source = let
>     name = (++".py") $ show $ hash source
>   in do
>       writeFile name source
>       output <- readProcess "python" [name] []
>       hPutStr stderr output
>       removeFile name

> main :: IO ()
> main = toJSONFilter runpy
