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

> import Text.Pandoc
> import Text.Pandoc.Walk
> import Data.Hashable (hash)
> import System.Directory (removeFile)
> import System.Process (readProcess)

> runpy :: Block -> IO [String]
> runpy cb@(CodeBlock attr text)
>   | isPython attr = do
>       output <- runPython text
>       return [output]
> runpy x = return []

Python code has the class... `python`. Unsurprisingly.

> isPython :: Attr -> Bool
> isPython (_, ["python"], _) = True
> isPython _ = False

This creates a file with the hashed name.

> runPython :: String -> IO String
> runPython source = let
>     name = (++".py") $ show $ hash source
>   in do
>       writeFile name source
>       output <- readProcess "python" [name] []
>       removeFile name
>       return output

> readDoc :: String -> Pandoc
> readDoc s = case readMarkdown def s of
>                  Right doc -> doc
>                  Left err  -> error (show err)

> main :: IO ()
> main = do
>   markdown <- getContents
>   output <- query runpy (readDoc markdown)
>   putStr $ unlines output
