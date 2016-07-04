Unruly
======

This file removes `<hr>`s from documents. It is based on the ["link remover"
example](http://pandoc.org/scripting.html#removing-links).

---

> import Text.Pandoc.JSON

`main` simply converts unruly to a JSON filter.

> main :: IO ()
> main = toJSONFilter unruly

To remove horizontal rules, we must return a "monad" for blocks Essentially,
this just involves returning a list. A list is a monad, you guys! An empty
list removes the element and a list containing the element is essentially the
identity function (it doesn't change anything).

> unruly :: Block -> [Block]
> unruly HorizontalRule = []
> unruly x              = [x]
