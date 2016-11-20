# elm-yinsh

The [Yinsh][yinsh] board game, written in [Elm][elm-lang]

## developing

```sh
npm i -g elm@0.18.0
elm make src/Main.elm
open index.html
```

For live-reloading, you can use `elm-live`:

```sh
npm i -g elm-live@2.6.0
elm live src/Main.elm
open http://localhost:800
```

[yinsh]: http://www.gipf.com/yinsh/
[rules]: https://web.archive.org/web/20161118010122/http://www.gipf.com/yinsh/index.html
[elm-lang]: http://elm-lang.org/ 
