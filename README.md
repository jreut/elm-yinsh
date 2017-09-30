# elm-yinsh

The [Yinsh][yinsh] board game, written in [Elm][elm-lang]

## developing

```
yarn install
elm package install
yarn watch
```

## TODO

- de-dupe run removal (can't use `Set (Player, Set Coordinate)` because `Player` is not comparable)
- figure out a better way to do the phase/move system
- performance (ha.)

[yinsh]: http://www.gipf.com/yinsh/
[rules]: https://web.archive.org/web/20161118010122/http://www.gipf.com/yinsh/index.html
[elm-lang]: http://elm-lang.org/ 
