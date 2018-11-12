# Trikl

"Terminal React for Clojure" => Trikl

[![CircleCI](https://circleci.com/gh/lambdaisland/trikl.svg?style=svg)](https://circleci.com/gh/lambdaisland/trikl) [![cljdoc badge](https://cljdoc.org/badge/lambdaisland/trikl)](https://cljdoc.org/d/lambdaisland/trikl) [![Clojars Project](https://img.shields.io/clojars/v/lambdaisland/trikl.svg)](https://clojars.org/lambdaisland/trikl)

Trikl lets you write terminal applications in a way that's similar to
React/Reagent. It's main intended use case is for hobbyist/indy games.

With Trikl you use (a dialect of) Hiccup to create your Terminal UI. As your
application state changes, Trikl re-renders the UI, diffs the output with what's
currently on the screen, and sends the necessary commands to the terminal to
bring it up to date.

This is still very much work in progress and subject to change.

See `trikl/demo.clj` for an example.

## True Color

In theory ANSI compatible terminals are able to render 24 bit colors (16 million
shades), but in practice they don't always do.

You can try this snippet, if you see nice continuous gradients from blue to
purple then you're all set.

``` clojure
(defn app [_]
  [:box
   (for [y (range 50)]
     [:span
      (for [x (range 80)]
        [:span {:styles {:bg [(* x 2) (* y 2) (+ x (* 4 y))]}} " "])
      "\n"])])
```

iTerm and gnome-terminal should both be fine, but if you're using Tmux and you're not getting the desired results, then add this to your `~/.tmux.conf`

``` conf
set -g default-terminal "xterm-256color"
set-option -ga terminal-overrides ",xterm-256color:Tc"
```

## License

Copyright &copy; 2018 Arne Brasseur

Licensed under the term of the Mozilla Public License 2.0, see LICENSE.
