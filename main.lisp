(require :asdf)
(push "./" asdf:*central-registry*)
(asdf:load-system "dungeon-crawler")

(game::main)
;(sb-ext:save-lisp-and-die "Dungeon-Crawler.exe"
;                   :toplevel #'game::main
;                   :executable t)