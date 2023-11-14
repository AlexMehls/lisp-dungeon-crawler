(require :asdf)
(push "./" asdf:*central-registry*)
(asdf:load-system "dungeon-crawler")
