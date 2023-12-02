(asdf:defsystem "dungeon-crawler"
  :depends-on ("cl-opengl" "cl-cffi-gtk" "png-read" "3d-matrices" "3d-matrices" "local-time" "queues" "queues.simple-queue")
  :components ((:file "waaf-cffi")
               (:file "textures" :depends-on ("waaf-cffi"))
               (:file "collision")
               (:file "player-input")
               (:file "sprite" :depends-on ("textures"))
               (:file "behavior")
               (:file "game-object" :depends-on ("collision" "sprite" "behavior"))
               (:file "rooms")
               (:file "tiles" :depends-on ("textures" "game-object" "collision" "rooms"))
               
               (:file "behaviors" :depends-on ("game-object" "behavior" "player-input" "collision"))
               (:file "game" :depends-on ("textures" "collision" "player-input" "sprite" "game-object" "behaviors" "tiles" "rooms"))))
