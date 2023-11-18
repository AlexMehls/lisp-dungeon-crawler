(asdf:defsystem "dungeon-crawler"
  :depends-on ("cl-opengl" "cl-cffi-gtk" "png-read" "3d-matrices" "3d-matrices" "local-time" "queues" "queues.simple-queue")
  :components ((:file "textures")
               (:file "collision")
               (:file "player-input")
               (:file "game" :depends-on ("textures" "collision" "player-input"))))
