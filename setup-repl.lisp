(require :asdf)
(push "./" asdf:*central-registry*)
(asdf:load-system :textures)

(asdf:load-system :png-read)
(asdf:load-system :cl-opengl)
(asdf:load-system :cl-cffi-gtk)
(asdf:load-system :3d-vectors)
(asdf:load-system :3d-matrices)
(asdf:load-system :local-time)
(asdf:load-system :queues)
(asdf:oos 'asdf:load-op :queues.simple-queue)