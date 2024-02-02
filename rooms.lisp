(defpackage :rooms
  (:use :common-lisp :room :level-loading)
  (:export ))

(in-package :rooms)

(defvar *room-1* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W room::tile-wall) (F room::tile-floor) (O NIL)))
                   :layout (make-array '(16 16) :initial-contents '((O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W F F W W W W W W W)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)))
                   :connections (make-connections-array :top '((7 2))
                                                        :bottom '((7 2)))))

(defvar *room-2* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W room::tile-wall) (F room::tile-floor) (O NIL)))
                   :layout (make-array '(16 16) :initial-contents '((O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O W W W W F F W W W W O O O)
                                                                    (O O O W F F F F F F F F W O O O)
                                                                    (O O O W F F F F F F F F W O O O)
                                                                    (W W W W F F F F F F F F W W W W)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (W W W W F F F F F F F F W W W W)
                                                                    (O O O W F F F F F F F F W O O O)
                                                                    (O O O W F F F F F F F F W O O O)
                                                                    (O O O W W W W F F W W W W O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)
                                                                    (O O O O O O W F F W O O O O O O)))
                   :connections (make-connections-array :top '((7 2))
                                                        :bottom '((7 2))  
                                                        :left '((7 2))
                                                        :right '((7 2)))
                   :game-objects '((7.5 7.5 room::stairs))))

(defvar *room-crossroad* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W room::tile-wall) (F room::tile-floor)))
                   :layout (make-array '(16 16) :initial-contents '((W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W F F W W W W W W W)))
                   :connections (make-connections-array :top '((7 2))
                                                        :bottom '((7 2))  
                                                        :left '((7 2))
                                                        :right '((7 2)))
                           :game-objects '((2 2 room::enemy-contact)
                                           (2 13 room::enemy-contact)
                                           (13 2 room::enemy-contact)
                                           (13 13 room::enemy-contact))))

(defvar *room-t-junction-not-B* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W room::tile-wall) (F room::tile-floor)))
                   :layout (make-array '(16 16) :initial-contents '((W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (F F F F F F F F F F F F F F F F)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W W W W W W W W W W)))
                   :connections (make-connections-array :top '((7 2))
                                                        :left '((7 2))
                                                        :right '((7 2)))
                           :game-objects '((13 2 room::enemy-ranged)
                                           (13 13 room::enemy-ranged))))

(defvar *room-t-junction-not-L* (copy-room-tiles-rotated *room-t-junction-not-B* 1))
(defvar *room-t-junction-not-T* (copy-room-tiles-rotated *room-t-junction-not-B* 2))
(defvar *room-t-junction-not-R* (copy-room-tiles-rotated *room-t-junction-not-B* 3))

(defvar *room-vertical* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W room::tile-wall) (F room::tile-floor)))
                   :layout (make-array '(16 16) :initial-contents '((W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W F F W W W W W W W)))
                   :connections (make-connections-array :top '((7 2))
                                                        :bottom '((7 2)))))

(defvar *room-horizontal* (copy-room-tiles-rotated *room-vertical* 1))

(defvar *room-corner-TR* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W room::tile-wall) (F room::tile-floor)))
                   :layout (make-array '(16 16) :initial-contents '((W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F F)
                                                                    (W F F F F F F F F F F F F F F F)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W W W W W W W W W W)))
                   :connections (make-connections-array :top '((7 2))
                                                        :right '((7 2)))))

(defvar *room-corner-RB* (copy-room-tiles-rotated *room-corner-TR* 1))
(defvar *room-corner-BL* (copy-room-tiles-rotated *room-corner-TR* 2))
(defvar *room-corner-LT* (copy-room-tiles-rotated *room-corner-TR* 3))

(defvar *room-dead-end-T* (make-instance 'room-tiles
                   :keys (make-hash-table-with-pairs '((W room::tile-wall) (F room::tile-floor)))
                   :layout (make-array '(16 16) :initial-contents '((W W W W W W W F F W W W W W W W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W F F F F F F F F F F F F F F W)
                                                                    (W W W W W W W W W W W W W W W W)))
                   :connections (make-connections-array :top '((7 2)))))

(defvar *room-dead-end-R* (copy-room-tiles-rotated *room-dead-end-T* 1))
(defvar *room-dead-end-B* (copy-room-tiles-rotated *room-dead-end-T* 2))
(defvar *room-dead-end-L* (copy-room-tiles-rotated *room-dead-end-T* 3))

;; Set rooms for use in level generation (level-loading package)
(setf *start-room* *room-1*)
(setf *end-room* *room-2*)
(setf *rooms* (list *room-crossroad*
                    *room-t-junction-not-B* *room-t-junction-not-L* *room-t-junction-not-T* *room-t-junction-not-R*
                    *room-vertical* *room-horizontal*
                    *room-corner-TR* *room-corner-RB* *room-corner-BL* *room-corner-LT*
                    *room-dead-end-T* *room-dead-end-R* *room-dead-end-B* *room-dead-end-L*))
