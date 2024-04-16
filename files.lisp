(in-package :yggdrasil)

;;; asset-path = #([root] [fonts] [images] [sounds]) 
(defparameter *asset-paths* (let ((content '("" "" "" ""))) (make-array (length content) :initial-contents content)))

(defun path-slash-filter (path)
  "trims out trailing / and \\ then adds one to the end to ensure paths are always valid paths"
  (let ((path-string (namestring path)))
    (if (> (length path-string) 0)
        (concatenate 'string (string-trim '(#\/ #\\) path-string) "/")
        "")))

(defun set-new-path-helper (new-path set-relative)
  (path-slash-filter (if set-relative
                         (namestring (merge-pathnames new-path (aref *asset-paths* 0)))
                         new-path)))

;; Refactor to not repeat the setf for every one
(defun set-path-helper (type new-path set-relative-to-root)
  (ccase type
    ((root assets asset) (setf (aref *asset-paths* 0) (set-new-path-helper new-path set-relative-to-root)))
    ((font fonts) (setf (aref *asset-paths* 1) (set-new-path-helper new-path set-relative-to-root)))
    ((image images) (setf (aref *asset-paths* 2) (set-new-path-helper new-path set-relative-to-root)))
    ((sound sounds) (setf (aref *asset-paths* 3) (set-new-path-helper new-path set-relative-to-root)))))

(defmacro set-path (type new-path &optional (set-relative-to-root t))
  "Sets the passed path to the new one
type = what type of asset you're setting [root, font, image, sound]
new-path = the new path
set-relative-to-root = wether it should be inside the root dir, or based on OS pathing (defaults to relative to executable)

ex: (set-path font 'text/fonts' :set-relative-to-root nil)"
  `(set-path-helper ',type ,new-path ,set-relative-to-root))


(defun get-path-helper (type)
  (case type
    ((root asset assets) (aref *asset-paths* 0))
    ((font fonts) (aref *asset-paths* 1))
    ((image images) (aref *asset-paths* 1))
    ((sound sounds) (aref *asset-paths* 1))))

(defmacro get-path (type)
  "Query for a path
type = the path you're looking for [root, font, image, sound]
Ex: (get-path font)"
  `(get-path-helper ',type))

(defun filter-extention ())

;;; File handling
(defun create-filename (filename file-extention)
  (format nil "~a.~a" filename file-extention))

(defun pathname-error-check (variable)
  (or (typep variable 'string) (typep variable 'pathname) (error (format nil "~a is not a string, or pathname"  variable))))

(defun create-file-path (filename file-path &optional file-extention)
  (when (and (pathname-error-check file-path) (pathname-error-check filename) t)
    (or (probe-file (merge-pathnames (if file-extention (create-filename filename file-extention) filename) file-path))
        (error (format nil "file: [~a~a] in [~a] does not exist" filename (if file-extention (uiop:strcat "." file-extention) "") file-path)))))

(defun verify-file (file path extention)
  "Verifies that the file exist"
  (or (namestring (uiop:probe-file* (create-file-path file path extention))) (error "[~a.~a] does not exist in ~a" file extention path)))
