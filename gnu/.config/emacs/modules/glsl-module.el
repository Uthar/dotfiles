;; -*- lexical-binding: t -*-

;; GLSL (OpenGL Shading Language) editing support

(add-to-list 'load-path (concat +vendor-dir+ "glsl-mode"))

(add-to-list 'auto-mode-alist
  '("\\.\\(?:frag\\|vert\\|geom\\|glsl\\|vs\\|fs\\)\\'" . glsl-mode))

(autoload 'glsl-mode "glsl-mode" "" t)
