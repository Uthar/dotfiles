(font-lock-add-keywords nil
  '(("http[s]?://.\\{70\\}\\(.*\\)"
     1 '(face font-lock-warning-face invisible long-link))))

(add-to-invisibility-spec '(long-link . t))
