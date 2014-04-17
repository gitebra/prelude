;;; skeleton-setup.el --- skeleton definitions
;;; Commentary:
;;; Code:

(message "Define skeletons...")

(define-skeleton setup-emacs-package
  "Insert required comments template for setup file"
  "Description: "
  ";;; " (buffer-name) " --- " str
  \n
  ";;; Commentary:
;;; Code:"
\n
"(" "provide '" (file-name-sans-extension (buffer-name)) ")
;;; " (buffer-name)
  " ends here")

(define-skeleton bash-skeleton
  "Insert bash shebang and mode line comments for emacs."
  "Description: "
  "#!/usr/bin/env bash"
  \n
  "# -*-  Mode: Shell-script -*-"
  \n
  "# " (buffer-name) " --- " str
  \n
  "# " (buffer-name) " ends here")

(define-skeleton clojure-skeleton
  "Insert clojure shebang and mode line comments for emacs."
  "Description: "
  "#!/usr/bin/env java -cp clojure.jar clojure.main"
  \n
  ";;; -*-  Mode: clojure -*-"
  \n
  ";;; " (buffer-name) " --- " str
  \n
  ";;; " (buffer-name) " ends here")

(define-skeleton erlang-skeleton
  "Insert erlang shebang and mode line comments for emacs."
  "Description: "
  \n
  "%% -*-  Mode: erlang -*-"
  \n
  "%% " (buffer-name) " --- " str
  \n
  "%% " (buffer-name) " ends here")

(define-skeleton haskell-skeleton
  "Insert mode line comments for emacs."
  "Description: "
  \n
  "-- -*-  Mode: io -*-"
  \n
  "-- " (buffer-name) " --- " str
  \n
  "-- " (buffer-name) " ends here")

(define-skeleton io-skeleton
  "Insert io shebang and mode line comments for emacs."
  "Description: "
  "#!/usr/bin/env io"
  \n
  "# -*-  Mode: io -*-"
  \n
  "# " (buffer-name) " --- " str
  \n
  "# " (buffer-name) " ends here")

(define-skeleton perl-skeleton
  "Insert perl shebang and mode line comments for emacs."
  "#!/usr/bin/env perl"
  \n
  "# -*-  Mode: perl -*-"
  \n
  "% " (buffer-name) " --- " str
  \n
  "% " (buffer-name) " ends here")

(define-skeleton prolog-skeleton
  "Insert prolog shebang and mode line comments for emacs."
  "#!/usr/bin/env swipl"
  \n
  "# -*-  Mode: prolog -*-"
  \n
  "% " (buffer-name) " --- " str
  \n
  "% " (buffer-name) " ends here")

(define-skeleton python-skeleton
  "Insert python shebang and mode line comments for emacs."
  "#!/usr/bin/env python"
  \n
  "# -*-  Mode: python -*-"
  \n
  "# " (buffer-name) " --- " str
  \n
  "# " (buffer-name) " ends here")

(define-skeleton ruby-skeleton
  "Insert ruby shebang and mode line comments for emacs."
  "#!/usr/bin/env ruby"
  \n
  "# -*-  Mode: ruby -*-"
  \n
  "# " (buffer-name) " --- " str
  \n
  "# " (buffer-name) " ends here")

(provide 'skeleton-setup)
;;; skeleton-setup.el ends here
