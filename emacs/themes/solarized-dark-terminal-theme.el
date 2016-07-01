(deftheme solarized-dark-terminal
	"Solarized Dark with modifications made for correct display in a terminal with Solarized theme.")

(custom-theme-set-variables
 'solarized-dark-terminal
 '(dabbrev-abbrev-skip-leading-regexp ":")
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("elpa" . "http://tromey.com/elpa/") ("melpa" . "http://melpa.milkbox.net/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(projectile-project-root-files (quote ("rebar.config" "project.clj" "SConstruct" "pom.xml" "build.sbt" "build.gradle" "Gemfile" "requirements.txt" "package.json" "gulpfile.js" "Gruntfile.js" "bower.json" "composer.json" "Cargo.toml" "mix.exs" "README.md"))))

(custom-theme-set-faces
 'solarized-dark-terminal
 '(font-lock-comment-delimiter-face ((t (:foreground "black"))))
 '(font-lock-comment-face ((t (:foreground "black"))))
 '(linum ((t (:foreground "black"))))
 '(font-lock-string-face ((t (:foreground "cyan"))))
 '(font-lock-constant-face ((t (:foreground "blue"))))
 '(font-lock-keyword-face ((t (:foreground "green"))))
 '(region ((t (:background "yellow"))))
 '(font-lock-variable-name-face ((t (:foreground "blue"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-builtin-face ((t (:foreground "green"))))
 '(sh-quoted-exec ((t (:foreground "red"))))
 '(font-lock-type-face ((t (:foreground "yellow"))))
 '(magit-diff-added ((t (:background nil :foreground "color-100"))))
 '(magit-diff-added-highlight ((t (:background "black" :foreground "color-100"))))
 '(magit-diff-context-highlight ((t (:background "black" :foreground "grey70"))))
 '(magit-diff-removed ((t (:background nil :foreground "color-88"))))
 '(magit-diff-removed-highlight ((t (:background "black" :foreground "color-88")))))

(provide-theme 'solarized-dark-terminal)
