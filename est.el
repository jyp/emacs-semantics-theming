;;; Commentary

;;; Code
(require 'color)
(require 'dash)

(defun est-scale-color (scale col)
  "Multiply every entry in COL by SCALE."
  (--map (* scale it) col))

(defun est-add-color (a b)
  "Add entries of A and B indexwise."
  (-zip-with '+ a b))

(defun est-sub-color (a b)
  "Subtract entries of B from A indexwise."
  (-zip-with '- a b))

(defcustom est-gamma 1.5
  "Gamma correction parameter for `est'.
2.2 is the standard value but for light palettes tends to yield
too bright colors due to too accurate additive color.")

(defun est-exp-gamma (x)
  "Apply gamma correction to X."
  (exp (* (log x) est-gamma)))

(defun est-exp-inv-gamma (x)
  "Unapply gamma correction to X."
  (exp (/ (log x) est-gamma)))

(defun est-clamp (components)
  "Make sure each of the COMPONENTS is in the [0,1] interval."
  (--map (max 0.0 (min 1.0 it)) components))

(defun est-over-composite (base addition)
  "Composite of ADDITION over BASE.
With premultiplied colors, without gamma correction."
  (let ((alpha (car addition)))
    (est-add-color (est-scale-color (- 1.0 alpha) base) addition)
    ))

(defun est-what-under-composite (over addition)
  "What was under OVER before ADDITION was painted?"
  (let ((alpha (car addition)))
    (est-scale-color (/ 1.0 (- 1.0 alpha))
                 (est-sub-color over addition))))

(defun est-argb-to-hex (components)
  "Encode COMPONENTS in the (a r g b) format to hex name.
Input is assumed to be alpha-premultiplied and gamma corrected."
  (pcase-let ((`(,alpha . ,rgb) components))
    (apply 'color-rgb-to-hex (-map 'est-exp-inv-gamma (est-clamp (est-scale-color (/ 1 alpha) rgb))))))

(defun est-alpha-name (alpha name)
  "Decode a pair of ALPHA color NAME to (a r g b) format.
Includes applying gamma and premultiply."
  (cons alpha (est-scale-color alpha (-map 'est-exp-gamma (color-name-to-rgb name)))))

(defun est-paint-over (base alpha addition)
"Paint ADDITION over BASE with opacity ALPHA.
Inputs colors are names."
  (est-argb-to-hex
   (est-over-composite (est-alpha-name 1.0   base)
                       (est-alpha-name alpha addition))))

(defun est-scrape-paint (base alpha addition)
"Recover what yielded BASE after painting ADDITION opacity ALPHA.
Inputs colors are names."
  (est-argb-to-hex
   (est-what-under-composite (est-alpha-name 1.0   base)
                             (est-alpha-name alpha addition))))

(defun est-color-set-hue (base hue)
  "Set HUE of BASE."
  (pcase-let ((`(,hue0 ,saturation ,lightness)
               (apply 'color-rgb-to-hsl (color-name-to-rgb base))))
    (apply 'color-rgb-to-hex (color-hsl-to-rgb hue saturation lightness))))


;;Note: bg colors MUST be contrasting with all other foreground colors.

(defcustom est-color-fg-default  nil
"Default foreground color." :type 'color :group 'sfl)
(defcustom est-color-bg-default nil
"Default background color."
:type 'color :group 'sfl)
(defcustom est-color-fg-salient nil
  "Color of persistent accents.
It is suitable for links and names which can be followed, such as
directories, required elisp modules, etc." :type 'color :group 'sfl)
(defcustom est-color-fg-popout   nil
"Accented color which should be easy to spot.
It is used mostly to attract attention immediately relevant
portions of the display, whose relevant state is transient.
Transience can be as low as isearch matches, balanced parens or
high TODOs, etc.  Attention can be grabbed using a remarkable hue
and high staturation." :type 'color :group 'sfl)
(defcustom est-color-fg-critical "#FF2F00"
"Color indicating that urgent attention is required.
Not taking care of the issue will most likely lead to problems
down the road: this is for critical problems, and can be
indicated with extra strong, even disturbing contrast, saturation
and a red hue." :type 'color :group 'sfl)
(defcustom est-color-bg-subtle nil
"A background color sublty different from the default.
This can be used for slight emphasis, or to delineate areas, such
as the region.  This color can even be used exceptionally as
foreground in case the text is not meant to be read, for example
to display weak separators."  :type 'color :group 'sfl)

(defcustom est-color-bg-selected nil
"A background color which is notably different from the default.
Still, it should be contrasting with all foreground colors to
enable easy reading.  It is used for to indicate selected menu
items, or delinate areas with stronger emphasis." :type 'color
:group 'sfl)



;;;;;;;;;;;;;;;
;; Faces


(defface est-separator nil
  "Face for separators (such as --------)" :group 'sfl)

(defface est-choice nil
  "Background face for the current choice (in helm, ivy, selectrum, etc.)"
  :group 'sfl)

(defface est-highlight-1 nil "Face for primary highlight" :group 'sfl)

(defface est-highlight-2 nil "Face for primary highlight" :group 'sfl)

(defface est-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group 'sfl)

(defface est-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group 'sfl)

(defface est-emph nil
  "An subtle emphasis face.
This effect can be achieved by using the `est-color-fg-emph'
color."
  :group 'sfl)


(defface est-strong nil
  "A strong emphasis face.
It should be used more sparingly that `est-emph', while still not
disturb reading."
  :group 'sfl)

(defface est-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group 'sfl)

(defface est-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'sfl)

(defface est-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group 'sfl)

(defface est-heading   nil "Face for headings"         :group 'sfl)
(defface est-heading-1 nil "Face for level 1 headings" :group 'sfl)
(defface est-heading-2 nil "Face for level 2 headings" :group 'sfl)
(defface est-heading-3 nil "Face for level 3 headings" :group 'sfl)


(defun est-build-theme (theme)
  (interactive)

  (setq est-color-bg-hilight1  (est-paint-over  est-color-bg-default 0.15 est-color-fg-popout))   ;; bg. highlight 1st kind
  (setq est-color-bg-hilight2  (est-paint-over  est-color-bg-default 0.15 est-color-fg-salient))  ;; bg. highlight 2nd kind
  (setq est-color-fg-shadowed  (est-paint-over  est-color-fg-default 0.6 est-color-bg-default))   ;; de-selected/disabled menu options
  (setq est-color-fg-faded     (est-paint-over  est-color-fg-default 0.2 est-color-bg-default))   ;; de-emphasized (comments, etc.)
  (setq est-color-fg-emph      (est-scrape-paint est-color-fg-default 0.2 est-color-bg-default))

  ;; palette-dependent faces
  (custom-theme-set-faces theme
   `(est-heading   ((t :inherit bold)))
   `(est-heading-1 ((t :height 1.3 :inherit  est-heading)))
   `(est-heading-2 ((t :height 1.2 :inherit  est-heading)))
   `(est-heading-3 ((t :height 1.15 :inherit est-heading)))
   `(est-faded     ((t :foreground ,est-color-fg-faded)))
   `(est-emph      ((t :foreground ,est-color-fg-emph)))
   `(est-strong    ((t :inherit (bold est-emph))))
   `(est-salient   ((t :foreground ,est-color-fg-salient)))
   `(est-popout    ((t :foreground ,est-color-fg-popout)))
   `(est-critical  ((t :foreground ,est-color-fg-critical)))

   `(est-separator ((t :foreground ,est-color-bg-selected)))

   `(est-subtle ((t :background ,est-color-bg-subtle)))
   `(est-choice ((t :background ,est-color-bg-selected :extend t)))
   `(est-highlight-1 ((t :background ,est-color-bg-hilight1)))
   `(est-highlight-2 ((t :background ,est-color-bg-hilight2)))

   `(default ((t :foreground ,est-color-fg-default :background ,est-color-bg-default)))
   `(cursor  ((t :background ,est-color-fg-default)))
   `(shadow  ((t :foreground ,est-color-fg-shadowed)))
   )

  ;; palette-independent faces
  (custom-theme-set-faces theme
  
   `(success ((t :inherit est-strong)))
   `(warning ((t :inherit (est-salient bold))))

   `(error   ((t :inherit est-critical)))
   `(link    ((t :inherit est-salient)))
   `(link-visited ((t :inherit link)))
   `(match   ((t :inherit est-popout))) ;;  "Face for matched substrings (helm, ivy, etc.) "

   `(region  ((t :inherit est-subtle)))
   `(secondary-selection ((t :inherit est-subtle)))
   `(fringe  ((t :inherit est-faded)))

   `(header-line         ((t :inherit est-heading)))
   `(hi-yellow           ((t :inherit est-highlight-1)))
   `(highlight           ((t :inherit est-choice)))
   `(hl-line             ((t :inherit est-subtle)))
   `(lazy-highlight      ((t :inherit est-subtle)))
   `(minibuffer-prompt   ((t :inherit est-emph)))
   `(show-paren-match    ((t :inherit est-popout)))
   `(show-paren-mismatch ((t :inherit est-critical)))
   `(trailing-whitespace ((t :inherit est-subtle)))
   
   `(mode-line           ((t :foreground ,est-color-bg-default :background ,est-color-fg-default)))
   `(mode-line-highlight ((t :foreground ,est-color-bg-default :background ,est-color-fg-faded)))
   `(mode-line-inactive  ((t :foreground ,est-color-fg-faded :background ,est-color-bg-subtle)))

   `(avy-background-face ((t :inherit shadow)))
   `(avy-lead-face       ((t :inherit est-popout)))
   `(avy-lead-face-0     ((t :inherit est-emph)))
   `(avy-lead-face-1     ((t :inherit est-emph)))
   `(avy-lead-face-2     ((t :inherit est-emph)))

   `(boon-modeline-ins ((t :foreground ,est-color-bg-default :background ,(est-paint-over est-color-fg-default 0.7 est-color-fg-popout))))
   `(boon-modeline-spc ((t :foreground ,est-color-bg-default :background ,(est-paint-over est-color-fg-default 0.7 est-color-fg-salient))))
   `(boon-modeline-cmd ((t :inherit (est-subtle default))))
   `(boon-modeline-off ((t :inherit error)))

   `(buffer-menu-buffer           ((t :inherit est-strong)))

   `(custom-group-tag-1 ((t :inherit est-heading-1)))
   `(custom-group-tag ((t :inherit est-heading-2)))
   `(custom-variable-tag ((t :inherit est-heading)))
   `(custom-state ((t :inherit est-emph)))
   `(custom-changed ((t :inherit est-highlight-1)))
   `(custom-modified ((t :inherit est-highlight-1)))
   `(custom-invalid ((t :inherit (est-critical est-subtle))))
   `(custom-rogue ((t :inherit (est-critical est-subtle))))
   `(custom-set ((t :inherit est-highlight-2)))
   `(custom-variable-obsolete ((t :inherit est-faded)))

   `(company-scrollbar-bg       ((t :inherit est-subtle)))
   `(company-tooltip            ((t :inherit est-subtle)))
   `(company-tooltip-selection  ((t :inherit est-choice)))
   `(company-tooltip-common     ((t :inherit est-emph)))
   `(company-tooltip-annotation ((t :inherit shadow)))
   `(company-scrollbar-bg       ((t :inverse-video t :inherit est-shadow)))
   `(company-scrollbar-fg       ((t :inverse-video t :inherit est-emph)))

   `(completions-common-part      ((t :inherit est-faded)))
   `(completions-first-difference ((t :inherit default)))

   `(dired-directory ((t :inherit est-salient)))

   `(font-lock-builtin-face       ((t :inherit default)))
   `(font-lock-comment-face       ((t :inherit est-faded)))
   `(font-lock-constant-face      ((t :inherit est-salient)))
   `(font-lock-function-name-face ((t :inherit est-strong)))
   `(font-lock-keyword-face       ((t :inherit est-emph)))
   `(font-lock-string-face        ((t :inherit est-faded)))
   `(font-lock-type-face          ((t :inherit default)))
   `(font-lock-variable-name-face ((t :inherit default)))
   `(font-lock-warning-face       ((t :inherit warning)))

   `(helm-candidate-number           ((t :inherit mode-line)))
   `(helm-candidate-number-suspended ((t :inherit (warning mode-line))))
   `(helm-ff-directory               ((t :inherit est-strong)))
   `(helm-ff-dotted-directory        ((t :inherit est-faded)))
   `(helm-ff-executable              ((t :inherit est-popout)))
   `(helm-ff-file                    ((t :inherit est-faded)))
   `(helm-ff-prefix                  ((t :inherit est-strong)))
   `(helm-grep-file                  ((t :inherit est-faded)))
   `(helm-grep-finish                ((t :inherit default)))
   `(helm-grep-lineno                ((t :inherit est-faded)))
   `(helm-grep-match                 ((t :inherit match)))
   `(helm-match                      ((t :inherit match)))
   `(helm-moccur-buffer              ((t :inherit est-strong)))
   `(helm-selection                  ((t :inherit highlight)))
   `(helm-separator                  ((t :inherit est-separator)))
   `(helm-source-header              ((t :inherit est-heading)))
   `(helm-swoop-target-line-face     ((t :inherit (est-strong est-subtle))))
   `(helm-visible-mark               ((t :inherit est-strong)))
   `(helm-buffer-size                ((t :inherit est-faded)))
   `(helm-buffer-process             ((t :inherit shadow)))
   `(helm-buffer-not-saved           ((t :inherit est-popout)))
   `(helm-buffer-directory           ((t :inherit est-salient)))
   `(helm-buffer-saved-out           ((t :inherit est-critical)))

   `(ido-only-match ((t :inherit match)))

   `(isearch       ((t :inherit highlight)))
   `(isearch-fail  ((t :inherit est-faded)))
   
   `(ivy-action                     ((t :inherit est-faded)))
   `(ivy-completions-annotations    ((t :inherit est-faded)))
   `(ivy-confirm-face               ((t :inherit est-faded)))
   `(ivy-current-match              ((t :inherit highlight)))
   `(ivy-cursor                     ((t :inherit est-strong)))
   `(ivy-grep-info                  ((t :inherit est-strong)))
   `(ivy-grep-line-number           ((t :inherit est-faded)))
   `(ivy-match-required-face        ((t :inherit est-faded)))
   `(ivy-minibuffer-match-face-1    ((t :inherit match)))
   `(ivy-minibuffer-match-face-2    ((t :inherit match)))
   `(ivy-minibuffer-match-face-3    ((t :inherit match)))
   `(ivy-minibuffer-match-face-4    ((t :inherit match)))
   `(ivy-minibuffer-match-highlight ((t :inherit est-strong)))
   `(ivy-modified-buffer            ((t :inherit est-popout)))
   `(ivy-modified-outside-buffer    ((t :inherit est-strong)))
   `(ivy-org                        ((t :inherit est-faded)))
   `(ivy-prompt-match               ((t :inherit est-faded)))
   `(ivy-remote                     ((t :inherit default)))
   `(ivy-separator                  ((t :inherit est-faded)))
   `(ivy-subdir                     ((t :inherit est-faded)))
   `(ivy-virtual                    ((t :inherit est-faded)))
   `(ivy-yanked-word                ((t :inherit est-faded)))

   `(magit-diff-added                  ((t :extend t :background ,(est-paint-over est-color-bg-default 0.1 "#00FF00"))))
   `(magit-diff-added-highlight        ((t :extend t :background ,(est-paint-over est-color-bg-selected 0.1 "#00FF00"))))
   `(magit-diff-context-highlight      ((t :extend t :background ,est-color-bg-selected)))
   `(magit-diff-hunk-heading           ((t :extend t :foreground ,est-color-fg-emph :background ,est-color-bg-subtle)))
   `(magit-diff-hunk-heading-highlight ((t :extend t :foreground ,est-color-fg-emph :background ,est-color-bg-selected :inherit est-strong)))
   `(magit-diff-removed                ((t :extend t :background ,(est-paint-over est-color-bg-default 0.1 "#FF0000"))))
   `(magit-diff-removed-highlight      ((t :extend t :background ,(est-paint-over est-color-bg-selected 0.1 "#FF0000"))))
   `(magit-hash                        ((t :inherit est-salient)))
   `(magit-section-heading             ((t :inherit est-strong)))
   `(magit-section-highlight           ((t :inherit est-choice)))

   `(makefile-space               ((t :inherit warning)))

   `(org-archived                 ((t :inherit est-faded)))
   `(org-block                    ((t :inherit hl-line)))
   `(org-block-begin-line         ((t :inherit est-faded)))
   `(org-block-end-line           ((t :inherit est-faded)))
   `(org-checkbox                 ((t :inherit est-faded)))
   `(org-checkbox-statistics-done ((t :inherit est-faded)))
   `(org-checkbox-statistics-todo ((t :inherit est-faded)))
   `(org-clock-overlay            ((t :inherit est-faded)))
   `(org-code                     ((t :inherit est-faded)))
   `(org-column                   ((t :inherit est-faded)))
   `(org-column-title             ((t :inherit est-faded)))
   `(org-date                     ((t :inherit est-faded)))
   `(org-date-selected            ((t :inherit est-faded)))
   `(org-default                  ((t :inherit est-faded)))
   `(org-document-info            ((t :inherit est-faded)))
   `(org-document-info-keyword    ((t :inherit est-faded)))
   `(org-document-title           ((t :inherit est-faded)))
   `(org-done                     ((t :inherit default)))
   `(org-drawer                   ((t :inherit est-faded)))
   `(org-ellipsis                 ((t :inherit est-faded)))
   `(org-footnote                 ((t :inherit est-faded)))
   `(org-formula                  ((t :inherit est-faded)))
   `(org-headline-done            ((t :inherit est-faded)))
   `(org-latex-and-related        ((t :inherit est-faded)))
   `(org-level-1                  ((t :inherit est-heading-1)))
   `(org-level-2                  ((t :inherit est-heading-2)))
   `(org-level-3                  ((t :inherit est-heading-3)))
   `(org-level-4                  ((t :inherit est-heading)))
   `(org-level-5                  ((t :inherit est-heading)))
   `(org-level-6                  ((t :inherit est-heading)))
   `(org-level-7                  ((t :inherit est-heading)))
   `(org-level-8                  ((t :inherit est-heading)))
   
   `(org-list-dt                  ((t :inherit est-faded)))
   `(org-macro                    ((t :inherit est-faded)))
   `(org-meta-line                ((t :inherit est-faded)))
   `(org-mode-line-clock          ((t :inherit est-faded)))
   `(org-mode-line-clock-overrun  ((t :inherit warning)))
   `(org-priority                 ((t :inherit est-faded)))
   `(org-property-value           ((t :inherit est-faded)))
   `(org-quote                    ((t :inherit est-faded)))
   `(org-scheduled                ((t :inherit est-faded)))
   `(org-scheduled-previously     ((t :inherit est-faded)))
   `(org-scheduled-today          ((t :inherit est-faded)))
   `(org-sexp-date                ((t :inherit est-faded)))
   `(org-special-keyword          ((t :inherit est-faded)))
   `(org-table                    ((t :inherit est-faded)))
   `(org-tag                      ((t :inherit est-faded)))
   `(org-tag-group                ((t :inherit est-faded)))
   `(org-target                   ((t :inherit est-faded)))
   `(org-time-grid                ((t :inherit est-faded)))
   `(org-todo                     ((t :inherit est-popout)))
   `(org-upcoming-deadline        ((t :inherit est-strong)))
   `(org-verbatim                 ((t :inherit est-emph)))
   `(org-verse                    ((t :inherit est-faded)))

   `(powerline-active1    ((t :inverse-video t :inherit (est-emph default))))
   `(powerline-active2    ((t :inherit powerline-active1)))
   `(powerline-inactive1  ((t :inherit (est-faded est-subtle))))
   `(powerline-inactive2  ((t :inherit powerline-inactive1)))

   `(selectrum-primary-highlight ((t :inherit match)))
   
   `(swiper-match-face-1 ((t :inherit match)))
   `(swiper-match-face-2 ((t :inherit match)))
   `(swiper-match-face-3 ((t :inherit match)))
   `(swiper-match-face-4 ((t :inherit match)))

   `(widget-field ((t :inherit (est-faded est-subtle))))
   ))


(defmacro est-build-theme-with (theme theme-feature custom-vars)
  "est"
  `(progn
     (custom-declare-theme ,theme ,theme-feature)
     (let (,@custom-vars)
       (est-build-theme ,theme))))


(est-build-theme-with 'est-lunarized 'est-themes
  ((est-color-fg-default  "#839496")
   (est-color-fg-salient  "#268bd2")
   (est-color-fg-popout   "#eee8d5")
   (est-color-bg-default   "#002b36")
   (est-color-bg-subtle    "#06303c")
   (est-color-bg-selected  "#073642")))

(est-build-theme-with 'est-day 'est-themes
 ((est-color-fg-default     "#3e4759")
  (est-color-bg-default     "#ffffff")
  (est-color-bg-subtle      "#e5e9f0")
  (est-color-bg-selected    "#eceff4")
  (est-color-fg-salient     "#5a8bca")
  (est-color-fg-popout      "#00e0ff")))


(est-build-theme-with 'est-night 'est-themes ;; dark palette
  ((est-color-bg-selected "#192435")
   (est-color-bg-subtle   "#242e41")
   (est-color-bg-default  "#2b3547")
   (est-color-fg-default  "#cccfd4")
   (est-color-fg-salient  "#5a8bca")
   (est-color-fg-popout   "#00c8ff")))

(provide 'est)


;;; est.el ends here