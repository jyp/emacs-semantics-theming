;;; Commentary

;; Copyright: FSF

;; Author: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; Maintainer: Jean-Philippe Bernardy <jeanphilippe.bernardy@gmail.com>
;; URL: https://github.com/jyp/emacs-semantics-theming
;; Created: January 2021
;; Keywords: theming
;; Package-Requires: ((emacs "26") (dash "2.17.0"))
;; Version: 1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file attempts to est ablish an Emacs Semantically-sound
;; Theming foundation. The goal is to provide a visually pleasant and
;; semantically coherent meta-theme, which is easy to customize.
;;
;; This a achieved through:
;;
;; - a small set of faces which are assigned a meaning. Sometimes
;; standard emacs faces can be used for that (`shadow'), but otherwise
;; we define new ones. Use customize-apropos-faces est- for a list of
;; those.
;;
;; - a common theme base, mapping (many) emacs faces to this small set.
;;
;; - a way to define the small set of standard faces, in a visually
;; coherent manner, from an even smaller palette of colors.

;;  This is
;; achieved by inheriting from   See also
;; `est-build-base-theme-with'"

;;; Code:

(require 'color)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customisation infrastructure

(defvar est-customs nil "List of customs to re-evaluate when applying est-theming")
(defvar est-faces   nil "List of faces to reset when applying est-theming")
(setq est-customs nil
      est-faces nil)
(defmacro est-defcustom (symbol standard doc &rest args)
  (declare (doc-string 3) (debug (name body)))
  `(progn
     (push ',symbol est-customs)
     (defcustom ,symbol ,standard ,doc ,@args)
     ))

(defun est-spec-symbol (face-symbol)
  (intern (concat (symbol-name face-symbol) "-spec")))

(defmacro est-defface (face-symbol spec doc &rest args)
  (let ((spec-symbol (est-spec-symbol face-symbol)))
    `(progn
       (custom-declare-variable ',spec-symbol ',spec ,doc ,@args)
       ()
       (push ',spec-symbol est-customs)
       (push ',face-symbol est-faces))))
; (setq est-faces nil)
; (setq est-customs nil)

(defun est-reevaluate ()
  ;; FIXME: est-customs should really be sorted according to their
  ;; dependencies. But we don't have them now. Rely on them being
  ;; declared in order of dependencies.
  (dolist (symbol (reverse est-customs))
    (custom-reevaluate-setting symbol))
  (dolist (face-symbol est-faces)
    ;; est-  faces are not controlled by custom. est- face specs are. So override the faces here.
    (face-spec-set face-symbol (purecopy (eval (est-spec-symbol face-symbol))) 'face-defface-spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color manipulation

(defcustom est-gamma 1.5
  "Gamma correction parameter for `est'.
2.2 is the standard value but for light palettes tends to yield
too bright colors due to too accurate additive color.")

(defun est-scale-color (scale col)
  "Multiply every entry in COL by SCALE."
  (--map (* scale it) col))

(defun est-add-color (a b)
  "Add entries of A and B indexwise."
  (-zip-with '+ a b))

(defun est-sub-color (a b)
  "Subtract entries of B from A indexwise."
  (-zip-with '- a b))

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



;;;;;;;;;;;
;; Colors


(defcustom est-color-fg-default "#3e4759"
  "Default foreground color." :type 'color :group 'est)

(defcustom est-color-bg-default "#ffffff"
"Default background color."
:type 'color :group 'est)

(defcustom est-color-fg-salient "#2056a2"
  "Color of persistent accents.
It is suitable for links and names which can be followed, such as
directories, required elisp modules, etc." :type 'color :group 'est)

(defcustom est-color-fg-popout "#00e0ff"
"Accented color which should be easy to spot.
It is used mostly to attract attention immediately relevant
portions of the display, whose relevant state is transient.
Transience can be as low as isearch matches, balanced parens or
high TODOs, etc.  Attention can be grabbed using a remarkable hue
and high staturation." :type 'color :group 'est)

(defcustom est-color-fg-critical "#FF2F00"
"Color indicating that urgent attention is required.
Not taking care of the issue will most likely lead to problems
down the road: this is for critical problems, and can be
indicated with extra strong, even disturbing contrast, saturation
and a red hue." :type 'color :group 'est)

(defcustom est-color-bg-subtle "#eef1f6"
"A background color sublty different from the default.
This can be used for slight emphasis, or to delineate areas, such
as the region.  This color can even be used exceptionally as
foreground in case the text is not meant to be read, for example
to display weak separators."  :type 'color :group 'est)

(defcustom est-color-bg-selected "#e5e9f0"
"A background color which is notably different from the default.
Still, it should be contrasting with all foreground colors to
enable easy reading.  It is used for to indicate selected menu
items, or delinate areas with stronger emphasis." :type 'color
:group 'est)


(defcustom est-taint-vc-added "#00FF00"
"A taint to indicate added stuff in VC contexts.
This is not used directly in faces, but blended with various background
colors. So it is fine to use saturated bright colors here." :type 'color :group
'est)

(defcustom est-taint-vc-removed "#FF0000"
"A taint to indicate removed stuff in VC contexts.
This is not used directly in faces, but blended with various background
colors.  So it is fine to use saturated bright colors here."
:type 'color :group 'est)

(est-defcustom est-color-bg-hilight1  (est-paint-over  est-color-bg-default 0.15 est-color-fg-popout)  "bg. highlight 1st kind" :group 'est)
(est-defcustom est-color-bg-hilight2  (est-paint-over  est-color-bg-default 0.15 est-color-fg-salient) "bg. highlight 2nd kind" :group 'est)
(est-defcustom est-color-fg-shadowed  (est-paint-over  est-color-fg-default 0.6 est-color-bg-default)  "de-selected/disabled menu options" :group 'est)
(est-defcustom est-color-fg-faded     (est-paint-over  est-color-fg-default 0.2 est-color-bg-default)  "de-emphasized (comments, etc.)" :group 'est)
(est-defcustom est-color-fg-emph      (est-scrape-paint est-color-fg-default 0.2 est-color-bg-default) "subtle emphasis" :group 'est)

(est-defcustom hl-paren-colors
               (list est-color-fg-salient
                     (est-paint-over est-color-fg-salient 0.25 est-color-fg-emph)
                     (est-paint-over est-color-fg-salient 0.5 est-color-fg-emph)
                     (est-paint-over est-color-fg-salient 0.75 est-color-fg-emph)) "todo: stolen by est")

;;;;;;;;;;;;;;;
;; Faces


(est-defface est-separator `((t :foreground ,est-color-bg-selected))
             "Face for separators (such as --------)" :group 'est)

(est-defface est-choice `((t :background ,est-color-bg-selected :extend t))
  "Background face for the current selection (in completions frameworks, but also magit, etc.) Not the region!"
  :group 'est)

(est-defface est-highlight-1 `((t :background ,est-color-bg-hilight1)) "Face for semi-transient
highlights. The meaning is similar to `est-popout', but for
backgrounds (when changing the foreground color is somehow
inappropriate)." :group 'est)

(est-defface est-highlight-2 `((t :background ,est-color-bg-hilight2)) "Face for secondary highlights" :group 'est)

(est-defface est-critical `((t :foreground ,est-color-fg-critical))
  "Critical face is for information that requires immediate action.
  See also `est-color-fg-critical'."
  :group 'est)

(est-defface est-popout `((t :foreground ,est-color-fg-popout))
  "Popout face is used for information that needs attention.
See also `est-color-fg-popout'."
  :group 'est)

(est-defface est-emph `((t :foreground ,est-color-fg-emph))
  "A mild emphasis face.
This roughly corresponds to italics in mainstream typsetting
practice. By defaut the emphasis effect is achieved by using the
`est-color-fg-emph' color, which is by default slightly more
contrasted than the default color.  Besides, italics do not mesh
well with monospace fonts."
  :group 'est)


(est-defface est-strong `((t :inherit (bold est-emph)))
  "A face with stronger emphasis than `est-emph'.
Accordingly it is used more sparingly. By default the effect is
achieved by using a bold weight together with a higher constrast
(`est-color-fg-emph')."
  :group 'est)

(est-defface est-salient `((t :foreground ,est-color-fg-salient))
  "Salient face is used for information of same urgency,
but different nature than regular text. See also
`est-color-fg-salient'."
  :group 'est)

(est-defface est-faded `((t :foreground ,est-color-fg-faded))
  "Faded face is for information that is less important.
It is achieved by using the same hue as the default foreground
color, but with a lesser contrast. It can be used for comments
and secondary information."
  :group 'est)

(est-defface est-subtle `((t :background ,est-color-bg-subtle))
  "Subtle face is used to suggest a physical area on the screen.
  See also `est-color-bg-subtle.'" :group 'est)


(est-defface est-heading-1 `((t :height 1.3 :inherit  est-heading)) "Face for level 1 headings" :group 'est)
(est-defface est-heading-2 `((t :height 1.15 :inherit est-heading)) "Face for level 2 headings" :group 'est)
(est-defface est-heading-3 `((t :height 1.15 :inherit est-heading)) "Face for level 3 headings" :group 'est)
(est-defface est-heading   `((t :inherit bold)) "Face for level 4 headings and below" :group 'est)


(est-defface mode-line                    `((t :foreground ,est-color-bg-default :background ,est-color-fg-default)) "todo")
(est-defface mode-line-highlight          `((t :foreground ,est-color-bg-default :background ,est-color-fg-faded)) "todo")
(est-defface mode-line-inactive           `((t :foreground ,est-color-fg-faded :background ,est-color-bg-subtle)) "todo")
(est-defface default                      `((t :foreground ,est-color-fg-default :background ,est-color-bg-default)) "todo")
(est-defface cursor                       `((t :background ,est-color-fg-default)) "todo")
(est-defface shadow                       `((t :foreground ,est-color-fg-shadowed)) "todo")
(est-defface magit-diff-removed           `((t :extend t :background ,(est-paint-over est-color-bg-default 0.1 est-taint-vc-removed))) "todo")
(est-defface magit-diff-added             `((t :extend t :background ,(est-paint-over est-color-bg-default 0.1 est-taint-vc-added))) "todo")
(est-defface magit-diff-removed-highlight `((t :extend t :background ,(est-paint-over est-color-bg-selected 0.1 est-taint-vc-removed))) "todo")
(est-defface magit-diff-added-highlight   `((t :extend t :background ,(est-paint-over est-color-bg-selected 0.1 est-taint-vc-added))) "todo")
(est-defface boon-modeline-ins            `((t :foreground ,est-color-bg-default :background ,(est-paint-over est-color-fg-default 0.7 est-color-fg-popout))) "todo")
(est-defface boon-modeline-spc            `((t :foreground ,est-color-bg-default :background ,(est-paint-over est-color-fg-default 0.7 est-color-fg-salient))) "todo")
(est-defface boon-modeline-cmd            `((t :inherit (est-subtle default))) "todo")
(est-defface boon-modeline-off            `((t :inherit error)) "todo")


;;;;;;;;;;;;;;;;;;;;;;
;; Styling theme

(deftheme est-style)

(custom-theme-set-faces
 'est-style
   `(buffer-menu-buffer           ((t :inherit est-strong)))
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
   

   `(avy-background-face ((t :inherit shadow)))
   `(avy-lead-face       ((t :inherit est-popout)))
   `(avy-lead-face-0     ((t :inherit est-emph)))
   `(avy-lead-face-1     ((t :inherit est-emph)))
   `(avy-lead-face-2     ((t :inherit est-emph)))


   `(custom-group-tag-1       ((t :inherit est-heading-1)))
   `(custom-group-tag         ((t :inherit est-heading-2)))
   `(custom-variable-tag      ((t :inherit est-heading)))
   `(custom-state             ((t :inherit est-emph)))
   `(custom-changed           ((t :inherit est-highlight-1)))
   `(custom-modified          ((t :inherit est-highlight-1)))
   `(custom-invalid           ((t :inherit (est-critical est-subtle))))
   `(custom-rogue             ((t :inherit (est-critical est-subtle))))
   `(custom-set               ((t :inherit est-highlight-2)))
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

   `(font-latex-sectioning-1-face   ((t :inherit est-heading-1)))
   `(font-latex-sectioning-2-face   ((t :inherit est-heading-2)))
   `(font-latex-sectioning-3-face   ((t :inherit est-heading-3)))
   `(font-latex-sectioning-4-face   ((t :inherit est-heading)))
   `(font-latex-sectioning-5-face   ((t :inherit est-heading)))

   `(font-latex-math-face           ((t :inherit est-salient)))
   `(font-latex-script-char-face    ((t :inherit est-salient)))
   `(font-latex-string-face         ((t :inherit est-faded)))
   `(font-latex-string-face         ((t :inherit est-faded)))
   `(font-latex-warning-face        ((t :inherit est-strong))) ; not really a warning face!
   `(font-latex-italic-face         ((t :inherit est-emph)))
   `(font-latex-verbatim-face       ((t :inherit est-faded)))

   `(magit-diff-hunk-heading           ((t :extend t :inherit est-heading)))

   `(magit-diff-hunk-heading-highlight ((t :extend t :inherit (est-heading est-choice))))
   `(magit-diff-context-highlight      ((t :extend t :inherit est-choice)))

   `(magit-section-heading             ((t :inherit est-heading-3)))
   `(magit-section-highlight           ((t :inherit est-choice)))

   `(magit-hash                        ((t :inherit est-salient)))

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
   `(org-table                    ((t :inherit default)))
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

   `(eshell-prompt ((t :inherit est-strong)))

   `(widget-field ((t :inherit (est-faded est-subtle)))))

(enable-theme 'est-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick palette re-theming

(defun est-lunarized-dark () ;; solarized inspire theme
  (let ((est-color-fg-default  "#839496")
        (est-color-fg-salient  "#268bd2")
        (est-color-fg-popout   "#eee8d5")
        (est-color-bg-default   "#002b36")
        (est-color-bg-subtle    "#06303c")
        (est-color-bg-selected  "#073642"))
    (est-reevaluate)))


(defun est-lunarized-light () ;; solarized inspired theme
  (interactive)
  (let ((est-color-fg-default  "#657b83")
        (est-color-fg-salient  "#268bd2")
        (est-color-fg-popout   "#d33682")
        (est-color-bg-default   "#fdf6e3")
        (est-color-bg-subtle    "#eee8d5")
        (est-color-bg-selected  "#ffffff"))
    (est-reevaluate)))

(defun est-cloudy-day () ;; light palette, blue tones
  (interactive)
  (let ((est-color-fg-default     "#3e4759")
        (est-color-bg-default     "#ffffff")
        (est-color-bg-subtle      "#eef1f6")
        (est-color-bg-selected    "#e5e9f0")
        (est-color-fg-salient     "#2056a2")
        (est-color-fg-popout      "#00e0ff"))
    (est-reevaluate)))

(defun est-cloudy-night () ;; dark palette, blue accents
  (interactive)
  (let ((est-color-bg-selected "#192435")
        (est-color-bg-subtle   "#242e41")
        (est-color-bg-default  "#2b3547")
        (est-color-fg-default  "#cccfd4")
        (est-color-fg-salient  "#5a8bca")
        (est-color-fg-popout   "#00c8ff"))
    (est-reevaluate)))

(defun est-starry-night-palette () ;; a masterpiece
  (interactive)
  (let ((est-color-bg-selected "#00128d")
        (est-color-bg-subtle   "#000010")
        (est-color-bg-default  "#000050")
        (est-color-fg-default  "#819ce6")
        (est-color-fg-salient  "#74a5b3")
        (est-color-fg-popout   "#e7d97b"))
    (est-reevaluate)))


(provide 'est)


;;; est.el ends here
