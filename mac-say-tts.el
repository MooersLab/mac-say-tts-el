;;; mac-say-tts.el --- Text-to-speech for macOS using built-in 'say' command -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: multimedia, convenience, accessibility
;; URL: https://github.com/yourusername/mac-say-tts

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; mac-say-tts provides a simple interface to macOS text-to-speech capabilities
;; using the built-in 'say' command.  It works with all Apple Silicon chips
;; (M1, M2, M3, M4, etc.) and has no external dependencies.
;;
;; Features:
;; - Speak words, sentences, paragraphs, or selected regions
;; - Access to all high-quality macOS voices
;; - Simple minor mode with convenient key bindings
;; - Works on all Apple Silicon Macs
;;
;; Usage:
;; Enable the mode with `mac-say-tts-mode' or globally with `global-mac-say-tts-mode'.
;; Toggle the mode quickly with `mac-say-toggle' (bound to C-c M t by default).
;;
;; When the mode is active, the following key bindings are available:
;; C-c M w - Speak the current word
;; C-c M s - Speak the current sentence
;; C-c M p - Speak the current paragraph
;; C-c M r - Speak the selected region
;; C-c M k - Stop all speech
;; C-c M v - List all available voices
;; C-c M d - Set the default voice

;;; Code:

;; Custom variable for default voice
(defcustom mac-say-default-voice "Samantha"
  "Default voice to use for text-to-speech.
Use `mac-say-list-voices' to see available voices on your system."
  :type 'string
  :group 'mac-say)

;; Basic text-to-speech function
(defun mac-say (text &optional voice)
  "Speak TEXT using macOS say command with VOICE.
If VOICE is not specified, use `mac-say-default-voice'."
  (interactive "sText to speak: ")
  (when (and text (stringp text) (not (string-empty-p text)))
    (let ((voice (or voice mac-say-default-voice)))
      (start-process "say" nil "say" "-v" voice text))))

;; Function with voice selection
(defun mac-say-with-voice (text voice)
  "Speak TEXT using the specified VOICE.
Interactively reads TEXT from the active region or prompts for input,
and allows selection of VOICE from available macOS voices."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Text to speak: "))
         (completing-read "Voice: " 
                          (split-string 
                           (shell-command-to-string 
                            "say -v '?' | awk '{print $1}'") 
                           "\n" t))))
  (mac-say text voice))

;; Set default voice function
(defun mac-say-set-default-voice (voice)
  "Set the default voice for text-to-speech to VOICE.
Interactively selects from available voices on the system."
  (interactive
   (list (completing-read "Set default voice to: " 
                          (split-string 
                           (shell-command-to-string 
                            "say -v '?' | awk '{print $1}'") 
                           "\n" t))))
  (setq mac-say-default-voice voice)
  (message "Default voice set to %s" voice))

;; Function to stop speech
(defun mac-say-stop ()
  "Stop all speech."
  (interactive)
  (shell-command "killall say"))

;; Function to speak region
(defun mac-say-region ()
  "Speak the selected region."
  (interactive)
  (if (region-active-p)
      (let ((text (buffer-substring-no-properties 
                   (region-beginning) (region-end))))
        (mac-say text))
    (message "No active region")))

;; Get the current word
(defun mac-say-current-word ()
  "Speak the word at point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (text (when bounds 
                 (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (if text
        (mac-say text)
      (message "No word at point"))))

;; Get the current sentence
(defun mac-say-current-sentence ()
  "Speak the sentence at point."
  (interactive)
  (save-excursion
    (let (start end text)
      ;; Find sentence boundaries
      (when (not (= (point) (point-min)))
        (backward-char 1))
      (when (not (looking-at "\\.\\|\\!\\|\\?"))
        (backward-sentence))
      (setq start (point))
      (forward-sentence)
      (setq end (point))
      (setq text (buffer-substring-no-properties start end))
      (if (and text (not (string-empty-p text)))
          (mac-say text)
        (message "No sentence at point")))))

;; Get current paragraph
(defun mac-say-current-paragraph ()
  "Speak the paragraph at point."
  (interactive)
  (save-excursion
    (let (start end text)
      (backward-paragraph)
      (setq start (point))
      (forward-paragraph)
      (setq end (point))
      (setq text (buffer-substring-no-properties start end))
      (if (and text (not (string-empty-p text)))
          (mac-say text)
        (message "No paragraph at point")))))

;; List available voices
(defun mac-say-list-voices ()
  "List available macOS voices."
  (interactive)
  (with-output-to-temp-buffer "*Mac Voices*"
    (princ "Available macOS voices:\n\n")
    (princ (shell-command-to-string "say -v '?' | sort"))))

;; Define keymap with new prefix C-c M
(defvar mac-say-tts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c M s") 'mac-say-current-sentence)
    (define-key map (kbd "C-c M w") 'mac-say-current-word)
    (define-key map (kbd "C-c M p") 'mac-say-current-paragraph)
    (define-key map (kbd "C-c M r") 'mac-say-region)
    (define-key map (kbd "C-c M k") 'mac-say-stop)
    (define-key map (kbd "C-c M v") 'mac-say-list-voices)
    (define-key map (kbd "C-c M V") 'mac-say-with-voice)
    (define-key map (kbd "C-c M d") 'mac-say-set-default-voice)
    map)
  "Keymap for Mac Say TTS mode.")

;; Define minor mode
(define-minor-mode mac-say-tts-mode
  "Toggle Mac Say Text-to-Speech mode.
This mode provides key bindings for using macOS text-to-speech features:
\\{mac-say-tts-mode-map}"
  :init-value nil
  :lighter " MacSay"
  :keymap mac-say-tts-mode-map
  :global nil
  (if mac-say-tts-mode
      (message "Mac Say TTS mode enabled. Use C-c M w/s/p/r for word/sentence/paragraph/region.")
    (message "Mac Say TTS mode disabled.")))

;; Global toggle function that's easier to remember
(defun mac-say-toggle ()
  "Toggle Mac Say TTS mode globally for the current buffer."
  (interactive)
  (mac-say-tts-mode 'toggle)
  (force-mode-line-update))

;; Create a global key binding for easy enabling/disabling
(global-set-key (kbd "C-c M t") 'mac-say-toggle)

;; Auto-enable for org-mode and text-mode if desired
(add-hook 'org-mode-hook 'mac-say-tts-mode)
(add-hook 'text-mode-hook 'mac-say-tts-mode)

;; Also define a global mode version
(define-globalized-minor-mode global-mac-say-tts-mode
  mac-say-tts-mode
  (lambda () (mac-say-tts-mode 1))
  :group 'mac-say)

;; Convenient function to enable global mode
(defun mac-say-enable-globally ()
  "Enable Mac Say TTS mode in all buffers."
  (interactive)
  (global-mac-say-tts-mode 1)
  (message "Mac Say TTS enabled globally. Use C-c M w/s/p/r for word/sentence/paragraph/region."))

(provide 'mac-say-tts)
;;; mac-say-tts.el ends here
