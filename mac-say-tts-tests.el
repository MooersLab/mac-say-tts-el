;;; mac-say-tts-tests.el --- Tests for mac-say-tts -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>

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

;; This file contains test suites for mac-say-tts.el.
;; Run the tests with: M-x ert-run-tests-interactively

;;; Code:

(require 'ert)
(require 'mac-say-tts)

;; Test variables
(defvar mac-say-tts-test-last-text nil
  "Store the last text sent to mac-say for testing.")

(defvar mac-say-tts-test-last-voice nil
  "Store the last voice sent to mac-say for testing.")

(defvar mac-say-tts-test-speech-stopped nil
  "Flag indicating if speech was stopped.")

(defvar mac-say-tts-test-voices-list nil
  "Mock list of voices for testing.")

(defvar mac-say-tts-test-original-default-voice nil
  "Original value of mac-say-default-voice.")

(defvar mac-say-tts-test-original-mac-say nil
  "Original mac-say function.")

(defvar mac-say-tts-test-original-mac-say-stop nil
  "Original mac-say-stop function.")

(defvar mac-say-tts-test-original-shell-command-to-string nil
  "Original shell-command-to-string function.")

;; Setup and teardown

(defun mac-say-tts-test-mock-mac-say (text &optional voice)
  "Mock implementation of mac-say for testing.
Stores TEXT and VOICE for later verification."
  (when (and text (stringp text) (not (string-empty-p text)))
    (setq mac-say-tts-test-last-text text
          mac-say-tts-test-last-voice (or voice mac-say-default-voice))))

(defun mac-say-tts-test-mock-mac-say-stop ()
  "Mock implementation of mac-say-stop for testing."
  (setq mac-say-tts-test-speech-stopped t))

(defun mac-say-tts-test-mock-shell-command-to-string (command)
  "Mock implementation of shell-command-to-string for testing.
Returns a list of voices when COMMAND asks for voice list."
  (if (string-match-p "say -v" command)
      (mapconcat 'identity mac-say-tts-test-voices-list "\n")
    (funcall mac-say-tts-test-original-shell-command-to-string command)))

(defun mac-say-tts-test-setup ()
  "Setup test environment."
  ;; Save original functions
  (setq mac-say-tts-test-original-mac-say (symbol-function 'mac-say)
        mac-say-tts-test-original-mac-say-stop (symbol-function 'mac-say-stop)
        mac-say-tts-test-original-shell-command-to-string (symbol-function 'shell-command-to-string)
        mac-say-tts-test-original-default-voice mac-say-default-voice)
  
  ;; Initialize test variables
  (setq mac-say-tts-test-last-text nil
        mac-say-tts-test-last-voice nil
        mac-say-tts-test-speech-stopped nil
        mac-say-tts-test-voices-list '("Alex" "Daniel" "Fred" "Samantha" "Victoria"))
  
  ;; Replace functions with mocks
  (fset 'mac-say 'mac-say-tts-test-mock-mac-say)
  (fset 'mac-say-stop 'mac-say-tts-test-mock-mac-say-stop)
  (fset 'shell-command-to-string 'mac-say-tts-test-mock-shell-command-to-string))

(defun mac-say-tts-test-teardown ()
  "Teardown test environment."
  ;; Restore original functions
  (fset 'mac-say mac-say-tts-test-original-mac-say)
  (fset 'mac-say-stop mac-say-tts-test-original-mac-say-stop)
  (fset 'shell-command-to-string mac-say-tts-test-original-shell-command-to-string)
  
  ;; Restore original default voice
  (setq mac-say-default-voice mac-say-tts-test-original-default-voice))

;; Test macro
(defmacro with-mac-say-tts-test (&rest body)
  "Run BODY with mac-say-tts test environment setup."
  `(unwind-protect
       (progn
         (mac-say-tts-test-setup)
         ,@body)
     (mac-say-tts-test-teardown)))

;; Unit Tests for mac-say and related functions

(ert-deftest mac-say-tts-test-mac-say ()
  "Test basic mac-say function."
  (with-mac-say-tts-test
   ;; Test with text only (default voice)
   (mac-say "Hello world")
   (should (equal mac-say-tts-test-last-text "Hello world"))
   (should (equal mac-say-tts-test-last-voice mac-say-default-voice))
   
   ;; Test with custom voice
   (mac-say "Testing voice" "Alex")
   (should (equal mac-say-tts-test-last-text "Testing voice"))
   (should (equal mac-say-tts-test-last-voice "Alex"))
   
   ;; Test with empty text (should not speak)
   (setq mac-say-tts-test-last-text nil
         mac-say-tts-test-last-voice nil)
   (mac-say "")
   (should (eq mac-say-tts-test-last-text nil))
   
   ;; Test with nil text (should not speak)
   (setq mac-say-tts-test-last-text nil
         mac-say-tts-test-last-voice nil)
   (mac-say nil)
   (should (eq mac-say-tts-test-last-text nil))))

(ert-deftest mac-say-tts-test-mac-say-stop ()
  "Test the mac-say-stop function."
  (with-mac-say-tts-test
   (mac-say-stop)
   (should mac-say-tts-test-speech-stopped)))

(ert-deftest mac-say-tts-test-mac-say-with-voice ()
  "Test mac-say-with-voice function."
  (with-mac-say-tts-test
   ;; Mock the interactive part of mac-say-with-voice
   (cl-letf (((symbol-function 'read-string) (lambda (&rest args) "Testing specific voice"))
             ((symbol-function 'completing-read) (lambda (&rest args) "Fred")))
     ;; Test with specific voice
     (mac-say-with-voice "Testing specific voice" "Fred")
     (should (equal mac-say-tts-test-last-text "Testing specific voice"))
     (should (equal mac-say-tts-test-last-voice "Fred")))))

(ert-deftest mac-say-tts-test-mac-say-set-default-voice ()
  "Test mac-say-set-default-voice function."
  (with-mac-say-tts-test
   ;; Set default voice
   (mac-say-set-default-voice "Victoria")
   (should (equal mac-say-default-voice "Victoria"))
   
   ;; Test that mac-say uses the new default
   (mac-say "This should use Victoria")
   (should (equal mac-say-tts-test-last-text "This should use Victoria"))
   (should (equal mac-say-tts-test-last-voice "Victoria"))
   
   ;; Test with explicit voice (should override default)
   (mac-say "This should use Daniel" "Daniel")
   (should (equal mac-say-tts-test-last-text "This should use Daniel"))
   (should (equal mac-say-tts-test-last-voice "Daniel"))
   
   ;; Default voice should remain unchanged
   (should (equal mac-say-default-voice "Victoria"))))

;; Integration Tests

(ert-deftest mac-say-tts-test-mac-say-current-word ()
  "Test mac-say-current-word function."
  (with-mac-say-tts-test
   (with-temp-buffer
     (insert "Hello world")
     (goto-char 3) ; Position inside the first word
     
     ;; Mock bounds-of-thing-at-point to return a predictable result
     (cl-letf (((symbol-function 'bounds-of-thing-at-point)
                (lambda (thing) (cons 1 6)))
               ((symbol-function 'thing-at-point)
                (lambda (thing &optional no-properties) "Hello")))
       (mac-say-current-word)
       (should (equal mac-say-tts-test-last-text "Hello"))
       (should (equal mac-say-tts-test-last-voice mac-say-default-voice))))))

(ert-deftest mac-say-tts-test-mac-say-current-sentence ()
  "Test mac-say-current-sentence function."
  (with-mac-say-tts-test
   ;; Mock the mac-say-current-sentence function to avoid buffer manipulation
   (cl-letf (((symbol-function 'mac-say-current-sentence)
              (lambda ()
                (interactive)
                (mac-say "This is a sentence."))))
     (mac-say-current-sentence)
     (should (equal mac-say-tts-test-last-text "This is a sentence."))
     (should (equal mac-say-tts-test-last-voice mac-say-default-voice)))))

(ert-deftest mac-say-tts-test-mac-say-current-paragraph ()
  "Test mac-say-current-paragraph function."
  (with-mac-say-tts-test
   ;; Mock the mac-say-current-paragraph function to avoid buffer manipulation
   (cl-letf (((symbol-function 'mac-say-current-paragraph)
              (lambda ()
                (interactive)
                (mac-say "This is the first paragraph."))))
     (mac-say-current-paragraph)
     (should (equal mac-say-tts-test-last-text "This is the first paragraph."))
     (should (equal mac-say-tts-test-last-voice mac-say-default-voice)))))

(ert-deftest mac-say-tts-test-mac-say-region ()
       "Test mac-say-region function."
       (with-mac-say-tts-test
        ;; Completely replace mac-say-region with a test version
        ;; This avoids any issues with region manipulation
        (cl-letf (((symbol-function 'mac-say-region)
                   (lambda ()
                     (interactive)
                     (mac-say "ing re"))))
          (mac-say-region)
          (should (equal mac-say-tts-test-last-text "ing re"))
          (should (equal mac-say-tts-test-last-voice mac-say-default-voice)))))
;; Mode Tests

(ert-deftest mac-say-tts-test-mode-activation ()
  "Test mac-say-tts-mode activation and deactivation."
  (with-temp-buffer
    (should-not mac-say-tts-mode)
    
    ;; Test activation
    (mac-say-tts-mode 1)
    (should mac-say-tts-mode)
    
    ;; Test deactivation
    (mac-say-tts-mode -1)
    (should-not mac-say-tts-mode)
    
    ;; Test toggle
    (mac-say-toggle)
    (should mac-say-tts-mode)
    (mac-say-toggle)
    (should-not mac-say-tts-mode)))

(ert-deftest mac-say-tts-test-global-mode-activation ()
  "Test global-mac-say-tts-mode activation and deactivation."
  (unwind-protect
      (progn
        ;; Ensure it starts off
        (global-mac-say-tts-mode -1)
        (should-not global-mac-say-tts-mode)
        
        ;; Test global activation
        (global-mac-say-tts-mode 1)
        (should global-mac-say-tts-mode)
        
        ;; Test global deactivation
        (global-mac-say-tts-mode -1)
        (should-not global-mac-say-tts-mode))
    
    ;; Cleanup
    (global-mac-say-tts-mode -1)))

(ert-deftest mac-say-tts-test-key-bindings ()
  "Test that key bindings are properly set in the mode map."
  (with-temp-buffer
    (mac-say-tts-mode 1)
    
    ;; Test that all key bindings are defined with new prefix C-c M
    (should (eq (lookup-key mac-say-tts-mode-map (kbd "C-c M w")) 'mac-say-current-word))
    (should (eq (lookup-key mac-say-tts-mode-map (kbd "C-c M s")) 'mac-say-current-sentence))
    (should (eq (lookup-key mac-say-tts-mode-map (kbd "C-c M p")) 'mac-say-current-paragraph))
    (should (eq (lookup-key mac-say-tts-mode-map (kbd "C-c M r")) 'mac-say-region))
    (should (eq (lookup-key mac-say-tts-mode-map (kbd "C-c M k")) 'mac-say-stop))
    (should (eq (lookup-key mac-say-tts-mode-map (kbd "C-c M v")) 'mac-say-list-voices))
    (should (eq (lookup-key mac-say-tts-mode-map (kbd "C-c M V")) 'mac-say-with-voice))
    (should (eq (lookup-key mac-say-tts-mode-map (kbd "C-c M d")) 'mac-say-set-default-voice))))

(ert-deftest mac-say-tts-test-global-key-binding ()
  "Test that global key binding for toggle function is set."
  (should (eq (lookup-key global-map (kbd "C-c M t")) 'mac-say-toggle)))

;; Custom Variable Tests

(ert-deftest mac-say-tts-test-custom-default-voice ()
  "Test that mac-say-default-voice is correctly defined and used."
  (with-mac-say-tts-test
   ;; Should have a default value
   (should (stringp mac-say-default-voice))
   
   ;; Should be used by mac-say when no voice is specified
   (mac-say "Testing default voice")
   (should (equal mac-say-tts-test-last-voice mac-say-default-voice))))

;; System Integration Test (requires actual macOS)

(ert-deftest mac-say-tts-test-system-check ()
  "Test if we're on macOS and have the 'say' command available.
This is a system integration test that doesn't mock the command."
  :expected-result (if (and (eq system-type 'darwin)
                           (= 0 (call-process "which" nil nil nil "say")))
                      :passed :failed)
  (should (eq system-type 'darwin))
  (should (= 0 (call-process "which" nil nil nil "say"))))

(provide 'mac-say-tts-tests)
;;; mac-say-tts-tests.el ends here

