![Version](https://img.shields.io/static/v1?label=mac-say-tts&message=0.1&color=brightcolor)
[![License: GPL 3](https://img.shields.io/badge/License-GPL3-blue.svg)](https://opensource.org/licenses/MIT)

# mac-say-tts

A lightweight text-to-speech (TTS) package for Emacs on macOS that uses the Mac OS built-in `say` command.

## What is this good for?

- Reading aloud text to catch confusing wording. This is often useful during the later stages of editing.
- Reading aloud chunks of text while preoccupied with mundane chores like cleaning up your desk or office.

## Similar Text-to-Speach Emacs packages

- [greader](https://gitlab.com/michelangelo-rodriguez/greader/tree/bc7702a53db6aae35187b83e23b1f56342a16252) Depends on the external program espeak.
- [read-aloud.el](https://github.com/gromnitsky/read-aloud.el/tree/d5f80ab72054a957aed25224639c1779cae5f4d1) This package can use the Mac `say` program, but it also works on other operating systems.
- [piper-mode](https://github.com/snowy-0wl/piper-mode) Mac only. Uses external dependencies.
- [speechd-el](https://github.com/brailcom/speechd-el/tree/0e509d392c7f82ca2451a59b97d551382136d2d5) This package is for the hearing impaired.
- [say-what-im-doing](https://melpa.org/#/say-what-im-doing) This package verbalizes the commands that you issue in Emacs.


  

## Features

- ✅ Works with all Apple Silicon chips (M1, M2, M3, M4, etc.)
- ✅ No external dependencies - uses native macOS TTS capabilities
- ✅ Speak words, sentences, paragraphs, or selected regions
- ✅ Access to all high-quality macOS voices
- ✅ Easy to install and use
- ✅ Configurable keybindings
- ✅ Minimal and maintainable code

## Installation

### Using `straight.el`

```elisp
(straight-use-package
  '(mac-say-tts :type git :host github :repo "MooersLab/mac-say-tts"))
```


### Using `use-package` with `straight.el`

```elisp
(use-package mac-say-tts
  :straight (mac-say-tts :type git :host github :repo "MooersLab/mac-say-tts")
  :config
  (add-hook 'org-mode-hook 'mac-say-tts-mode)
  (add-hook 'text-mode-hook 'mac-say-tts-mode))
```

### Using package-vc-install

This is a new (Emacs 29+), built-in interactive function. You will be prompted for the GitHub repo URL.

### Manual Installation

1. Download `mac-say-tts.el` to your local machine
2. Add it to your load path and require it:

```elisp
(add-to-list 'load-path "/path/to/directory/containing/mac-say-tts.el")
(require 'mac-say-tts)
```

## Usage

### Enabling the Mode

The package provides several ways to enable the text-to-speech functionality:

- **Toggle in current buffer**: `M-x mac-say-toggle` or `C-c t t`
- **Enable in specific modes**: Add to mode hooks
  ```elisp
  (add-hook 'org-mode-hook 'mac-say-tts-mode)
  (add-hook 'latex-hook 'mac-say-tts-mode)
  (add-hook 'markdown-hook 'mac-say-tts-mode)
  ```
- **Enable globally**: `M-x mac-say-enable-globally`

### Available Commands

When the mode is active, the following commands are available:

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c M w`  | `mac-say-current-word` | Speak the word at point |
| `C-c M s`  | `mac-say-current-sentence` | Speak the sentence at point |
| `C-c M p`  | `mac-say-current-paragraph` | Speak the paragraph at point |
| `C-c M r`  | `mac-say-region` | Speak the selected region |
| `C-c M k`  | `mac-say-stop` | Stop all speech |
| `C-c M v`  | `mac-say-list-voices` | List all available voices for the Mac say program |
| `C-c M t`  | `mac-say-toggle` | Toggle the mode in current buffer |
| `C-c M V`  | `mac-say-with-voice | Enables quick tests of different voices |
| `C-c M d`  | `mac-say-set-default-voice` | Reset the default voice |
|


## Customization

### Changing Key Bindings

You can customize the key bindings by modifying `mac-say-tts-mode-map` in your configuration:

```elisp
(define-key mac-say-tts-mode-map (kbd "C-c s w") 'mac-say-current-word)
```

### Setting a Default Voice

To change the default voice (uses `Samantha` by default), you can use the `C-c t d` key binding or `advice` the `mac-say` function:

```elisp
(defadvice mac-say (around use-alex activate)
  "Use Alex voice by default."
  (let ((voice (or (ad-get-arg 1) "Alex")))
    (ad-set-arg 1 voice)
    ad-do-it))
```

## Compatibility

- Requires macOS (works on all versions)
- Works with all Apple Silicon chips (M1, M2, M3, M4, etc.)
- Tested with Emacs 30, but should work with earlier versions

## Contributing

Contributions are welcome! Feel free to submit issues or pull requests.

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.

## Sources of funding

- NIH: R01 CA242845
- NIH: R01 AI088011
- NIH: P30 CA225520 (PI: R. Mannel)
- NIH: P20 GM103640 and P30 GM145423 (PI: A. West)
