;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;; Set dotfiles directory
(setq dotfiles-dir "~/dotfiles")
(setq emacs-dir (format "%s/%s" dotfiles-dir "emacs"))
(setq user-init-file load-file-name)

;; System-specific configuration (macOS)
(if (eq system-type 'darwin)
    (progn
      (setq gcc-lib-path "/opt/homebrew/lib/gcc/current/:/opt/homebrew/Cellar/gcc/13.2.0/lib/gcc/current/gcc/aarch64-apple-darwin23/13/")
      (setenv "LIBRARY_PATH"
              (if (getenv "LIBRARY_PATH")
                  (format "%s:%s" gcc-lib-path (getenv "LIBRARY_PATH"))
                gcc-lib-path))
      (setenv "LD_LIBRARY_PATH"
              (if (getenv "LD_LIBRARY_PATH")
                  (format "%s:%s" gcc-lib-path (getenv "LD_LIBRARY_PATH"))
                gcc-lib-path))))

;; Custom variables (auto-generated by Emacs) - loaded early to avoid conflicts
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("73911b9ca48d0db91e4016c8d6e6f6438ca9435171350343fd6b5f35a9b5ef68"
     "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18"
     "9fb561389e5ac5b9ead13a24fb4c2a3544910f67f12cfcfe77b75f36248017d0"
     "95167736741bef2ad3e0543ed545dada5b95fef309883253387a2b14ab67db8d"
     "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7"
     "1ea82e39d89b526e2266786886d1f0d3a3fa36c87480fad59d8fab3b03ef576e"
     "db86c52e18460fe10e750759b9077333f9414ed456dc94473f9cf188b197bc74"
     "7613ef56a3aebbec29618a689e47876a72023bbd1b8393efc51c38f5ed3f33d1"
     "c1638a7061fb86be5b4347c11ccf274354c5998d52e6d8386e997b862773d1d2"
     "703a3469ae4d2a83fd5648cac0058d57ca215d0fea7541fb852205e4fae94983"
     "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850"
     "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2"
     "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940"
     "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a"
     "dbf0cd368e568e6139bb862c574c4ad4eec1859ce62bc755d2ef98f941062441"
     "f079ef5189f9738cf5a2b4507bcaf83138ad22d9c9e32a537d61c9aae25502ef"
     "755fc94932731e7c043d6374bcf488a00cc84235d4a3ca0b412d061281be2c64"
     "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3"
     default))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

;; Add config directory to load path
(add-to-list 'load-path emacs-dir)

;; Load custom features
(require 'functions)
(require 'visual-selection-mode)

;; Load modular configuration files
(require 'config-packages)
(require 'config-ui)
(require 'config-completion)
(require 'config-development)
(require 'config-terminal)
(require 'config-navigation)
(require 'config-org)
(require 'config-llm)
(require 'config-keybindings)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:box (:line-width 6 :style flat-button)))))
 '(mode-line-inactive ((t (:box (:line-width 6 :style flat-button))))))

;;; init.el ends here
