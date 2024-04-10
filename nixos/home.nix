{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
  ];

  home = {
    username = "alex";
    homeDirectory = "/home/alex";
  };


  home.packages = with pkgs; [
    manix
    telegram-desktop
    gnomeExtensions.dash-to-dock
    vivaldi
    floorp
    google-chrome
    chromium
    lxappearance
    vim
    nicotine-plus
    oh-my-zsh
    zsh
    zsh-completions
    zsh-syntax-highlighting
    zsh-powerlevel10k
    qt5ct
    qt6ct
    rofi-wayland
    rofi-power-menu
    waybar
    eww
    dunst
    libnotify
    swww
    grimblast
    xdg-desktop-portal-hyprland
    kitty
    networkmanagerapplet
    killall
    file
    gcc
    python3
    # Fonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    jetbrains-mono
    mplus-outline-fonts.githubRelease
    dina-font
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    steam
  ];


  programs.zsh = {
    enable = true;
    initExtra = ''
    [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
    source ~/dotfiles/.zshrc
'';
    plugins = [
      {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
    ];
      
    oh-my-zsh = {
      theme = "robbyrussell";
      enable = true;
      plugins = [
        "sudo"
        "terraform"
        "systemadmin"
      ];
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    extraConfig = "source=~/dotfiles/.config/hypr/hyprland.conf";
  };

  xdg.portal.enable = true;
  xdg.portal.extraPortals = with pkgs; [
    xdg-desktop-portal-hyprland
    xdg-desktop-portal-gtk
  ];
  xdg.portal.config.common.default = "*";
  
  home.sessionVariables = {
    WLR_NO_HARDWARE_CURSORS = "1";
    NIXOS_OZONE_WL = "1";
  };

  gtk = {
    enable = true;
    # cursorTheme = {
    #   name = "Adwaita";
    #   package = pkgs.adwaita;
    # };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
  };

  fonts.fontconfig.enable = true;
  
  # xdg.configFile = {
  #   "gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
  #   "gtk-4.0/gtk.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
  #   "gtk-4.0/gtk-dark.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
  # };
  
  # home.pointerCursor = {
  #   gtk.enable = true;
  #   name = "Catppuccin-Mocha-Light-Cursors";
  #   package = pkgs.catppuccin-cursors.mochaLight;
  #   size = 16;
  # };

  #programs.wezterm.enable = true;
  
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: with epkgs; [
      vterm
      copilot
    ];
  };
  services.emacs.enable = true;
  home.file.".emacs".text = ''(load "~/dotfiles/.emacs")'';

  # home.file.".config/hypr/hyprland.conf".text = ''
  #   source=~/dotfiles/.config/hypr/hyprland.conf
  # '';

  programs.firefox.enable = true;

  programs.git = {
    enable = true;
    lfs.enable = true;
    userName = "Alex Zheleznov";
    userEmail = "unholywhale@gmail.com";
  };
 
  # dconf.settings = {
  #   "org/gnome/desktop/wm/preferences" = {
  #     focus-new-windows = "smart";
  #     auto-raise = "true";
  #   };
  #   "org/gnome/shell" = {
  #     enabled-extensions = [
  #       "dash-to-dock@micxgx.gmail.com"
  #       "apps-menu"
  #       "places-menu"
  #     ];
  #   };
  # };
  # gsettings set org.gnome.desktop.wm.preferences focus-new-windows 'smart'

  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.11";
}
