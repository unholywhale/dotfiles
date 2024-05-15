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
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    chromium
    cmake
    libwebp
    ffmpeg
    gnumake
    openssl
    tldr
    tdlib
    btop
    ranger
    speedtest-cli
    whitesur-gtk-theme
    pavucontrol
    clang-tools
    ollama
    distrobox
    cudaPackages.cudatoolkit
    dina-font
    ripgrep
    dunst
    eww
    file
    fira-code
    fira-code-symbols
    floorp
    theme-vertex
    gcc
    gnome.adwaita-icon-theme
    gnomeExtensions.dash-to-dock
    google-chrome
    grimblast
    jetbrains-mono
    killall
    kitty
    liberation_ttf
    libnotify
    lutris
    lxappearance
    manix
    mplus-outline-fonts.githubRelease
    networkmanagerapplet
    nicotine-plus
    nodePackages.pyright
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    oh-my-zsh
    python3
    qt5ct
    qt6ct
    rofi-power-menu
    rofi-wayland
    swww
    telegram-desktop
    vim
    vivaldi
    waybar
    wine
    xdg-desktop-portal-hyprland
    zsh
    zsh-completions
    zsh-powerlevel10k
    zsh-syntax-highlighting
  ];

  home.pointerCursor = {
    package = pkgs.gnome.adwaita-icon-theme;
    name = "Adwaita";
    size = 24;
    gtk.enable = true;
    x11.enable = true;
    x11.defaultCursor = "left_ptr";
  };
  
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
    # xdg-desktop-portal-hyprland
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
    # platformTheme = "gtk";
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
    package = pkgs.emacs29-gtk3;
    extraPackages = epkgs: with epkgs; [
      vterm
      copilot
    ];
  };
  #services.emacs.enable = true;
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
