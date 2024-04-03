# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule
    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
  ];

  home = {
    username = "alex";
    homeDirectory = "/home/alex";
  };
  

  home.packages = with pkgs; [
    manix
    telegram-desktop
    gnomeExtensions.dash-to-dock
    lxappearance
    vim
    nicotine-plus
  ];

  gtk = {
    enable = true;
    theme = {
      name = "Catppuccin-Macchiato-Compact-Pink-Dark";
      package = pkgs.catppuccin-gtk.override {
        accents = [ "lavender" ];
        size = "compact";
        tweaks = [ "rimless" ];
        variant = "mocha";
      };
    };
  };
  xdg.configFile = {
    "gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
    "gtk-4.0/gtk.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
    "gtk-4.0/gtk-dark.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
  };
  home.pointerCursor = {
    gtk.enable = true;
    name = "Catppuccin-Mocha-Light-Cursors";
    package = pkgs.catppuccin-cursors.mochaLight;
    size = 16;
  };

  programs.wezterm.enable = true;
  
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: with epkgs; [
      vterm
      copilot
    ];
  };
  services.emacs = {
    enable = true;
  };
  home.file.".emacs".text = ''(load "~/dotfiles/.emacs")'';

  home.file.".config/hypr/hyprland.conf".text = ''
    source=~/dotfiles/.config/hypr/hyprland.conf
  '';

  programs.firefox.enable = true;

  programs.git = {
    enable = true;
    lfs.enable = true;
    userName = "Alex Zheleznov";
    userEmail = "unholywhale@gmail.com";
  };
 
  dconf.settings = {
    "org/gnome/desktop/wm/preferences" = {
      focus-new-windows = "smart";
      auto-raise = "true";
    };
    "org/gnome/shell" = {
      enabled-extensions = [
        "dash-to-dock@micxgx.gmail.com"
        "apps-menu"
        "places-menu"
      ];
    };
  };
  # gsettings set org.gnome.desktop.wm.preferences focus-new-windows 'smart'

  programs.home-manager.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "23.11";
}
