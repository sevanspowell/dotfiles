# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  emacsOverlay = import (builtins.fetchTarball {url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;});
  emacs2 = pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.readFile ./emacs/init.el;
    extraEmacsPackages = epkgs: [ epkgs.emacs-libvterm ];
  };
in

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./direnv.nix
      ./keybase.nix
      ./yubikey.nix
    ];

  nixpkgs.overlays = [ emacsOverlay ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 3;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "sam-home-nixos"; # Define your hostname.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Australia/Perth";

  services.emacs.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = (with pkgs; [
    cabal-install
    cabal2nix
    chromium
    cntr
    # dhcpcd
    docker
    dmenu
    # emacs
    emacs2
    firefox
    feh
    ghc
    go-jira
    git
    gnumake
    ledger
    libreoffice
    mitscheme
    mosh
    nixops
    openssl
    openvpn
    pandoc
    pavucontrol
    patchutils
    pass
    ripgrep
    rofi
    rxvt_unicode-with-plugins
    silver-searcher
    stack
    spotify
    toxiproxy
    tlaplusToolbox
    tree
    unzip
    vim
    wget 
    weechat
    xscreensaver
    zathura
  ]) ++ 
  (with pkgs.haskellPackages; [
    ghcid
    hasktags
    hoogle
    xmobar
  ]);

  fonts.fonts = with pkgs; [
    fira-code
    iosevka
    hack-font
    hasklig
    meslo-lg
    source-code-pro
  ];

  environment.interactiveShellInit = ''
    alias dropbox="docker exec -it dropbox dropbox"
    alias dropbox-start="docker run -d --restart=always --name=dropbox \
      -v /home/sam/Dropbox:/dbox/Dropbox \
      -v /home/sam/.dropbox:/dbox/.dropbox \
      -e DBOX_UID=1000 -e DBOX_GID=100 janeczku/dropbox"
    alias work-vpn-up='sudo systemctl start openvpn-work-vpn.service'
    alias work-vpn-down='sudo systemctl stop openvpn-work-vpn.service'
    alias work-vpn-status='sudo systemctl status openvpn-work-vpn.service; echo ""; echo "/etc/resolv.conf:"; cat /etc/resolv.conf'
    alias ssh-iohk='ssh -F ~/.ssh/iohk.config'
  '';

  # List services that you want to enable:
  services.sshd.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.enableOnBoot = true;

  virtualisation.libvirtd.enable = true;
  networking.firewall.checkReversePath = false;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # Enable wifi
  # networking.networkmanager.enable = true;
  networking.wireless.enable = true;
  networking.wireless.networks = {
    "Aussie Broadband 4089" = {
      pskRaw = "7ffdd238802cf375e7dd250e7137b4495d790cc2f14db762791eaf985a03af8f";
    };
    iiNetB25B7F = {
      pskRaw = "7d7d98bf565e4fe7ff00a0f3188b172cb05b7f9c300ac79f22722b6e94a6ae49";
    };
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    desktopManager.xterm.enable = false;
    xkbOptions="ctrl:nocaps";
    videoDrivers = ["nvidia"];

    displayManager.defaultSession = "none+xmonad";

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;
  services.openvpn.servers = {
    work-vpn = {
      config = "config /home/sam/vpn/work-vpn/config.ovpn";
      autoStart = false;
      updateResolvConf = true;
    };
  };

  services.uwsgi = {
    enable = true;
    plugins = [ "python2" ];
    instance = {
      type = "emperor";
      vassals = {
        moin = {
          type = "normal";
          pythonPackages = self: with self; [ moinmoin ];
          socket = "${config.services.uwsgi.runDir}/uwsgi.sock";
        };
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.sam = {
    createHome = true;
    extraGroups = ["wheel" "video" "audio" "disk" "networkmanager" "docker" "libvirtd" "dialout"];
    group = "users";
    home = "/home/sam";
    isNormalUser = true;
    uid = 1000;
  };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ "sam" ];

  nix.trustedUsers = ["root" "sam"];
  nix.sandboxPaths = ["/home/sam/.ssh"];
  nix.extraOptions = ''
    plugin-files = ${pkgs.nix-plugins}/lib/nix/plugins/libnix-extra-builtins.so
  '';

  nix.binaryCaches = [
    "https://cache.nixos.org"
    "https://iohk.cachix.org"
    "https://hydra.iohk.io"
  ];
  nix.binaryCachePublicKeys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];

  nixpkgs.config.allowUnfree = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

  services.logind.extraConfig = ''
    RuntimeDirectorySize=8G
  '';
}
