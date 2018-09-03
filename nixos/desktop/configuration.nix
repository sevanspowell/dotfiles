# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader = {
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot";
    };
    grub = {
      devices = [ "nodev" ];
      efiSupport = true;
      enable = true;
      extraEntries = ''
        menuentry "Windows 10" {
          insmod part_gpt
          insmod fat
          insmod search_fs_uuid
          insmod chain
          search --fs--uid --set=root E130-88D6
          chainloader /EFI/Microsoft/Boot/bootmgfw.efi
        }
      '';
      version = 2;
    };
  };
  boot.extraModprobeConfig = ''
    options snd slots=snd-hda-intel
  '';
  boot.blacklistedKernelModules = [ "snd_pcsp" ];

  networking.networkmanager.enable = true;
  networking.hostName = "orchid"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Australia/Perth";

  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qap | grep wget
  environment.systemPackages = (with pkgs; [
    autoconf
    arandr
    bind
    emacs
    cabal2nix
    cabal-install
    chromium
    direnv
    dmenu
    firefox
    hlint
    ghc
    git
    gcc-arm-embedded
    parted
    htop
    llvm_6
    lsof
    mgba
    musl
    networkmanagerapplet
    nix-prefetch-scripts
    nix-repl
    openssl
    pandoc
    postgresql
    rxvt_unicode-with-plugins
    scrot
    silver-searcher
    sqlite
    stalonetray
    swift
    syncthing
    tetex
    unzip
    vagrant
    vim
    wget
    which
    xscreensaver
    (pkgs.texlive.combine {
      inherit (texlive) scheme-small collection-langkorean algorithms cm-super tikz-3dplot tikz-kalender tikzcodeblocks tikzpfeile tikz-bayesnet tikz-opm tikzducks tikzposter tikz-cd tikz-optics tikzinclude tikzscale tikz-dependency tikz-page tikzmark tikzsymbols tikz-dimline tikz-palattice tikzorbital tikz-feynman tikz-qtree tikzpagenodes tikz-inet tikz-timing tikzpeople;
      pkgFilter = pkg:
          pkg.tlType == "run" || pkg.tlType == "bin" || pkg.pname == "cm-super";
    })
  ]) ++
  (with pkgs.haskellPackages; [
    ghcid
    hasktags
    hlint
    hoogle
    ncurses
    stylish-haskell
    xmobar
  ]);

  fonts.fonts = with pkgs; [
    meslo-lg
    fira-code
    source-code-pro
    anonymousPro
    inconsolata-lgc
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.authorizedKeysFiles = ["/home/sam/id_rsa.pub"];

  # Syncthing
  services.syncthing = {
    enable = true;
    systemService = true;
    openDefaultPorts = true;
    user = "sam";
    dataDir = "/home/sam/.syncthing";
  };

  # Enable PostgreSQL
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql100;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all ::1/128 trust
      host all all 127.0.0.1/32 trust
    '';
    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE ROLE sam WITH LOGIN PASSWORD 'sam' CREATEDB;
    '';
    extraConfig = "timezone = 'Australia/Perth'";
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    desktopManager.default = "none";
    desktopManager.xterm.enable = false;
    displayManager.slim.defaultUser = "sam";
    xkbOptions = "ctrl:nocaps";

    windowManager.default = "xmonad";
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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.sam = {
    createHome = true;
    extraGroups = ["wheel" "video" "audio" "disk" "networkmanager"];
    group = "users";
    home = "/home/sam";
    isNormalUser = true;
    uid = 1000;
  };

  nix.binaryCaches= [
    "https://cache.nixos.org/"
    "https://nixcache.reflex-frp.org"
    "http://hydra.qfpl.io"
  ];
  nix.binaryCachePublicKeys= [
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "qfpl.io:xME0cdnyFcOlMD1nwmn6VrkkGgDNLLpMXoMYl58bz5g="
  ];
  nix.trustedUsers = [ "root" "sam" ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

}
