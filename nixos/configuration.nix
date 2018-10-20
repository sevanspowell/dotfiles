# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # ./keybase.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.networkmanager.enable = true;
  networking.hostName = "sam-nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Australia/Brisbane";

  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qap | grep wget
  environment.systemPackages = (with pkgs; [
    arandr
    autoconf
    autofs5
    bind
    binutils
    cabal-install
    cabal2nix
    chromium
    direnv
    dmenu
    dropbox
    docker
    gimp
    sbt
    scala
    jetbrains.idea-community
    python
    emacs
    # firefox
    ghc
    git
    gitAndTools.gitflow
    gnumake
    gcc
    hlint
    imgur-screenshot
    scrot
    jq
    llvm_6
    lsof
    networkmanagerapplet
    nix-prefetch-scripts
    nix-repl
    nodejs
    openssl
    openvpn
    pandoc
    postgresql
    pstree
    ruby
    rxvt_unicode-with-plugins
    silver-searcher
    sqlite
    stack2nix
    tree
    unzip
    vagrant
    vim
    virtualbox
    wget
    which
    xscreensaver
    yubikey-personalization
    yubikey-personalization-gui
  ] ++
    (with pythonPackages; [
      docker_compose
    ])
  ) ++
  (with pkgs.haskellPackages; [
    ghcid
    hasktags
    happy
    hlint
    hoogle
    ncurses
    stylish-haskell
    xmobar
  ]) ++
  (with pkgs.nodePackages; [
    js-beautify
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

  services.sshd.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.authorizedKeysFiles = ["/home/sam/id_rsa.pub"];

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
    # displayManager.lightdm = {
    #   enable = true;
    #   autoLogin.enable = true;
    #   autoLogin.user = "sam";
    # };
    displayManager.slim.defaultUser = "sam";
    xkbOptions = "ctrl:nocaps";
    videoDrivers = ["nvidia"];

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
    extraGroups = ["wheel" "video" "audio" "disk" "networkmanager" "docker"];
    group = "users";
    home = "/home/sam";
    isNormalUser = true;
    uid = 1000;
  };
  users.extraUsers.ephox = {
    createHome = true;
    home = "/home/ephox";
    isNormalUser = true;
    isSystemUser = false;
    description = "Ephox";
    uid = 1001;
  };

  services.postgresql = {
    enable = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all ::1/128 trust
      host all all 127.0.0.1/32 trust
    '';
    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE ROLE squid WITH LOGIN PASSWORD 'squid' CREATEDB;
      CREATE DATABASE squid;
      GRANT ALL PRIVILEGES ON DATABASE squid TO squid;
      CREATE ROLE tributary WITH LOGIN PASSWORD 'tributary' CREATEDB;
      CREATE DATABASE tributary;
      GRANT ALL PRIVILEGES ON DATABASE tributary TO tributary;
    '';
  };
  services.redis.enable = true;
  services.autofs = {
    enable = true;
    autoMaster = "/net -hosts";
  };
  services.rpcbind.enable = true; # needed for autofs
  virtualisation.docker.enable = true;
  virtualisation.docker.enableOnBoot = true;

  virtualisation.virtualbox.host.enable = true;

  # services.openvpn.servers = {
  #   officeVPN = { config = '' config /root/nixos/openvpn/officeVPN.conf ''; };
  # };

  nix.binaryCaches= [
    "https://cache.nixos.org/"
    "https://nixcache.reflex-frp.org"
    "http://hydra.qfpl.io"
    "http://nixcache.ephox.intra"
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
