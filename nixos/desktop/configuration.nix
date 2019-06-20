# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "sam-home-nixos"; # Define your hostname.
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Australia/Brisbane";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = (with pkgs; [
    cabal-install
    cabal2nix
    chromium
    docker
    dmenu
    emacs
    ghc
    git
    gnumake
    openssl
    openvpn
    pavucontrol
    ripgrep
    rxvt_unicode-with-plugins
    silver-searcher
    toxiproxy
    unzip
    vim
    wget 
    xscreensaver
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
  '';

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  virtualisation.docker.enable = true;
  virtualisation.docker.enableOnBoot = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

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
    xkbOptions="ctrl:nocaps";
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
  services.openvpn.servers = {
    work-vpn = {
      config = "config /home/sam/vpn/work-vpn/config.ovpn";
      autoStart = false;
      updateResolvConf = true;
    };
  };

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

  # mount /ephox
  fileSystems."/ephox/repos" = {
    device = "repos:/ephox/repos";
    fsType = "nfs";
  };

  nix.trustedUsers = ["root" "sam"];
  nix.sandboxPaths = ["/home/sam/.ssh"];
  nixpkgs.config.allowUnfree = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}
