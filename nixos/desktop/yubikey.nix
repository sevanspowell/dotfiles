{ config, pkgs, lib, ... }:

{
  # ~/.gnupg/gpg.conf
  # personal-cipher-preferences AES256 AES192 AES
  # personal-digest-preferences SHA512 SHA384 SHA256
  # personal-compress-preferences ZLIB BZIP2 ZIP Uncompressed
  # default-preference-list SHA512 SHA384 SHA256 AES256 AES192 AES ZLIB BZIP2 ZIP Uncompressed
  # cert-digest-algo SHA512
  # s2k-digest-algo SHA512
  # s2k-cipher-algo AES256
  # charset utf-8
  # fixed-list-mode
  # no-comments
  # no-emit-version
  # no-greeting
  # keyid-format 0xlong
  # list-options show-uid-validity
  # verify-options show-uid-validity
  # with-fingerprint
  # require-cross-certification
  # no-symkey-cache
  # use-agent
  # throw-keyids

  # ~/.gnupg/scdaemon.conf
  # reader-port Yubico Yubikey

  programs = {
    ssh.startAgent = false;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "curses";
    };
  };

  services.pcscd.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization ];

  services.udev.extraRules = let
    dependencies = with pkgs; [ coreutils gnupg gawk gnugrep ];
    clearYubikey = pkgs.writeScript "clear-yubikey" ''
      #!${pkgs.stdenv.shell}
      export PATH=${pkgs.lib.makeBinPath dependencies};
      keygrips=$(
        gpg-connect-agent 'keyinfo --list' /bye 2>/dev/null \
          | grep -v OK \
          | awk '{if ($4 == "T") { print $3 ".key" }}')
      for f in $keygrips; do
        rm -v ~/.gnupg/private-keys-v1.d/$f
      done
      gpg --card-status 2>/dev/null 1>/dev/null || true
    '';
    clearYubikeyUser = pkgs.writeScript "clear-yubikey-user" ''
      #!${pkgs.stdenv.shell}
      ${pkgs.sudo}/bin/sudo -u sam ${clearYubikey}
    '';
  in ''
    ACTION=="add|change", SUBSYSTEM=="usb", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0407", RUN+="${clearYubikeyUser}"
  '';

  environment.systemPackages = with pkgs; [
    gnupg
    pinentry-curses
    paperkey
    yubioath-desktop
    yubikey-manager
    ccid
    gpgme.dev
  ];

  environment.shellInit = ''
    export GPG_TTY="$(tty)"
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
  '';
}
