self: super: {
  lib = super.lib // {
    importDir = import ../import-dir.nix;
  };
}
