self: super: {
  lib = super.lib // {
    compose = f: g: x: f (g x);
    composeList = super.lib.foldr self.lib.compose super.lib.id;
  };
}
