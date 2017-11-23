path:
  with builtins; (
    let content = readDir path;
     in map (n: import (path + ("/" + n)))
          (filter
            (n:
              match ".*\\.nix" n != null
              || pathExists (path + ("/" + n + "/default.nix"))
            )
            (attrNames content)
          )
  )
