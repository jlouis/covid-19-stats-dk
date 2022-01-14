let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      R
      curl
      libxml2
      openssl
      rPackages.tidyverse
      rPackages.jsonlite
      rPackages.hexbin
      rPackages.rmarkdown
      rPackages.knitr
      rPackages.languageserver
      rPackages.jsonlite
      rPackages.rlang
      rPackages.treemapify
      rPackages.ggthemes
      pandoc
      bashInteractive
    ];
  }
