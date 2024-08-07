
# Requirement

* [`nix`](https://nixos.org/) with [flake](https://nixos.wiki/wiki/Flakes) support
* [`direnv`](https://github.com/direnv/direnv) >= 2.30.0

# Usage

## Repogitory setup

* Initialize flake project using this template
  ```
  nix flake init -t github.com/Hogeyama/haskell-template
  ```
* `mv ./my-sample.cabal ./<your-package>.cabal`
* `find . -type f -not -path '*/.*' | xargs sed -i -e 's/my-sample/<your-package>/g'`
* Update README

## Build

```
nix build
```

## Development

```
direnv allow
```

And you have `ghc`, `cabal`, `haskell-language-server` and so on.

If your `nix` command supports flake but your `direnv` does not support `use flake` command, try

```
nix develop
```

