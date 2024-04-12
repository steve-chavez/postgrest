{ compiler }:

self: super:
let
  inherit (self.haskell) lib;

  overrides =
    _: prev:
    rec {
      # To pin custom versions of Haskell packages:
      #   protolude =
      #     prev.callHackageDirect
      #       {
      #         pkg = "protolude";
      #         ver = "0.3.0";
      #         sha256 = "<sha256>";
      #       }
      #       { };
      #
      # To temporarily pin unreleased versions from GitHub:
      #   <name> =
      #     prev.callCabal2nixWithOptions "<name>" (super.fetchFromGitHub {
      #       owner = "<owner>";
      #       repo  = "<repo>";
      #       rev = "<commit>";
      #       sha256 = "<sha256>";
      #    }) "--subpath=<subpath>" {};
      #
      # To fill in the sha256:
      #   update-nix-fetchgit nix/overlays/haskell-packages.nix
      #
      # - Nowadays you can just delete the sha256 attribute above and nix will assume a fake sha.
      # Once you build the derivation it will suggest the correct sha.
      # - If the library fails its test suite (usually when it runs IO tests), wrap the expression with `lib.dontCheck ()`
      # - <subpath> is usually "."
      # - When adding a new library version here, postgrest.cabal and stack.yaml must also be updated
      #
      # Notes:
      # - This should NOT be the first place to start managing dependencies. Check postgrest.cabal.
      # - When adding a new package version here, you have to update stack:
      #   + For stack.yml add:
      #   extra-deps:
      #     - <package>-<ver>
      #   + For stack.yml.lock, CI should report an error with the correct lock, copy/paste that one into the file
      # - To modify and try packages locally, see "Working with locally modified Haskell packages" in the Nix README.

      configurator-pg =
        prev.callHackageDirect
          {
            pkg = "configurator-pg";
            ver = "0.2.9";
            sha256 = "sha256-UqFiOgPlksbIdHBVO0wYhCnboB+mxKJcXVhY9C1V7Hg=";
          }
          { };

      # Before upgrading fuzzyset to 0.3, check: https://github.com/PostgREST/postgrest/issues/3329
      fuzzyset =
        prev.callHackageDirect
          {
            pkg = "fuzzyset";
            ver = "0.2.4";
            sha256 = "sha256-lpkrTFcR0B4rT/P6x7ui31Twgq7BBj6KIvjKyqXKdpc=";
          }
          { };

      hasql-pool =
        lib.dontCheck (prev.callHackageDirect
          {
            pkg = "hasql-pool";
            ver = "1.0.1";
            sha256 = "sha256-Hf1f7lX0LWkjrb25SDBovCYPRdmUP1H6pAxzi7kT4Gg=";
          }
          { }
        );

      postgresql-libpq = lib.dontCheck
        (prev.postgresql-libpq_0_10_0_0.override {
          postgresql = super.libpq;
        });

      hasql-notifications = lib.dontCheck (prev.callHackageDirect
        {
          pkg = "hasql-notifications";
          ver = "0.2.1.1";
          sha256 = "sha256-oPhKA/pSQGJvgQyhsi7CVr9iDT7uWpKUz0iJfXsaxXo=";
        }
        { }
      );

    };
in
{
  haskell =
    super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" =
          super.haskell.packages."${compiler}".override { inherit overrides; };
      };
    };
}
