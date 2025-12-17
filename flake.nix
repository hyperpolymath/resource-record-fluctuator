# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
#
# resource-record-fluctuator - Nix Flake (RSR fallback)
# Primary: guix.scm | Fallback: flake.nix
#
# Usage:
#   nix develop           # Enter development shell
#   nix build             # Build the project
#   nix flake check       # Run checks
{
  description = "HINFO-LOC Fluctuator - DNS record randomization tool for deprecated HINFO and LOC records";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        # Development shell with GNAT Ada compiler
        devShells.default = pkgs.mkShell {
          name = "hinfo-loc-fluctuator-dev";

          buildInputs = with pkgs; [
            # Ada/GNAT toolchain
            gnat
            gprbuild

            # Build tools
            gnumake

            # Optional: SPARK prover (if available)
            # gnatprove

            # Utilities
            git
            just
          ];

          shellHook = ''
            echo "HINFO-LOC Fluctuator Development Environment"
            echo "============================================"
            echo "Ada/GNAT toolchain loaded"
            echo ""
            echo "Quick start:"
            echo "  cd hinfo_loc_fluctuator_ada"
            echo "  make          # Build debug"
            echo "  make release  # Build release"
            echo "  make clean    # Clean build artifacts"
            echo ""
          '';
        };

        # Package definition
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "resource-record-fluctuator";
          version = "0.1.0";

          src = ./.;

          nativeBuildInputs = with pkgs; [
            gnat
            gprbuild
            gnumake
          ];

          buildPhase = ''
            cd hinfo_loc_fluctuator_ada
            make release
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp hinfo_loc_fluctuator_ada/bin/hinfo_loc_fluctuator $out/bin/ || true

            mkdir -p $out/share/hinfo-loc-fluctuator
            cp -r hinfo_loc_fluctuator_ada/data $out/share/hinfo-loc-fluctuator/
          '';

          meta = with pkgs.lib; {
            description = "DNS record randomization tool for deprecated HINFO and LOC records";
            homepage = "https://github.com/hyperpolymath/resource-record-fluctuator";
            license = with licenses; [ agpl3Plus ];
            platforms = platforms.unix;
            maintainers = [];
          };
        };

        # Checks
        checks = {
          # Basic syntax check
          build = self.packages.${system}.default;
        };
      }
    );
}
