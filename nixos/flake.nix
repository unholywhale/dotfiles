{
  description = "NixOS system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

  };

  outputs = { self, nixpkgs, home-manager, ... } @ inputs:
    let
      inherit (self) outputs;
    in {
      nixosConfigurations = {
        nixos = nixpkgs.lib.nixosSystem {
  	      specialArgs = {inherit inputs outputs;};
  	      modules = [
  	        ./configuration.nix
  	        ./hardware-configuration.nix
	          home-manager.nixosModules.home-manager {
	            home-manager.useGlobalPkgs = true;
	            home-manager.useUserPackages = true;
              home-manager.users.alex = import ./home.nix;
            }
  	      ];
  	    };
      };
      homeConfigurations = {
        "alex@nixos" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires 'pkgs' instance
          extraSpecialArgs = {inherit inputs outputs;};
          # > Our main home-manager configuration file <
          modules = [./home.nix];
      };
    };
  };
}
