# nix dots

These are my nix dotfiles.
There are many like them, but these are mine.

If you came here looking for the declarative cachix thing, it's been moved to [a separate repo](https://github.com/jonascarpay/declarative-cachix).

## Highlights

### flakes

[I use flakes now](https://github.com/jonascarpay/nix/blob/master/flake.nix).
Highly recommended.

### age

I use [agenix](https://github.com/ryantm/agenix) to manage my secrets, and [I like it a lot](https://jonascarpay.com/posts/2021-07-27-agenix.html).

### unbound

Want to run pihole but don't want to deal with pihole setup?
Just set up an [`unbound` server with a custom blocklist](https://github.com/jonascarpay/nix/tree/master/system/unbound.nix).

### wireguard

My pi is a [wireguard server](https://github.com/jonascarpay/nix/blob/master/system/wireguard.nix), my [laptops are wireguard clients](https://github.com/jonascarpay/nix/blob/master/machines/paninix.nix#L4=).
Config-wise, there's nothing here that's not also on [the Nix wiki](https://nixos.wiki/wiki/WireGuard), but wireguard is great and you should consider it if you're setting up a VPN.
