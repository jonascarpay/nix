# nix dots

These are my nix dotfiles. There are many like them, but these are mine.

If you came here looking for the declarative cachix thing, it's been moved to [a separate repo](https://github.com/jonascarpay/declarative-cachix).

## potentially interesting things

### flakes

[I use flakes now](https://github.com/jonascarpay/nix/blob/master/flake.nix).
I would recommend them.

### st

I use the st terminal.
You configure it by applying [patches](https://st.suckless.org/patches/) to the source code.
[Nix makes this very easy](https://github.com/jonascarpay/nix/blob/master/desktop/st/default.nix).

### age

I use [agenix](https://github.com/ryantm/agenix) to manage my secrets, and I like it a lot.
It allows me to put my secrets in [git](https://github.com/jonascarpay/nix/tree/master/secrets) and in [my config](https://github.com/jonascarpay/nix/blob/master/system/xc-cache.nix), yet they'll never be in my store unencrypted.

### unbound

Want to run pihole but don't want to sacrifice an entire pi?
Just set up an [`unbound` server with a custom blocklist](https://github.com/jonascarpay/nix/tree/master/system/unbound.nix).

### editor configuration

I have a little module system that I use for both emacs and vim.
It allows me to individually declare plugin/configuration combinations.
The simplest use case is declaring a plugin and adding some configuration:
```
airline.config = ''
	let g:airline_powerline_fonts = 1
	let g:airline#extensions#branch#displayed_head_limit = 10
'';
```

You can see it managing my [vim here](https://github.com/jonascarpay/nix/blob/master/home/vim/default.nix), my [emacs here](https://github.com/jonascarpay/nix/blob/master/desktop/emacs/default.nix), and the code itself [here](https://github.com/jonascarpay/nix/blob/master/lib/editor-config.nix).
