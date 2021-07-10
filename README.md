# nix dots

These are my nix dotfiles. There are many like them, but these are mine.

If you came here looking for the declarative cachix thing, it's been moved to [a separate repo](https://github.com/jonascarpay/declarative-cachix).

## potentially interesting things

### flakes

[I use flakes now](https://github.com/jonascarpay/nix/blob/master/flake.nix).
I would recommend them.

### st

I use the st terminal.
You configure it by [applying patches to the source code](https://github.com/jonascarpay/nix/blob/master/desktop/st/default.nix).
Nix makes this very easy.

### age

I use [agenix](https://github.com/ryantm/agenix) to manage my secrets, and I like it a lot.
As you can see [here](https://github.com/jonascarpay/nix/tree/master/secrets), it allows me to put my secrets in git, yet they'll never be in my store unencrypted.
I use it, among other things, to configure the office's private nix cache [like so](https://github.com/jonascarpay/nix/blob/master/system/xc-cache.nix).

### unbound

Want to run pihole but don't want to sacrifice an entire pi to do it?
Just set up an [`unbound` server with a custom blocklist](https://github.com/jonascarpay/nix/tree/master/system/unbound.nix).

### editor configuration

I have a little module system that I use for both emacs and vim.
It allows me to individually declare plugin/configuration combinations.
By default it will assume the name of the module is also the name of the plugin, so adding and configuring `airline`, for example, takes three lines:
```
airline.config = ''
	let g:airline_powerline_fonts = 1
	let g:airline#extensions#branch#displayed_head_limit = 10
'';
```

You can see it managing my [vim here](https://github.com/jonascarpay/nix/blob/master/home/vim/default.nix), my [emacs here](https://github.com/jonascarpay/nix/blob/master/desktop/emacs/default.nix), and the code itself [here](https://github.com/jonascarpay/nix/blob/master/lib/editor-config.nix).
