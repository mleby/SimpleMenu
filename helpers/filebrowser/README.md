# simplemenu file browser

Menu generator for browse filesystem for simplemenu.

# Install

Compile source code or Download compiled binary from [releases](https://github.com/martinlebeda/IceMenu/releases).

# Usage

You can use `--help`:

```
Usage of filebrowser:
  -dir string
    	directories for menu, : is separator (default "$HOME")
  -norecur
    	prevent recursive directories 
  -open string
    	open script for files (default "xdg-open")
  -suffix string
    	include only suffixes, default is all, : is separator
```

example usage in `~/.icewm/menu` (if `filebrowser` is in `$PATH`):
```
menuprogreload "Home" folder 10 /home/martin/bin/filebrowser -dir /home/martin
menuprogreload Podcasts podcast.png 0 filebrowser -dir /home/martin/Mobile/ExtraPodcast:/home/martin/Video/other:/home/martin/Downloads -suffix .avi:.mp4:.flv:.webm:.mkv:.ts:.mp3 -open /home/martin/bin/playAndDelete.sh -norecur
```

# Configuration

## Additional static menu

If exists file `$HOME/.icewm/filebrowser/static`, their content is use as first items, `%s` is changed to current path.
As local menu is used `.filebrowser/static` in local directory and `.filebrowser_recursive/static` for local directory and all subdirectories.

Example:
```
prog "Commander"  doublecmd -T Stažené '%s'
prog "Terminál"  sakura -r 40 -c 120 --working-directory='%s'
prog "Thunar" thunar '%s'
```

## Additional dynamic menu

All files in `$HOME/.icewm/filebrowser/`, `.filebrowser/` and `.filebrowser_recursive/` except `static` are used if their names is contained in current directory.
Then `_` is used as wildchar.

Example:
- `~/.icewm/filebrowser/.git` used if current directory contain `.git` file or directory
- `~/.icewm/filebrowser/_mp3` used if current directory contain `*mp3` file or directory


# Future plans 

in unspecified future/maybe if I get feel or I need it:

- work with submenu generated at once with main (for files)
- insert param for switch hiden files
- some other functionality...
