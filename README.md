# SimpleMenu

Minimalistic menu launcher for desktop. 

Heavy inspired by menu of IceWm windows manager, but enhanced by ideas from some other sources.

Principles and ideas:
  - small and friendly for system resources
  - no process run on background
  - easy and intuitive keyboard controll
  - no unnecessary balast and features

# Command line

```
    Usage: simpleMenu -(f|p|m|w) "menu file or cmd" [options...]
             one of -f/-p/-m/-w must be specified as start point for menu
        -h --help             show this help
        -k --keep             keep menu open after choise
        -f X --file=X         path to menu file used as start point for menu
        -m X --menuitem=X     text content of menu
        -p X --process=X      command for generate menu
        -s X --search=X       count of menu items for automatic enable find
        -q X --query=X        automatic enable find entry and fill start query
        -r X --reload=X       dynamic menu with minimal chars for search
        -x X --showfile=X     extra options for menu cmd
        -w --windowmenu       window menu
        -1 X -execone=X       automatic execute if matched only one item, execute X if 0 items found or append X to menu

        
```

# Keyboard shortcut

TODO

# Search in menu

TODO

# Syntax of menu file

TODO MITprog, MITmenu, MITrunonce, MITmenufile, MITmenuprog, MITmenuprogreload, MITseparator, MITEndMenu, MITNone

Base syntax:
```
menu NAME {
  prog NAME COMMANDLINE
}
```

TODO

- `NAME` - name of menuitem, submenu... Can be with space with `"` 
  (ie: "name with space"). With first character `_` explicitly make shortcut (ie: `na_me`
  will show `name` ans `m` as shortcut. Shortcut will be automatic append if
  user explicitly not write it. For off automatic insert shortcut, can be use
  as shortcut space (ie: `"name_ "`). 
  
  In name of separator and prog can use `%clipbrd%` as placeholder for text
  from system clipboard.
  
- `COMMANDLINE` - external program, which write menu structure to its standard output.
  
## Dynamic submenu

```
menuprogreload NAME INTERVAL COMMANDLINE
```

- `INTERVAL` - count of seconds for rebuild menu. 
  - With positive value submenu will be simly rebuilded if is newly opened
    after count of seconds from last build.
  - With negative value submenu will be opened with search edit and will be
    rebuilded after search pattern is changed. Absolute value of `INTERVAL` is
    minimal count characters for execute build (usable for search in realy big
    count of items). Placeholder `%s` in commandline will be substitute by
    search pattern.
    
- `COMMANDLINE` - external program, which write menu structure to its standard output.
