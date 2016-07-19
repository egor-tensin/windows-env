Windows environment variables
=============================

A collection of simple programs to manage environment variables on Windows,
created mainly to:

* learn a bit of Haskell,
* make it easier to add paths to the `PATH` variable, automatically setup
`_NT_SYMBOL_PATH`, etc.

Building
--------

Using [stack]:

```
> stack setup
...

> stack build
```

[stack]: http://docs.haskellstack.org/en/stable/README/

Installation
------------

```
> stack install
```

Usage
-----

The complete list of utilities is given below.

* [list_path] &mdash; List directories in your `PATH`.
* [add_path] &mdash; Add directories to your `PATH`.
* [remove_path] &mdash; Remove directories from your `PATH`.
* [set_env] &mdash; Set environment variables.
* [unset_env] &mdash; Unset environment variables.

Pass the `--help` flag to a utility to examine its detailed usage information.
Some examples are given below.

[list_path]: #list_path
[add_path]: #add_path
[remove_path]: #remove_path
[set_env]: #set_env
[unset_env]: #unset_env

### list_path

List directories in your `PATH` variable:

```
> list_path
- C:\Program Files\Haskell\bin
+ C:\Program Files\Haskell Platform\8.0.1\lib\extralibs\bin
+ C:\Program Files\Haskell Platform\8.0.1\bin
+ C:\Users\Egor\AppData\Roaming\local\bin
- C:\Users\Egor\AppData\Roaming\cabal\bin
...
```

Lines starting with `+` denote existing directories.
Conversely, lines starting with `-` denote missing directories.

### add_path

Add "C:\test" to current user's `PATH`:

```
> add_path C:\test
Saving variable 'PATH' to 'HKCU\Environment'...
        Old value: C:\Users\Egor\AppData\Roaming\local\bin;C:\Users\Egor\AppData\Roaming\cabal\bin
        New value: C:\Users\Egor\AppData\Roaming\local\bin;C:\Users\Egor\AppData\Roaming\cabal\bin;C:\test
Continue? (y/n) y

>
```

### remove_path

Remove "C:\test" from both current user's and global `PATH`s, skipping the
confirmation prompt:

```
> remove_path --global -y C:\test
```

### set_env

Assign `bar` to the variable `foo` for all users:

```
> set_env -g foo bar
Saving variable 'foo' to 'HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment'...
        Value: bar
Continue? (y/n) y

>
```

### unset_env

Unset the variable `foo` for current user, skipping the confirmation prompt:

```
> unset_env --yes foo
```

License
-------

This project, including all of the files and their contents, is licensed under
the terms of the MIT License.
See [LICENSE.txt] for details.

[LICENSE.txt]: LICENSE.txt
