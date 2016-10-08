Windows environment variables
=============================

A collection of simple utilities to manage Windows environment variables,
created mainly to:

* learn a bit of Haskell,
* make it easier to add directories to the `PATH` variable, list missing
directories in your `PATH`, etc.

Building
--------

Using [stack]:

```
> stack setup
...

> stack build
...
```

[stack]: http://docs.haskellstack.org/en/stable/README/

Installation
------------

```
> stack install
...
```

Usage
-----

The complete list of utilities is given below.

* [list_path] &mdash; List directories in your `PATH`.
* [add_path] &mdash; Add directories to your `PATH`.
* [remove_path] &mdash; Remove directories from your `PATH`.
* [set_env] &mdash; Assign values to environment variables.
* [unset_env] &mdash; Delete environment variables.

Pass the `--help` flag to an utility to examine its detailed usage information.
Some examples are given below.

[list_path]: #list_path
[add_path]: #add_path
[remove_path]: #remove_path
[set_env]: #set_env
[unset_env]: #unset_env

### list_path

List directories in your `PATH`:

```
> list_path
C:\Program Files\Haskell\bin
C:\Program Files\Haskell Platform\8.0.1\lib\extralibs\bin
C:\Program Files\Haskell Platform\8.0.1\bin
C:\Users\Egor\AppData\Roaming\local\bin
C:\Users\Egor\AppData\Roaming\cabal\bin
...
```

Only list missing directories in your `PATH`:

```
> list_path --missing
C:\Users\Egor\AppData\Roaming\cabal\bin
...
```

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

Add "C:\test" to the global `PATH`, skipping the confirmation prompt:

```
> add_path --global -y C:\test
```

### remove_path

Remove "C:\test" from current user's `PATH`:

```
> remove_path C:\test
Saving variable 'PATH' to 'HKCU\Environment'...
        Old value: C:\Users\Egor\AppData\Roaming\local\bin;C:\Users\Egor\AppData\Roaming\cabal\bin;C:\test
        New value: C:\Users\Egor\AppData\Roaming\local\bin;C:\Users\Egor\AppData\Roaming\cabal\bin
```

Remove "C:\test" from both current user's and the global `PATH`s, skipping the
confirmation prompt:

```
> remove_path --global -y C:\test
```

### set_env

Assign `bar` to the variable `foo` in current user's environment, skipping the
confirmation prompt:

```
> set_env -y foo bar
```

Assign `bar` to the variable `foo` in the global environment:

```
> set_env --global foo bar
Saving variable 'foo' to 'HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment'...
        Value: bar
Continue? (y/n) y
```

### unset_env

Delete the variable `foo` from current users's environment, skipping the
confirmation prompt:

```
> unset_env -y foo
```

Delete the variable `foo` from the global environment:

```
> unset_env --global foo
Deleting variable 'foo' from 'HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment'...
Continue? (y/n) y
```

License
-------

Distributed under the MIT License.
See [LICENSE.txt] for details.

[LICENSE.txt]: LICENSE.txt
