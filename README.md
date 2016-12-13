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

* [paths] &mdash; List directories in your `PATH`.
* [addpath] &mdash; Add directories to `PATH`.
* [delpath] &mdash; Remove directories from `PATH`.
* [setenv] &mdash; Assign values to environment variables.
* [delenv] &mdash; Delete environment variables.

Pass the `--help` flag to an utility to examine its detailed usage information.
Some examples are given below.

[paths]: #paths
[addpath]: #addpath
[delpath]: #delpath
[setenv]: #setenv
[delenv]: #delenv

### paths

List directories in your `PATH`:

```
> paths
C:\Program Files\Haskell\bin
C:\Program Files\Haskell Platform\8.0.1\lib\extralibs\bin
C:\Program Files\Haskell Platform\8.0.1\bin
C:\Users\Egor\AppData\Roaming\local\bin
C:\Users\Egor\AppData\Roaming\cabal\bin
...
```

Only list missing directories in your `PATH`:

```
> paths --missing
C:\Users\Egor\AppData\Roaming\cabal\bin
...
```

### addpath

Add "C:\test" to current user's `PATH`:

```
> addpath C:\test
Saving variable 'PATH' to 'HKCU\Environment'...
        Old value: C:\Users\Egor\AppData\Roaming\local\bin;C:\Users\Egor\AppData\Roaming\cabal\bin
        New value: C:\Users\Egor\AppData\Roaming\local\bin;C:\Users\Egor\AppData\Roaming\cabal\bin;C:\test
Continue? (y/n) y
```

Add "C:\test" to the global `PATH`, skipping the confirmation prompt:

```
> addpath --global -y C:\test
```

### delpath

Remove "C:\test" from current user's `PATH`:

```
> delpath C:\test
Saving variable 'PATH' to 'HKCU\Environment'...
        Old value: C:\Users\Egor\AppData\Roaming\local\bin;C:\Users\Egor\AppData\Roaming\cabal\bin;C:\test
        New value: C:\Users\Egor\AppData\Roaming\local\bin;C:\Users\Egor\AppData\Roaming\cabal\bin
```

Remove "C:\test" from both current user's and the global `PATH`s, skipping the
confirmation prompt:

```
> delpath --global -y C:\test
```

### setenv

Assign `bar` to the variable `foo` in current user's environment, skipping the
confirmation prompt:

```
> setenv -y foo bar
```

Assign `bar` to the variable `foo` in the global environment:

```
> setenv --global foo bar
Saving variable 'foo' to 'HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment'...
        New value: bar
Continue? (y/n) y
```

### delenv

Delete the variable `foo` from current users's environment, skipping the
confirmation prompt:

```
> delenv -y foo
```

Delete the variable `foo` from the global environment:

```
> delenv --global foo
Deleting variable 'foo' from 'HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment'...
Continue? (y/n) y
```

License
-------

Distributed under the MIT License.
See [LICENSE.txt] for details.

[LICENSE.txt]: LICENSE.txt
