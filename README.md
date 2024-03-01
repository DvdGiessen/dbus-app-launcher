# dbus-app-launcher

A simple service that allows executing a program via D-Bus.

Note: Allowing D-Bus clients to execute arbitrary programs may pose a security
risk. Use carefully.

## Installation

To build the code, you will need Haskell and the `stack` command line utility.
Get it [here](https://docs.haskellstack.org/en/stable/).

To build and install, run `stack install`. This will build the code and copy the
executable to `~/.local/bin`.

Next, set up a D-Bus service description that will automatically launch the new
service whenever it is accessed:

`/usr/share/dbus-1/services/nl.dvdgiessen.dbusapplauncher.service`

```ini
[D-BUS Service]
Name=nl.dvdgiessen.dbusapplauncher
Exec=/home/USERNAME/.local/bin/dbus-app-launcher
```

## Using the service

The service currently exports one interface, `Exec`. Calling any of the methods
of this interface will start the program using the `exec()` syscall.

- Service name: `nl.dvdgiessen.dbusapplauncher`
- Object path: `/nl/dvdgiessen/DBusAppLauncher`
- Interface: `nl.dvdgiessen.dbusapplauncher.Exec`
- Methods:
  - `Cmd(String cmd)`
  - `CmdArgs(String cmd, List<String> args)`
  - `CmdArgsEnv(String cmd, List<String> args, Map<String,String> env)`

## Issues and contributing

This is my first Haskell program, so there's probably plenty that could be
improved upon. Feel free to fork it or open pull requests if you find a bug.

I might add more functionality in the future, for example for running programs
and returning their exit code and stdout/stderr output. Currently my own use
case for this program is for starting applications from KWin scripts (see
[kwin-toggleterminal](https://github.com/DvdGiessen/kwin-toggleterminal) if
you're interested in that sort of thing), so adding more functionality probably
won't be a priority until I have a need for it myself.

## License

`dbus-app-launcher` is freely distributable under the terms of the
[MIT license](https://github.com/DvdGiessen/dbus-app-launcher/blob/master/LICENSE).
