# 蛤兄弟: simplistic hexdump processor and editor

## Features

`haxd dump`: dumps the input into a `xxd`-like hexdump
format.

`haxd load`: loads the format produced by `dump`, convert
it back into binary. Note that it currently ignores the
offset part on the left and the ASCII preview part on
the right.

`haxd edit`: summons your `$EDITOR` to edit a binary file
as the hexdump format. Same as above, the preview part of
the dump are currently ignored.

See `haxd [subcommand] --help` for possible flags and
arguments.

## License

`haxd` is distributed under the terms of the GNU
AGPL. See [LICENSE](LICENSE) for details.

