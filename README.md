Install dependencies and build:

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

Run with options:

```
$ dist/build/connect4/connect4 --help
connect4 - Play connect 4 from a command line interface

Usage: connect4 [--connect N] [-r|--rows ROWS] [-c|--cols COLS] [--ascii]
  Change the connection length and board size

Available options:
  -h,--help                Show this help text
  --connect N              The number of pieces to connect to achieve victory
  -r,--rows ROWS           The number of rows for the board
  -c,--cols COLS           The number of columns for the board
  --ascii                  Draw an ascii board instead of unicode
```

Play the game:

```
...
╓───┬───┬───┬───┬───┬───┬───╖
║   │   │   │   │   │   │   ║
╟───┼───┼───┼───┼───┼───┼───╢
║   │   │ ○ │   │   │   │   ║
╟───┼───┼───┼───┼───┼───┼───╢
║   │   │ ○ │ ○ │ ● │   │   ║
╟───┼───┼───┼───┼───┼───┼───╢
║   │ ● │ ○ │ ● │ ○ │   │   ║
╟───┼───┼───┼───┼───┼───┼───╢
║   │ ● │ ● │ ○ │ ● │ ○ │ ● ║
╟───┼───┼───┼───┼───┼───┼───╢
║   │ ○ │ ○ │ ● │ ● │ ○ │ ● ║
╚═══╧═══╧═══╧═══╧═══╧═══╧═══╝
  a   b   c   d   e   f   g  

○ wins!

```