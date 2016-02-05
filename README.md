Build with [stack](https://github.com/commercialhaskell/stack):

```
$ stack install
```

Run with options:

```
$ connect4 --help
connect4 - Play connect 4 from a command line interface

Usage: connect4 [--connect N] [-r|--rows ROWS] [-c|--cols COLS] [--ascii]
                [--ai DIFFICULTY]
  Change the connection length and board size

Available options:
  -h,--help                Show this help text
  --connect N              The number of pieces to connect to achieve victory
  -r,--rows ROWS           The number of rows for the board
  -c,--cols COLS           The number of columns for the board
  --ascii                  Draw an ascii board instead of unicode
  --ai DIFFICULTY          How difficult to make the computer player
```

Play against the computer:

```
...
╓───┬───┬───┬───┬───┬───┬───╖
║   │   │   │ ● │ ● │ ○ │   ║
╟───┼───┼───┼───┼───┼───┼───╢
║   │   │   │ ○ │ ○ │ ● │   ║
╟───┼───┼───┼───┼───┼───┼───╢
║   │   │ ○ │ ○ │ ● │ ○ │   ║
╟───┼───┼───┼───┼───┼───┼───╢
║   │   │ ● │ ○ │ ● │ ● │   ║
╟───┼───┼───┼───┼───┼───┼───╢
║   │   │ ○ │ ● │ ○ │ ○ │   ║
╟───┼───┼───┼───┼───┼───┼───╢
║ ● │ ○ │ ● │ ● │ ● │ ○ │   ║
╚═══╧═══╧═══╧═══╧═══╧═══╧═══╝
  a   b   c   d   e   f   g

○ wins!


```