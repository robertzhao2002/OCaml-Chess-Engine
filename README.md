# Text-based Chess Engine #

## About ##

This is a text-based Chess game engine that was created using OCaml.  

## Installation ##

Please go [here](INSTALL.md) for installation instructions. 

## Pieces ##

### White Pieces ###

- `♔ - White King`
- `♕ - White Queen`
- `♖ - White Rook`
- `♗ - White Bishop`
- `♘ - White Knight`
- `♙ - White Pawn`

### Black Pieces ###

- `♚ - Black King`
- `♛ - Black Queen`
- `♜ - Black Rook`
- `♝ - Black Bishop`
- `♞ - Black Knight`
- `♟ - Black Pawn`

## Features ##

- Text interface using UTF-8 characters of each piece
- **Timed Mode**: Each side has a timer of their total time, which deducts the time taken on each turn. If a player exceeds the time limit on their turn, they automatically lose. The total time taken by a player's moves (accumulative) must not exceed that initial time.  
- **Casual Mode**: No timer.
- Pre-made board setups

## Creating Board Setups ##

You can create certain board setups by entering a series of moves into a `.txt` file and placing it into the `\data` folder. Each board will start with the initial setup. Add valid moves to move pieces. The final setup of the board is after all of the inputted moves have been made.

Move notations use the standard Chess coordinates (e.g. `e2 e4`). An invalid move (i.e. an illegal move, a coordinate with no piece on it, etc. ) will not work. Separate each move with a new line (1 move per line).

## Before the Game Begins ##

### Load Existing Board Setup ###

Type the name of your text file in the `data` folder without the `.txt` extension. It will automatically check if the moves are valid. If you want to make a subfolder inside `data`, type the name of the sub-folder, foward-slash, and the file name without the `.txt` extension: `[subfolder-name]/[name]`.

### Normal Game ###

`regular`, or any case-insensitive form of the word, will start a normal game of Chess.

### Timed/Untimed Mode ###

Enter a time in minutes or type `casual` or `untimed` for untimed mode.

## Commands ##

### Moves ###

Moves are determined by a start and end position on the board using standard coordinates. Each coordinate must have the form `^([a-h]|[A-H])([1-8]$)`. They must be 2 characters. The first must be a letter between `a` and `h`, inclusive (case insensitive). The second must be a number between `1` and `8`, inclusive.

Each move must have 2 coordinates separated by space(s). The engine will determine whether a valid move was made and make the move if it is valid. If it is not valid, an error message will be displayed, and you will continue to be prompted for a valid move. In timed mode, your timer will continue to run.

### Draw ###

`draw`, or any case-insensitive form of the word, will propose a draw to the opposition. Any case-insensitive form of these words are valid inputs.

- `yes` to accept the draw
- `no` to reject the draw

If the opposition agrees, the game ends in a draw. If they do not agree, the game continues with your current turn.

### Help ###

`help`, or any case-insensitive form of the word, will display a help menu. You may need to scroll up a bit, since it will display above the current board. It will come in handy if you are stuck.  

### Score ###

`score`, or any case-insensitive form of the word, will display each side's score in this fashion. Each piece's value is the standard Chess piece point-value. Capturing that piece will add that number of points to the opposition.  

`White: [White's score]`

`Black: [Black's score]`

### Undo ###

`undo`, or any case-insensitive form of the word, will go to the previous turn.

### Reset ###

`reset`, or any case-insensitive form of the word, will reset the current game with the same conditions (e.g. same initial timer values).

## Quit ###

`quit`, or any case-insensitive form of the word, will immediately end the game.