# Tic Tac Toe Game

## Overview
This Tic Tac Toe game features a core implementation of the game logic created in **Beginner Student Language with Lambda**, adhering to the restriction of having only one expression per function. The full Racket language was utilized solely for server-related functionalities.

## Includes
- Full HTDF recipes for each function
- 3 Modes to play in (Online, Computer, Local)
- Computer Minimax AI with adjustable difficulty

## Download Links
You can download the executable files for both the server and client from the links below:

- [Download Server (Windows .exe)](https://github.com/Void4020/tic-tac-toe-multiplayer/blob/main/TicTacToeServer.zip)
- [Download Client (Windows .exe)](https://github.com/Void4020/tic-tac-toe-multiplayer/blob/main/TicTacToeClient.zip)
- [Download Server (Mac .dmg)](link-to-server-dmg)
- [Download Client (Mac .dmg)](link-to-client-dmg)

## Multiplayer Setup
To play multiplayer, ensure that you have set up port forwarding on your router. Here’s how it works:

1. **Public Access**: If you are hosting the server on your Local IP, it will be publicly accessible through your Public IP. However, **Port Forwarding** is needed to allow this.
   
2. **Port Forwarding**:
   - Forward a port using your router settings using your Local IP and an available port.
   - Once you have port forwarded a port on your router, you will need to enter your Local IP.
   - The Local IP of your machine can be found next to **IPv4** when the command `ipconfig` is run on Command Prompt.

3. **Starting The Server**: 
   - You will be prompted whether you want to have the server accessable through localhost (`127.0.0.1`) or your Public IP.
   - Lastly, you will have to enter the port number of the port that has been forwarded.

4. **Connecting The Client**
   - You will be prompted to enter the server's IP address.
   - If the server is on localhost, just enter localhost or `127.0.0.1`
   - If the server is accessable through a public IP, all you have to do is enter the server's **PUBLIC IP ADDRESS** (not local, that is hidden). It can be found anywhere online, e.g., [whatismyipaddress.com](https://whatismyipaddress.com)) along with the port that you ran the server on.

## Game Rules
The rules of Tic Tac Toe are as follows:
- The game is played on a 3x3 grid.
- Players take turns placing their mark (X or O) in an empty cell.
- The first player to get three of their marks in a row (vertically, horizontally, or diagonally) wins the game.
- If all cells are filled and no player has three in a row, the game ends in a draw.

## Contribution
Feel free to contribute to the project by submitting issues, suggestions, or pull requests.

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
