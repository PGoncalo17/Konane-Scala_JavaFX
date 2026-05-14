package Project

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.util.Try
import java.io.{PrintWriter, File}
import scala.io.Source

import Project.MyRandom 
import scala.io.StdIn.readLine

object TUI {
    //show initial prompt
    def showInitialPrompt(): Unit = { //Initial Prompt
        println("\n(d)imension, (s)tart game or (q)uit.")
    }

    //show prompt during game
    def showPromptInGame(): Unit = {
        println("\n(r)estart the game, (u)ndo or q(uit).")
    }

    //get user's input
    def getUserInput(): String = readLine.trim.toUpperCase
    
    //build a single line   
    def buildLine(line:Int, col:Int, maxCols:Int, board:Board): String = {
        (col >= maxCols) match {
            case true => ""  //end of line
            case false =>        
                val symbol = board.get((line, col)) match {     //choose symbol for each piece
                    case None => "   "
                    case Some(Stone.White) => " W "
                    case Some(Stone.Black) => " B "
                }
                symbol + buildLine(line, col + 1, maxCols, board)  //next colunm
        }
    }
    //build all matrix
    def buildAllLines(line:Int, maxLines:Int, maxCols:Int, board:Board): String = {
        (line >= maxLines) match {
            case true => ""     //end of colunm
            case false =>
                val lineSymbols = buildLine(line, 0, maxCols, board)        //build line
                val lineLable = if(line < 10) " " + line.toString + " " else line.toString + " "    //fix spaces for numbers < 10
                lineLable + lineSymbols + "\n" + buildAllLines(line + 1, maxLines, maxCols, board)  //next line
        }
    }

    //print board
    def printBoard(board: Board, lines: Int, cols: Int): Unit = {
        //aux recursive function to create a list of numbers for each colunm
        def buildRange(n: Int): List[Int] = {
            if(n >= cols) Nil else n::buildRange(n + 1)
        }
        val colIndexes = buildRange(0)      //Create list of numbers for each colunm
        val headderLetters = (colIndexes foldRight ("")){ (c, acc) =>   //foldRight to put numbers in right place
            val letter = (c < 10) match {
                case true => s" $c " 
                case false => s"$c "
            }
            letter + acc
            }
        println("   " + headderLetters)
        println(buildAllLines(0, lines, cols, board))       //build all board
    }

    //print game over
    def printGameOver(): Unit = println("\n=== GAME OVER ===")

    //print program over
    def printProgramOver(): Unit = println("\n== PROGRAM OVER===")

        // Main Loop 
    @tailrec
    def mainLoop(gameState: GameState, r: RandomWithState): Unit = {
        showInitialPrompt()                                                                                   // Creates prompt asking dimension, start or quit to the user

        // Everytime the loop starts, saves the seed
        Konane.saveSeed(r.seed)
        val userInput = getUserInput()                                                                        // Reads the input from user

        userInput match {
            case "D" | "d" =>                                                                                 // User is choosing dimension
                println("Lines: ")                                                                            // Asks user to choose number of lines
                val l = Try(scala.io.StdIn.readLine().toInt).getOrElse(-1)                                                              // Stores variable with the desired amount of lines
                println("Columns: ")                                                                          // Asks user to choose number of columns
                val c = Try(scala.io.StdIn.readLine().toInt).getOrElse(-1)                                                              // Stores variable with the desired amount of columns
                
                if ((l > 0 && c > 0 && (l % 2 != 0 && c % 2 != 0)) || (l <= 0 || c <= 0)) {                   // Rules of dimension       
                    println("Invalid dimension. Try again!")                                                  // If it doesn't follow the rules, it's invalid
                    mainLoop(gameState, r)                                                                    // Restarts the main loop
                } else {                                                                                        
                    println("Dimension accepted")                                                           
                    mainLoop(gameState.copy(lines = l, cols = c), r)                                          // Starts the board with the desired lines and columns
                }

            case "S" | "s" =>                                                                                 // User wants to start the game
               if (gameState.lines > 0 && gameState.cols > 0) {                                               // If the dimension is correctly chosen
                    val (board, newR, initialLstOpenCoords) = Konane.initBoard(gameState.lines, gameState.cols, r)   // Initialize the board with the dimensions
                    println("\n--- GAME START --- \nBlack's Turn (You)")
                    printBoard(board, gameState.lines, gameState.cols)                                        // Prints the board in the Terminal
                    gameLoop(board, initialLstOpenCoords, Stone.Black, newR, gameState.lines, gameState.cols, Nil) // Starts the game loop
                } else {
                    println("Invalid Dimension. Create it in (d)imension first")                              // Invalid Dimension
                    mainLoop(gameState, r)                                                                    // Restarts main loop
                }

            case "Q" | "q" =>                                                                                 // User wants to quit 
                printProgramOver()
                sys.exit(0) 


            case _ =>                                                                                         // User types any different input
                println("Wrong input. Try again")                                                             
                mainLoop(gameState, r)                                                                        // Restarts main loop
        }
    }

    // Game Loop
    @tailrec                                                                                                           // history to undo
    def gameLoop(board: Board, lstOpenCoords: List[Coord2D], player: Stone, r: RandomWithState, lines: Int, cols: Int, history: List[(Board, List[Coord2D])]): Unit = {
        
        if (Konane.validTargets(board, lstOpenCoords, player).isEmpty) {                                             // If there are no more available plays
            val winner = if (player == Stone.Black) "White (CPU)" else "Black (You)"                          // Chooses the winner and stores it in a variable
            println(s"\nNo more moves for ${player}. $winner WINS!")                                          
            printGameOver()
            mainLoop(GameState(lines, cols, board, lstOpenCoords), r)                                         // Restarts main loop                                                                                           // Stops the execution
        }

        if (player == Stone.Black) {                                                                          // If it's user's turn
            println(s"\n--- YOUR TURN (Black) ---")

            showPromptInGame()
            println("Enter your move (format: rowFrom colFrom rowTo colTo):")
            
            val input = scala.io.StdIn.readLine().trim                                            // Reads the input and stores it in a variable

            

            input match {
                case "Q" | "q" =>                                                                 // If input is q
                    println("Quiting the game...")
                    mainLoop(GameState(lines, cols), r)                                           // Returns to main loop

                case "R" | "r" =>                                                                 // If input is r
                    println("Restarting the game...")
                    val (newBoard, newR, newLstOpenCoords) = Konane.initBoard(lines, cols, r)             // Initiates the board with the lines ans cols already given
                    printBoard(newBoard, lines, cols)                                                     // Prints the board
                    gameLoop(newBoard, newLstOpenCoords, Stone.Black, newR, lines, cols, Nil)             // Starts the game loop(Black's first)

                case "U" | "u" =>
                    history match {                                                                     
                        case Nil =>                                                                 // Case there are no undos to do
                            println("No moves to undo.")
                            gameLoop(board, lstOpenCoords, player, r, lines, cols, Nil)
                        
                        case (previousBoard, previousOpenCoords) :: olderHistory =>                 // Case there are undos to do(there is a previous board)
                            println("Undoing last move...")
                            printBoard(previousBoard, lines, cols)                                  // Prints the previous board
                            // Voltamos ao turno do jogador humano (Black) com o estado anterior
                            gameLoop(previousBoard, previousOpenCoords, Stone.Black, r, lines, cols, olderHistory)  // Continues gameLoop from previous board
                    }

                case other =>
                    val input = other.split(" ")
                
                    if (input.length == 4) {                                                                          // If user's input is correct
                        val from = (input(0).toInt, input(1).toInt)                                                   // First 2 numbers are the piece he wants to move
                        val to = (input(2).toInt, input(3).toInt)                                                     // Last 2 numbers are the place where he wants to move the selected piece
                        
                        val (resultBoard, nextLstOpenCoords) = Konane.play(board, player, from, to, lstOpenCoords)              // Makes the play
                        
                        resultBoard match {                                                                           
                            case Some(newBoard) =>                                                                    // If any play was made 
                                println(s"\nYou played from $from to $to.")
                                printBoard(newBoard, lines, cols)                                                     // Prints updated board
                                
                                val (finalBoard, finalOpenCoords, finalR) = handleJumpChoice(newBoard, to, player, nextLstOpenCoords, r, lines, cols)   // Handles the possibility of various jumps from the user
                                gameLoop(finalBoard, finalOpenCoords, Stone.White, finalR, lines, cols, (board, lstOpenCoords)::history)               // Updates game loop for the next turn
                                
                            case None =>                                                                              // If you don't choose a valid move
                                println("Invalid Move. Please try again.")                                     
                                gameLoop(board, lstOpenCoords, player, r, lines, cols, history)                                // Resets the game loop to previous state
                        }
                    } else {                                                                                          // If user's input is incorrect
                        println("Invalid input format. Use four numbers separated by spaces.")                        
                        gameLoop(board, lstOpenCoords, player, r, lines, cols, history)                                        // Resets game loop to previous state
                    }
            }
        } else {                                                                                              // If it's the CPU's turn
            val (resultBoard, newR, nextLstOpenCoords, coordTo) = Konane.playRandomly(board, r, player, lstOpenCoords, Konane.randomMove)    // Plays randomly and stores the changes in variables
            
            resultBoard match {     
                case None =>                                                                                  // If CPU can't play
                    println(s"\nNo more moves for White (CPU). YOU WIN!")
                    printGameOver()
                    mainLoop(GameState(lines, cols, board, lstOpenCoords), newR)                              // Resets the main loop to the begining
                case Some(newBoard) =>                                                                        // If CPU plays
                    println(s"\nCPU (White) played and finished at ${coordTo.getOrElse("?")}.")
                    printBoard(newBoard, lines, cols)
                    gameLoop(newBoard, nextLstOpenCoords, Stone.Black, newR, lines, cols, history)                     // Uptades game loop for the next turn
            }
        }
    }

   // Handles multiples jumps from user
    @tailrec
    def handleJumpChoice(board: Board, currentPos: Coord2D, player: Stone, lstOpenCoords: List[Coord2D], r: RandomWithState, lines: Int, cols: Int): (Board, List[Coord2D], RandomWithState) = {
        if (!Konane.canStillJump(board,player, currentPos, lstOpenCoords)){                                                                 // If doesn't have any jumps left
            (board, lstOpenCoords, r)                                                                                                       // Returns board
        } else {                                                                                                                            // If has more jumps to do
            println(s"Multi-jump available for piece at $currentPos! Do you want to jump again? (format: rowTo colTo or 'n' to stop)")
            val input = scala.io.StdIn.readLine().trim.toLowerCase                                                                          // Reads the input

            if (input == "n" ){                                                                                                             // If player chooses not to jump
                (board, lstOpenCoords, r)                                                                                                   // Returns board
            } else {
                val parts = input.split(" ")                                                                                                // Splits the space in the input
                if(parts.length == 2) {                                                                                                     // Check if input is valid
                    val nextTo = (parts(0).toInt, parts(1).toInt)
                    val (newBoardOpt, newLstOpenCoords) = Konane.handleUserJumps(board, currentPos, nextTo, player, lstOpenCoords)          // Tries to make the jump

                    newBoardOpt match {
                        case Some(newBoard) =>                                                                                              // If the play was successfull
                            printBoard(newBoard,lines, cols)                                                                                // Prints the board
                            handleJumpChoice(newBoard, nextTo, player, newLstOpenCoords, r, lines, cols)                                    // Calls the function recursively 
                        case None =>
                            println("Invalid multi-jump target. Try again or type 'n'.")                                                    // Case the play was invalid
                            handleJumpChoice(board, currentPos, player, lstOpenCoords, r, lines, cols)                                       // Calls the function recursively
                    }
                } else {                                                                                                                    // If the input is incorrect
                    print("Invalid format. Use: rowTo colTo")
                    handleJumpChoice(board, currentPos, player, lstOpenCoords, r, lines, cols)                                               // Calls the function recursively
                }
            }
        }
    }
}