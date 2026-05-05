package Project

import Project.Utils.*
import Project.MyRandom
import scala.annotation.tailrec
import scala.util.Try
import java.io.{PrintWriter, File}
import scala.io.Source

def saveSeed(seed:Long):Unit = {
    val writer = new PrintWriter(new File("Seed.txt"))
    writer.write(seed.toString())
    writer.close
}

def loadSeed():Long = {
    // Tryes to read de file. If doesn´t exists, chooses a random number that we chose(42) 
    val source = Try(Source.fromFile("seed.txt"))
    val seed = source.map(_.getLines().next().trim.toLong).getOrElse(42L)
    source.foreach(_.close()) // Closes the file
    seed
}

type Coord2D = (Int, Int)
type Board = Map[Coord2D, Stone]

enum Stone:
    case Black, White

case class GameState(lines: Int, cols: Int, board: Board = Map(), lstOpenCoords: List[Coord2D] = Nil)

object Konane extends App {

    // Loads the seed from the file
    val initialSeed = loadSeed()
    val r = MyRandom(initialSeed)

    mainLoop(GameState(0, 0), r)                                                                              // Responsible for keeping the game going

    // Main Loop 
    @tailrec
    def mainLoop(gameState: GameState, r: RandomWithState): Unit = {
        showInitialPrompt()                                                                                   // Creates prompt asking dimension, start or quit to the user

        // Everytime the loop starts, saves the seed
        saveSeed(r.seed)
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
                    val (board, newR, initialLstOpenCoords) = initBoard(gameState.lines, gameState.cols, r)   // Initialize the board with the dimensions
                    println("\n--- GAME START --- \nBlack's Turn (You)")
                    printBoard(board, gameState.lines, gameState.cols)                                        // Prints the board in the Terminal
                    gameLoop(board, initialLstOpenCoords, Stone.Black, newR, gameState.lines, gameState.cols) // Starts the game loop
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
    @tailrec
    def gameLoop(board: Board, lstOpenCoords: List[Coord2D], player: Stone, r: RandomWithState, lines: Int, cols: Int): Unit = {
        
        if (validTargets(board, lstOpenCoords, player).isEmpty) {                                             // If there are no more available plays
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
                    val(newBoard, newR, newLstOpenCoords) = initBoard(lines, cols, r)             // Initiates the board with the lines ans cols already given
                    printBoard(newBoard, lines, cols)                                             // Prints the board
                    gameLoop(newBoard, newLstOpenCoords, Stone.Black, newR, lines, cols)          // Starts the game loop(Black's first)

                case other =>
                    val input = other.split(" ")
                
                    if (input.length == 4) {                                                                          // If user's input is correct
                        val from = (input(0).toInt, input(1).toInt)                                                   // First 2 numbers are the piece he wants to move
                        val to = (input(2).toInt, input(3).toInt)                                                     // Last 2 numbers are the place where he wants to move the selected piece
                        
                        val (resultBoard, nextLstOpenCoords) = play(board, player, from, to, lstOpenCoords)              // Makes the play
                        
                        resultBoard match {                                                                           
                            case Some(newBoard) =>                                                                    // If any play was made 
                                println(s"\nYou played from $from to $to.")
                                printBoard(newBoard, lines, cols)                                                     // Prints updated board
                                
                                val (finalBoard, finalOpenCoords, finalR) = handleUserJumps(newBoard, to, player, nextLstOpenCoords, r, lines, cols)   // Handles the possibility of various jumps from the user
                                gameLoop(finalBoard, finalOpenCoords, Stone.White, finalR, lines, cols)               // Updates game loop for the next turn
                                
                            case None =>                                                                              // If you don't choose a valid move
                                println("Invalid Move. Please try again.")                                     
                                gameLoop(board, lstOpenCoords, player, r, lines, cols)                                // Resets the game loop to previous state
                        }
                    } else {                                                                                          // If user's input is incorrect
                        println("Invalid input format. Use four numbers separated by spaces.")                        
                        gameLoop(board, lstOpenCoords, player, r, lines, cols)                                        // Resets game loop to previous state
                    }
            }
        } else {                                                                                              // If it's the CPU's turn
            val (resultBoard, newR, nextLstOpenCoords, coordTo) = playRandomly(board, r, player, lstOpenCoords, randomMove)    // Plays randomly and stores the changes in variables
            
            resultBoard match {     
                case None =>                                                                                  // If CPU can't play
                    println(s"\nNo more moves for White (CPU). YOU WIN!")
                    printGameOver()
                    mainLoop(GameState(lines, cols, board, lstOpenCoords), newR)                              // Resets the main loop to the begining
                case Some(newBoard) =>                                                                        // If CPU plays
                    println(s"\nCPU (White) played and finished at ${coordTo.getOrElse("?")}.")
                    printBoard(newBoard, lines, cols)
                    gameLoop(newBoard, nextLstOpenCoords, Stone.Black, newR, lines, cols)                     // Uptades game loop for the next turn
            }
        }
    }

   // Handles multiples jumps from user
    @tailrec
    def handleUserJumps(board: Board, currentPos: Coord2D, player: Stone, lstOpenCoords: List[Coord2D], r: RandomWithState, lines: Int, cols: Int): (Board, List[Coord2D], RandomWithState) = {
        if (canStillJump(board, player, currentPos, lstOpenCoords)) {                                         // If user can jump more than 1 time
            println(s"Multi-jump available for your piece at $currentPos. Do you want to keep on jumping? (Y/N)")             
            val ans = scala.io.StdIn.readLine().trim.toUpperCase                                              // Reads input from user (if he wants to continue jumping or not)
            
            if (ans == "Y") {                                                                                 // If he wants to continue jumping
                println("Where do you want to go? (format: rowTo colTo):")
                val input = scala.io.StdIn.readLine().trim.split(" ")                                         // Stores the desired destination in a variable
                
                if (input.length == 2) {                                                                      // If user's input is correct
                    val to = (input(0).toInt, input(1).toInt)                                                 // Coordinates of the destiny he chose
                    val (resultBoard, nextLstOpenCoords) = play(board, player, currentPos, to, lstOpenCoords) // Makes the play
                    
                    resultBoard match {             
                        case Some(newBoard) =>                                                                // If the play happened
                            println(s"\nJumped to $to.")
                            printBoard(newBoard, lines, cols)
                            handleUserJumps(newBoard, to, player, nextLstOpenCoords, r, lines, cols)          // Checks, recursively, if he has any more jumps available
                        case None =>                                                                          // If the coordinates chose where invalid
                            println("Invalid jump! Try again.")
                            handleUserJumps(board, currentPos, player, lstOpenCoords, r, lines, cols)         // Asks to jump again, choosing an available position
                    }
                } else {                                                                                      // If user's input was incorrect
                    println("Invalid input. Format: rowTo colTo")
                    handleUserJumps(board, currentPos, player, lstOpenCoords, r, lines, cols)                 // Asks to jump again, following the cordinate's rules
                }
            } else if(ans == "N"){                                                                            // If answer is no
                (board, lstOpenCoords, r)                                                                     // Returns the previous board, because user doesn't want to jump                                                           
            } else {
                println("Invalid input. Try again")                                                           // If answer is an invalid input
                handleUserJumps(board, currentPos, player, lstOpenCoords, r, lines, cols)                     // Asks again
            }
        } else {
            (board, lstOpenCoords, r)                                                                         // Returns the previous board, because user can't jump                                                       
        }
    }

    // Create random move
    def randomMove(lstOpenCoords: List[Coord2D], rand: RandomWithState): (Coord2D, RandomWithState) = {
        val (randPos, newRand) = rand.nextInt(lstOpenCoords.length)  // Choose random pos in lstOpenCoords
        val chosenCoord = lstOpenCoords(randPos)                     // Gets the correspondent coord
        (chosenCoord, newRand) 
    }

    // Make a play
    def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
        val coordMiddle = ((coordFrom._1 + coordTo._1)/2, (coordFrom._2 + coordTo._2)/2)        // Gets the middle position
        
        validPlay(board, coordFrom, coordTo, coordMiddle, player, lstOpenCoords) match {        
            case false => (None, lstOpenCoords)                                                 // If it's not a valid play, it doesn't make a play
            case true =>                                                                        // If it's a valid play
                val newBoard = board - coordFrom - coordMiddle + (coordTo -> player)            // Updates the board
                val addToLstOpenCoords = coordFrom :: coordMiddle :: lstOpenCoords              // Adds coordFrom and coordMiddle to lstOpenCoords
                val removeToOpenCoords = addToLstOpenCoords.filter(c => c != coordTo)           // Remove coordTo from lstOpenCoords
                (Some(newBoard), removeToOpenCoords)                                            // Returns the updated board     
        }
    }

    // Checks if a play can be made
    def validPlay(board: Board, coordFrom: Coord2D, coordTo: Coord2D, coordMiddle: Coord2D, player: Stone, lstOpenCoords: List[Coord2D]): Boolean = {
        val range = Math.abs(coordFrom._1 - coordTo._1) + Math.abs(coordFrom._2 - coordTo._2)   // Gets the range distance from the initial place to the desired place
        val opponent = if(player == Stone.Black) Stone.White else Stone.Black                   // Gets the oponnent piece     

        val rangeOk = range == 2                                                                // If the range is exactly 2 it's correct            
        val validStoneToPlay = board.get(coordFrom) == Some(player)                             // Checks if the originals coords correspond to a player's piece
        

        val validPosToGo = lstOpenCoords.contains(coordTo)                                      // Checks if the desired coords are inside the board                                         
        
        val validOppMid = board.get(coordMiddle) == Some(opponent)                              // Checks if the middle piece is an opponents piece
        val straightLine = (coordFrom._1 == coordTo._1 || coordFrom._2 == coordTo._2)           // Checks if it play vertically or horizontally

        rangeOk && validStoneToPlay && validPosToGo && validOppMid && straightLine              // If every condition defined above is True, then it's a valid play
    }

    def playRandomly(board: Board, r: RandomWithState, player: Stone, lstOpenCoords: List[Coord2D], f: (List[Coord2D], RandomWithState) => (Coord2D, RandomWithState)): (Option[Board], RandomWithState, List[Coord2D], Option[Coord2D]) = {   
        val validCoords = validTargets(board, lstOpenCoords, player)                                          // Gets possible coords to go to(Possible coordTo)

        if (validCoords.isEmpty){
            (None, r, lstOpenCoords, None)
        } else{
            validCoords match {
                case Nil => (None, r, lstOpenCoords, None)                                                        // If there are no possible coords to go to, ends game
                case _ =>                                                                                         // Chooses randomly a coordTo with the f function(Random)
                    val (chosenCoordTo, newR1) = f(validCoords, r)                                                // Finds a playable piece that can go to the coordTo
                    val playablePieces = validSources(board, chosenCoordTo, player, lstOpenCoords)                // Chooses randomly a coordFrom with the f function(Random)
                    val (chosenCoordFrom, newR2) = f(playablePieces, newR1)
                    
                    println(s"\nCPU (White) jumped from $chosenCoordFrom to $chosenCoordTo")
                    val (firstPlayBoard, firstLstOpenCoords) = play(board, player, chosenCoordFrom, chosenCoordTo, lstOpenCoords)   //Makes the play

                    @tailrec    //Aux function to playRandomly, handle multiple jumps to Random
                    def processMultiJumps(currentBoard: Board, currentPos: Coord2D, currentOpen: List[Coord2D], currentR: RandomWithState): (Board, List[Coord2D], RandomWithState, Coord2D) = {
                        if (!canStillJump(currentBoard, player, currentPos, currentOpen)) {                                                     // If there are not more jumps available
                            (currentBoard, currentOpen, currentR, currentPos)                                                                   // Returns noral game state
                        } else {                                                                                                                // If there are more jumps to do
                            val targets = findPossiblePlays(currentPos).filter { t =>                                                           // Filters where the piece can go to(checks if it´s a valid play)
                                val mid = ((currentPos._1 + t._1) / 2, (currentPos._2 + t._2) / 2)
                                validPlay(currentBoard, currentPos, t, mid, player, currentOpen)
                            }
                            
                            val choices = currentPos :: targets                                                                                 // Adds currentPos to targets list because it can choose not to jump
                            val (nextTarget, nextR) = f(choices, currentR)                                                                      // Chooses a random position to go to with the function f(Random)
                            
                            if (nextTarget == currentPos) {                                                                                     // If currentPos is the chosen target
                                println(s"  -> CPU decided to stop jumping and stayed at $currentPos.")
                                (currentBoard, currentOpen, nextR, currentPos)                                                                  // Returns the board the same as before(if decided not to jump, board stayed the same)
                            } else {                                                                                                            // If any coordTo is chosen as a target
                                val (newBoardOpt, nextLstOpenCoords) = play(currentBoard, player, currentPos, nextTarget, currentOpen)          // Does a play
                                
                                newBoardOpt match {
                                    case None => (currentBoard, currentOpen, nextR, currentPos)                                                 // If didn't jump again, game state stays the same
                                    case Some(newBoard) =>                                                                                      // If decided to jump
                                        println(s"  -> CPU continued jumping: from $currentPos to $nextTarget!")
                                        processMultiJumps(newBoard, nextTarget, nextLstOpenCoords, nextR)                                       // Checks if can jump again(recursively)
                                    
                                }
                            }
                        }
                    }

                    firstPlayBoard match {
                        case Some(b) =>                                                                                                             // If does one more jump
                            val (finalBoard, finalOpenCoords, finalR, finalPos) = processMultiJumps(b, chosenCoordTo, firstLstOpenCoords, newR2)    // Tries to jump again
                            (Some(finalBoard), finalR, finalOpenCoords, Some(finalPos))
                        case None =>                                                                                                                // Case there no more jumps to do
                            (None, newR2, lstOpenCoords, None)
                    }
            }
        }        
    }  
    
    def findPossiblePlays(coord: Coord2D): List[Coord2D] = {                                   // Aux to playRandomly and validSources, returns all the coords that can go to a coord
        val (line, col) = coord                                                                // Final coord position
        List((line - 2, col), (line + 2, col), (line, col - 2), (line, col + 2))               // All the possible positions (left, right, above, below)
    }

    def validSources(board: Board, coordTo: Coord2D, player: Stone, lstOpenCoords: List[Coord2D]): List[Coord2D] = {    // Aux to PlayRandomly and validTargets(), filter all valid coordFrom that can go to coordTo(empty space)
        findPossiblePlays(coordTo).filter { coordFrom =>                                                                
            val coordMiddle = ((coordFrom._1 + coordTo._1)/2, (coordFrom._2 + coordTo._2)/2)                            // Chooses middle position  
            validPlay(board, coordFrom, coordTo, coordMiddle, player, lstOpenCoords)                                    // Checks if it's a valid play
        }
    }

    def validTargets(board: Board, lstOpenCoords: List[Coord2D], player: Stone): List[Coord2D] = {                      // Aux to PlayRandomly and gameLoop, filters the list keeping just the coordTo that can be played to
        @tailrec
        def aux(remaining: List[Coord2D], acc: List[Coord2D]): List[Coord2D] = remaining match {                        
            case Nil => acc.reverse                                                                                     // If there are no more valid targets, reverse the list we have
            case coordTo :: tail =>                                                                                     // If there are still any possible positions
                if (validSources(board, coordTo, player, lstOpenCoords).nonEmpty) aux(tail, coordTo :: acc)             // If it's a valid target, add it to the accumulator
                else aux(tail, acc)                                                                                     // Searches more valid positions in the list
        }
        aux(lstOpenCoords, Nil)                                                                                         // calls aux recursively
    }

    def canStillJump(board: Board, player: Stone, coordFrom: Coord2D, lstOpenCoords: List[Coord2D]): Boolean = {
        val potentialTargets = findPossiblePlays(coordFrom)                                 // gets all the potential plays
        
        @tailrec
        def checkTargets(targets: List[Coord2D]): Boolean = targets match {                 
            case Nil => false                                                               // If there are no targets, it can't jump
            case target :: tail =>                                                          // If there are targets
                val mid = ((coordFrom._1 + target._1) / 2, (coordFrom._2 + target._2) / 2)  // Gets the middle position
                if (validPlay(board, coordFrom, target, mid, player, lstOpenCoords)) true   // Checks if it's a valid play, return true
                else checkTargets(tail)                                                     // If it's not, checks the rest of the potentialTargets 
        }
        checkTargets(potentialTargets)                                                      // Checks the potential targets
    }

    // ==== Initialize Board ====

    def generatePieces(lines: Int, cols: Int, maxLines: Int, maxCols: Int): List[(Coord2D, Stone)] = {
        if(lines >= maxLines) Nil                                                           // If we have already printed all the lines stop generating pieces
        else if(cols >= maxCols) generatePieces(lines + 1, 0, maxLines, maxCols)            // If we have already generated the entire line, go to the following one
        else {
            val stone = if((lines + cols) % 2 == 0) Stone.Black else Stone.White            // If it's a pair position, generate White, else Black
            ((lines, cols), stone) :: generatePieces(lines, cols + 1, maxLines, maxCols)    // goes to the next collumn
        }
    }

    def initBoard(lines: Int, cols: Int, r: RandomWithState): (Board, RandomWithState, List[Coord2D]) = { 
        val piecesList = generatePieces(0, 0, lines, cols)                      // Gets a list of all the pieces generated
        val allCoords = piecesList.map(_._1)                                    // Gets the coords from all the pieces

        val (randomIndex1, r1) = r.nextInt(allCoords.length)                    // Gets a random position
        val piece1 = allCoords(randomIndex1)                                     // Gets the first gap position

        val adjacents = getAdjacentCoord(piece1, lines, cols)                   // Gets all the adjacent coords to the piece
        val (randomIndex2, r2) = r1.nextInt(adjacents.length)                   // Choose an adjacent position to the first one
        val piece2 = adjacents(randomIndex2)                                     // Gets the position of the second gap

        val board = piecesList.toMap - piece1 - piece2                          // Gets all the positions but the gaps
        (board, r2, List(piece1, piece2))                                       // Returns the available positions as well as the gaps
    }

    def getAdjacentCoord(coord: Coord2D, lines: Int, cols: Int): List[Coord2D] = {
        val (l, c) = coord                                                                                                          // Gets coords from the desired position
        List((l - 1, c), (l + 1, c), (l, c - 1), (l, c + 1)).filter{ case (x, y) => x >= 0 && x < lines && y >= 0 && y < cols }     // Returns all the adjacent position inside the board
    }
}