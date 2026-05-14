// To use ParMap
//> using dep "org.scala-lang.modules::scala-parallel-collections:1.2.0"
// To use javaFx
//> using dep "org.scalafx::scalafx:20.0.0-R31"
//> using dep "org.openjfx:javafx-controls:26.0.1"
//> using dep "org.openjfx:javafx-fxml:26.0.1"
//> using dep "org.openjfx:javafx-graphics:26.0.1"

package Project

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import scala.util.Try
import java.io.{PrintWriter, File}
import scala.io.Source


type Coord2D = (Int, Int)
type Board = ParMap[Coord2D, Stone]

enum Stone:
    case Black, White

case class GameState(lines: Int, cols: Int, board: Board = ParMap.empty[Coord2D, Stone], lstOpenCoords: List[Coord2D] = Nil)

object Konane {

    // Saves the current seed
    def saveSeed(seed:Long):Unit = {
        val writer = new PrintWriter(new File("seed.txt"))  // Prints in the seed.txt file
        writer.write(seed.toString())                       // Writes the seed
        writer.close                                        // Closes the writer
    }

    def loadSeed():Long = {
        // Tryes to read de file. If doesn't exists, chooses a random number that we chose(42) 
        val source = Try(Source.fromFile("seed.txt"))
        val seed = source.map(_.getLines().next().trim.toLong).getOrElse(42L)
        source.foreach(_.close()) // Closes the file
        seed
    }

    // Loads the seed from the file
    val initialSeed = loadSeed()
    val r = MyRandom(initialSeed)

    def undo(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
        val coordMiddle = ((coordFrom._1 + coordTo._1)/2, (coordFrom._2 + coordTo._2)/2)                                // Gets middle coord

        val opponent = if (player == Stone.Black) Stone.White else Stone.Black                                          // Checks if is opponent piece(to remove coordMiddle)
       
        val newBoard = board - coordTo + (coordFrom -> player) + (coordMiddle -> opponent)                              // Removes coordTo and adds coordFrom and coordMiddle to board
        val addToLstOpenCoords = coordTo :: lstOpenCoords                                                               // Adds coordTo to lstOpenCoords
        val removeToOpenCoords = addToLstOpenCoords.filter(c => c != coordMiddle && c != coordFrom)                     // Removes coordMiddle and coordFrom from lstOpenCoords
        (Some(newBoard), removeToOpenCoords)                                                                            // Return board and lstOpenCoords
    }

    // Handles users jumps(multi-jumps)
    def handleUserJumps(board: Board, coordFrom: Coord2D, coordTo: Coord2D, player: Stone, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
        val coordMiddle = ((coordFrom._1 + coordTo._1)/2, (coordFrom._2 + coordTo._2)/2)           // Creates coordMiddle
        if(validPlay(board, coordFrom, coordTo, coordMiddle, player, lstOpenCoords)){                  // If has a valid play
            play(board, player, coordFrom, coordTo, lstOpenCoords)                                  // Plays
        } else {
            (None, lstOpenCoords)                                                                   // Else, doesn't play    
        }
    }


    // Create random move
    def randomMove(lstOpenCoords: List[Coord2D], rand: RandomWithState): (Coord2D, RandomWithState) = {
        val (randPos, newRand) = rand.nextInt(lstOpenCoords.length)  // Choose random pos in lstOpenCoords
        val chosenCoord = lstOpenCoords(randPos)                     // Gets the correspondent coord
        (chosenCoord, newRand)                                       // Has a validation in playRandomly()
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

        if (validCoords.isEmpty){                                                                             // If validCoords is empty
            (None, r, lstOpenCoords, None)                                                                    // Doesn´t make a play
        } else{ 
            validCoords match {
                case Nil => (None, r, lstOpenCoords, None)                                                        // If there are no possible coords to go to, ends game
                case _ =>                                                                                         // Chooses randomly a coordTo with the f function(Random)
                    val (chosenCoordTo, newR1) = f(validCoords, r)                                                // Finds a playable piece that can go to the coordTo
                    val playablePieces = validSources(board, chosenCoordTo, player, lstOpenCoords)                // Chooses randomly a coordFrom with the f function(Random)
                    val (chosenCoordFrom, newR2) = f(playablePieces, newR1)
                    
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
                                (currentBoard, currentOpen, nextR, currentPos)                                                                  // Returns the board the same as before(if decided not to jump, board stayed the same)
                            } else {                                                                                                            // If any coordTo is chosen as a target
                                val (newBoardOpt, nextLstOpenCoords) = play(currentBoard, player, currentPos, nextTarget, currentOpen)          // Does a play
                                
                                newBoardOpt match {
                                    case None => (currentBoard, currentOpen, nextR, currentPos)                                                 // If didn't jump again, game state stays the same
                                    case Some(newBoard) =>                                                                                      // If decided to jump
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
    
    def findPossiblePlays(coord: Coord2D): List[Coord2D] = {                                   // Aux to playRandomly() and validSources(), returns all the coords that can go to a coord
        val (line, col) = coord                                                                // Final coord position
        List((line - 2, col), (line + 2, col), (line, col - 2), (line, col + 2))               // All the possible positions (left, right, above, below)
    }

    def validSources(board: Board, coordTo: Coord2D, player: Stone, lstOpenCoords: List[Coord2D]): List[Coord2D] = {    // Aux to playRandomly() and validTargets(), filter all valid coordFrom that can go to coordTo(empty space)
        findPossiblePlays(coordTo).filter { coordFrom =>                                                                
            val coordMiddle = ((coordFrom._1 + coordTo._1)/2, (coordFrom._2 + coordTo._2)/2)                            // Chooses middle position  
            validPlay(board, coordFrom, coordTo, coordMiddle, player, lstOpenCoords)                                    // Checks if it's a valid play
        }
    }

    def validTargets(board: Board, lstOpenCoords: List[Coord2D], player: Stone): List[Coord2D] = {                     // Aux to playRandomly(), TUI.gameLoop() and GameController.handleCPUMove(), filters the list keeping just the coordTo that can be played to
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

  
    // ==== Initialize Board ====//

    // Aux function used in initBoard() and GameController.scala
    def generatePieces(lines: Int, cols: Int, maxLines: Int, maxCols: Int): List[(Coord2D, Stone)] = {
        if(lines >= maxLines) Nil                                                           // If we have already printed all the lines stop generating pieces
        else if(cols >= maxCols) generatePieces(lines + 1, 0, maxLines, maxCols)            // If we have already generated the entire line, go to the following one
        else {
            val stone = if((lines + cols) % 2 == 0) Stone.Black else Stone.White            // If it's a pair position, generate White, else Black
            ((lines, cols), stone) :: generatePieces(lines, cols + 1, maxLines, maxCols)    // goes to the next collumn
        }
    }
    

    def initBoard(lines: Int, cols: Int, r: RandomWithState): (Board, RandomWithState, List[Coord2D]) = { 
        def getAdjacentCoord(coord: Coord2D, lines: Int, cols: Int): List[Coord2D] = {
            val (l, c) = coord                                                                                                          // Gets coords from the desired position
            List((l - 1, c), (l + 1, c), (l, c - 1), (l, c + 1)).filter{ case (x, y) => x >= 0 && x < lines && y >= 0 && y < cols }     // Returns all the adjacent position inside the board
        }
        
        val piecesList = generatePieces(0, 0, lines, cols)                      // Gets a list of all the pieces generated
        val allCoords = piecesList.map(_._1)                                    // Gets the coords from all the pieces

        // Creates new list from allCoords that returns the middle and the corners
        val takeOutList = allCoords.filter{ case (r, c) => (r == 0 && c == 0) || (r == 0 && c == cols - 1) || (r == lines - 1 && c == 0) || (r == lines - 1 && c == cols - 1) || (r == lines / 2 && c == cols / 2)}

        val (randomIndex1, r1) = r.nextInt(takeOutList.length)                    // Gets a random position
        val piece1 = takeOutList(randomIndex1)                                     // Gets the first gap position

        val adjacents = getAdjacentCoord(piece1, lines, cols)                   // Gets all the adjacent coords to the piece
        val (randomIndex2, r2) = r1.nextInt(adjacents.length)                   // Choose an adjacent position to the first one
        val piece2 = adjacents(randomIndex2)                                     // Gets the position of the second gap

        val board = piecesList.toMap - piece1 - piece2                          // Gets all the positions but the gaps
        val boardPar: Board = board.par                                         // Converts to ParMap
        (boardPar, r2, List(piece1, piece2))                                    // Returns the available positions as well as the gaps
    }    
}