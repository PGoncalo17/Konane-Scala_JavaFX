package Project

import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParMap
import javafx.fxml.FXML
import javafx.scene.layout.GridPane
import javafx.scene.control.{Button, Label}
import javafx.scene.shape.Circle
import javafx.scene.paint.Color
import javafx.fxml.FXMLLoader
import javafx.scene.Parent
import scalafx.scene.Scene
import javafx.scene.control.Alert
import javafx.scene.control.Alert.AlertType
import javafx.animation.AnimationTimer
import scala.compiletime.uninitialized
import java.io.{File, PrintWriter}
import scala.io.Source

class GameController {

    @FXML 
    private var BoardGrid: GridPane = uninitialized
  
    @FXML 
    private var TurnLabel: Label = uninitialized

    @FXML
    private var BtnYes: Button = uninitialized

    @FXML
    private var BtnNo: Button = uninitialized

    @FXML 
    private var BtnUndo: Button = uninitialized

    @FXML 
    private var BtnReset: Button = uninitialized

    @FXML 
    private var BtnQuit: Button = uninitialized

    @FXML
    var TimerDisplay: Label = uninitialized 

    @FXML 
    private var MultiJumpBox: javafx.scene.layout.VBox = uninitialized

    //Variable to get current state
    private var currentBoard: Board = ParMap.empty[Coord2D, Stone]                                 // Variable to get current board
    private var lstOpenCoords: List[Coord2D] = Nil                                                 // Variable to get lstOpenCoords
    private var history: List[(Board, List[Coord2D], Stone, Option[Coord2D], Int, Long)] = Nil     // Variable to get history for undo, Option[Coord2D] to save locked piece, Int to count jumpCount, Long to timer
    private var gameLines: Int = 0                                                                 // Variable to get game lines
    private var gameCols: Int = 0                                                                  // Variable to get game cols
    private var currentTurn: Stone = Stone.Black                                                   // Variable to get current turn
    private var currentRandom: RandomWithState = uninitialized                                     // Variable to get current random
    private var selectedCoord: Option[Coord2D] = None                                              // Var to save the piece player chooses to play(to change colour to yellow)
    private var isMultiJump: Boolean = false                                                       // Var to check multiple jumps
    private var lockedToPiece: Boolean = false                                                     // Var to not let choose another piece while doing multiple jumps
    private var jumpCount: Int = 0                                                                 // Var to count jumps
    private var timeLimitMillis: Option[Long] = None                                               // Time variable
    private var turnStartTime: Long = 0                                                            // Start turn time
    private var timeRunning: Boolean = false                                                       // Time variable to choose between limited and infinite time mode
    private var selectedDifficulty: String = "Easy"                                                // var to see difficulty selected

    def initializeGame(lines: Int, cols: Int, timerSeconds: Option[Int]): Unit = {
        gameLines = lines
        gameCols = cols
        timeLimitMillis = timerSeconds.map(s => s*1000L)    // Convert seconds to miliseconds
        val seedInFile = Konane.loadSeed()
        val rInitial = MyRandom(seedInFile)
        val (board, nextR, gaps) = Konane.initBoard(lines, cols, rInitial)

        currentBoard = board
        lstOpenCoords = gaps
        currentRandom = nextR
        currentTurn = Stone.Black
        history = Nil
        selectedCoord = None
        isMultiJump = false

        Konane.saveSeed(nextR.seed)

        if (timeLimitMillis.isDefined) {
            TimerDisplay.setVisible(true)
            resetTurnTimer()
        } else {
            TimerDisplay.setText("Mode: No Timer")
        }
        updateUI()
    }


    private def resetTurnTimer(): Unit = {
        turnStartTime = System.currentTimeMillis()                  // Only need to save initial time if limit exists
        if (timeLimitMillis.isDefined) {
            timerLoop.start()                                       // Starts or resets current time
        }
    }

    private val timerLoop: AnimationTimer = new AnimationTimer() {                      // AnimationTimer to handle time
        override def handle(now: Long): Unit = {
            timeLimitMillis match {
                case Some(limit) =>
                    val elapsed = System.currentTimeMillis() - turnStartTime
                    val remaining = limit - elapsed

                    if (remaining <= 0) {
                        TimerDisplay.setText("TIME'S UP!")
                        handleTimeOut()
                        stop() // Stops this loop
                    } else {
                        // Updates the Label (ex: "Time: 14.5s")
                        val seconds = remaining / 1000.0
                        TimerDisplay.setText(f"Time: $seconds%.1fs")
                        
                        // Changes colour to Red when 5 seconds left
                        if (remaining < 5000) TimerDisplay.setStyle("-fx-text-fill: red;")
                        else TimerDisplay.setStyle("-fx-text-fill: white;")
                    }
                case None => stop()
            }
        }
    }

    private def handleTimeOut(): Unit = {
        // If timer ends, change turn
        if (currentTurn == Stone.Black) {
            changeTurns() // Changes to CPU
        }
    }


    def updateUI(): Unit = {
        TurnLabel.setText(s"Turn: $currentTurn")
        drawBoard()
  }
  

    def drawBoard(): Unit = {
        BoardGrid.getChildren.clear()

        // Gets a list with all the positions
        val allCells = Konane.generatePieces(0, 0, gameLines, gameCols)

        // Calculates a play only if one piece is selected
        val (activeOrigin, activeTargets) = selectedCoord match {
            case Some(coord) => 
                // Verificamos se a peça selecionada pode realmente saltar
                val targetsForThisPiece = Konane.validTargets(currentBoard, lstOpenCoords, currentTurn).filter(t => {
                    val mid = ((coord._1 + t._1) / 2, (coord._2 + t._2) / 2)
                    Konane.validPlay(currentBoard, coord, t, mid, currentTurn, lstOpenCoords)
                })
                
                // If chosen piece has valid targets, the piece can be played
                if (targetsForThisPiece.nonEmpty) (Some(coord), targetsForThisPiece) else (None, Nil)
                
            case None => (None, Nil)
        }

        // Draws board
        allCells.foldRight(()) { case (((l, c), _), _) =>
            val cell = new Button()
            cell.setPrefSize(55, 55)

            // Only the piece selected and their valid targets stay yellow
            val isYellow = activeOrigin.contains((l, c)) || activeTargets.contains((l, c))
            
            val backgroundColor = if (isYellow) "#fff59d" else "#8d6e63"
            
            cell.setStyle(s"-fx-background-color: $backgroundColor; -fx-background-radius: 5; -fx-border-color: #5d4037;")

            // Draws pieces
            currentBoard.get((l, c)) match {
                case Some(Stone.Black) => cell.setGraphic(new Circle(20, Color.web("#212121")))
                case Some(Stone.White) => cell.setGraphic(new Circle(20, Color.web("#f5f5f5")))
                case None => 
            }

            cell.setOnAction(_ => { if (currentTurn == Stone.Black) handlePlayerClick(l, c)})

            BoardGrid.add(cell, c, l)
        }
    }



    def handlePlayerClick(l: Int, c: Int): Unit = {
        // If is waiting for a multiJump decision(Yes or No), clicks are ignored
        if (isMultiJump) return 

        selectedCoord match {
            case None =>
                if (!lockedToPiece && currentBoard.get((l, c)) == Some(Stone.Black)) {
                    selectedCoord = Some((l, c))
                    drawBoard()
                }
            case Some(from) =>
                val to = (l, c)

                if(from == to){                 // If click on the same piece, removes the selection
                    if(!lockedToPiece){         // If it's locked, can´t remove select from the piece
                        selectedCoord = None
                        drawBoard()
                    }
                } else{
                    val (resultBoard, nextGaps) = Konane.play(currentBoard, currentTurn, from, to, lstOpenCoords)       // Tries to make the play

                    resultBoard match {
                        case Some(newBoard) =>
                            history = (currentBoard, lstOpenCoords, currentTurn, selectedCoord, jumpCount, turnStartTime) :: history
                            currentBoard = newBoard
                            lstOpenCoords = nextGaps
                            jumpCount += 1  // Add 1 because piece jumped(increments jump count)
                            lockedToPiece = false   // After jumping, temporary lock turns off

                            if (Konane.canStillJump(currentBoard, currentTurn, to, lstOpenCoords)) {
                                selectedCoord = Some(to)        // Piece moves to 'to'
                                showMultiJumpMenu(true)         // Activate Yes/No buttons
                                updateUI()
                            } else {
                                changeTurns()
                            }
                        case None =>                            // If movement is invalid clears the selection
                            if(!lockedToPiece){                 // But if the piece is in multi-jump, keeps the selection
                                selectedCoord = None
                                drawBoard()
                            }
                    }
                }
        }
        saveGameState()                                          // Saves the game state if player decides to play the game later
    }

    // Functions to multi-jump buttons
    @FXML
    def onActionYes(): Unit = {
        isMultiJump = false
        lockedToPiece = true
        showMultiJumpMenu(false)
        timerLoop.start()
        updateUI()
    }

    @FXML
    def onActionNo(): Unit = {
        isMultiJump = false
        showMultiJumpMenu(false)
        changeTurns()
    }

    @FXML
    private def showMultiJumpMenu(visible: Boolean): Unit = {
        isMultiJump = visible
        MultiJumpBox.setVisible(visible)
        BtnYes.setVisible(visible)
        BtnNo.setVisible(visible)
        BtnUndo.setDisable(visible)                                 //Deactivate game buttons when this menu is vivible
        BtnReset.setDisable(visible)
        BtnQuit.setDisable(visible)

        if (visible) {
            timerLoop.stop()                                        // Stops timer
            TurnLabel.setText("EXTRA JUMP POSSIBLE!")
        } else {
            TurnLabel.setText(s"Turn: $currentTurn")
        }
    }

    private def changeTurns(): Unit = {
        selectedCoord = None
        isMultiJump = false
        lockedToPiece = false
        showMultiJumpMenu(false)
        currentTurn = Stone.White
        jumpCount = 0
        timerLoop.stop()                            // Stops Timer
        updateUI()
        
        val pause = new javafx.animation.PauseTransition(javafx.util.Duration.millis(600))      // Makes a small wait to make a CPU move
        pause.setOnFinished(_ => handleCPUMove())
        pause.play()
    }

    def handleCPUMove(): Unit = {
        val (resultBoard, nextR, nextGaps, _) = selectedDifficulty match {                                                    // See which matches the selected difficulty
            case "Medium" => getMediumMove()                                                                                  // Calls getMediumMove() if difficulty is medium
            case "Hard" => getHardMove()                                                                                      // Calls getHardMove() if difficulty is hard
            case _ => Konane.playRandomly(currentBoard, currentRandom, Stone.White, lstOpenCoords, Konane.randomMove)         // Calls playRandomly: (board, r, player, gaps, f)
        } 
            
        currentRandom = nextR                                                                                                                           // Saves new state for next play
        Konane.saveSeed(nextR.seed)
        
        resultBoard match {
            case Some(newBoard) =>                                                                                                                      // Case CPU makes a move
                currentBoard = newBoard                                                                                                                 // Updates variables
                lstOpenCoords = nextGaps
                currentTurn = Stone.Black
                Konane.saveSeed(nextR.seed)                                                                                                             // Saves random seed
                resetTurnTimer()
                updateUI()
                saveGameState()                                                                                                                         // Saves game state

                if (Konane.validTargets(currentBoard, lstOpenCoords, Stone.Black).isEmpty){                                                             // If player can't make more plays
                    timerLoop.stop()
                    TurnLabel.setText("GAME OVER - CPU WON!")
                    TimerDisplay.setStyle("-fx-text-fill: white;")                                                              // Makes sure timer isn't red(because of the 5 sec remaining)
                    TimerDisplay.setText("Womp womp!")
                    deleteSave()                                                                                                // Deletes saveGame.txt
                }
            
            case None =>                                                                                                        // Case CPU can't make more plays
                timerLoop.stop()
                TurnLabel.setText("GAME OVER - YOU WON!")
                TimerDisplay.setStyle("-fx-text-fill: white;")                                                                  // Makes sure timer isn't red(because of the 5 sec remaining)
                TimerDisplay.setText("Good Game!")
                deleteSave()                                                                                                    // Deletes saveGame.txt
        }
    }

    def setDifficulty(difficulty: String): Unit = {                                                                             // Function that gives chosen difficulty
        selectedDifficulty = difficulty
    }

    // aux function to list all movements (from -> to)
    private def getAllPossibleMoves(board: Board, lstOpenCoords: List[Coord2D], player: Stone): List[(Coord2D, Coord2D)] = {
        val targets = Konane.validTargets(board, lstOpenCoords, player)                                                                 // Gets all targets

        def collectMoves(remainingTargets: List[Coord2D], acc: List[(Coord2D, Coord2D)]): List[(Coord2D, Coord2D)] = {                  // Aux function to 
            remainingTargets match {
                case Nil => acc
                case to :: tail =>
                    val sources = Konane.validSources(board, to, player, lstOpenCoords)
                    val movesFromThisTarget = sources.map(from => (from, to))
                    collectMoves(tail, acc ++ movesFromThisTarget)
            }
        }
        collectMoves(targets, Nil)
    }

    // Medium Difficulty
    private def getMediumMove(): (Option[Board], RandomWithState, List[Coord2D], Option[Coord2D]) = {
        val moves = getAllPossibleMoves(currentBoard, lstOpenCoords, Stone.White)                                               // Gets all the possible moves
        if (moves.isEmpty) return (None, currentRandom, lstOpenCoords, None)                                                    // If there are no moves to do, returns None

        val bestMove = moves.find { case (coordFrom, coordTo) =>                                                                // Chooses best move
            val (simBoard, simLstOpenCoords) = Konane.play(currentBoard, Stone.White, coordFrom, coordTo, lstOpenCoords)         // Gets all possible plays
            simBoard.exists(b => Konane.canStillJump(b, Stone.White, coordTo, simLstOpenCoords))                                // Gets the plays that still has jumps after
        }.getOrElse(moves.head)                                                                                                 // If they are all the same, chooses first

        val (newBoard, newLstOpenCoords) = Konane.play(currentBoard, Stone.White, bestMove._1, bestMove._2, lstOpenCoords)      // Makes the best play
        (newBoard, currentRandom, newLstOpenCoords, Some(bestMove._2))                                                          // Returns new board
    }

    // Difficult mode
    private def getHardMove(): (Option[Board], RandomWithState, List[Coord2D], Option[Coord2D]) = {
        val moves = getAllPossibleMoves(currentBoard, lstOpenCoords, Stone.White)                                               // Gets all the possible moves
        if(moves.isEmpty) return (None, currentRandom, lstOpenCoords, None)                                                      // If there are no moves to do, returns None

        val bestMove = moves.minBy { case (coordFrom, coordTo) =>                                                               // Chooses the play where user gets the least valid targets
            val (simBoard, simLstOpenCoords) = Konane.play(currentBoard, Stone.White, coordFrom, coordTo, lstOpenCoords)        // Gets all possible plays
            simBoard.map(b => Konane.validTargets(b, simLstOpenCoords, Stone.Black).size).getOrElse(999)                        // Gets the play that has the least amount of valid targets
        }

        val (newBoard, newLstOpenCoords) = Konane.play(currentBoard, Stone.White, bestMove._1, bestMove._2, lstOpenCoords)     // Makes the best play
        (newBoard, currentRandom, newLstOpenCoords, Some(bestMove._2))                                                            // Returns new board

    }

    @FXML
    def onUndo(): Unit = {
        history match {
            case (oldBoard, oldGaps, oldTurn, oldSelected, oldJumpCount, oldStartTime) :: tail =>                   // Case there is an undo to do
                currentBoard = oldBoard                                                                             // Return previous variables
                lstOpenCoords = oldGaps
                currentTurn = oldTurn
                selectedCoord = oldSelected
                turnStartTime = oldStartTime
                jumpCount = oldJumpCount
                history = tail
                
                // Just show menu if jumpCount > 0
                if (jumpCount > 0){
                    isMultiJump = true
                    lockedToPiece = true
                    showMultiJumpMenu(true)
                } else {
                    isMultiJump = false
                    lockedToPiece = false
                    showMultiJumpMenu(false)

                }

                updateUI()

            case Nil =>                                                                     // Create and show visual warning message
                val alert = new Alert(AlertType.WARNING)
                alert.setTitle("Undo Not Possible")
                alert.setHeaderText(null)                                                   // Removes the header for aesthetics
                alert.setContentText("There are no more moves to undo!")
                alert.showAndWait()
        }
    }

    @FXML
    def onReset(): Unit = {
        if(isMultiJump) return                                                              // Extra block when multi-jump is available
                  
        val timerInSeconds = timeLimitMillis.map(ms => (ms / 1000L).toInt)                  // Convert again from Option[Long] ms to Option[Int] s

        initializeGame(gameLines, gameCols, timerInSeconds)                                 // Restars the game
    }

    @FXML
    def onQuitGame(): Unit = {
        if(isMultiJump) return                                                              // Extra block when multi-jump is available

        val fxmlFile = new java.io.File("InitialMenu.fxml")                                 // Load InitialMenu.scala / InitialMenu.fxml
    
        val loader = new FXMLLoader(fxmlFile.toURI.toURL)
        val root: Parent = loader.load()

        val stage = BoardGrid.getScene.getWindow.asInstanceOf[javafx.stage.Stage]
        stage.getScene.setRoot(root)
        stage.setFullScreen(true);
    }


    // Saves the game state in case there was an unfished match
    def saveGameState(): Unit = {
        val writer = new PrintWriter(new File("saveGame.txt"))                                                  // Writes in the file saveGame.txt
        writer.println(s"$gameLines,$gameCols")                                                                 // Saves the dimensions
        writer.println(s"$currentTurn,${currentRandom.seed},$selectedDifficulty,$jumpCount")                    // Saves turn, seed, jump count and selected difficulty

        val selCoordStr = selectedCoord.map(c => s"${c._1} ${c._2}").getOrElse("None")                          // Gets the selected coord if it exists
        writer.println(s"$isMultiJump,$lockedToPiece,$selCoordStr")                                             // Saves MultiJump, locked piece and selected coord
            
        val remaining = timeLimitMillis.map { limit =>                                                          // Gets timer
            val elapsed = System.currentTimeMillis() - turnStartTime
            limit - elapsed
        }.getOrElse(-1L)                                                                                        // If oesn't have timer
        writer.println(remaining)                                                                               // Saves timer

        val boardData = currentBoard.map { case ((r, c), s) => s"$r $c $s" }.mkString(";")                      // Get board
        writer.println(boardData)                                                                               // Saves board
            
        writer.println(lstOpenCoords.map(c => s"${c._1} ${c._2}").mkString(";") )                               // Saves lstOpenCoords
            
        writer.close()                                                                                          // Closes writer
    }

    def deleteSave(): Unit = {                                                                                  // Deletes saveGame.txt
        new File("saveGame.txt").delete()
    }

    def hasSaveGame(): Boolean = {                                                                              // Checks if saveGame.txt exists
        val f = new File("saveGame.txt")
        f.exists() && f.length() > 0
    }

    // Function called in InitialMenu.scala(Ceates the board with the variables in saveGame.txt)
    def loadExistingGame(): Unit = {
        val file = new File("saveGame.txt") 
        if (!file.exists()) return                                                                              // If saveGame.txt doesn't exist ends

        val source = Source.fromFile(file)                                                                      // Reads saveGame.txt
        val lines = source.getLines().toList
        source.close()

        try {
            val dims = lines(0).split(",")                                                                      // Gets dimensions
            gameLines = dims(0).trim.toInt                                                                      // Gets lines
            gameCols = dims(1).trim.toInt                                                                       // Gets cols

            val meta = lines(1).split(",")
            currentTurn = if (meta(0).trim == "Black") Stone.Black else Stone.White                             // Gets player's turn
            currentRandom = MyRandom(meta(1).trim.toLong)                                                       // Gets seed
            selectedDifficulty = meta(2).trim                                                                   // Gets selected difficulty
            jumpCount = if (meta.length > 3) meta(3).trim.toInt else 0                                          // Gets jumpCount, if it doesn't exist is 0

            val multi = lines(2).split(",")
            isMultiJump = multi(0).toBoolean                                                                    // Gets multiJump
            lockedToPiece = multi(1).toBoolean                                                                  // Gets locked piece
            selectedCoord = if (multi.length < 3 || multi(2) == "None") None else {                             // Gets selected coord
                val parts = multi(2).split(" ")
                Some((parts(0).toInt, parts(1).toInt))
            }

            val remaining = lines(3).trim.toLong                                                                // Gets timer
            if (remaining > 0) {
                timeLimitMillis = Some(remaining)
                TimerDisplay.setVisible(true)
                resetTurnTimer() 
            } else {
                timeLimitMillis = None                                                                          // If doesn't have timer
                TimerDisplay.setText("Mode: No Timer")
            }

            val boardEntries = lines(4).split(";").filter(_.nonEmpty).map { entry =>                            // Gets board
                val parts = entry.split(" ")
                (parts(0).toInt, parts(1).toInt) -> (if (parts(2) == "Black") Stone.Black else Stone.White)
            }
            currentBoard = boardEntries.toMap.par

            lstOpenCoords = if (lines.length > 5 && lines(5).nonEmpty) {                                        // Gets lstOpenCoords
                lines(5).split(";").filter(_.nonEmpty).map { entry =>
                    val parts = entry.split(" ")
                    (parts(0).toInt, parts(1).toInt)
                }.toList
            } else Nil

            history = Nil                                                                                       // Updates interface
            if (isMultiJump) {
                showMultiJumpMenu(true)
            } else {
                showMultiJumpMenu(false)
            }

            updateUI()

        } catch {                                                                                               // Case there is an error
            case e: Exception => 
                println(s"Error loading file: ${e.getMessage}")
        }
    }
}