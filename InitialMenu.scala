package Project

import javafx.fxml.FXML
import javafx.scene.control.{Button, TextField, Alert, Label}
import scala.compiletime.uninitialized
import javafx.scene.control.Alert.AlertType
import javafx.fxml.FXMLLoader
import javafx.scene.Parent
import java.io.File

class InitialMenu {
  @FXML
  private var DimensionID: Button = uninitialized
  
  @FXML
  private var StartGameID: Button = uninitialized
  
  @FXML
  private var QuitID1: Button = uninitialized

  @FXML
  private var LinesID1: Label = uninitialized

  @FXML
  private var LinesID2: TextField = uninitialized

  @FXML
  private var ColsID1: Label = uninitialized

  @FXML
  private var ColsID2: TextField = uninitialized

  @FXML
  private var OkDimensionButtonID: Button = uninitialized

  @FXML
  private var TimerButtonID: Button = uninitialized

  @FXML
  private var TimerLabel: Label = uninitialized

  @FXML
  private var TimerField: TextField = uninitialized

  @FXML 
  private var OkTimerButtonID: Button = uninitialized

  @FXML
  private var ResumeGameLabel: Label = uninitialized

  @FXML
  private var BtnContinue: Button = uninitialized                         // To resume game

  @FXML
  private var BtnIgnore: Button = uninitialized                           // To ignore resume game

  @FXML 
  private var StartNewGameId: javafx.scene.layout.VBox = uninitialized



  // Variables to save the dimensions
  var chosenLines: Int = 0
  var chosenCols: Int = 0
  // Variable to save the chosen time, if player doesn't choose the game starts with infinite time
  var chosenTimer: Option[Int] = None


  def initialize(): Unit = {                  // Need initialize() because only shows if saveGame.txt exists
    val saveFile = new File("saveGame.txt")
    
    if (saveFile.exists()) {
        StartNewGameId.setVisible(true)
    }
  }
  
  @FXML
  def OnDimensionClicked(): Unit = {
    LinesID1.setVisible(true)
    LinesID2.setVisible(true)
    ColsID1.setVisible(true)
    ColsID2.setVisible(true)
    OkDimensionButtonID.setVisible(true)
  }

  @FXML
  def OnTimerClicked(): Unit = {
    TimerLabel.setVisible(true)
    TimerField.setVisible(true)
    OkTimerButtonID.setVisible(true)
  }

  @FXML
  def OnOkTimerClicked(): Unit = {
    try{
      val t = TimerField.getText.toInt
      if(t < 5){                                            // Defined min time
        showError("Timer must be at least 5 seconds!")
      } else{
        chosenTimer = Some(t)
        TimerLabel.setVisible(false)
        TimerField.setVisible(false)
        OkTimerButtonID.setVisible(false)
      }
    } catch {
        case _: NumberFormatException => showError("Invalid Timer Value!")
    }
  }

  @FXML
  def OnStartGameClicked(): Unit = {
    try{
      if ((chosenLines < 3 || chosenCols < 3)){                   // Boards < 3 can't be played
        showError("Invalid Dimensions. Choose Dimensions First!") 
      } else{
        //Create new FXML
          val fxmlFile = new java.io.File("GameBoard.fxml")       // Loads GameController.scala / GameBoard.fxml
  
          val loader = new FXMLLoader(fxmlFile.toURI.toURL)
          val root: javafx.scene.Parent = loader.load() 

          val controller = loader.getController[GameController]()
          controller.initializeGame(chosenLines, chosenCols, chosenTimer)

          val stage = StartGameID.getScene.getWindow.asInstanceOf[javafx.stage.Stage]
          stage.getScene.setRoot(root)
          stage.setFullScreen(true);
        
      }
    } catch {
        case _: NumberFormatException => showError("Invalid Dimensions. Choose Dimensions First!")
    }
  }

    @FXML
    def onActionContinue(): Unit = {
      val fxmlFile = new java.io.File("GameBoard.fxml")                           // Loads GameController.scala / GameBoard.scala
      val loader = new FXMLLoader(fxmlFile.toURI.toURL)
      val root: Parent = loader.load()

      val controller = loader.getController[GameController]()
        
      // Loads all the data from saveGame.txt
      controller.loadExistingGame()                                               // Loads the game with the existing data

      val stage = StartGameID.getScene.getWindow.asInstanceOf[javafx.stage.Stage]
      stage.getScene.setRoot(root)
      stage.setFullScreen(true)
  }

  @FXML
  def onActionIgnore(): Unit = {
    StartNewGameId.setVisible(false)
  }

  @FXML
  def OnOkDimensionsClicked(): Unit = {
    try{
        val l = LinesID2.getText.toInt                                                                 // Saves dimensions from user input 
        val c = ColsID2.getText.toInt

         if ((l > 0 && c > 0 && (l % 2 != 0 && c % 2 != 0)) || (l <= 0 || c <= 0)) {                   // Rules of dimension       
                    showError("Invalid Dimensions. Try again!")                                        // If it doesn't follow the rules, it's invalid                                                                  // Restarts the main loop
                } else {  
                    chosenLines = l
                    chosenCols = c     

                    LinesID1.setVisible(false)
                    LinesID2.setVisible(false)
                    ColsID1.setVisible(false)
                    ColsID2.setVisible(false)
                    OkDimensionButtonID.setVisible(false)                                                                                                     
                }
    } catch {
        case _: NumberFormatException =>
            showError("Invalid Dimensions.")
    }
  }

  private def showError(msg: String): Unit = {            // Private functions to show errors
    val alert = new Alert(AlertType.WARNING)
    alert.setTitle("Can't start the game")
    alert.setHeaderText(null)
    alert.setContentText(msg)
    alert.showAndWait()
  }

  @FXML
  def OnQuit1Clicked(): Unit = {                          // Function to end program
    sys.exit(0)
  }
  
}
