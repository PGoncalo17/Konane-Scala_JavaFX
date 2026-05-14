package Project

import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.Includes.*
import javafx.fxml.FXMLLoader
import javafx.scene.Parent


object GUI {
    private object App extends JFXApp3 {  
        override def start(): Unit = {
            //Loads FXML
            val fxmlFile = new java.io.File("InitialMenu.fxml")             // Loads InitialMenu.scala / InitialMenu.fxml
        
            val loader = new FXMLLoader(fxmlFile.toURI.toURL)
            val root: Parent = loader.load()

            stage = new JFXApp3.PrimaryStage {
                title = "Konane Game"
                scene = new Scene(root)
                fullScreen = true
            }
        }  
    }  

    // Simple function to simplify Main call
    def execute(args: Array[String]): Unit = {
        App.main(args)
    }
}                                                                          