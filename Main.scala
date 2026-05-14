package Project

import scala.io.StdIn.readLine

object Main {
    def main(args: Array[String]): Unit = {
        println("Welcome to Konane! How would you like to play?")
        println("(0): Quit")
        println("(1): Graphic User Interface (GUI)")
        println("(2): Terminal User Interface (TUI)")
        println("Option: ")

        val choice = readLine().trim                                // Reads user iput

        choice match {  
            case "0" =>                                             // Ends program
                TUI.printProgramOver()                               
                sys.exit(0)
            case "1" =>                                             // Execute GUI
                println("Starting GUI...")
                GUI.execute(args) 

            case "2" =>                                             // Execute TUI
                println("Starting TUI...")
                TUI.mainLoop(GameState(0, 0), Konane.r)

            case _ =>                                               // Handles exceptions
                println("Wrong input. Try again.")
                main(args)
        }
    }
}