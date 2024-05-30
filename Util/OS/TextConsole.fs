module Util.TextConsole


open System
open System.Text
open System.Collections.Generic

let init(width:int, height:int) = 

    let w, h = 
        let ww = if(width < Console.LargestWindowWidth) then width else Console.LargestWindowWidth
        let hh = if(height < Console.LargestWindowHeight) then height else Console.LargestWindowHeight
        ww, hh

    System.Console.WindowWidth <- w
    System.Console.WindowHeight <- h

    Console.Clear()

let write_whole_line(pos:int,color:ConsoleColor)(text:string) =

    Console.SetCursorPosition(0, pos)
    Console.ForegroundColor <- color
    Console.Write(text.PadRight(Console.WindowWidth, ' '))



