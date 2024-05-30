module Util.Console

open System
open System.Threading
open System.Threading.Tasks


let lockee = new Object()

let colorx(cf:ConsoleColor,cb:ConsoleColor)(s:string) = 
    lock(lockee)(fun () ->
        Console.ForegroundColor <- cf
        Console.BackgroundColor <- cb
        Console.Write(s)
        Console.ForegroundColor <- ConsoleColor.White
        Console.BackgroundColor <- ConsoleColor.Black)

let color(cf:ConsoleColor)(s:string) = 
    lock(lockee)(fun () ->
        Console.ForegroundColor <- cf
        Console.Write(s)
        Console.ForegroundColor <- ConsoleColor.White)

let cprint isNewLine =
    let lockObj = obj()
    fun color s ->
        lock lockObj (fun _ ->
            Console.ForegroundColor <- color
            if isNewLine then
                printfn "%s" s
            else
                printf "%s" s
            Console.ResetColor())

let clog = cprint false
let clogn = cprint true

let vcomplete = clog ConsoleColor.Magenta 
let vok = clog ConsoleColor.Green
let vinfo = clog ConsoleColor.Cyan
let vwarn = clog ConsoleColor.Yellow
let verror = clog ConsoleColor.Red
let verrorn = clogn ConsoleColor.Red