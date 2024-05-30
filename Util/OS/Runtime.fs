module Util.Runtime

open System

let mutable troubleshooting = false

let halt output (s:string)(msg:string) = 
    output msg
    output s
    output "HALT"
    while true do
        Threading.Thread.Sleep 1000

let ptf predicate t f = 
    if predicate then
        t
    else
        f

