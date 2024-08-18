module Util.Flow

let ifThen hIf hThen predicate = 
    if predicate then
        hIf()
    else
        hThen()

let ifOnlyWithDefault defaultVal hIf predicate =
    if predicate then
        hIf()
    else
        defaultVal

let ifOnly hIf = ifOnlyWithDefault ()