module Util.HtmlFilter

open System.Text.RegularExpressions



let production = true

let debugPrintfn format =
    let empty_out = new System.IO.StringWriter()
    if not production then printfn format else (fprintfn empty_out format)
    
// https://www.washington.edu/accesscomputing/webd2/student/unit2/common_tags.html
type Token = 
    OpenTag of tag: string * literalValue: string
    | CloseTag of tag: string * literaValue: string
    | NonContainerTag of tag: string * literalValue: string
    | ErrorTag of string
    | Text of string
    | Eof
    
let ParseContext (input:string) = 
    let mutable _i = 0
    let _buf = input
    let tagMatcher = Regex("[a-zA-Z][a-zA-Z0-9]*", RegexOptions.Compiled)

    let isSpace ch = 
        match ch with
        | '\u0020'| '\u0009'| '\u000a'| '\u000c'| '\u000d' -> true
        | _ -> false

    let hasPrefix pattern = 
        (_i < _buf.Length) && (_buf.[_i] = pattern)
        
    let consumeQuotedString () = 
        let current = _buf.[_i]
        while not (hasPrefix current) do
            _i <- _i + 1
        ()

    let findToken (literal: string) = 
        let m = tagMatcher.Match literal
        if m.Success then m.Value else ""

    let dispatchTagType (literalValue: string): Token =
        if literalValue.Length < 2 || (findToken literalValue = "") then
            ErrorTag literalValue
        else if literalValue.[1] = '/' then
            CloseTag(findToken literalValue, literalValue)
        else if literalValue.[literalValue.Length - 2] = '/' then
            NonContainerTag(findToken literalValue, literalValue)
        else
            OpenTag(findToken literalValue, literalValue)

    let emitTag () : Token =
        let len = _buf.Length
        let pStart = _i
        if (_i + 1) = len || (_i + 1 < len && _buf.[_i + 1] <> '/' && not (System.Char.IsLetter(_buf.[_i + 1]))) then
            _i <- _i + 1
            ErrorTag _buf.[pStart.._i - 1]
        else
            while _i < len && not (hasPrefix '>') do
                while _i < len && not (hasPrefix '>' || hasPrefix '"' || hasPrefix '\'' || hasPrefix '`') do
                    _i <- _i + 1
                if hasPrefix '"' || hasPrefix '\"' || hasPrefix '`' then
                    consumeQuotedString ()
                if not (hasPrefix '>') then _i <- _i + 1
            _i <- _i + 1
            dispatchTagType _buf.[pStart.._i - 1]

    let emitText () =
        let start = _i
        while _i < _buf.Length && not (hasPrefix '<') do
            _i <- _i + 1
        Text(_buf.[start.._i - 1])
        
    let emitToken () =
        if _i < _buf.Length then
            match _buf.[_i] with
            | '<' -> emitTag ()
            | _ -> emitText ()
        else Eof
        
    emitToken
        
let KeepPTag (input: string) =

    let emitToken = ParseContext input

    let mutable loopend = false
    let mutable result = ""
    let mutable tagEnvLevel = 0
    let mutable unwantedTagEnvLevel = 0
    let addToText (text:string) =
        if ((tagEnvLevel > 0) && (unwantedTagEnvLevel = 0)) then
            result <- result + text.Trim()
            debugPrintfn "updated result: %s" result
        else
            ignore text
        ()

    while not loopend do
        let token = emitToken ()
        match token with
        | OpenTag(tag, lit) ->

            debugPrintfn "open: %s" tag

            if tag = "p" || tag = "h1" || tag = "h2" || tag = "h3" then
                result <- result + lit
            else
                ignore tag
            tagEnvLevel <- tagEnvLevel + 1
            if tag = "script" || tag = "style" then
                unwantedTagEnvLevel <- unwantedTagEnvLevel + 1
        | CloseTag(tag, lit) ->
                
            debugPrintfn "close: %s" tag

            if tag = "p" || tag = "h1" || tag = "h2" || tag = "h3" then
                result <- result + lit
            else
                ignore tag
            tagEnvLevel <- tagEnvLevel - 1
            if tag = "script" || tag = "style" then
                unwantedTagEnvLevel <- unwantedTagEnvLevel - 1
        | NonContainerTag(_, _) ->
            ignore ()
        | ErrorTag(text) ->
            debugPrintfn "errortag: %s" text
            addToText text
        | Text(text) ->
            debugPrintfn "level = %d, badlevel = %d text: %s" tagEnvLevel unwantedTagEnvLevel text
            debugPrintfn "--end of text--"
            addToText text
        | Eof -> 
            loopend <- true
            ()

    debugPrintfn "end of loop: %s" result

    result

let SelectDivByPattern (pattern: string) (input: string) = 
    let emitToken = ParseContext input

    let mutable loopend = false
    let mutable result = ""
    let mutable divTagCounter = 0
    while not loopend do
        let token = emitToken ()
        match token with
        | OpenTag(tag, lit) ->

            debugPrintfn "open: %s" tag

            if divTagCounter = 0 && tag = "div" && lit.Contains(pattern) then
                divTagCounter <- divTagCounter + 1
            elif divTagCounter > 0 then
                if tag = "div" then
                    divTagCounter <- divTagCounter + 1
                result <- result + lit

            debugPrintfn "div cnt = %d" divTagCounter

        | CloseTag(tag, lit) ->

            debugPrintfn "close: %s" tag

            if divTagCounter > 0 then
                if tag = "div" then
                    divTagCounter <- divTagCounter - 1
            if divTagCounter > 0 then
                result <- result + lit
        | NonContainerTag(_, _) ->
            ignore ()
        | Text(text) | ErrorTag(text) ->
            if divTagCounter > 0 then
                result <- result + text
            else
                ignore text
        | Eof -> 
            loopend <- true
            ()
    result

// let private prefix (input: string) index pattern = 
//     index < input.Length && (input[index]) = pattern
    
// let private parseTag (input : string) : (Token * string) = 
//     let mutable i = 0
//     let len = input.Length
//     while i < len && not (prefix input i '<') do
//             ()

    

