module Util.Json

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Text

open Util.Text
open Util.Time

(*
JSON有三类元素： 

1、结构体（关键字：大括号）
2、键值对（关键字：冒号）
3、数组（关键字：中括号）

组织规则：
结构体中放一个或者多个键值对。
键只能是字符串。
值可以是：字符串、数字、null、true、false、结构体或者数组。
数组中存放一个或者多个值。

{
    "name": "BeJson",
    "url": "http://www.bejson.com",
    "page": 88,
    "isNonProfit": true,
    "address": {
        "street": "科技园路.",
        "city": "江苏苏州",
        "country": "中国"
    },
    "links": [
        {
            "name": "Googl // <aaa> \" e",
            "url": "http://www.google.com"
        },
        {
            "name": "Baidu",
            "url": "http://www.baidu.com"
        },
        {
            "name": "SoSo",
            "url": "http://www.SoSo.com"
        }
    ]
}

*)

type Json = 
| Str of string
| Html of string
| Clean of string
| Num of string
| True
| False
| NoBraket of (string * Json)[]
| Braket of (string * Json)[]
| Ary of Json[]
| Null

let empty = Json.Braket [| |]

type Token = 
| StrQuoted of string
| StrGeneral of string
| Symbol of char // { } [ ] , :
| Undefined

let rec json__str (w:TextBlockWriter) json = 
    match json with
    | Json.Str s ->
        if s.IndexOf("\"") >= 0 && (s.IndexOf("[") >= 0 || s.IndexOf("{") >= 0) then
            "\"" + (System.Web.HttpUtility.JavaScriptStringEncode s) + "\"" |> w.newline
        else "\"" + s.Replace("\\u","%u").Replace("\r","").Replace("\n","").Replace("\t","").Replace("\"","'").Replace("\\n","").Replace("\\r","").Replace("\\","") + "\"" |> w.newline
    | Json.Html s -> "\"" + (System.Web.HttpUtility.JavaScriptStringEncode s) + "\"" |> w.newline
    | Json.Clean s -> "\"" + s.Replace("\\u","%u").Replace("\r","").Replace("\n","").Replace("\t","").Replace("\"","'").Replace("\\n","").Replace("\\r","").Replace("\\","") + "\"" |> w.newline
    | Json.Num s -> s |> w.newline
    | Json.True -> "true" |> w.newline
    | Json.False -> "false" |> w.newline
    | Json.NoBraket items -> 
        if items.Length > 0 then
            items
            |> Array.iter(fun (s,j) -> 
                ",\"" + s + "\":" |> w.newline
                json__str w j
                )
    | Json.Braket items -> 
        "{" |> w.newline
        if items.Length > 0 then
            items
            |> Array.iter(fun (s,j) -> 
                "\"" + s + "\":" |> w.newline
                json__str w j
                "," |> w.newline)
            w.backspace()
        "}" |> w.newline
    | Json.Ary items -> 
        "[" |> w.newline
        if items.Length > 0 then
            items
            |> Array.iter(fun j -> 
                json__str w j
                "," |> w.newline)
            w.backspace()
        "]" |> w.newline
    | Json.Null -> "null" |> w.newline

let json__strFinal json = 
    let w = empty__TextBlockWriter()
    json__str w json
    w.text()

let tryFindByAtt attName json =
    match json with
    | Json.Braket items -> items |> Array.tryFind(fun (n,_) -> n = attName)
    | _ -> None

let tryFindStrByAtt attName json =
    match json with
    | Json.Braket items -> 
        match 
            items 
            |> Array.tryFind(fun (n,_) -> n = attName) with
        | Some (n,v) -> 
            match v with
            | Json.Str s -> s
            | _ -> ""
        | _ -> ""
    | _ -> ""

let tryFindNumByAtt attName json =
    match json with
    | Json.Braket items -> 
        match 
            items 
            |> Array.tryFind(fun (n,_) -> n = attName) with
        | Some (n,v) -> 
            match v with
            | Json.Num s -> s
            | _ -> ""
        | _ -> ""
    | _ -> ""

let tryFindAryByAtt attName json =
    match json with
    | Json.Braket items -> 
        match 
            items 
            |> Array.tryFind(fun (n,_) -> n = attName) with
        | Some (n,v) -> 
            match v with
            | Json.Ary s -> s
            | _ -> [| |]
        | _ -> [| |]
    | _ -> [| |]

let inline tryAddBracket<'T> json (attName, attValue:'T)  =
    let appendItems items = Array.singleton >> Array.append items

    match json with
    | Json.Braket items ->
        match box attValue with
        | :? string -> 
            (attName,Json.Str(attValue.ToString()))
            |> appendItems items
        | :? int32 | :? int64  | :? float -> 
            (attName,Json.Num(attValue.ToString()))
            |> appendItems items
        | :? bool as b ->
            if b then
                (attName,Json.True)
            else
                (attName,Json.False)
            |> appendItems items
        | _ -> items
        |> Json.Braket
        |> Some
    | _ -> None
    
let inline tryAddBrackets<'T> json (attKVs:(string * 'T)[]) =

    match json with
    | Json.Braket items ->
        attKVs
        |> Array.choose (fun (attName,attValue) ->
            match box attValue with
            | :? string -> 
                Some (attName,Json.Str(attValue.ToString()))
            | :? int32 | :? int64  | :? float -> 
                Some (attName,Json.Num(attValue.ToString()))
            | :? bool as b ->
                if b then
                    (attName,Json.True)
                else
                    (attName,Json.False)
                |> Some
            | _ -> None
        )
        |> Array.append items
        |> Json.Braket
        |> Some
    | _ -> None


let str__tokens (s:string) = 

    let tokens = new List<Token>()

    let mutable quoted = false
    let sb = new StringBuilder()

    let mutable index = 0
    while index <= s.Length - 1 do
        
        let c = s[index]

        let isQuoteMark = 
            if c = '"' then
                if index = 0 then
                    true
                else
                    not(s[index - 1] = '\\')
            else
                false

        if isQuoteMark then 
            if quoted = false then
                sb.Clear() |> ignore
                quoted <- true
            else
                sb.ToString() 
                |> unescape_unicode
                |> Token.StrQuoted 
                |> tokens.Add
                sb.Clear() |> ignore
                quoted <- false
        else 
            if quoted then
                c |> sb.Append |> ignore
            else
                match c with
                | '{' 
                | '}'
                | ':'
                | ','
                | '['
                | ']' -> 
                    let s = sb.ToString().Trim()
                    sb.Clear() |> ignore

                    if s.Length > 0 then
                        s |> Token.StrGeneral |> tokens.Add

                    c |> Token.Symbol |> tokens.Add
                | _ -> 
                    c |> sb.Append |> ignore

        index <- index + 1

    tokens.ToArray()
    |> Array.filter(fun i -> 
        match i with
        | Token.StrGeneral s -> s.Trim().Length > 0
        | _ -> true)

let rec parseBraket (index:int ref, tokens: Token []) = 
    
    let fields = new List<string * Json>()

    let mutable finished = false

    while finished = false && index.Value < tokens.Length do
        match tokens[index.Value] with
        | Symbol c -> 

            match c with
            | ':' ->
                if index.Value > 0 && index.Value + 1 < tokens.Length then
                    match tokens[index.Value - 1] with
                    | Token.StrQuoted k -> 

                        match tokens[index.Value + 1] with
                        | Token.StrQuoted v -> 
                            (k,v |> Json.Str) |> fields.Add
                            index.Value <- index.Value + 2
                        | Token.StrGeneral v -> 
                            match v with 
                            | "true" ->
                                (k,Json.True) |> fields.Add
                                index.Value <- index.Value + 2
                            | "false" ->
                                (k,Json.False) |> fields.Add
                                index.Value <- index.Value + 2
                            | "null" ->
                                (k,Json.Null) |> fields.Add
                                index.Value <- index.Value + 2
                            | _ ->
                                (k,v |> Json.Num) |> fields.Add
                                index.Value <- index.Value + 2
                        | Token.Symbol s ->
                            match s with
                            | '[' -> 
                                index.Value <- index.Value + 2
                                let v = parseArray(index,tokens)
                                (k,v) |> fields.Add
                            | '{' -> 
                                index.Value <- index.Value + 2
                                let v = parseBraket(index,tokens)
                                (k,v) |> fields.Add
                            | _ -> 
                                index.Value <- index.Value + 2

                        | _ -> 
                            index.Value <- index.Value + 2

                    | _ -> 
                        index.Value <- index.Value + 2

            | ']' -> 
                finished <- true
                index.Value <- index.Value + 1

            | '}' -> 
                finished <- true
                index.Value <- index.Value + 1

            | _ -> index.Value <- index.Value + 1

        | _ -> index.Value <- index.Value + 1

    fields.ToArray() |> Json.Braket


and parseArray (index:int ref, tokens: Token []) = 
    
    let items = new List<Json>()

    let mutable finished = false

    while finished = false && index.Value < tokens.Length do
        match tokens[index.Value] with
        | Symbol c -> 
            match c with
            | ']' -> 
                finished <- true
                index.Value <- index.Value + 1
            | '{' -> 
                let v = parseBraket(index,tokens)
                v |> items.Add
            | _ ->  index.Value <- index.Value + 1
            
        | Token.StrQuoted v -> 
            v |> Json.Str |> items.Add
            index.Value <- index.Value + 1

        | Token.StrGeneral v -> 
            match v with 
            | "true" ->
                Json.True |> items.Add
                index.Value <- index.Value + 1
            | "false" ->
                Json.False |> items.Add
                index.Value <- index.Value + 1
            | "null" ->
                Json.Null |> items.Add
                index.Value <- index.Value + 1
            | _ ->
                v |> Json.Num |> items.Add
                index.Value <- index.Value + 1

        | _ -> index.Value <- index.Value + 1

    items.ToArray() |> Json.Ary

let str__root (s:string) = 
    let tokens = s |> str__tokens
    
    if tokens.Length = 0 then
        Json.Null
    else
        match tokens[0] with
        | Token.Symbol s -> 
            if s = '[' then 
                parseArray (ref 0,tokens)
            else
                parseBraket (ref 0,tokens)
        | _ -> parseBraket (ref 0,tokens)

let kvp k v = k,v

let json__braketo json = 
    match json with
    | Json.Braket vv -> Some vv
    | _ -> None

let json__aryo json = 
    match json with
    | Json.Ary items -> Some items
    | _ -> None

let json__tryFindByName json name = 
    match json with
    | Json.Braket items -> 
        match 
            items
            |> Array.tryFind (fun i -> fst i = name)
        with
        | Some item -> item |> snd |> Some
        | None -> None
    | _ -> None

let json__aryItems attName json =

    match
        json
        |> tryFindByAtt attName with
    | Some (n,j) -> 
        match j with
        | Json.Ary items -> items |> Some
        | _ -> None
    | None -> None

let json__arySomeItems (json__itemo:Json -> 'T Option) attName json =

    match
        json
        |> tryFindByAtt attName with
    | Some (n,j) -> 
        match j with
        | Json.Ary items -> 
            items 
            |> Array.map json__itemo
            |> Array.filter(fun o -> o.IsSome)
            |> Array.map(fun o -> o.Value)
            |> Some
        | _ -> None
    | None -> None

//let str__jsonVal (s:string) = 
//    let a = Newtonsoft.Json.JsonConvert.SerializeObject(s)
//    a.Substring(1,a.Length-2)

//let remove_quote(str:string) =
//    if(str.Contains("\"")) then
//        str
//    else
//        Util.Text.regex_match(regex_remove_quote)(str)

//let matchBracket = matchPair('{','}')
//let matchBrackets(key) = matchPairs(key,'{','}')
//let matchArray s = s |> matchPair('[',']') |> matchBrackets


let json__items json = 
    
    let sd = new Dictionary<string, string>()

    match json with
    | Json.Braket items -> 
        items
        |> Array.iter(fun i -> 
            let k1,v = i
            let k = k1.ToLower()
            if sd.ContainsKey k = false then
                match v with
                | Json.Str s -> sd.Add(k,s)
                | Json.Num s -> sd.Add(k,s)
                | Json.True -> sd.Add(k,"true")
                | Json.False -> sd.Add(k,"false")
                | Json.Null -> sd.Add(k,"null")
                | _ -> ())
    | _ -> ()

    sd

let jsonstr__items = str__root >> json__items

let name__valo braket name = 
    match braket with
    | Json.Braket items ->
        match 
            items
            |> Array.tryFind(fun i -> fst i = name) with
        | Some v -> Some(snd v)
        | None -> None
    | _ -> None

let name__array braket name = 
    match braket with
    | Json.Braket items ->
        match 
            items
            |> Array.tryFind(fun i -> fst i = name) with
        | Some v ->
            match snd v with
            | Json.Ary items -> items
            | _ -> [||]
        | None -> [||]
    | _ -> [||]

//let _json__items(json:string) =

//    let segments =
//        let marks = new List<int>()
//        let chars = json.ToCharArray()
//        let mutable quoted = false
//        let mutable i = 0

//        while(i < chars.Length) do
//            match chars.[i] with
//            | '\\' -> i <- i + 1
//            | '"' -> quoted <- not quoted
//            | ',' -> if(i > 0 && i < chars.Length - 1 && quoted = false) then marks.Add(i)
//            | '{' -> if(i > 0 && i < chars.Length - 1 && quoted = false) then marks.Add(i)
//            | '}' -> if(i > 0 && i <= chars.Length - 1 && quoted = false) then marks.Add(i)
//            | _ -> ()
//            i <- i + 1

//        let res = new List<string>()
//        let mutable index = 0
//        marks.ToArray() 
//        |> Array.iter(fun m ->
//            let v = json.Substring(index, m - index).Trim()
//            if (res.Contains v) = false then
//                res.Add(json.Substring(index, m - index).Trim())
//            index <- m)
//        let v = json.Substring(index)
//        if (res.Contains v ) = false then
//            res.Add(json.Substring(index))

//        res

//    let sd = new Dictionary<string, string>()

//    segments.ToArray()
//    |> Array.filter(fun s -> s.IndexOf("\"") >= 0)
//    |> Array.iter(fun s1 ->
//            let s2 = s1 |> Util.Text.regex_match(regex_field)
//            let s3 = s1.Substring(s1.IndexOf("\"") + s2.Length)

//            let k = (s2 |> Util.Text.regex_match(regex_key)).ToLower()
//            let v = 
//                if(s3.StartsWith("\"")) then
//                    s3 |> Util.Text.findBidirectional("\"","\"")
//                else
//                    s3 |> Util.Text.regex_match(regex_val)
//            if(k.Length > 0 && sd.ContainsKey(k) = false) then
//                sd.Add(k,v))

//    sd

let check_mandatory_fields(fields:Dictionary<string,string>,keys:string[]) =
    let lost = new ResizeArray<string>()
    keys
    |> Array.iter(fun k ->
        if(fields.ContainsKey(k) = false) then
            lost.Add(k))
    lost.ToArray()

let checkfieldWithDefault
    isIgnoreCase
    defaltVal
    (fields:Dictionary<string,string>) 
    (k:string) =

    let key = if isIgnoreCase then k.ToLower() else k
    if fields = null then
        defaltVal
    else if fields.ContainsKey key = false then
        defaltVal
    else
        let s = fields.[key].Replace("'","&apos;").Trim()
        if s = "null" then
            defaltVal
        else
            s

let checkfield:(Dictionary<string,string> -> string -> string) =
    checkfieldWithDefault true ""

let checkFieldWithCase:(Dictionary<string,string> -> string -> string)  =
    checkfieldWithDefault false ""

let checkFieldCaseWithDefault:(string -> Dictionary<string,string> -> string -> string)  =
    checkfieldWithDefault false

let checkFieldIgnoreCaseWithDefault:(string -> Dictionary<string,string> -> string -> string)  =
    checkfieldWithDefault true

let checkfieldx fields (k:String,n) =
    let key = k.ToLower()
    let src = checkfield fields key
    if src.Length > n then
        src.Substring(0,n).Trim()
    else
        src.Trim()

let checkfieldz fields (k:String) n =
    let key = k.ToLower()
    let src = checkfield fields key
    if src.Length > n then
        src.Substring(0,n).Trim()
    else
        src.Trim()


//// Parsing ================================================================

//type FieldVal =
//| Text of string
//| Integer of int64
//| Float of float
//| Array of Node[]
//and Node = {
//    fields: List<string*FieldVal>;
//    parent: Node option }

//type TokenType =
//| Bra
//| Ket
//| ArrayStart
//| ArrayEnd
//| Comma
//| Col
//| QuotedString of string
//| Integer of int64
//| Float of float

//type Token = { tokentype: TokenType; depth:int }

//let token__string(token) =
//    let str =
//        match token.tokentype with
//        | Bra -> "{"
//        | Ket -> "}"
//        | ArrayStart -> "["
//        | ArrayEnd -> "]"
//        | Comma -> ","
//        | Col -> ":"
//        | QuotedString(v) -> "\"" + v.ToString() + "\""
//        | Integer(v) -> v.ToString()
//        | Float(v) -> v.ToString()

//    let tab = "    "
//    let mutable s = token.depth.ToString("00") + "  "
//    [0..token.depth - 1] |> Seq.iter(fun i -> s <- s + tab)
//    s + str

//let quotemark(cs:char[]) =

//    let mutable quoted = false

//    [0..cs.Length - 1]
//    |> Seq.map(fun i ->

//        let c = cs.[i]

//        let quot =
//            if( c <> '\"') then
//                false
//            else if(i = 0) then
//                true
//            else
//                cs.[i-1] <> '\\'

//        if(quot) then
//            quoted <- not quoted
//            true
//        else
//            quoted)
//    |> Seq.toArray

//let depthmark(cs:char[], quotemark:bool[]) =

//    let mutable depth = 0
//    let mutable addnext = false
//    [0..cs.Length-1] |> Seq.map(fun i ->

//        if(addnext) then
//            depth <- depth + 1
//            addnext <- false

//        if(quotemark.[i] = false) then
//            match cs.[i] with
//            | '{'
//            | '[' ->
//                addnext <- true
//            | '}'
//            | ']' ->
//                depth <- depth - 1
//            | _ -> ()

//        depth) |> Seq.toArray

//let lex_quote_depth(text:string) =
//    let cs = text.ToCharArray()
//    let quotemark = quotemark(cs)
//    let depthmark = depthmark(cs, quotemark)
//    depthmark, quotemark, cs

//let match_bracket_with_depth
//    (src:string, cs:char[], quotemark:bool[], depthmark:int[])
//    (startingindex, searchlength)
//    (depth) =
//    let res = new ResizeArray<int*int*string>()
//    let mutable starting = 0
//    [0..searchlength - 1]
//    |> Seq.iter(fun ii ->
//        let i = startingindex + ii
//        if(quotemark.[i] = false && depthmark.[i] = depth) then
//            let c = cs.[i]
//            if(c = '{' || c = '[') then
//                starting <- i
//            else if(c = '}' || c = ']') then
//                let index, length = starting, i - starting + 1
//                res.Add(index, length, src.Substring(index, length)))
//    res.ToArray()

//let split_with_depth
//    (src:string, cs:char[], quotemark:bool[], depthmark:int[])
//    (startingindex, searchlength)
//    (depth) =
//    let positions = new ResizeArray<int>()
//    let mutable starting = 0
//    [0..searchlength - 1]
//    |> Seq.iter(fun ii ->
//        let i = startingindex + ii
//        if(quotemark.[i] = false && depthmark.[i] = depth) then
//            if(cs.[i] = ',') then
//                positions.Add(i))

//    let res = new ResizeArray<int*int*string>()

//    let inf =
//        let mutable anchor = startingindex - 1
//        [0..searchlength - 1]
//        |> Seq.iter(fun ii ->
//            let i = startingindex + ii
//            if(quotemark.[i] = false && depthmark.[i] = depth) then
//                if(anchor = startingindex - 1) then
//                    anchor <- i)
//        anchor

//    let mutable p = inf

//    [0..positions.Count - 1]
//    |> Seq.iter(fun i ->
//        let pp = positions.[i]
//        let index = p + 1
//        let length = pp - index
//        if(length > 1) then
//            res.Add(index, length, src.Substring(index, length))
//        p <- pp)

//    let sup =
//        let mutable anchor = p
//        [p..startingindex + searchlength - 1]
//        |> Seq.iter(fun i ->
//            if(quotemark.[i] = false && depthmark.[i] = depth) then
//                anchor <- i)
//        anchor

//    let index = p + 1
//    let length = sup - index
//    if(length > 1) then
//        res.Add(index, length, src.Substring(index, length))

//    res.ToArray()

//let lexer(text:string) =

//    let cs = text.ToCharArray()

//    let tokens = new ResizeArray<Token>()
//    let buffer = new ResizeArray<char>()

//    let mutable quoted = false
//    let mutable depth = 0
//    [0..cs.Length - 1]
//    |> Seq.iter(fun i ->

//        let c = cs.[i]

//        let quot =
//            if( c <> '\"') then
//                false
//            else if(i = 0) then
//                true
//            else
//                cs.[i-1] <> '\\'

//        if(quot) then
//            if(quoted) then
//                let s = new string(buffer.ToArray())
//                tokens.Add({ tokentype = QuotedString(s.Substring(1)); depth = depth })
//                buffer.Clear()
//            quoted <- not quoted

//        if(quoted) then
//            buffer.Add(c)
//        else
//            match c with
//            | '{' ->
//                tokens.Add({ tokentype = Bra; depth = depth })
//                depth <- depth + 1
//            | '}' ->
//                depth <- depth - 1
//                tokens.Add({ tokentype = Ket; depth = depth })
//            | '[' ->
//                tokens.Add({ tokentype = ArrayStart; depth = depth })
//                depth <- depth + 1
//            | ']' ->
//                depth <- depth - 1
//                tokens.Add({ tokentype = ArrayEnd; depth = depth })
//            | ',' ->
//                tokens.Add({ tokentype = Comma; depth = depth })
//            | ':' ->
//                tokens.Add({ tokentype = Col; depth = depth })
//            | _ -> ())

//    tokens.ToArray()

//type TextNode = 
//    {
//        depth:int;
//        children:ResizeArray<TextNode>;
//        index:int;
//        length:int;
//        mutable content:string;
//        text:string }

//let create_textnode(depth, index, length, text) = {
//    depth = depth;
//    children = new ResizeArray<TextNode>();
//    index = index;
//    length = length;
//    content = "";
//    text = text }

//let parser(src:string) =

//    let depthmark, quotemark, cs = lex_quote_depth(src)

//    let mask = Array.create(cs.Length)(false)

//    let m(startingindex, searchlength, depth) =

//        if(depth > 100) then
//            if(Util.Runtime.troubleshooting) then
//                Console.WriteLine("=======================")
//                Console.WriteLine(src)
//                Console.WriteLine("=======================")
//                Console.WriteLine(src.Substring(startingindex,searchlength))
//                Console.WriteLine("Json.parser() HALT")
//                Console.ReadLine() |> ignore

//        match_bracket_with_depth
//            (src, cs, quotemark, depthmark)
//            (startingindex, searchlength)
//            (depth)

//    let s(startingindex, searchlength, depth) =
//        split_with_depth
//            (src, cs, quotemark, depthmark)
//            (startingindex, searchlength)
//            (depth)

//    let rec parsing(n:TextNode) =
//        try
//            m(n.index, n.length, n.depth)
//            |> Seq.map(fun item ->
//                let i,l,s = item
//                let child = create_textnode(n.depth + 1, i, l, s)

//                parsing(child)

//                child)

//            |> n.children.AddRange

//            let ra = new ResizeArray<char>()
//            [0..n.length-1]
//            |> Seq.iter(fun k ->
//                let w = n.index + k
//                if(mask.[w] = false) then
//                    ra.Add(cs.[w])
//                    mask.[w] <- true)

//            n.content <- (new string(ra.ToArray())).Trim()
//        with ex ->
//            if(Util.Runtime.troubleshooting) then
//                Console.WriteLine(ex.ToString())
//                Console.WriteLine("n:TextNode.content = " + n.content)
//                Console.WriteLine("n:TextNode.content_length = " + n.content.Length.ToString())
//                Console.WriteLine("n:TextNode.depth = " + n.depth.ToString())
//                Console.WriteLine("n:TextNode.children = " + n.children.Count.ToString())
//                Console.WriteLine("SYS HALTED ...Json.fs/parser()") 
//                Console.ReadLine() |> ignore



//    let root = {
//        depth = 0;
//        children = new ResizeArray<TextNode>();
//        index = 0;
//        length = src.Length
//        content = "";
//        text = src}

//    parsing(root)

//    root

//let get_json_value(strjson:string)=
//   let jo = JObject.Parse(strjson)
//   let values = jo.Properties().Select(fun item -> item. .ToString()).ToArray()
       


// ============ populate

let list__json item__json (w:TextBlockWriter) (lines:'T[]) = 
    if lines.Length > 0 then
        lines.[0] |> item__json w
        [| 1..lines.Length - 1 |]
        |> Array.iter(fun i ->
            "," |> w.newline
            lines.[i] |> item__json w)

let json_text_clean(txt:string) =
    //System.Web.HttpUtility.JavaScriptStringEncode(txt)
    let cleaned = 
        txt.Replace("\\u","%u").Replace("\r","").Replace("\n","").Replace("\t","").Replace("\"","'").Replace("\\n","").Replace("\\r","").Replace("\\","")
    cleaned
let json_text_clean2(txt:string) =
    //System.Web.HttpUtility.JavaScriptStringEncode(txt)
    let cleaned = 
        txt.Replace("\\","\\\\").Replace("\r","\\r").Replace("\n","\\n").Replace("\t","\\t").Replace("\"","'")
    cleaned

let cleanStr = json_text_clean2 >> cleanUselessChars


(*
██████████████████████████████████████████████████████████████████████████████
██████████████████████████████████████████████████████████████████████████████
████       ███████████████████████████████████████████████████████████████████
██████████████████████████████████████████████████████████████████████████████
██████████████████████████████████████████████████████████████████████████████
*)

let int64__json (v:int64) = v.ToString() |> Json.Num
let int32__json (v:int32) = v.ToString() |> Json.Num
let uint32__json (v:uint) = v.ToString() |> Json.Num
let float__json (v:float) = v.ToString() |> Json.Num
let float32__json (v:float32) = v.ToString() |> Json.Num
let bool__json (v:bool) = if v then Json.True else Json.False
let str__json (v:string) = v |> Json.Str
let DateTime__json (v:DateTime) = v |> Time.wintime__unixtime |> int64__json

let json__int64o json = 
    match json with
    | Json.Num s -> s |> try_parse_int64
    | _ -> None

let json__int32o json = 
    match json with
    | Json.Num s -> s |> try_parse_int32
    | _ -> None

let json__uint32o json = 
    match json with
    | Json.Num s -> s |> try_parse_uint32
    | _ -> None

let json__floato json = 
    match json with
    | Json.Num s -> s |> try_parse_float
    | _ -> None

let json__float32o json = 
    match json with
    | Json.Num s -> s |> try_parse_float32
    | _ -> None

let json__boolo json = 
    match json with
    | Json.True -> Some true
    | Json.False -> Some false
    | _ -> None

let json__stro json = 
    match json with
    | Json.Str s -> s |> Some
    | _ -> None

let json__DateTimeo json = 
    match json with
    | Json.Num s -> 
        match s |> try_parse_int64 with
        | Some v -> v |> Time.unixtime__wintime |> Some
        | None -> None
    | _ -> None

let array__json item__json (array:'T[]) =
    array
    |> Array.map item__json
    |> Json.Ary

let json__arrayo (json__itemo:Json -> 'T option) json = 
    match json with
    | Json.Ary jsons -> 
        jsons
        |> Array.map json__itemo
        |> Array.filter(fun o -> o.IsSome)
        |> Array.map(fun o -> o.Value)
        |> Some
    | _ -> None

let List__json<'T> item__json (ls:List<'T>) = 
    ls.ToArray() |> array__json item__json

let json__Listo<'T> (json__itemo:Json -> 'T option) json = 
    match json__arrayo json__itemo json with
    | Some items -> new List<'T>(items) |> Some
    | None -> None

let ListImmutable__json<'T> item__json (ls:'T list) = 
    ls |> List.toArray |> array__json item__json

let json__ListImmutableo<'T> (json__itemo:Json -> 'T option) json = 
    match json__arrayo json__itemo json with
    | Some items -> items |> Array.toList |> Some
    | None -> None

let Option__json<'T> item__json (o:'T option) =
    match o with
    | Some v -> v |> item__json
    | None -> Json.Null

let json__Optiono<'T> (json__itemo:Json -> 'T option) json:'T option option = 
    match json with
    | Json.Null -> None
    | _ -> json |> json__itemo |> Some

let Dictionary__json
    key__json
    val__json
    (dict:Dictionary<'k,'v>) =
        lock dict (fun _ ->
            dict.Keys
            |> Seq.toArray
            |> Array.map(fun key -> 
                let k = 
                    match key__json key with
                    | Json.Str s -> s
                    | Json.Num s -> "\"" + s + "\""
                    | _ -> ""

                let v = val__json dict[key]
                (k,v))
            |> Json.Braket)

let json__Dictionaryo<'K,'V> 
    (json__keyo:Json -> 'K option)
    (json__valo:Json -> 'V option)
    (dict:Dictionary<'K,'V>)
    json = 
    lock dict (fun _ ->
        dict.Clear()
        match json with
        | Json.Ary items -> 
            items
            |> Array.iter(fun i -> 
                let keyo = json__tryFindByName i "key"
                let valo = json__tryFindByName i "val"
                if keyo.IsSome && valo.IsSome then
                    let ko = json__keyo keyo.Value
                    let vo = json__valo valo.Value
                    if ko.IsSome && vo.IsSome then
                        dict.Add(ko.Value,vo.Value))
            Some dict
        | _ -> None)

let SortedDictionary__json
    key__json
    val__json
    (dict:SortedDictionary<'k,'v>) =
        lock dict (fun _ ->
            dict.Keys
            |> Seq.toArray
            |> Array.map(fun key -> 
                let k = 
                    match key__json key with
                    | Json.Str s -> s
                    | Json.Num s -> "\"" + s + "\""
                    | _ -> ""

                let v = val__json dict[key]
                (k,v))
            |> Json.Braket)

let json__SortedDictionaryo<'K,'V> 
    (json__keyo:Json -> 'K option)
    (json__valo:Json -> 'V option)
    (dict:SortedDictionary<'K,'V>)
    json = 
    lock dict (fun _ ->
        dict.Clear()
        match json with
        | Json.Ary items -> 
            items
            |> Array.iter(fun i -> 
                let keyo = json__tryFindByName i "key"
                let valo = json__tryFindByName i "val"
                if keyo.IsSome && valo.IsSome then
                    let ko = json__keyo keyo.Value
                    let vo = json__valo valo.Value
                    if ko.IsSome && vo.IsSome then
                        dict.Add(ko.Value,vo.Value))
            Some dict
        | _ -> None)

let ConcurrentDictionary__json
    key__json
    val__json
    (dict:ConcurrentDictionary<'k,'v>) =
        lock dict (fun _ ->
            dict.Keys
            |> Seq.toArray
            |> Array.map(fun k -> 
                [|  "key",(key__json k)
                    "val",val__json dict[k] |]
                |> Json.Braket)
            |> Json.Ary)

let json__ConcurrentDictionaryo<'K,'V> 
    (json__keyo:Json -> 'K option)
    (json__valo:Json -> 'V option)
    (dict:ConcurrentDictionary<'K,'V>)
    json = 
    lock dict (fun _ ->
        dict.Clear()
        match json with
        | Json.Ary items -> 
            items
            |> Array.iter(fun i -> 
                let keyo = json__tryFindByName i "key"
                let valo = json__tryFindByName i "val"
                if keyo.IsSome && valo.IsSome then
                    let ko = json__keyo keyo.Value
                    let vo = json__valo valo.Value
                    if ko.IsSome && vo.IsSome then
                        dict[ko.Value] <- vo.Value)
            Some dict
        | _ -> None)

let findStrValueByAttNameWithDefault def attName j =
    match tryFindByAtt attName j with
    | None -> def
    | Some (_,value) ->
        match value |> json__stro with
        | None -> def
        | Some value -> value

let findFloatValueByAttNameWithDefault def attName j =
    match tryFindByAtt attName j with
    | None -> def
    | Some (_,value) ->
        match value |> json__floato with
        | None -> def
        | Some value -> value

let findInt64ValueByAttNameWithDefault def attName j =
    match tryFindByAtt attName j with
    | None -> def
    | Some (_,value) ->
        match value |> json__int64o with
        | None -> def
        | Some value -> value

let findInt32ValueByAttNameWithDefault def attName j =
    match tryFindByAtt attName j with
    | None -> def
    | Some (_,value) ->
        match value |> json__int32o with
        | None -> def
        | Some value -> value

let findBoolValueByAttNameWithDefault def attName j =
    match tryFindByAtt attName j with
    | None -> def
    | Some (_,value) ->
        match value |> json__boolo with
        | None -> def
        | Some value -> value

let findUnixDateTimeValueByAttNameWithDefault def attName j =
    match tryFindByAtt attName j with
    | None -> def
    | Some (_,value) ->
        match value |> json__DateTimeo with
        | None -> def
        | Some value -> value
        
let findStrValue = findStrValueByAttNameWithDefault "" 
let findFloatValue = findFloatValueByAttNameWithDefault 0.0
let findInt32Value = findInt32ValueByAttNameWithDefault 0 
let findInt64Value = findInt64ValueByAttNameWithDefault 0
let findBoolValue = findBoolValueByAttNameWithDefault false
let findDateTimeValueFromUnixTime =
    utc_starting
    |> findUnixDateTimeValueByAttNameWithDefault
    
let processBraketOnlyWithDefault defaultVal h json =
    match json with
    | Json.Braket items -> items |> h
    | _ -> defaultVal

let processBraketOnly =  processBraketOnlyWithDefault ()

let processAryOnlyWithDefault defaultVal h json =
    match json with
    | Json.Ary items -> items |> h
    | _ -> defaultVal

let processAryOnly =  processAryOnlyWithDefault ()

let processStrOnlyWithDefault defaultVal h json =
    match json with
    | Json.Str s -> s |> h
    | _ -> defaultVal

let processStrOnly =  processStrOnlyWithDefault ()

let processNumOnlyWithDefault defaultVal h json =
    match json with
    | Json.Num s -> s |> h
    | _ -> defaultVal

