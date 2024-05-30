module Util.Text

open System
open System.Text
open System.Text.RegularExpressions
open Microsoft.VisualBasic

open System.Collections.Generic
open System.Security.Cryptography


[<Literal>]
let cr = "\r"
let lf = "\n"
let crlf = "\r\n"
let crlfcrlf = crlf + crlf

let tab = "    "

let tabs = 
    let mutable s = ""
    [| 0..255 |]
    |> Array.map(fun i -> 
        let res = s
        s <- s + tab
        res)

// CR = \xD
// LF = \xA
[<Literal>]
let regexCRLF = @"\x0D\x0A"
    
let uselessHtmlChars = [|
        ' '
    |]

let uselessStrings =
    [|
        "&nbsp;"
        "（）"
    |]

    
let containsIgnoreCase(pattern:string)(s:string) = s.IndexOf(pattern, StringComparison.OrdinalIgnoreCase) >= 0

let equalIgnoreCase(a,b) = (String.Compare(a,b,StringComparison.OrdinalIgnoreCase) = 0)

let compareStrWithEnum enumItem str = equalIgnoreCase (str, enumItem.ToString())

let first num (txt:string) = 
    if txt.Length < num then
        txt
    else
        txt.Substring(0,num)
    
type StrIndexed = string * Ref<int>

//██████████████████████████████████████████████████████████████████████████████
//=== Regex
    
let regex_options = RegexOptions.Singleline ||| RegexOptions.ExplicitCapture ||| RegexOptions.Compiled
let regexOptionsIgnoreCase = RegexOptions.Singleline ||| RegexOptions.ExplicitCapture ||| RegexOptions.Compiled ||| RegexOptions.IgnoreCase

let string__regex s = new Regex(s, regex_options)
let string__regexIgnoreCase s = new Regex(s, regexOptionsIgnoreCase)

let regex_localize(regex: Regex)(text: string) =
    let m = regex.Match(text)
    if(m.Success) then
        m.Index, m.Length
    else
        0, 0

let regex_localizes (regex: Regex) (text: string) =
    let mc = regex.Matches text
    [| 0..mc.Count - 1 |] 
    |> Array.map(fun i ->
        let m = mc.[i]
        m.Index, m.Length)

let regex_match (regex: Regex) (text: string) =
    let m = regex.Match text
    if m.Success then
        m.Value
    else
        ""

let regexMatchCascade regexes (text: string) = 
    let mutable s = text
    regexes
    |> Array.iter(fun regex -> s <- regex_match(string__regex(regex))(s))
    s

let regex_matches (regex: Regex)(text: string) =
    let mc = regex.Matches(text)
    [| 0..mc.Count - 1 |] 
    |> Array.map(fun i -> mc.[i].Value)

let str__groups(regex: Regex)(text: string) =
    let m = regex.Match(text)
    if(m.Success) then
        [| 0..m.Groups.Count - 1 |] 
        |> Array.map(fun i -> m.Groups.[i].Value)
    else
        [| |]

let regex_match_group(regex: Regex)(text: string) =
    let fields = new List<string>()
    let m = regex.Match(text)
    if(m.Success) then
        [| 0..m.Groups.Count - 1 |] 
        |> Array.iter(fun i -> fields.Add(m.Groups.[i].Value))
    fields

let regex_match_groups(regex: Regex)(text: string) =
    let mc = regex.Matches(text)
    [| 0..mc.Count - 1|]
    |> Array.map(fun i ->
        let fields = new List<string>()
        let m = mc.[i]
        [| 0..m.Groups.Count - 1 |] 
        |> Array.iter(fun i -> fields.Add(m.Groups.[i].Value))
        fields)

let regex_match_uncompiled(regex:string)(text:string) = regex_match(new Regex(regex, regex_options))(text)
let regex_matches_uncompiled(regex:string)(text:string) = regex_matches(new Regex(regex, regex_options))(text)

let regex_replace(regex:Regex, replacing:string -> string)(text) =
        
    let ms = (regex.Matches text) |> Seq.toArray
    if ms.Length = 0 then
        text
    else
        let sb = new StringBuilder()

        let mutable index = 0
        [| 0..ms.Length - 1|]
        |> Array.iter(fun i -> 
            let m = ms.[i]
            text.Substring(index,m.Index - index) |> sb.Append |> ignore
            m.Value |> replacing |> sb.Append |> ignore
            index <- m.Index + m.Length)
            
        text.Substring(index,text.Length - index) |> sb.Append |> ignore

        sb.ToString()

let regex_clear(regex:Regex) = regex_replace(regex, (fun _ -> ""))

let regexMatchLink (regex:Regex) src = 
        
    (regex.Matches src)
    |> Seq.toArray
    |> Array.map(fun m -> 
        m.Index,m.Value)

//let replaceIgnoreCase (patternEscaped:string,replacement:string) (s:string) = 
//    Regex.Replace(s,(Regex.Escape patternEscaped),replacement,RegexOptions.IgnoreCase)

let replaceWithCase caseSensitive (pattern:string) (replacing:string -> string) (s:string) = 

    let res = new List<int>()

    let mutable i =
        if caseSensitive then
            s.IndexOf(pattern)
        else
            s.IndexOf(pattern,StringComparison.OrdinalIgnoreCase)
    res.Add i

    while(i >= 0) do
        if caseSensitive then
            i <- s.IndexOf(pattern,i + pattern.Length)
        else
            i <- s.IndexOf(pattern,i + pattern.Length,StringComparison.OrdinalIgnoreCase)
        res.Add i

    let sb = new StringBuilder()
    i <- 0
    [| 0..res.Count - 2|]
    |> Array.iter(fun ii -> 
        let index = res.[ii]
        let left = s.Substring(i,index - i)
        let r = replacing(s.Substring(index,pattern.Length))
        i <- index + pattern.Length
        sb.Append(left) |> ignore
        sb.Append(r) |> ignore)

    sb.Append(s.Substring(i)) |> ignore

    sb.ToString()

let splitWithCase caseSensitive (pattern:string) (s:string) = 

    let res = new List<int>()

    let mutable i =
        if caseSensitive then
            s.IndexOf(pattern)
        else
            s.IndexOf(pattern,StringComparison.OrdinalIgnoreCase)
    res.Add i

    while(i >= 0) do
        if caseSensitive then
            i <- s.IndexOf(pattern,i + pattern.Length)
        else
            i <- s.IndexOf(pattern,i + pattern.Length,StringComparison.OrdinalIgnoreCase)
        res.Add i

    let list = new System.Collections.Generic.List<string>()

    i <- 0
    [| 0..res.Count - 2|]
    |> Array.iter(fun ii -> 
        let index = res.[ii]
        let left = s.Substring(i,index - i)
        let r = s.Substring(index,pattern.Length)
        i <- index + pattern.Length
        list.Add left
        list.Add r)

    list.Add(s.Substring(i))

    list

let regexUnsignedInt = string__regex @"\d+"

//██████████████████████████████████████████████████████████████████████████████
//=== Non-Regex

let find (head:string,tail:string) (s:string) = 
    let i = s.IndexOf(head)
    if(i>=0) then
        let ii = s.IndexOf(tail,i+head.Length)
        if(ii >= i) then
            s.Substring(i + head.Length, ii - i - head.Length)
        else
            ""
    else
        ""
let findInLines(head:string,tail:string) lines = 
    let ohead = lines |> Array.tryFindIndex (fun line -> line = head)
    let otail = lines |> Array.tryFindIndexBack (fun line -> line = tail)
    if ohead.IsSome && otail.IsSome then
        let ihead = ohead.Value
        let itail = otail.Value
        if ihead + 1 <itail then
            Array.sub lines (ihead + 1) (itail - ihead - 1)
        else
            [||]
    else
        [||]

let findBidirectional(head:string,tail:string) (s:string) = 
    let i = s.IndexOf(head)
    if(i>=0) then
        let ii = s.LastIndexOf(tail,s.Length)
        if(ii >= i) then
            let start = i + head.Length
            let step1 = ii - i
                
            if (s.Length >= start) &&
                step1 >= head.Length &&
                s.Substring(start).Length >= (step1 - head.Length) then
                s.Substring(start, step1 - head.Length)
            else ""
        else ""
    else ""

let findFrom(head:string)(s:string) = 
    let i = s.IndexOf(head)
    if(i >=0 ) then
        s.Substring(i + head.Length)
    else
        ""

let findTo(tail:string)(s:string) = 
    let i = s.IndexOf(tail)
    if(i >=0) then
        s.Substring(0,i)
    else
        ""

let findInverse(head:string,tail:string)(s:string) = 
    let tail_ = new string(head.ToCharArray() |> Array.rev)
    let head_ = new string(tail.ToCharArray() |> Array.rev)
    let s_ = new string(s.ToCharArray() |> Array.rev)
        
    let res = find(head_,tail_) s_
    new string(res.ToCharArray() |> Array.rev)

let removeQuote(head:string,tail:string)(s:string) = 
    if(s.StartsWith(head) && s.EndsWith(tail)) then
        s.Substring(head.Length, s.Length - head.Length - tail.Length)
    else
        s

let matchPair(head:char,tail:char)(s:string) = 
    let sb = new StringBuilder()
    let mutable i = 0
    let mutable depth = 0

    while(i < s.Length) do
            
        let c = s.[i]

        if(c = tail) then
            depth <- depth - 1
            if(depth > 0) then
                sb.Append(c) |> ignore
        else if(c = head) then
            if(depth > 0) then
                sb.Append(c) |> ignore
            depth <- depth + 1
        else
            if(depth > 0) then
                sb.Append(c) |> ignore

        i <- i + 1

    sb.ToString()

let matchPairs(anchor:string,head:char,tail:char)(s:string) = 

    let items = new List<string>()

    let sb = new StringBuilder()
    let mutable i = s.IndexOf(anchor,0) + anchor.Length
    let mutable depth = 0

    while(i >= 0 && i < s.Length) do
            
        let c = s.[i]

        if(c = tail) then
            depth <- depth - 1
            if(depth > 0) then
                sb.Append(s.[i]) |> ignore
            else
                items.Add(sb.ToString())
                sb.Clear() |> ignore
                let ii = s.IndexOf(anchor,i)
                if(ii < 0) then
                    i <- s.Length
                else
                    i <- ii + anchor.Length - 1
        else if(c = head) then
            if(depth > 0) then
                sb.Append(s.[i]) |> ignore
            depth <- depth + 1
        else
            if(depth > 0) then
                sb.Append(s.[i]) |> ignore

        i <- i + 1

    if(sb.Length > 0) then
        items.Add(sb.ToString())

    items.ToArray()

let checkReplace (pattern:string) replaced (s:string) = 
    if s.Contains pattern then
        s.Replace(pattern,replaced)
    else
        s

let checkRemove (pattern:string) (s:string) = 
    if s.Contains pattern then
        s.Replace(pattern,"")
    else
        s

// Lines ============================================================

let str__lines (delim:string) (s:string) =
        
    let n = delim.Length

    let mutable index = 0

    let lines = new List<string>()

    let mutable terminate = false
    while not terminate do
        let i = s.IndexOf(delim, index + n)
        if i < 0 then
            s.Substring(index + n) |> lines.Add 
            terminate <- true
        else
            if index = 0 then
                s.Substring(index,i - index) |> lines.Add 
            else
                s.Substring(index + n,i - index - n) |> lines.Add 
            index <- i
            if index + 2 < s.Length then
                ()        
            else
                terminate <- true

    lines.ToArray()

// Time ==============================================================

let date_key(utc:DateTime)=
    utc.Year.ToString("0000") + utc.Month.ToString("00") + utc.Day.ToString("00")

let date(utc:DateTime)=
    utc.Year.ToString("0000") + "/" + utc.Month.ToString("00") + "/" + utc.Day.ToString("00")

let time_key(utc:DateTime)=
    utc.Year.ToString("0000") + utc.Month.ToString("00") + utc.Day.ToString("00") + utc.Hour.ToString("00") + utc.Minute.ToString("00") + utc.Second.ToString("00")

let time(utc:DateTime)=
    utc.Year.ToString("0000") + "/" + utc.Month.ToString("00") + "/" + utc.Day.ToString("00") + " " + utc.Hour.ToString("00") + ":" + utc.Minute.ToString("00") + ":" + utc.Second.ToString("00")

let timeonly_key(utc:DateTime)=
    utc.Hour.ToString("00") + utc.Minute.ToString("00") + utc.Second.ToString("00")

let timeonly(utc:DateTime)=
    utc.Hour.ToString("00") + ":" + utc.Minute.ToString("00") + ":" + utc.Second.ToString("00")

let now_date_key() = date_key DateTime.UtcNow

let now_date() = date DateTime.UtcNow

let now_time_key() = time_key DateTime.UtcNow

let now_time() = time DateTime.UtcNow

// 8 -> Oct
let month__xxx m =
    match m with
    | 1 -> "Jan"
    | 2 -> "Feb"
    | 3 -> "Mar"
    | 4 -> "Apr"
    | 5 -> "May"
    | 6 -> "Jun"
    | 7 -> "Jul"
    | 8 -> "Oct"
    | 9 -> "Sep"
    | 10 -> "Oct"
    | 11 -> "Nov"
    | _ -> "Dec"


let regex_int = new Regex(@"[\x2b+,\x2d]?\d+", regex_options)

let parse_regex_int32 (text, regex : Regex) =
    let matched = regex.Match(text)
    if(matched.Success = true) then
        Int32.Parse(matched.Value)
    else
        0

let parse_regex_Datatime (text, regex : Regex) =
        let matched = regex.Match(text)
        if(matched.Success = true) then
            DateTime.Parse(matched.Value)
        else
            DateTime.UtcNow
let parse_DateTime(text:string)=
        parse_regex_Datatime(text, regex_int)
    
let parse_yyyy_MM_dd__DateTime(dateString) =
    let formatString = "yyyy-MM-dd"
    if dateString <>"" then 
        DateTime.ParseExact(dateString, formatString, null)
    else DateTime.MinValue

let try_parse_int32(s:string) =
    let mutable res = ref 0
    if Int32.TryParse(s,res) then
        Some res.Value
    else
        None

let try_parse_uint32(s:string) =
    let mutable res = ref 0u
    if UInt32.TryParse(s,res) then
        Some res.Value
    else
        None

let try_parse_int64(s:string) =
    let mutable res = 0L
    if(Int64.TryParse(s,&res)) then
        Some res
    else
        None

let parse_int32(s:string) =
    let mutable res = ref 0
    Int32.TryParse(s,res) |> ignore
    res.Value
    
let parse_int64(s:string) =
    let mutable res = 0L
    Int64.TryParse(s.Trim(),&res) |> ignore
    res

let parse_double(text:string) =
    if(text.Length > 0) then
        Double.Parse(text)
    else
        (double)0.0
let parse_bool(text:string)=
    if(text="true"||text="1") then
        true
    else    
        false

let int__DateTime (timestamp:int64) =
    let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
    start.AddSeconds(float timestamp).ToLocalTime()

let str_DateTime() =
    let mutable dt = System.DateTime.UtcNow
    System.DateTime.TryParse("12-20-04 12:21:00", &dt) |> ignore
    dt

let parse_regex_double (text, regex : Regex) =
    let matched = regex.Match(text)
    if(matched.Success = true) then
        parse_double(matched.Value)
    else
        (double)0.0

let parse_float(text:string) =
    if(text.Length > 0) then
        let mutable res = 0.0
        if(Double.TryParse(text,&res)) then
            res
        else
            0.0
    else
        0.0

let try_parse_float(s:string) =
    let mutable res = 0.0
    if(Double.TryParse(s,&res)) then
        Some res
    else
        None

let parse_regex_float (text, regex : Regex) =
    let matched = regex.Match(text)
    if(matched.Success = true) then
        parse_float(matched.Value)
    else
        0.0

let parse_float32(text:string) =
    if(text.Length > 0) then
        let mutable res = 0.0f
        if(Single.TryParse(text,&res)) then
            res
        else
            0.0f
    else
        0.0f

let try_parse_float32(s:string) =
    let mutable res = 0.0f
    if(Single.TryParse(s,&res)) then
        Some res
    else
        None

let parse_deciaml(text:string) = 
    if text.Length > 0 then
        let mutable res = 0.0M
        if Decimal.TryParse(text,&res) then
            res
        else 
            0.0M
    else
        0.0M

let fix_digit(digit:int)(text:string) =
    if(text.Contains(".")) then
        let t1, t2 =
            let index = text.IndexOf(".")
            text.Substring(0, index), text.Substring(index + 1)
        if(digit > 0) then
            if(t2.Length > digit) then
                t1 + "." + t2.Substring(0, digit)
            else
                t1 + "." + t2.PadRight(digit, '0')
        else
            t1
    else if(digit > 0) then
        text + "." + "".PadRight(digit, '0')
    else
        text

let fix_digit_float(digit:int)(v:float) =
    let formatter =
        if(digit > 0) then
            "0." + "".PadRight(digit, '0')
        else
            "0"
    let vv = v.ToString(formatter)
    fix_digit(digit)(vv)

let split(splitor:string)(src:string) =
    let ra = new ResizeArray<string>()
    let mutable i = 0
    let mutable next = 0
    while (next >= 0) do
        next <- src.IndexOf(splitor, i)
        if(next >= 0) then
            ra.Add(src.Substring(i, next - i))
            i <- next + splitor.Length
        else
            ra.Add(src.Substring(i))
    ra.ToArray()

// =========== Encoding ========================

let str__md5_32(input:string) =
    
    use md5 = System.Security.Cryptography.MD5.Create()
    
    let inputBytes:byte[] = System.Text.Encoding.ASCII.GetBytes(input)
    let hashBytes:byte[] = md5.ComputeHash(inputBytes)

    let sb = new List<string>()
    hashBytes
    |> Seq.iter(fun b -> sb.Add(b.ToString("X2")))

    sb |> String.Concat

let html__str(src:string) = 
    let mutable s = src
    s <- s.Replace("&nbsp;"," ")
    s <- s.Replace("&hellip;","…")
    s <- s.Replace("&mdash;","—")
    s <- s.Replace("&rsquo;","'")
    s <- s.Replace("&ldquo;","“")
    s <- s.Replace("&rdquo;","” ")
    s <- s.Replace("&middot;","·")
    //s <- s.Replace("&amp;","%x26")
    //s <- s.Replace("&","\%26")
    s.Trim()

let html__decode = System.Web.HttpUtility.HtmlDecode

// =========== User friendly display ========================

let intstr__userFriendly (v:string) = 
    let mutable s = v
    if s.Length > 12 then
        s <- s.Insert(s.Length - 12," ")
        s <- s.Insert(s.Length - 9," ")
        s <- s.Insert(s.Length - 6," ")
        s <- s.Insert(s.Length - 3," ")
    elif s.Length > 9 then
        s <- s.Insert(s.Length - 9," ")
        s <- s.Insert(s.Length - 6," ")
        s <- s.Insert(s.Length - 3," ")
    elif s.Length > 6 then
        s <- s.Insert(s.Length - 6," ")
        s <- s.Insert(s.Length - 3," ")
    elif s.Length > 3 then
        s <- s.Insert(s.Length - 3," ")
    s

let int64__userFriendly (v:int64) = 
    v.ToString() |> intstr__userFriendly

let int32__userFriendly (v:int32) = 
    v.ToString() |> intstr__userFriendly

let float__userFriendly00 (v:float) = 
    if v > 0.001 then
        let s = v.ToString("0.00")
        let index = s.IndexOf(".")
        let integer = 
            s.Substring(0,index)
            |> intstr__userFriendly
        integer + s.Substring(index)
    else
        let mutable s = v.ToString("0.000000000")
        if s.Length > 9 then
            s <- s.Insert(8," ")
            s <- s.Insert(5," ")
        elif s.Length > 6 then
            s <- s.Insert(5," ")
        s

let str_abstract(max:int)(s:string) =
    if(s.Length > max) then
        s.Substring(0,max) + "...("+s.Length.ToString()+")"
    else
        s

let float__str(v:float) = 
    let mutable tmp = Math.Abs(v)
    if(tmp > 1.0)then
        v.ToString()
    else if(tmp = 0.0) then
        v.ToString()
    else
        let mutable offset = 0
        while(tmp < 0.001) do
            tmp <- tmp * 1000.0
            offset <- offset + 3

        let mutable s = tmp.ToString("")
            
        let mutable index = 
            let i = s.IndexOf(".")
            if(i < 0) then
                s.Length
            else
                i

        s <- s.Replace(".","")
            
        index <- index - offset
        if(index < 0)then
            s <- s.PadLeft(s.Length - index, '0')
            s <- "0." + s
        else
            s <- s.Insert(index,".")
                
        if(v<0.0) then
            s <- "-" + s

        s

let timespan__userfriendly(ts:TimeSpan) =
    if(ts.TotalDays < -1.0)then
        ts.TotalDays.ToString("0.00")+" D"
    else if(ts.TotalHours < -1.0)then
        ts.TotalHours.ToString("0.00")+" H"
    else if(ts.TotalMinutes < -1.0)then
        ts.TotalMinutes.ToString("0.00")+" M"
    else if(ts.TotalSeconds < -1.0)then
        ts.TotalSeconds.ToString("0.00")+" S"
    else if(ts.TotalDays > 1.0)then
        ts.TotalDays.ToString("0.00")+" D"
    else if(ts.TotalHours > 1.0)then
        ts.TotalHours.ToString("0.00")+" H"
    else if(ts.TotalMinutes > 1.0)then
        ts.TotalMinutes.ToString("0.00")+" M"
    else 
        ts.TotalSeconds.ToString("0.00")+" S"

let timestamp__userfriendly(dt:DateTime) = DateTime.UtcNow.Subtract(dt) |> timespan__userfriendly

let getverifycode()=
    let ticks = DateTime.UtcNow.Ticks.ToString()
    ticks.Substring(ticks.Length-6)


let IsInteger(str:string)=
    let pattern = @"^\d*$";
    Regex.IsMatch(str,pattern);
            
let float_formats(f:float,dec:float)=
    let mutable i=1.0
    let mutable m=0.1M
    while(i<dec)do
        m<-m*0.1M
        i<- i+1.0
    //let a= Math.Pow(0.1,(decimal)dec)
    let b= Math.Floor(f/(float)m)
    b*(float)m

let truncated_float(value:float,digits:int)=
    Math.Round(value,digits,MidpointRounding.AwayFromZero)

let intercept_float(d:float,s:float)=
    let sp = Convert.ToDouble(Math.Pow(10.00, s))      
    if d < 0.00 then
        Math.Truncate(d) + Math.Ceiling((d - Math.Truncate(d)) * sp) / sp;
    else
        Math.Truncate(d) + Math.Floor((d - Math.Truncate(d)) * sp) / sp

// ========= PERFORMANCE =================

let lines__sb(sb:System.Text.StringBuilder,delim:string,head:string,tail:string)(lines:string[]) = 
    sb.Append(head) |> ignore

    if(lines.Length > 0) then
        lines.[0] |> sb.Append |> ignore
        [| 1 .. lines.Length - 1 |]
        |> Array.iter(fun i -> 
            if(delim.Length > 0)then
                delim |> sb.Append |> ignore
            lines.[i] |> sb.Append |> ignore)

    sb.Append(tail) |> ignore

let lines__populatorSb(sb:System.Text.StringBuilder,delim:string,head:string,tail:string,populator)(lines:'T[]) = 
    sb.Append(head) |> ignore

    if(lines.Length > 0) then
        populator sb lines.[0]
        [| 1 .. lines.Length - 1 |]
        |> Array.iter(fun i -> 
            if(delim.Length > 0)then
                delim |> sb.Append |> ignore
            populator sb lines.[i])

    sb.Append(tail) |> ignore

let lines__str(delim:string,head:string,tail:string)(lines:string[]) = 
    let sb = new System.Text.StringBuilder()

    sb.Append(head) |> ignore

    if(lines.Length > 0) then
        lines.[0] |> sb.Append |> ignore
        [| 1 .. lines.Length - 1 |]
        |> Array.iter(fun i -> 
            if(delim.Length > 0)then
                delim |> sb.Append |> ignore
            lines.[i] |> sb.Append |> ignore)

    sb.Append(tail) |> ignore
    sb.ToString()

let linesConcatSb (sb:StringBuilder) = Array.iter(fun (i:string) -> i |> sb.Append |> ignore)

let linesConcat(lines:string[]) = 
    let sb = new System.Text.StringBuilder()
    linesConcatSb sb lines
    sb.ToString()

let checkUselessCharacters (chars:string) : bool =
    let mutable res = true
    for ss in uselessStrings do
        if equalIgnoreCase(ss, chars) then
            res <- false
    res
        
let fastConcat(delim:string)(lines:string[]) = 
    if(lines.Length = 0) then
        String.Empty
    else
        let sb = new System.Text.StringBuilder()
        lines.[0] |> sb.Append |> ignore
        [| 1 .. lines.Length - 1 |]
        |> Array.iter(fun i ->
                
            let line = lines.[i]
                       
            if(delim.Length > 0)then
                delim |> sb.Append |> ignore
                    
            line |> sb.Append |> ignore)
        sb.ToString()

let fastIfConcatSb
    (sb:System.Text.StringBuilder,populator,delim:string)
    (lines:'a[]) = 
    if lines.Length > 0 then
        lines
        |> Array.iter(fun i ->
            if populator sb i then
                if delim.Length > 0 then
                    delim 
                    |> sb.Append 
                    |> ignore)
    if sb.Chars(sb.Length - 1) = ',' then
        sb.Remove(sb.Length - 1, 1)|>ignore

let fastConcatSb(sb:System.Text.StringBuilder,populator,delim:string)(lines:'a[]) = 
    if(lines.Length > 0) then
        populator sb lines.[0]
        [| 1 .. lines.Length - 1 |]
        |> Array.iter(fun i -> 
            if(delim.Length > 0)then
                delim |> sb.Append |> ignore
            populator sb lines.[i]
        )

// .NET Framework

//let cht__chs s = 
//    Strings.StrConv(s,VbStrConv.SimplifiedChinese)
        
//let chs__cht s = 
//    Strings.StrConv(s,VbStrConv.TraditionalChinese)

// .NET Core

let chConvert = 
    Microsoft.International.Converters.TraditionalChineseToSimplifiedConverter.ChineseConverter.Convert

let cht__chs s = 
    chConvert(s,Microsoft.International.Converters.TraditionalChineseToSimplifiedConverter.ChineseConversionDirection.TraditionalToSimplified)

let chs__cht s = 
    chConvert(s,Microsoft.International.Converters.TraditionalChineseToSimplifiedConverter.ChineseConversionDirection.SimplifiedToTraditional)



//let rUnicode = string__regex(@"[\x5c][uU][0-9A-Fa-f]{4}")
let rUnicode = string__regex(@"[%\\][uU][0-9A-Fa-f]{4}")

let checkEscape(src:string) =
        
    let mutable s = src

    [| ("\\","\\\\");
        ("\"","\\\"");
        ("\r","\\r");
        ("\n","\\n");
        ("\t","\\t");
        ("","?") |]
    |> Array.iter(fun item -> 
        let a,b = item
        s <- s.Replace(a,b))

    let sb = new StringBuilder()

    let list = new List<char>(s.ToCharArray())
    [| 0 .. list.Count - 1|]
    |> Array.iter(fun i ->
        let c = list.[i]

        if(c = '\"') then
            if(i > 0) then
                if(list.[i-1] <> '\\') then
                    sb.Append('\\') |> ignore
        sb.Append(c) |> ignore)

    sb.ToString()

let regex_remove_quote = new Regex(@"(?<=\x22).*?(?=\x22)", regex_options)

let regex_bigbracket = new Regex(@"\x7b.*?\x7d", regex_options) // {}
let regex_midbracket = new Regex(@"\x5b.*?\x5d", regex_options) // []
let regex_smlbracket = new Regex(@"\x28.*?\x29", regex_options) // ()

let regex_field = new Regex(@"\x22.+?\x22:\s*", regex_options)
let regex_key = new Regex(@"(?<=\x22).*?(?=\x22:)", regex_options)
let regex_val = new Regex(@"[^\x22]+", regex_options)  

let str__escape(s:string) = 

    let mutable res = s

    [| ("\\","\\\\");
        ("\"","\\\"");
        ("\r","\\r");
        ("\n","\\n");
        ("\t","\\t");
        ("","?") |]
    |> Array.iter(fun item -> 
        let a,b = item
        res <- res.Replace(a,b))

    res

let escape__str(s:string) = 

    let mutable res = s

    [| ("\\t","\t");
        ("\\n","\n");
        ("\\r","\r");
        ("\\\\","\\") |]
    |> Array.iter(fun item -> 
        let a,b = item
        res <- res.Replace(a,b))

    res

let unescape_unicode(u:string) =
    regex_replace (rUnicode,(fun src -> 
        let b1 = src.Substring(2,2)
        let b2 = src.Substring(4,2)
        let c1 = Byte.Parse(b1,System.Globalization.NumberStyles.AllowHexSpecifier)
        let c2 = Byte.Parse(b2,System.Globalization.NumberStyles.AllowHexSpecifier)
        let bytes = [|c2;c1|]
        let uni = Encoding.Unicode.GetChars(bytes)
        string uni[0]
        )) u

let escape_unicode(u:string) =
    let sb = new StringBuilder()
    u |> Seq.iter (
        fun x->
            let c = sprintf "%x" (int64 x)
            if c.Length=4 then
                sb.Append( "\\u" + c)|>ignore
            else sb.Append(x)|>ignore
    )
    sb.ToString()

let cleanStartEndUselessChar (target:string) =
    let mutable s = target
    uselessHtmlChars
    |> Array.iter (fun c ->
        if s.StartsWith c then
            s <- s.Substring(1)
        if s.EndsWith c then
            s <- s.Substring(0,s.Length-1)
        )
    s

let concatStringWithConditionAtLineEnd (lines:string[]) =
    if(lines.Length = 0) then
        String.Empty
    else
        let sb = StringBuilder()
        lines
        |> Array.iter (fun line ->
            if line.StartsWith "* " then
                sb.Append (line + crlf) |> ignore
            else sb.Append (line + crlf + crlf) |> ignore
        )
            
        sb.ToString()

let reg1 = string__regex("(.*?)([\r\n]+)")
let getLineAndSplitterArr(src:string) =
    let indexOfFstNone (arr: (int*string*string*string)[]) : int=
        arr |> Array.findIndex (fun t4 ->
            let i,_,_,_ = t4
            i = -1
        )

    let headMatches = reg1.Matches src
    let head = 
        [| 0..headMatches.Count - 1 |] 
        |> Array.map(fun i -> headMatches.[i].Value)
        |> String.concat ""

    // (flag, content, line, Spliter)
    let contentArr: (int*string*string*string)[] = Array.create (headMatches.Count + 2) (-1,"","","")

    for m in headMatches do
        let g = m.Groups
        if g.Count = 3 then 
            contentArr[indexOfFstNone contentArr] <- (0,g[0].Value,g[1].Value,g[2].Value)
        else
            contentArr[indexOfFstNone contentArr] <- (0,m.Value,"","")

    if src.Length > head.Length then
        let last = src.Substring(head.Length, src.Length - head.Length)
        contentArr[indexOfFstNone contentArr] <- (0,last,last,"")
    
    contentArr 
    |> Array.filter (fun t4 ->
        let i,_,_,_ = t4
        i <> -1 )

let uselessChars = [|
    // "\x09"
    // "\x0A"
    // "\x0D"
    "\x00"
    "\x01"
    "\x02"
    "\x03"
    "\x04"
    "\x05"
    "\x06"
    "\x07"
    "\x08"
    "\x0B"
    "\x0C"
    "\x0E"
    "\x0F"
    "\x10"
    "\x11"
    "\x12"
    "\x13"
    "\x14"
    "\x15"
    "\x16"
    "\x17"
    "\x18"
    "\x19"
    "\x1A"
    "\x1B"
    "\x1C"
    "\x1D"
    "\x1E"
    "\x1F"
    "\x7F"
|]

let cleanUselessChars (str: string) =
    let mutable s = str
    uselessChars
    |> Array.iter (fun c -> s <- s.Replace(c, "") )
    
    s

let htmlCharsDict =
    let d = Dictionary<string,string>()
    d["&ldquo;"] <- "“"
    d["&rdquo;"] <- "”"
    d["&amp;"] <- "&"
    d[" "] <- " "
    d["&apos;"] <- "'"
    d["&nbsp;"] <- " "
    d["&iquest;"] <- "¿"
    d["&sbquo;"] <- "‚"
    d["&prime;"] <- "′"
    d["&Prime;"] <- "″"
    d["&lt;"] <- "<"
    d["&gt;"] <- ">"
    d["&acute;"] <- "´"
        
    d

    
let convertHtmlEncodedChar (s:string) =
    let mutable ns = s
    for kv in htmlCharsDict do
        ns <- ns.Replace(kv.Key, kv.Value)
    ns

let reg_gt2Blank = string__regex "[\r\n]+(\s{2,})?"
let tidyLineCharAndNeedlessBlanks line =
    reg_gt2Blank.Replace(line ," ")
 


// Diff =============================================================

// type DiffPoint<'T> = 
// | Insert of 'T
// | Remove of int
// | Keep

// let diffPoint__bin (data:DiffPoint<'T>) = [||]
// let bin__diffPoint (bin:byte[]) = DiffPoint<'T>.Keep

// type Diff<'T> = {
//     editorID: int64
//     editedat: DateTime
//     vernum: int64
//     diffPoints: DiffPoint<'T>[]
// }

// let diff__bin = [||]
// let bin__diff = [||]


// type VersionHistory<'T> = {
//     current: 'T[]
//     history: Diff<'T> list
// }

// let diff2 (current:'T[]) (prev:'T[]) = [||]

// let rebuild (current:'T[]) (diff2:DiffPoint<'T>[]) = [||]

// // 文章编辑的Undo功能
// let rollback (current:'T[]) (diff2:DiffPoint<'T>[]) = [||]

// type VersionHistoryTxt = VersionHistory<char>    
// type VersionHistoryBin = VersionHistory<byte>

let txt__uniform (str:string) =
    str.Trim().ToLower()
    |> cht__chs
        
let txt__webUniform (str:string) =
    str
    |> txt__uniform
    |> System.Web.HttpUtility.JavaScriptStringEncode

let formatStringMap =
    let decodeSlashes (str : string) =
        // Kestrel has made the weird decision to
        // partially decode a route argument, which
        // means that a given route argument would get
        // entirely URL decoded except for '%2F' (/).
        // Hence decoding %2F must happen separately as
        // part of the string parsing function.
        //
        // For more information please check:
        // https://github.com/aspnet/Mvc/issues/4599
        str.Replace("%2F", "/").Replace("%2f", "/")

    let toGuid (shortGuid : string) =
        shortGuid.Replace("_", "/")
                 .Replace("-", "+")
        |> (fun str -> str + "==")
        |> Convert.FromBase64String
        |> Guid

    let toUInt64 (shortId : string) =
        let bytes =
            shortId.Replace("_", "/")
                   .Replace("-", "+")
            |> (fun str -> str + "=")
            |> Convert.FromBase64String
            |> (fun arr ->
                match BitConverter.IsLittleEndian with
                | true  -> Array.Reverse arr; arr
                | false -> arr)
        BitConverter.ToUInt64 (bytes, 0)

    let parseGuid (str : string) =
        match str.Length with
        | 22 -> toGuid str
        | _  -> Guid str

    let guidPattern =
        "([0-9A-Fa-f]{8}\-[0-9A-Fa-f]{4}\-[0-9A-Fa-f]{4}\-[0-9A-Fa-f]{4}\-[0-9A-Fa-f]{12}|[0-9A-Fa-f]{32}|[-_0-9A-Za-z]{22})"

    let shortIdPattern = "([-_0-9A-Za-z]{10}[048AEIMQUYcgkosw])"

    dict [
    // Char    Regex                    Parser
    // -------------------------------------------------------------
        'b', ("(?i:(true|false)){1}",   (fun (s : string) -> bool.Parse s)   >> box)  // bool
        'c', ("([^/]{1})",              char                                 >> box)  // char
        's', ("([^/]+)",                decodeSlashes                        >> box)  // string
        'i', ("(-?\d+)",                int32                                >> box)  // int
        'd', ("(-?\d+)",                int64                                >> box)  // int64
        'f', ("(-?\d+\.{1}\d+)",        float                                >> box)  // float
        'O', (guidPattern,              parseGuid                            >> box)  // Guid
        'u', (shortIdPattern,           toUInt64                     >> box)  // uint64
    ]

let convertToRegexPatternAndFormatChars (formatString : string) =
    let rec convert (chars : char list) =
        match chars with
        | '%' :: '%' :: tail ->
            let pattern, formatChars = convert tail
            "%" + pattern, formatChars
        | '%' :: c :: tail ->
            let pattern, formatChars = convert tail
            let regex, _ = formatStringMap.[c]
            regex + pattern, c :: formatChars
        | c :: tail ->
            let pattern, formatChars = convert tail
            c.ToString() + pattern, formatChars
        | [] -> "", []

    let inline formatRegex pattern =
        "^" + pattern + "$"

    formatString
    |> List.ofSeq
    |> convert
    |> (fun (pattern, formatChars) -> formatRegex pattern, formatChars)

let tryMatchInput (format : PrintfFormat<_,_,_,_, 'T>) (input : string) =
    try
        let pattern, formatChars =
            format.Value
            |> Regex.Escape
            |> convertToRegexPatternAndFormatChars 

        let options =
            RegexOptions.IgnoreCase

        let result = Regex.Match(input, pattern, options)

        if result.Groups.Count <= 1
        then None
        else
            let groups =
                result.Groups
                |> Seq.cast<Group>
                |> Seq.skip 1

            let values =
                (groups, formatChars)
                ||> Seq.map2 (fun g c ->
                    let _, parser   = formatStringMap.[c]
                    let value       = parser g.Value
                    value)
                |> Seq.toArray

            let result =
                match values.Length with
                | 1 -> values.[0]
                | _ ->
                    let types =
                        values
                        |> Array.map (fun v -> v.GetType())
                    let tupleType = FSharp.Reflection.FSharpType.MakeTupleType types
                    FSharp.Reflection.FSharpValue.MakeTuple(values, tupleType)
            result
            :?> 'T
            |> Some
    with
    | _ -> None


//██████████████████████████████████████████████████████████████████████████████
//=== List-based text buffer

type TextBlock = 
| AppendEnd of string
| MultiLine of string[]
| Newline of string
| NewlineBlank
| NewlineBlankIndent of int
| NewlineIndent of int * string
| MultiLineIndent of int * string[]

let writeTextBlock (buffer:List<string>) tb = 
    
    match tb with
    | AppendEnd s -> 
        if buffer.Count = 0 then
            buffer.Add s
        else
            buffer[buffer.Count - 1] <- buffer[buffer.Count - 1] + s
    | MultiLine lines -> lines |> buffer.AddRange
    | Newline s -> s |> buffer.Add
    | NewlineBlank -> "" |> buffer.Add
    | NewlineBlankIndent indent -> tabs[indent] |> buffer.Add
    | NewlineIndent (indent,s) -> tabs[indent] + s |> buffer.Add
    | MultiLineIndent (indent,lines) -> 
        lines
        |> Array.map(fun s -> tabs[indent] + s)
        |> buffer.AddRange


type TextBlockWriter (buffer:List<string>) = 

    let buffer = buffer

    member this.Buffer() = buffer
    member this.lineCount() = buffer.Count
    member this.index__line index = buffer[index]

    member this.appendEnd = AppendEnd >> writeTextBlock buffer
    member this.multiLine = MultiLine >> writeTextBlock buffer
    member this.multiLineConcate delim = Array.toSeq >> String.concat delim >> Newline >> writeTextBlock buffer
    member this.newline = Newline >> writeTextBlock buffer
    member this.newlineBlank() = NewlineBlank |> writeTextBlock buffer
    member this.newlineBlankIndent = NewlineBlankIndent >> writeTextBlock buffer
    member this.newlineIndent indent s = (indent,s) |> NewlineIndent |> writeTextBlock buffer
    member this.multiLineIndent indent lines = (indent,lines) |> MultiLineIndent |> writeTextBlock buffer

    member this.backspace() = 
        let index = buffer.Count - 1
        if index >= 0 then
            let line = buffer[buffer.Count - 1]
            if line.Length > 0 then
                buffer[buffer.Count - 1] <- line.Substring(0,line.Length - 1)
            else
                buffer.RemoveAt index

    member this.text() = 
        buffer.ToArray()
        |> String.Concat

    member this.count() = 
        let ary = buffer.ToArray()
        if ary.Length = 0 then 
            0
        else
            ary
            |> Array.map(fun i -> i.Length)
            |> Array.sum

    member this.clear() = buffer.Clear()

    member this.concat delim = 
        buffer.ToArray()
        |> String.concat delim

    member this.index__char index = 
        this.text()[index]

    member this.lastindex__char index = 
        let s = this.text()
        s[s.Length - 1 - index]

    member this.removeBrakets() =
        if buffer.Count > 1 then
            if buffer.[0].Length > 0 then
                if buffer.[0].[0] = '{' then
                    buffer.RemoveAt 0
                    this.backspace()
                    this.backspace()

let empty__TextBlockWriter() = new TextBlockWriter(new List<string>())

let fastConcatTbw delim populator (lines:'a[]) = 
    let w = empty__TextBlockWriter()
    lines
    |> Array.map (
        fun i -> 
            let w1 = empty__TextBlockWriter()
            populator w1 i
            w1.text()
    )
    |> Array.filter(fun i -> i.Length > 0)
    |> Array.iter(w.newline)
    w.concat delim

let fastConcatTbwNotNull = fastConcatTbw