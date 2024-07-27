module Util.Html

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions

open Util.Json
open Util.Perf
open Util.Text



let r1 = string__regexIgnoreCase "<.*?>"
let r9 = string__regexIgnoreCase "\S+"
let r3 = string__regexIgnoreCase "<SCRIPT.*?</SCRIPT>"
let r4 = string__regexIgnoreCase "<!--.*?-->"
let r5 = string__regexIgnoreCase "<STYLE.*?</STYLE>"

let unCharRegex_nonBracket = str__regex "\W+"    // 非单词字符
let charRegex_nonBracket = str__regex "\w+"    // 单词字符
let charRegex = str__regex(@"(\W+)")
let blankRegex = str__regex(@"(\s+)")
let escapeCharRegex = str__regex(@"(\\+)")

type ContentOfPosition =
    | Head = 0
    | Tail = 1
        
type Node = 
    {
        mutable index: int;
        mutable length: int;
        mutable depth: int;
        mutable matched: string;
        mutable name: string;
        mutable parent: int;
        mutable closing: int;
        mutable offsprings: List<Node>;
        mutable data: string;
        mutable text: string;
        mutable next: Node option }

    with override this.ToString() = 

            let stext = 
                if(this.text.Length > 50) then
                    this.text.Substring(0,50) + " ..."
                else
                    this.text

            [|  this.index.ToString().PadLeft(4);
                " -> ";
                this.parent.ToString().PadLeft(4);
                " / ";
                this.closing.ToString().PadLeft(4);
                "".PadLeft(1 + this.depth * 4);
                " [" + this.name + "]";
                " (" + this.offsprings.Count.ToString() + ")";
                " \"" + stext + "\""   |]
            |> linesConcat

let empty__Node(index,text:string,matched:string) = {
    index = index;
    length = text.Length;
    depth = 0;
    matched = matched;
    name = "";
    text = text;
    parent = 0;
    closing = index;
    data = "";
    offsprings = new List<Node>();
    next = None }

let rec output n depth = 
        
    System.Console.WriteLine(n.ToString())

    n.offsprings.ToArray()
    |> Array.iter(fun i -> 
        output i (depth + 1))

let rec dfs h (n:Node) = 
    h n
    match n.next with
    | Some nn -> dfs h nn
    | None -> ()

let findNode (name:string,properties:string) n = 
        
    let mutable reso = None

    let mutable o = Some n
    while reso.IsNone && o.IsSome do
            
        let nn = o.Value

        let mutable matched = equalIgnoreCase(nn.name,name)
        if(matched) then
            matched <- containsIgnoreCase properties nn.text
        if matched then
            reso <- Some nn
            let mutable flag = 0
            let mutable o1 = reso.Value.next
            while flag = 0 && o1.IsSome do
                if o1.Value.closing = reso.Value.index then
                    o1.Value.next <- None
                o1 <- o1.Value.next

        o <- nn.next
        if o.IsSome then
            if o.Value.closing = n.index then
                o <- None

    reso

let findNodeAll (name:string,properties:string) n = 
        
    let list = new List<Node>()

    let mutable o = Some n
    while o.IsSome do
            
        let nn = o.Value

        let mutable matched = equalIgnoreCase(nn.name,name)
        if(matched) then
            matched <- containsIgnoreCase properties nn.text
        if matched then
            list.Add nn

        o <- nn.next
        if o.IsSome then
            if o.Value.closing = n.index then
                o <- None

    list.ToArray()

let rec findPath (path:(string*string)[]) n =
    let mutable flag = 0
    let mutable count = 0
    let mutable o = None
    while flag = 0 && count < path.Length do
        o <- findNode path.[count] n
        if o.IsSome then
            let mutable x = o
            while x.Value.next.IsSome do
                x <- x.Value.next
            if x.Value.name <> "/" + o.Value.name then
                o <- None
            flag <- 1
        count <- count + 1

    o
    
/// <summary>
/// find the last index of param "path" in the dom tree hierarchically
/// </summary>
/// <param name="path">Hierarchical path, such as [| ("a","class=axxx");("b","class=bxxx")|];The "b" tag pair are in "a" tag pair</param>
/// <param name="n">dom.root</param>
let rec findPathHierarchically (path:(string*string)[]) n =
    let mutable o = findNode path.[0] n
    [| 1..path.Length-1 |]
    |> Array.iter(fun i -> 
        if(o.IsSome) then
            o <- findNode path.[i] (o.Value))
    o


let rec findLeaf n (name:string,properties:string) = 
        
    if n.offsprings.Count = 0 then
        let mutable matched = equalIgnoreCase(n.name,name)
        if(matched) then
            matched <- containsIgnoreCase properties n.text
        if(matched) then
            Some n
        else
            None

    else
        let mutable o = None
        n.offsprings.ToArray()
        |> Array.iter(fun i -> 
            if(o.IsNone) then
                o <- findLeaf i (name,properties))
        o
            
let rec findLeafs (res:List<Node>) n (name:string,properties:string) = 
        
    if n.offsprings.Count = 0 then
        if n.name.StartsWith "/" = false then
            let mutable matched = equalIgnoreCase(n.name,name)
            if(matched) then
                matched <- containsIgnoreCase properties n.text
            if(matched) then
                res.Add n

    else
        n.offsprings.ToArray()
        |> Array.iter(fun i -> 
            findLeafs res i (name,properties))

let deleteNodes (path:(string*string)[]) node =
    use cw = new CodeWrapper("WebInterop.WebCrawl.htmlPath__node - Html.deleteNodes")

    for (tagName, attr) in path do
        let rec findLinkedListSpecificElem (parent:Node option) (child:Node) =
                
            match child.next with
            | Some childNext ->
                match equalIgnoreCase(child.name, tagName) && containsIgnoreCase attr child.text with
                | true ->
                    let nnOffsprings = child.offsprings
                    // tag 对, 有子集时
                    match nnOffsprings.Count > 0 with
                    | true ->
                        // 匹配到的tag的最后一个子元素.next = current_closeTag
                        let lastOffspring = nnOffsprings.[nnOffsprings.Count - 1]
                        let currentCloseTagNode = lastOffspring.next
                        // currentCloseTagNode.next 为 同级下一个 node, 如果 = null ,则 把最终给到
                        match parent with
                        | Some p -> 
                            match currentCloseTagNode with
                            | Some cctn ->
                                match cctn.next with
                                | Some cctnn ->
                                    p.next <- Some cctnn
                                    findLinkedListSpecificElem parent cctnn
                                | None ->
                                    p.next <- None
                            | None ->
                                p.next <- None

                        | None ->
                            match currentCloseTagNode with
                            | Some cctn ->
                                match cctn.next with
                                | Some cctnn ->
                                    child.next <- Some cctnn
                                    child.name <- ""
                                    child.text <- ""
                                    findLinkedListSpecificElem (Some child) cctnn
                                | None ->
                                    child.next <- None
                                    child.name <- ""
                                    child.text <- ""

                            | None ->
                                    child.next <- None
                                    child.name <- ""
                                    child.text <- ""

                    // 单 tag <a><img xxx></a>
                    | false ->
                        match parent with
                        | Some p -> 
                            p.next <- Some childNext
                            findLinkedListSpecificElem parent childNext
                        | None ->
                            child.name <- ""
                            child.text <- ""
                            findLinkedListSpecificElem (Some child) childNext

                | false ->
                    findLinkedListSpecificElem (Some child) childNext
                                
            | None ->
                if equalIgnoreCase(child.name, tagName)
                    && containsIgnoreCase attr child.text
                    && parent.IsSome
                    then
                        parent.Value.next <- None
                
        findLinkedListSpecificElem None node
        

//let rec findDeepestLast n = 
//    if(n.offsprings.Count = 0) then
//        n
//    else
//        findDeepestLast(Array.last (n.offsprings))

let print head = 
    let mutable o = Some head
    while o.IsSome do  
        let n = o.Value
        [|  n.index.ToString().PadLeft(4);
            " -> ";
            n.parent.ToString().PadLeft(4);
            " / ";
            n.closing.ToString().PadLeft(4);
            "".PadLeft(1 + n.depth * 4);
            " [" + n.name + "]";
            " (" + n.offsprings.Count.ToString() + ")";
            " \"" + n.text + "\""   |]
        |> linesConcat
        |> System.Console.WriteLine
        o <- n.next

let tag__property (domain:string)  names n = 

    let res = Dictionary<string,string>()
    let srcs = ResizeArray<string>()
    names |> Array.iter(fun i -> res.Add(i,""))

    let splits =
        n.text
        |> split "\""
        |> Array.map(fun i -> i.Trim())
        |> Array.filter(fun i -> i.Length > 0)
        
    splits
    |> Array.iteri(fun i v ->
        names
        |> Array.iter(fun k ->
            if (v.ToLower().Contains $"{k.ToLower()}=") && i+1 <= splits.Length-1 then
                let mutable kv = splits.[i+1]
                    
                if v.ToLower().Contains "src" && not (kv.Contains " ") then
                    srcs.Add kv
                    
                if not (v.ToLower().Contains "src") then
                    res.[k] <- kv
            )
        )

    for src in srcs do
        if src.StartsWith "http" then
            res["src"] <- src
                
    if res.ContainsKey "src" && res["src"].Length = 0 && srcs.Count > 0 then
        let res1 = srcs |> Seq.toArray |> Array.tryFind (fun s -> not (s.ToLower().Contains "default") )  
        if res1.IsSome then
            res["src"] <-
                if domain.Length = 0 then
                    res1.Value
                else ( domain + "/" + res1.Value)
        else 
            res["src"] <- 
                if domain.Length = 0 then
                    srcs[0]
                else ( domain + "/" + srcs[0])

    res


let maxStepLength2FindGuysNode = 1
let rec getNoTagGuysNode (stepLength:int, node:Node) : Node option=
        
    (*
    child, 向后 
    步数 depth
    +1    +1    走到结束标签
    +2    +1    name.length <> 0    -》 delete
    +2    +1    name.length = 0 && text.trim().length = 0 -> delete
        +3    +2  </em></strong>.next.name.length = 0 && </em></strong>.next.text.trim().length <> 0 -> child.next.next.next.text <- " "+child.next.next.next.text
    +2    +1    name.length = 0 && text.trim().length <> 0 -> 即 child.next.next.text <- " "+child.next.next.text

    *)

    if node.name.Trim().Length = 0 &&
        node.text.Length > 0 &&
        stepLength >= 0
        then
        Some node
    elif node.next.IsSome && stepLength > 0 then
        getNoTagGuysNode (stepLength-1, node.next.Value)
    else
        // if node.name.Length > 0 then
        //     match node.text.ToUpper().Equals node.matched with
        //     | true -> node.text <- " "
        //     | false -> node.text <- node.text + " "
        None


let handleStars (dom:Node) =
    use cw = new CodeWrapper("WebInterop.WebCrawl.htmlPath__node - Html.handleStars")

    let checkContentChars (target:string) =
        let mutable state = 0
        let mutable head = ""
        let mutable tail = ""
        let mutable tailChars = ""

        let blankMatches = blankRegex.Matches(target)
        let charRatches = charRegex.Matches(target)

        if blankMatches.Count > 0 then
            if target.StartsWith blankMatches[0].Value then
                state <- state + 1
                head <- blankMatches[0].Value

        if charRatches.Count > 0 then
            tailChars <- charRatches[charRatches.Count-1].Value
            if target.EndsWith tailChars then
                if tailChars.Length = target.Length then
                    state <- 2
                else
                    state <- state + 2
                    
                tail <- tailChars
                
        state,head,tail

    // toAssembling: [|"";"";"";"";""|] index: 0:head, 1:content, 2:tail
    let rec checkStars (grandpa:Node option) (parent:Node option) (child:Node) =

        let moveEscape2NextGuy (parent:Node) (child:Node) =
            let handle (le:string, n:Node) =
                match getNoTagGuysNode (4, n) with
                | Some guy ->
                    parent.text <- parent.text.Substring(0,parent.text.Length-le.Length)
                    guy.text <- le + guy.text
                | None -> () // 没有可以存放 \ 的 content

                
            // \<b>content -> <b>\content
            let charMatches = escapeCharRegex.Matches(parent.text)
            if charMatches.Count > 0 then
                let lastEscape = charMatches[charMatches.Count-1]
                if parent.text.EndsWith lastEscape.Value then
                    if child.name.Length <> 0 then
                        handle (lastEscape.Value, child)
                    else
                        match child.next with
                        | Some cn -> handle (lastEscape.Value, cn)
                        | None -> ()
            else ()

        let checkAndAddBlank2Parent  (parent:Node) (childNext:Node) =

            match childNext.name.Length = 0 with
            | false -> ()
            | true ->   // childNext content node
                    
                let cnChars = charRegex.Match childNext.text   
                let parentChars = charRegex.Matches parent.text
                    
                if childNext.text.StartsWith cnChars.Value then    // <b>\abc</b> <b>.a</b>
                    if parentChars.Count > 0 then   // parent.text 中含有" "， 判断后一位
                        let parentTail = parentChars[parentChars.Count-1].Value
                        match parent.text.EndsWith parentTail with
                        | false ->  // parent.text 不是以 \s 结尾
                            parent.text <- parent.text + " "
                        | true -> ()
                    else
                        parent.text <- parent.text + " "

        let handleContentStartChars () =
            child.text <- child.text.TrimStart()
            match grandpa with
            | Some gp ->
                match gp.name.Length > 0 with
                | true -> ()
                | false ->
                    match gp.text.Trim().Length > 0 with
                    | true ->
                        gp.text <- gp.text + " "
                    | false -> ()
            | None -> ()
                                      
        let handleContentEndChars (grandpa:Node option) (parent:Node option) (child:Node) (childNext:Node) (tail:string) =
            let handleContent () =
                match childNext.next with   // </b>.next  <b>"
                | Some cnn ->
                    match getNoTagGuysNode (maxStepLength2FindGuysNode, childNext) with
                    | Some c ->
                        child.text <- child.text.Substring(0,child.text.Length-tail.Length)
                        c.text <- tail + c.text
                        if child.text.Length = 0 then
                            match parent with
                            | Some p ->
                                p.next <- Some childNext
                                checkStars None grandpa p
                            | None ->
                                checkStars parent (Some child) cnn
                        else
                            checkStars parent (Some child) childNext

                    | None -> // 没有后续可用节点，则当前尾tag ** 大概率是有效的
                        let charMatches = escapeCharRegex.Matches(child.text)
                        if charMatches.Count > 0 then
                            let lastEscape = charMatches[charMatches.Count-1].Value
                            // 这里导致将原文中这一行最后的 \ 去掉， 因为没有找到可以存放这个\的地方
                            child.text <- child.text.Substring(0,child.text.Length-lastEscape.Length)
                            
                        let blankMatches = blankRegex.Matches(child.text)
                        if blankMatches.Count > 0 then
                            let lastBlank = blankMatches[blankMatches.Count-1].Value
                            if child.text.EndsWith lastBlank then
                                child.text <- child.text.Substring(0,child.text.Length-lastBlank.Length)
                                
                                if child.text.Length = 0 then
                                    match parent with
                                    | Some p ->
                                        p.next <- Some childNext
                                        checkStars grandpa parent childNext  
                                    | None -> checkStars parent (Some child) childNext  
                                else checkStars parent (Some child) childNext  
                            else checkStars parent (Some child) childNext  
                        else checkStars parent (Some child) childNext  
                | None -> child.text <- child.text.TrimEnd() // 让**生效并结束

            if child.text.Trim().Length = 0 && grandpa.IsSome && parent.IsSome then
                match grandpa.Value.name with
                | "B" | "STRONG" | "I" | "EM" ->
                    match parent.Value.name with
                    | "B" | "STRONG" | "I" | "EM" ->
                        match childNext.name with
                        | "/B" | "/STRONG" | "/I" | "/EM" ->
                            match childNext.next with
                            | Some cnn ->
                                match cnn.name with
                                | "/B" | "/STRONG" | "/I" | "/EM" ->
                                    grandpa.Value.next <- Some cnn
                                        
                                    grandpa.Value.name <- " "
                                    grandpa.Value.text <- " "
                                    cnn.name <- ""
                                    cnn.text <- ""
                                    checkStars None grandpa cnn
                                | _ -> handleContent ()
                            | _ -> handleContent ()
                        | _ -> handleContent ()
                    | _ -> handleContent ()
                | _ -> handleContent ()
            elif childNext.name = "BR/" then ()
            else handleContent ()

        match child.next with
        | Some childNext ->
            match child.name with
            | "B" | "STRONG" | "I" | "EM"-> // 开始处理
                // 判断 是否是 <b></b>
                if childNext.name.Equals ("/"+child.name) then
                    match childNext.next with
                    | Some cnn ->
                        match parent with
                        | Some p -> // <p><b></b>
                            match p.name.Length > 0 with
                            | true ->  // tag node
                                p.next <- Some cnn
                                checkStars grandpa parent cnn
                            | false ->  // content node c<b></b>
                                match cnn.name.Length > 0 with
                                | true ->
                                    p.next <- Some cnn
                                    checkStars grandpa parent cnn
                                | false ->
                                    p.text <- p.text + cnn.text
                                    match cnn.next with
                                    | Some cnnn ->
                                        p.next <- Some cnnn
                                        checkStars grandpa parent cnnn
                                    | None ->
                                        p.next <- None
                        | None ->   // <b></b>
                            child.name <- ""
                            childNext.name <- ""
                            child.text <- ""
                            childNext.text <- ""
                            checkStars None None cnn

                    | None ->
                        if parent.IsSome then
                            parent.Value.next <- None
                        else
                            child.name <- ""
                            childNext.name <- ""
                            child.text <- ""
                            childNext.text <- ""                             
                else              
                    // 注意前面的 \
                    match parent with
                    | Some p ->
                        match p.name.Length > 0 with
                        | false -> 
                            moveEscape2NextGuy p child  // 检查并前面的 \
                            checkAndAddBlank2Parent p childNext  // \w**\w 只存在于p存在
                                
                            checkStars parent (Some child) childNext
                        | true -> 
                            checkStars parent (Some child) childNext
                    | None -> checkStars parent (Some child) childNext

            | "/B" | "/STRONG" -> // 是否重新开始，</b><b>衔接
                if parent.IsSome && 
                    (parent.Value.name.Equals "B" || parent.Value.name.Equals "STRONG") &&
                    grandpa.IsSome then
                        grandpa.Value.next <- Some childNext
                        checkStars None grandpa childNext
                else
                    match childNext.name with 
                    | "B" | "STRONG" ->     // </b><b>
                        match childNext.next with  // content node
                        | Some cnn -> // content node
                            match parent with   // 
                            | Some p -> 
                                match cnn.name with
                                | "/B" | "/STRONG" ->
                                    p.next <- Some cnn
                                    checkStars grandpa parent cnn
                                | "" -> // content
                                    match p.name.Length > 0 with    // 此时 p 一般应该是 centnet
                                    | true ->  // <i></i><i>content
                                        p.next <- Some cnn
                                        checkStars grandpa parent cnn
                                    | false -> 
                                        p.text <- p.text + cnn.text
                                        match cnn.next with // </b>
                                        | Some cnnn ->  // </b> or other tag
                                            p.next <- Some cnnn
                                            // 处理 p.text
                                            checkStars grandpa parent cnnn
                                        | None -> p.next <- Some cnn
                                | _ -> checkStars (Some child) (Some childNext) cnn
                            | None -> checkStars None (Some childNext) cnn
                        | None -> child.next <- None
                    | _ -> checkStars parent (Some child) childNext
                        
            | "/I" | "/EM" ->
                if parent.IsSome && 
                    (parent.Value.name.Equals "I" || parent.Value.name.Equals "EM") &&
                    grandpa.IsSome then
                        grandpa.Value.next <- Some childNext
                        checkStars None grandpa childNext
                else
                    match childNext.name with 
                    | "I" | "EM" ->     // </b><b>
                        match childNext.next with  // content node
                        | Some cnn -> // content node
                            match parent with   // 
                            | Some p -> 
                                match cnn.name with
                                | "/I" | "/EM" ->
                                    p.next <- Some cnn
                                    checkStars grandpa parent cnn
                                | "" -> // content
                                    match p.name.Length > 0 with
                                    | true ->  // <i></i><i>content
                                        p.next <- Some cnn
                                        checkStars grandpa parent cnn
                                    | false -> 
                                        p.text <- p.text + cnn.text
                                        match cnn.next with // </b>
                                        | Some cnnn ->  // </b> or other tag
                                            p.next <- Some cnnn
                                            // 处理 p.text
                                            checkStars grandpa parent cnnn
                                        | None -> p.next <- Some cnn
                                | _ -> checkStars (Some child) (Some childNext) cnn
                            | None -> checkStars None (Some childNext) cnn
                        | None -> child.next <- None
                    | _ -> checkStars parent (Some child) childNext    
                
            | "" ->  // content
                if (parent.IsSome && [| "I"; "EM"; "B"; "STRONG" |] |> Array.contains parent.Value.name) ||
                    ([| "/I"; "/EM"; "/B"; "/STRONG" |] |> Array.contains childNext.name) then

                        
                    if (parent.IsSome && [| "I"; "EM"; "B"; "STRONG" |] |> Array.contains parent.Value.name)
                        && child.text.Trim() |> regex_match unCharRegex_nonBracket |> String.length > 0
                        && [| "/I"; "/EM"; "/B"; "/STRONG" |] |> Array.contains childNext.name
                        then
                            if grandpa.IsSome
                            then
                                grandpa.Value.next <- Some child
                                child.next <- childNext.next
                                    
                                if childNext.next.IsSome then
                                    checkStars grandpa (Some child) child.next.Value
                    elif child.text.Length > 0 then // 当前内容为空格且对下一个tag有意义时
                        child.text <- child.text.Trim()
                        match checkContentChars child.text with
                        | 1,h,t ->  // start
                            // 判断 p isNone
                            handleContentStartChars ()
                            checkStars parent (Some child) childNext  
           
                        | 2,h,t ->  // end
                            match childNext.name with
                            | "B" | "STRONG" | "I" | "EM" ->
                                moveEscape2NextGuy child child
                                checkStars parent (Some child) childNext
                            | "/B" | "/STRONG" | "/I" | "/EM" ->
                                match childNext.next with
                                | Some cnn -> 
                                    checkStars (Some child) child.next cnn
                                | None -> ()
                                        
                            | _ -> handleContentEndChars grandpa parent child childNext t
                                
                        | 3,h,t -> // start end
                            match parent with
                            | Some p ->
                                match p.name with
                                | "/I" | "/EM" | "/B" | "/STRONG" ->
                                    handleContentEndChars grandpa parent child childNext t    
                                | _ ->
                                    handleContentStartChars ()
                                    handleContentEndChars grandpa parent child childNext t
                            | None -> ()
                        | _ -> checkStars parent (Some child) childNext
                    else
                        // 当前text为若干空字符 <b>   </b>
                        if
                            parent.IsSome &&
                            childNext.name.Length > 1 &&
                            parent.Value.name.Equals (childNext.name.Substring(1,childNext.name.Length-1)) &&
                            grandpa.IsSome                                       
                            then
                                    match childNext.next with
                                    | Some cnnn ->  
                                        grandpa.Value.next <- Some cnnn
                                        match grandpa.Value.name.Length > 0 with
                                        | true -> // 无法把空格加到grandpa， 则加到后面
                                            match getNoTagGuysNode (maxStepLength2FindGuysNode, childNext) with
                                            | Some c ->
                                                c.text <- child.text + c.text
                                                checkStars None grandpa cnnn
                                            | None ->
                                                    grandpa.Value.next <- Some child
                                                    child.next <- Some cnnn
                                                    checkStars grandpa (Some child) cnnn
                                        | false ->
                                            grandpa.Value.text <- grandpa.Value.text + child.text
                                            checkStars None grandpa cnnn
                                    | None -> grandpa.Value.next <- None
                                     
                        else checkStars parent (Some child) childNext
                else checkStars parent (Some child) childNext
            // | "BR" | "BR/" ->
            //     match grandpa with
            //     | Some gp ->
            //         match gp.name with
            //         | "B" | "STRONG" ->
            //             child.name <- ""
            //             child.text <- "**\r\n**"
            //         | "EM" | "I" ->
            //             child.name <- ""
            //             child.text <- "*\r\n*"
            //         | _ -> ()
            //     | None -> ()
            //     checkStars parent (Some child) childNext
            | _ -> checkStars parent (Some child) childNext
        | None -> () // parent grandpa 

    checkStars None None dom

    
let rec getContentAndNextTag (dict:Dictionary<string,string>) (kvArr:string[]) (key:string) (indexOfS:int) =
        
    let mutable s = kvArr[indexOfS].Trim()
        
    if s.Length > 0 then
        let getCleanStr (s:string) =
            let mutable ns = s
                
            if ns.StartsWith "<" then
                ns <- ns.Substring(1).Trim()
                
            if ns.EndsWith "/>" then
                ns <- ns.Substring(0,ns.Length-2).Trim()
            elif ns.EndsWith ">" then
                ns <- ns.Substring(0,ns.Length-1).Trim()
            else ns <- ns.Trim()
            ns
            
        let cleanQuotes (forClean:string) =
               
            let mutable ns = forClean
            if ns.StartsWith "\"" then
                ns <- ns.Substring(1,ns.Length-1).Trim()

            if ns.EndsWith "\"" then
                ns <- ns.Substring(0,ns.Length-1).Trim()
                
            ns
            
        if s.StartsWith "\"" then    // tag & value
                
            s <- getCleanStr s
                
            if not (s.EndsWith "\"") then 
                let splits = s.Split " "
                    
                match splits.Length >= 2 with
                | true ->
                    let k = splits[splits.Length-1].ToLower() // next tag
                    if not (dict.ContainsKey k) then
                        dict[k] <- ""   // k: next tag
                            
                    if dict.ContainsKey key then    // key: target tag for current value
                        dict[key] <- cleanQuotes (s.Substring(0,s.Length-k.Length).Trim())
                    else dict[key] <- ""

                    if indexOfS+1 < kvArr.Length then
                        getContentAndNextTag dict kvArr k (indexOfS+1)

                | false -> dict[key] <- s |> (getCleanStr >> cleanQuotes)
            else dict[key] <- s |> (getCleanStr >> cleanQuotes)
        elif s.StartsWith "<" then
            s <- getCleanStr s
            let ss = s.Split " "
            if ss.Length >= 2 then
                let newTag = ss[ss.Length-1].ToLower()
                dict[newTag] <- ""

                if indexOfS+1 < kvArr.Length then
                    getContentAndNextTag dict kvArr newTag (indexOfS+1)
            else dict[key] <- s |> (getCleanStr >> cleanQuotes)
        else dict[key] <- s |> (getCleanStr >> cleanQuotes)

    else ()

let regex_linkStartWithChar = str__regex "^\w"
let regex_prefixEndWith = str__regex "/+$"
let handlUrlProtocol (link: string) (prefix: string) =
    let mutable newLink = link
    let nPrefix = regex_prefixEndWith.Replace(prefix,"").Trim()

    let start = link |> regex_match regex_linkStartWithChar 

    if newLink.StartsWith "//" then
        newLink <- "https:" + newLink
    elif newLink.StartsWith "/" then
        newLink <- nPrefix + newLink
    elif newLink.StartsWith "http" then
        ()
    elif start.Length > 0 then
        if nPrefix.EndsWith "=" then
            newLink <- nPrefix + link
        else
            newLink <- nPrefix + "/" + link

    newLink.Trim()

let handleImgLink (src:string) (domain:string) (d:Dictionary<string,string> option) =
    let mutable res = ""
    if d.IsSome && d.Value.ContainsKey "height" && d.Value["height"].Length = 1 then
        res <- ""
    elif src.Contains "banner_top" || src.Contains "banner_bottom" then
        res <- ""
    else
        res <- src
        if not (res.StartsWith "http") && d.IsSome && d.Value.ContainsKey "data-src" then
            res <- d.Value["data-src"] 
            
        if not (res.StartsWith "http") && d.IsSome && d.Value.ContainsKey "data-original" then
            res <- d.Value["data-original"] 
            
        if not (res.StartsWith "http") &&
            d.IsSome &&
            d.Value.ContainsKey "data-src" &&
            d.Value["data-src"].StartsWith "http" then
                res <- d.Value["data-src"]

        if not (res.StartsWith "http") && d.IsSome && d.Value.ContainsKey "srcset" then
            let ss = d.Value["srcset"]
            let ssA = ss.Split ","
            if ssA.Length > 0 then
                let ssAeA = ssA[ssA.Length-1].Split " "
                if ssAeA.Length > 0 then
                    res <- ssAeA[0]

    if res.StartsWith "http" then res else ""
        
let handleStarsInString(s:string) =
    let f1 = 1000
    let starStrR = Regex("\*+(.*?)\*+")
    let starR = Regex("\*+")
    let getBlanks(n:int) =
        let sb = StringBuilder()
        for i = 1 to n do
            sb.Append " " |> ignore
        sb.ToString()    
    let getResult (d:Dictionary<int,string>) =
        let sb = StringBuilder()
        for k in d.Keys |> Seq.sort do
            sb.Append d[k] |> ignore
        sb.ToString()

    let dict = Dictionary<int,string>()
    let mutable index = 0
    let mutable dictCount = 0
    let mutable reCheckIndCount = 0
    let reCheckIndD = Dictionary<int,List<int>>()
    let m2 = starStrR.Matches s
    if m2.Count > 0 then
        m2 |> Seq.iter (fun m ->
            let ind = dictCount * f1
            if index < m.Index && index <> 0 then
                dict[ind] <- s.Substring(index,m.Index-index)
                index <- index + m.Index-index
            else
                dict[ind+1] <- s.Substring(index,m.Index)
                index <- index + m.Index
            dict[ind+2] <- m.Value
            index <- index + m.Value.Length
            dictCount <- dictCount + 1
            )
        
    dict[dictCount*f1+index] <- s.Substring(index)

    let mutable starRelation = 0   // 1: i b b i , 2: b i i b
    for kv in dict do
        if kv.Value.Contains "*" then
            let m = starStrR.Match kv.Value
            if m.Groups.Count > 0 then
                    
                let starStr = m.Groups[0]
                let inStar = m.Groups[1]

                let smssb = StringBuilder()
                let sms = starR.Matches starStr.Value
                if sms.Count = 2 then
                    let h = sms[0].Value
                    let t = sms[1].Value
        
                    if h.Length = t.Length then
                        // start end
                        if inStar.Value.Trim().Length = 0 then ()
                        else 
                            let bms = blankRegex.Matches inStar.Value
                            if bms.Count > 0 then
                                if inStar.Value.StartsWith bms[0].Value then
                                    smssb.Append ((getBlanks bms[0].Length)+h) |> ignore
                                else smssb.Append h |> ignore
                                    
                                smssb.Append (inStar.Value.Trim())  |> ignore
                                    
                                if inStar.Value.EndsWith bms[bms.Count-1].Value then
                                    smssb.Append (t + (getBlanks bms[bms.Count-1].Length)) |> ignore
                                else smssb.Append t |> ignore
                            else smssb.Append starStr |> ignore
        
                    elif h.Length > t.Length then
                        if starRelation = 0 then
                            starRelation <- 2   // b i i b
                            reCheckIndCount <- reCheckIndCount + 1

                            if inStar.Value.Trim().Length = 0 then
                                smssb.Append (h + t) |> ignore
                            else                             
                                let bms = blankRegex.Matches inStar.Value
                                if bms.Count > 0 && inStar.Value.StartsWith bms[0].Value then
                                    smssb.Append ((getBlanks bms[0].Length) + h) |> ignore
                                    smssb.Append (inStar.Value.TrimStart() + t)  |> ignore
                                else smssb.Append starStr |> ignore
                                
                                
                            let l = List<int>()
                            l.Add kv.Key
                            reCheckIndD[reCheckIndCount] <- l
        
                        else
                            // if starRelation = 1 then    // i b b xx i
                            // ** xxx * backward
                            if inStar.Value.Trim().Length = 0 then
                                smssb.Append (h + t) |> ignore
                            else                             
                                let bms = blankRegex.Matches inStar.Value
                                if bms.Count > 0 && inStar.Value.EndsWith bms[bms.Count-1].Value then
                                    smssb.Append (h + inStar.Value.TrimEnd())  |> ignore
                                    smssb.Append (t + (getBlanks bms[bms.Count-1].Length)) |> ignore
                                else smssb.Append starStr |> ignore
                                
                            starRelation <- 0
                            let l = reCheckIndD[reCheckIndCount]
                            l.Add kv.Key

                    elif h.Length < t.Length then
                           
                        if starRelation = 0 then
                            starRelation <- 1   // i b b i
                            reCheckIndCount <- reCheckIndCount + 1

                            if inStar.Value.Trim().Length = 0 then
                                smssb.Append (h + t) |> ignore
                            else  
                                let bms = blankRegex.Matches inStar.Value
                                if bms.Count > 0 && inStar.Value.StartsWith bms[0].Value then
                                    smssb.Append ((getBlanks bms[0].Length) + h) |> ignore
                                    smssb.Append (inStar.Value.TrimStart() + t)  |> ignore
                                else smssb.Append starStr |> ignore
        
                            let l = List<int>()
                            l.Add kv.Key
                            reCheckIndD[reCheckIndCount] <- l

                        else
                            if inStar.Value.Trim().Length = 0 then
                                smssb.Append (h + t) |> ignore
                            else  
                                let bms = blankRegex.Matches inStar.Value
                                if bms.Count > 0 && inStar.Value.EndsWith bms[bms.Count-1].Value then
                                    smssb.Append (h + inStar.Value.TrimEnd())  |> ignore
                                    smssb.Append (t + (getBlanks bms[bms.Count-1].Length)) |> ignore
                                else smssb.Append starStr |> ignore

                            starRelation <- 0
                            let l = reCheckIndD[reCheckIndCount]
                            l.Add kv.Key
        
                    else smssb.Append starStr |> ignore
                    dict[kv.Key] <- smssb.ToString()
                else dict[kv.Key] <- ""
        
    if reCheckIndD.Count > 0 then
        for kv in reCheckIndD do
        let inds = kv.Value |> Seq.toList |> List.sort
            
        let nsl = List<string>()
        let indss = List<int>()
        if inds.Length = 2 then
                
            for k in dict.Keys do
                if k >= inds[0] && k <= inds[1] then
                    nsl.Add dict[k]
                    indss.Add k

        if nsl.Count >= 3 then
                
            let s1 = nsl[0]
                
            let s2 = 
                let nsb = StringBuilder()
                for i = 1 to nsl.Count - 2 do
                    nsb.Append nsl[i] |> ignore
                nsb.ToString()

            let s3 = nsl[nsl.Count-1]
                
            let sms1 = starR.Matches s1
            let sms3 = starR.Matches s3
                
            let mutable s1h = ""
            let mutable h = ""
            let mutable t = ""
            let mutable s3t = ""
            if sms1.Count = 1 && sms1[0].Value.Length = s1.Length && sms3.Count = 2 then

                t <- s3.Substring(0,sms3[0].Length)
                s3t <- s3.Substring(t.Length)
                    
                s1h <- s1.Substring(0,s1.Length-t.Length)
                h <- t
                    
            if sms1.Count = 2 && sms3.Count = 1 && sms3[0].Value.Length = s3.Length then
                s1h <- s1.Substring(0,s1.Length-sms1[1].Length)
                h <- s1.Substring(sms1[1].Index)
                    
                t <- h
                s3t <- s3.Substring(t.Length) 
                    
            if sms1.Count = 2 && sms3.Count = 2 then
                s1h <- s1.Substring(0,s1.Length-sms1[1].Length)
                h <- s1.Substring(sms1[1].Index)
                t <- s3.Substring(0,sms3[0].Length)
                s3t <- s3.Substring(t.Length)                    

            if s1h.Length > 0 && h.Length > 0 && t.Length > 0 && s3t.Length > 0 then
                    
                if s2.Trim().Length = 0 then
                        
                    let s1hms = blankRegex.Matches s1h
                    let s3tms = blankRegex.Matches s3t
                    if s1hms.Count > 0 && s1h.EndsWith s1hms[s1hms.Count-1].Value then
                        s1h <- s1h.Substring(0,s1h.Length-s1hms[s1hms.Count-1].Length)
                        s3t <- s3t + s1hms[s1hms.Count-1].Value
                        
                    if s3tms.Count > 0 && s3t.StartsWith (s3tms[0].Value) then
                        s1h <- s3tms[0].Value + s1h
                        s3t <- s3t.Substring(s3tms[0].Length)

                    if indss.Count >= 3 then
                        for i1 = 0 to indss.Count - 2 do
                            dict.Remove indss[i1] |> ignore
                        dict[indss[indss.Count-1]] <- s1h + s3t
                else 
                    let isb = StringBuilder()
                    let bms = blankRegex.Matches s2
                    if bms.Count > 0 then
                            
                        isb.Append s1h |> ignore
                            
                        if s2.StartsWith bms[0].Value then
                            isb.Append ((getBlanks bms[0].Length)+h) |> ignore
                        else isb.Append h |> ignore
                            
                        isb.Append (s2.Trim())  |> ignore
                            
                        if s2.EndsWith bms[bms.Count-1].Value then
                            isb.Append (t + (getBlanks bms[bms.Count-1].Length)) |> ignore
                        else isb.Append t |> ignore

                        isb.Append s3t |> ignore

                        if indss.Count >= 3 then
                            for i1 = 0 to indss.Count - 2 do
                                dict.Remove indss[i1] |> ignore
                            dict[indss[indss.Count-1]] <- isb.ToString()
                    else ()

    getResult dict

    
let node__markdown
    (excludedContent:Dictionary<ContentOfPosition,string[]> option)     // 过滤含有指定内容的行
    (domain:string)      // 域名
    n
    = 

    let h nn =
        
        let checkText (node:Node) char =
            match node.text.ToUpper().Equals node.matched with
            | true -> char
            | false ->
                match node.name with
                | "I" | "EM" -> node.text + char
                | "STRONG" | "B" -> node.text + char
                | "/EM" | "/I" -> char + node.text
                | "/STRONG" | "/B" -> char + node.text
                | _ -> node.text + char

        match nn.name with
        | "" -> ()
        | "HR" | "HR/" -> nn.text <- "---"
        | "I" | "EM" | "/EM" | "/I" -> nn.text <- checkText nn "*"
        | "STRONG" | "B" | "/STRONG" | "/B" -> nn.text <- checkText nn "**"
        | "H1" -> nn.text <- "# "
        | "H2" -> nn.text <- "## "
        | "H3" -> nn.text <- "### "
        | "H4" -> nn.text <- "#### "
        | "H5" -> nn.text <- "##### "
        | "/H1" | "/H2" | "/H3" -> nn.text <- crlf
        | "LI" -> nn.text <- "* "
        | "P" | "/P" | "DIV" | "/DIV" | "UL" | "/UL" | "/LI" -> nn.text <- checkText nn crlf
        | "U" | "/U" | "SPAN" | "/SPAN" -> nn.text <- checkText nn ""
        | "Q" | "/Q" -> nn.text <- "\""
        | "BR/" | "BR" -> nn.text <- crlf + crlf // nn.text <- " " 
        | "IMG" ->
            let mutable src = ""
            let mutable alt = ""
            let mutable href = ""

            let arr = nn.text.Replace("=\"", "|-|\"").Split "|-|"

            if arr.Length >= 2 then
                let d = Dictionary<string,string>()
                    
                getContentAndNextTag d arr "" 0
                    
                if d.ContainsKey "href" then
                    href <- d["href"]
                if d.ContainsKey "alt" then
                    alt <- d["alt"].Replace("\r","").Replace("\n"," ")
                if d.ContainsKey "src" then
                    src <- d["src"]

                // if not (src.StartsWith "http") then
                //     let l = List<string>()
                //     for kv in d do
                //         if kv.Key.Contains "src" then
                //             l.Add kv.Value
                //     
                //     if l.Count > 0 then
                //         let mutable flag = 0
                //         let mutable count = 0
                //         let re = Regex(@"http\S+\.(jpg|png)")
                //         
                //         while flag = 0 && count < l.Count do
                //             let s1 = l[count]
                //             let res = re.Match s1
                //             if res.Length > 0 then
                //                 src <- res.Value
                //                 flag <- 1
                //             count <- count + 1
                //         
                //     else ()
                // else ()
                    
                // handleImgLink 中定制各个爬取源的不需要的图片
                src <- handleImgLink src domain (Some d)
                    
            if src.Length <> 0 then
                if alt.Contains "\"" || alt.Contains "&quot;" then
                    alt <- alt.Replace("\"","'")
                    alt <- alt.Replace("&quot;","'")
                nn.text <- $"![{alt}]({src} \"{alt}\")"
            else
                nn.text <- ""
                
        | "VIDEO" | "AUDIO" ->
            nn.text <- crlf + (nn |> tag__property domain [| "src" |])["src"]
        | "/VIDEO" | "/AUDIO" -> nn.text <- crlf
        | "A" | "/A"  -> nn.text <- checkText nn ""
        | "BR/" | "BR" | _ -> nn.text <- checkText nn " "

    dfs h n

    let sb = new StringBuilder()
    let mutable o = Some n
    while o.IsSome do
        let nn = o.Value
        nn.text |> sb.Append |> ignore
        o <- nn.next
        if o.IsSome then
            if o.Value.closing = n.index then
                o <- None

    let lines = sb.ToString() |> split crlf

    let mutable blank = lines.[0].Length = 0    // 第一个元素一定是一个标签 且被替换为 markdown 中的符号
    let lineNums = 
        [| 1..lines.Length - 1|]    
        |> Array.filter(fun i ->    // 消除多余的空白符
            if lines.[i].Length > 0 then
                blank <- false
                true
            else
                if blank then
                    false
                else
                    blank <- true
                    true
        )
        
    if lines.[0].Length > 0 then [| [|0|]; lineNums |]|> Array.concat else lineNums 
    |> Array.filter(fun i -> checkUselessCharacters(lines.[i]))
    |> Array.map(fun i ->
        let mutable newLine = //lines.[i]
            // 清理多余空白字符,降低后续cpu使用率
            lines.[i].Split("\n")
            |> Array.map (fun s -> s.Trim().Replace("&nbsp;"," ") )   
            |> String.concat("")
                
        if excludedContent.IsSome then
            excludedContent.Value.Keys
            |> Seq.iter ( fun k ->
                match k with
                | ContentOfPosition.Head ->
                    excludedContent.Value.[ContentOfPosition.Head]
                    |> Array.iter (fun str ->
                        let ind = newLine.ToLower().IndexOf (str.ToLower())
                        if ind >=0 && ind <= 10 then
                            newLine <- ""
                        )
                | _ -> ()
            )
                    
        newLine
    )
    |> Array.filter(fun i -> i.Length <> 0)
    |> fun (lines: string[]) ->
            
        if(lines.Length = 0) then
            String.Empty
        else
            let sb = StringBuilder()
            lines
            |> Array.iter (fun line ->
                if line.StartsWith "* " then
                    sb.Append (line + crlf) |> ignore
                else sb.Append (crlf + line + crlf) |> ignore
                )

            sb.ToString()
        

type Dom = 
    {
        root: Node;
        html: string }

    member this.find (name:string,properties:string) = 
        findNode (name,properties) this.root


    member this.node__htmlOuter n = 
        match n.next with
        | Some(v) -> 
            if n.name.StartsWith "/" then
                ""
            else
                this.html.Substring(n.index,v.index - n.index)
        | None -> this.html.Substring(n.index)

    member this.node__htmlInner n = 
        match n.next with
        | Some(v) -> 
            if n.name.StartsWith "/" then
                ""
            else
                this.html.Substring(n.index + n.text.Length,v.index - n.index)
        | None -> this.html.Substring(n.index)


let split (array:List<Node>) n i = 
        
    let s = n.text
    let m = n.matched
    // 从指定的string index相互截取内容生成后续node
    let inserted = empty__Node(n.index + 1,s.Substring i,m.Substring i)
    array.Add inserted

    n.text <-
        s.Substring(0,i)   // 获得 tag 内容
        // let content = s.Substring(0,i)  
        // if content.Length > 0 then
        //     cleanStartEndUselessChar content 
        // else
        //     content
    n.matched <- m.Substring(0,i)
    n.name <- 
        (n.text 
        |> find("<",">")
        |> (regex_match r9)).ToUpper()  // 第一个非空白字符, 即获取tag名称
    n.next <- Some inserted

    inserted

let lex (head: Node) = 
        
    let array = new List<Node>()
    array.Add head

    let mutable o = Some head

    while o.IsSome do
        let mutable n = o.Value
        if n.name.Length = 0 then
            let mutable m = r1.Match n.text
            while m.Success do
                n <- split array n m.Index
                n <- split array n m.Value.Length
                m <- r1.Match n.text

        o <- n.next

    let removals = 
        let list = new List<int>()
        o <- Some head
        while o.IsSome do
            let n = o.Value
            match n.next with
            | Some nx -> 
                // if nx.name.Length = 0 && nx.text.Trim().Length = 0 then
                if nx.name.Length = 0 && nx.text.Length = 0 then
                    n.next <- nx.next
                    list.Add nx.index
            | None -> ()

            o <- n.next
        list.ToArray()

    array.ToArray()
    |> Array.filter(fun i -> 
        (removals |> Array.tryFind(fun ii -> ii = i.index)).IsNone)

let handleSuperLink (domain:string) (node:Node) =


    let rec loop (state:int) (toAssembling: string[]) (child:Node) =
        match child.next with
        | Some childNext ->
            match child.name.Equals "A" with
            | true -> // 切分 href
                let mutable href = (tag__property "" [| "href" |] child)["href"]
                if (href.StartsWith "http") = false && domain.Length > 0 then
                    href <- domain + "/" + href
                    
                toAssembling[1] <- href
                child.text <- ""
                loop 1 toAssembling childNext
            | false ->
                match state with
                | 1 -> 

                    match child.name.Length = 0 with
                    | true -> // 有内容 
                        toAssembling[0] <- toAssembling[0] + child.text
                        child.text <- ""
                        loop 1 toAssembling childNext
                    | false ->
                        match child.name with
                        | "/A" ->
                            child.text <-
                                let mutable all =

                                    let content = handleStarsInString toAssembling[0]
                                    let img = handleImgLink toAssembling[2] domain None
                                    if img.Length <> 0 then
                                        $"[{content}]({toAssembling[1]})\r\n\r\n{img}\r\n\r\n"
                                    else $"[{content}]({toAssembling[1]})"

                                all
                            loop 0 [|"";"";""|] childNext
                        | "STRONG" | "B" | "/STRONG" | "/B" ->
                            toAssembling[0] <- toAssembling[0] + "**"
                            child.name <- ""
                            child.text <- ""
                                
                            loop 1 toAssembling childNext
                        | "EM" | "I" | "/EM" | "/I" ->
                            toAssembling[0] <- toAssembling[0] + "*"
                            child.name <- ""
                            child.text <- ""
                            loop 1 toAssembling childNext
                        | "U" | "/U" | "SPAN" | "/SPAN" ->
                            child.name <- ""
                            child.text <- ""
                            loop 1 toAssembling childNext
                        | "Q" | "/Q" ->
                            toAssembling[0] <- toAssembling[0] + "\""
                            child.name <- ""
                            child.text <- ""
                            loop 1 toAssembling childNext
                        | _ ->
                            if child.name.Equals "IMG" then
                                    
                                let mutable src,alt,srcset =                 
                                    let d = child |> tag__property domain [| "src"; "alt"; "srcset" |]
                                    d.["src"],d.["alt"],d.["srcset"]
                                    
                                if src.Length = 0 && srcset.Length > 0 then
                                    src <- srcset
                                    
                                if alt.Contains "\"" || alt.Contains "&quot;" then
                                    alt <- alt.Replace("\"","'")
                                    alt <- alt.Replace("&quot;","'")
                                toAssembling[2] <- $"![{alt}]({src} \"{alt}\")"
                                child.name <- ""
                                child.text <- ""
            
                            loop 1 toAssembling childNext

                | _ -> loop 0 [|"";"";""|] childNext
                                
        | None -> () // 没有节点
    // [|"";"";""|] 0: content , 1: link , 2: img
    loop 0 [|"";"";""|] node
    
let html__Dom (html:string) = 
    use cw = new CodeWrapper("WebInterop.WebCrawl.htmlPath__node - Html.html__Dom")

    let mutable s = html

    let replacing (s:string) = "".PadLeft(s.Length,' ')

    s <- Util.Text.regex_replace (r4,replacing) s
    s <- Util.Text.regex_replace (r3,replacing) s
    s <- Util.Text.regex_replace (r5,replacing) s

    let dom = {
        root = empty__Node(0,s,s.ToUpper());
        html = html }

    let array = lex dom.root

    let stack = new List<Node>()
    stack.Add dom.root

    // Build tree
    let mutable o = Some dom.root
    while o.IsSome do

        let n = o.Value
        if n.matched.Length > 0 then

            if n.name.StartsWith("/") = false then

                n.depth <- stack.Count
                n.parent <- stack.[stack.Count - 1].index

                let closing = 
                    if n.name.EndsWith("/") then
                        // <br/>
                        true
                    else
                        // <hr>
                        if [|"META";"LINK";"HR";"BR";"SOURCE"|] |> Array.contains(n.name) then
                            n.name <- n.name + "/"
                            true
                        elif n.name.Trim().Length = 0 then  // 避免与文本同级的标签的depth被增加,实际是同级的
                            true
                        else
                            false

                // <div class="abc">
                if closing = false then
                    stack.Add n

            // </div>
            else
                let openTag = n.name.Substring(1)
                if stack.Exists(fun item -> item.name = openTag) then
                    let index = stack.FindLastIndex(fun item -> item.name = openTag)
                    if(index > 0) then
                        n.closing <- stack.[index].index
                        stack.RemoveRange(index,stack.Count - index)
                n.depth <- stack.Count
                n.parent <- stack.[stack.Count - 1].index

        else
            n.depth <- stack.Count
            n.parent <- stack.[stack.Count - 1].index

        o <- n.next
        
    o <- Some dom.root
    while o.IsSome do
        let n = o.Value
        array
        |> Array.filter(fun i -> i.index > 0 && i.parent = n.index)
        |> n.offsprings.AddRange
        o <- n.next

    dom

/// nodeChain: string[]
///     样式为：["p","a","","/a","/p"], ["p","em","","em","a","","/a","","/p"]
///     
///     效果：删除该 <p><a k=v ...>.*</a></p> 一段， 且<p>与<a>之间没有内容
///     说明：意在删除特定组合/结构的标签， 极易误删
let deleteSpecificNodeChain (node:Node) (nodeChain:string[]) =
    use cw = new CodeWrapper("WebInterop.WebCrawl.htmlPath__node - Html.deleteSpecificNodeChain")
    // p a b "" /b /a /p
    let chainLength = nodeChain.Length
    let rec checkAndDeleteChain (step:int) (ancient:Node option) (child:Node)(* (originalNode:Node) *) =
        match child.next with
        | Some cn ->
            match step = chainLength-1 with
            | true ->
                ancient.Value.next <- child.next
                checkAndDeleteChain 0 ancient cn //originalNode.next.Value
            | false ->
                match child.name.Equals (nodeChain.[step].ToUpper()) with   // tag 匹配
                | true ->   // tag 匹配
                    match step < chainLength-1 with
                    | true -> checkAndDeleteChain (step + 1) ancient cn //originalNode
                    | false -> checkAndDeleteChain 0 (Some child) cn
                | false -> // tag 不匹配 img，""
                    checkAndDeleteChain 0 (Some child) cn
        | None ->
            if step = chainLength-1 then
                ancient.Value.next <- None
    checkAndDeleteChain 0 None node
        
    
let cleanBlankCharsInHtml(html:string) = str__regex("(?<=>)\s+(?=<)").Replace(html,"")

