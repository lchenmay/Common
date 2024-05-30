module Util.Diff

open System
open System.Text
open System.IO
open System.Collections.Generic
open Util.Bin
open Util.Text
open Util.FileSys
open Util.Perf
open LanguagePrimitives

[<Literal>] 
let EMPTYCHAR = Char.MinValue
// longest common substring algorithm
let iter_lcs (dest:char list) (src:char list) =
    let v = Array2D.create (src.Length+1) (dest.Length+1) -1
    let row = v |> Array2D.length1
    let col = v |> Array2D.length2
    for i = row-1  downto 0 do
        for j=col-1  downto 0 do
            v[i,j] <-
                if i = row-1  || j = col-1  then  0
                elif dest[j] = src[i] then  1 + v[i+1,j+1]
                else  max v[i+1,j] v[i,j+1]
    v

let lcs_subseq v (dest:char list) (src:char list)= 
    let sb = new StringBuilder()
    let row = v |> Array2D.length1
    let col = v |> Array2D.length2
    let mutable i = 0
    let mutable j = 0
    while  i<row-1 && j<col-1 do
        if dest[j] = src[i] then 
            sb.Append(dest[j])|>ignore
            i <- i+1
            j <- j+1
        elif v[i+1,j] >= v[i,j+1] then 
            i <- i+1
            j <- j
        else  
            i <- i
            j <- j+1
    sb

// Diff2 (myers)
let myers (src:char list) (dst:char list) =
    let m,n,max=src.Length,dst.Length,src.Length+dst.Length
    let mutable x,y,d,k,cont = 0,0,0,0,true
    let mutable v,trace = new Dictionary<string,int>(),ResizeArray<Dictionary<string,int>>()
    for i in -max..max do
        v.Add(i.ToString(),0)
    while cont && d<=max do
        let mutable nv = new Dictionary<string,int>()
        v |> Seq.iter (fun x -> nv.Add(x.Key,x.Value))
        trace.Add nv
        k <- (-d)
        while cont && k<=d do
            x <- if (-d)=k || (k<>d && v[string (k-1)]<v[string (k+1)]) then
                    v[string (k+1)]
                    else
                    v[string (k-1)] + 1
            y <- x-k
            
            while (x<m && y<n && src[x]=dst[y]) do
                x <- x + 1
                y <- y + 1
                
            v[string k] <- x 
            if x>=m && y>=n && cont then
                cont <- false
            k <- k + 2
        d <- d + 1
    trace.ToArray()

type data ={
    px:int
    py:int
    nx:int
    ny:int
}
let traceback (src:char list) (dst:char list) =
    let trace = myers src dst
    let len = Seq.length trace
    let mutable x,y = src.Length,dst.Length
    let mutable k,prev_k,prev_x,prev_y = 0,0,0,0
    let mutable v = trace[0]
    let mutable ret = ResizeArray<data>()
        
    // printfn "src = %A\ndst = %A" src dst

    for d=len-1 downto 0 do
        v <- trace[d]
        k <- x - y
        prev_k <- if (-d)=k || (k<>d && v[string (k-1)]<v[string (k+1)]) then
                    k + 1
                    else
                    k - 1

        prev_x <- v[string prev_k]
        prev_y <- prev_x - prev_k

        while x > prev_x && y > prev_y do
            ret.Add {px=x - 1
                        py=y - 1
                        nx=x
                        ny=y}
            // 可以输出 (7,6) -> (7,5) 的形式
            // printfn "(%d,%d) -> (%d,%d) *" (x-1) (y-1) x y
            x <- x - 1 
            y <- y - 1

        if d > 0 then 
            {   px=prev_x
                py=prev_y
                nx=x
                ny=y } |> ret.Add 
                    // 可以输出 (7,6) -> (7,5) 的形式
                    // printfn "(%d,%d) -> (%d,%d)" prev_x prev_y x y
        x <- prev_x
        y <- prev_y

    let point_array = ret.ToArray()
    // point_array |> Array.iteri (fun k e -> printf "round=%A, data=(%A,%A) --> (%A,%A) %c %c\n" 
    //                                         k e.px e.py e.nx e.ny src[e.px] dst[e.py]) 
    point_array

type EdType = 
    | DELETE = 0
    | INSERT = 1
    | NORMAL = 2   
    
type Edit = 
    {   edtype:EdType
        c:char // 将来修改为 byte 或是 char ? 
        pos:int32
    }

type EdLink = 
    {   editorID: int64
        editorName: string
        editdate: DateTime
        vernum: int // ver数值越大, 表明版本越新
        edlink: Edit array
    }
    
type VersionLink2D = Dictionary<int,EdLink>

let diff (srcStr:string) (dstStr:string) = 
    let src = Seq.toList srcStr
    let dst = Seq.toList dstStr

    // printfn "src = %A" src
    // printfn "dst = %A" dst

    let m,n=src.Length,dst.Length
    let v = traceback src dst
    // v |> Array.iteri (fun i e -> printf "i=%A, data=(%A,%A) --> (%A,%A)" 
    //                                     i e.px e.py e.nx e.ny
    //                              let x = if e.px - 1 >= 0 then e.px - 1 else 0
    //                              let y = if e.py - 1 >= 0 then e.py - 1 else 0
    //                              printf " %c %c \n" src[x] src[y]) 
    let mutable buf = ResizeArray<Edit>()
    v |> Seq.iter (fun e ->
        // F# 的字符串不是以'\0'结尾, 此处模拟'\0'结尾字符串
        let a_line = if e.px<m then src[e.px].ToString() else ""
        let b_line = if e.py<n then dst[e.py].ToString() else ""
        if e.px=e.nx then
            buf.Add{edtype=EdType.INSERT
                    c=b_line[0]
                    pos=e.nx}
        elif e.py=e.ny then
            buf.Add{edtype=EdType.DELETE
                    c=a_line[0]
                    pos=e.nx}
        // 不变化的字符不记录
        // 导致的结果是,不知道插入到哪个位置?
        // 例如
        // + A:0  和   C:0
        // + B:0     + A:0
        //   C:0     + B:0
        // 如果按照 1型 恢复的序列是 : ABC
        // 如果按照 2型 恢复的序列是 : CAB
        // 问题原因是,插入有两个位置, 在序列前插入和在序列后插入
        // 删除就没有这样的问题, 因此其只有一个位置

        // else
        //     buf.Add{edtype=" "
        //             c=a_line
        //             pos=e.nx}
        )
    buf.ToArray() |> Array.rev
    
// let article (first:string) (second:string) =
//     let src_lines = first.Split('\010')
//     let dst_lines = second.Split('\010')

let rebuild (src:string) (diff2: Edit array)=
    let srcfile = Seq.toList src 
    let mutable ver2 = ResizeArray<char>()

    let len = srcfile.Length
    // printfn "srcfile length = %d" len
    // [0..len] , 此时 0 表示位置 -1
    // let cur = p.pos-1
    // cur = -1 : 在第一个字符之前插入序列
    // 假设待插入序列 = MNP
    // eg. src = ABCD cur=-1
    // ?   A   B   C   D  
    //-1   0   1   2   3
    // * 在此位置插入 MNP 结果就是 MNPABC
    // cur = 0 : 在0位置之后插入序列 MNP
    // 结果就是 ABCMNP
    // 之所以单独处理0位置, 是因为 -1 的索引越界了!
    for k = 0 to len do
        let mid = diff2 |> Array.filter (fun p -> p.pos = k )
        // ?   A   B   C   D  
        // ""  0   1   2   3
        // 当 k=0 时且"+"类型, 相当于是在虚拟位置 ? 之后插入字符序列
        // 当 k=0 时 , k-1 = -1 越界了
        let e = if k=0 then EMPTYCHAR else srcfile[k-1]
        match mid with
        | [||] ->   //printfn "* %A : %d" e (k+1);
                    if e<>EMPTYCHAR then ver2.Add(e) //empty
        | lst  ->   //printfn "* src:%A dst:%A : %d" e lst k;
                    lst |> Array.iteri (fun ki elem ->
                            match elem.edtype with
                            | EdType.INSERT -> //printfn "+ src:%A dst:%A : %d" e elem.c (k+1)
                                        if (ki=0 && e<>EMPTYCHAR) then ver2.Add(e)
                                        ver2.Add(elem.c) 
                                        ()
                            | EdType.DELETE | EdType.NORMAL | _ ->   //printfn "- %A : %d" elem.c (k+1);
                                        ())

                    // 表示当前是 p.pos = 0 , 在字符前插入序列
                    // 即先插入序列, 在插入 src[0] 当前字符
                    // if (k=0 && lst[0].edtype = "+") then ver2.Add(e.ToString())
    let res = ver2 |> Seq.map string |> Seq.reduce (+)
    res

// difftwo 的 verLink 是保存所有差异性的二维结构结构
// 在 read_diff 和 write_diff 的时候使用的是 VersionLink2D
// 实际上, difftwo 应该返回一个 editLink , 然后再插入到 VersionLink2D结构上
// 也就是 diff2 
let difftwo (srcStr:string) (dstStr:string) editorID (ver2D:VersionLink2D)= 
    let editArray = diff srcStr dstStr
    let header = ver2D.Count + 1//header 模拟 Hash 指针
    let editLink = {
                editorID = editorID
                editdate = System.DateTime.UtcNow 
                editorName = "John"
                vernum = header 
                edlink = editArray
            }

    ver2D.Add(header ,editLink)

let prt_difftwo (verLink:VersionLink2D) =
    verLink |> Seq.sortByDescending (fun e -> e.Value.vernum) // 按照 新->旧 版本号排序
            |> Seq.iter (fun e -> 
            printfn "key\t\t: %d\neditorId\t: %u\neditorName\t: %s\neditdate\t: %A\nvernum\t\t: %A"
                    e.Key e.Value.editorID e.Value.editorName e.Value.editdate e.Value.vernum
                
            let len = e.Value.edlink |> Seq.length
            let head =e.Value.edlink |> Seq.head 
            printfn "edLink array\t: [edtype=%A,c=%A,pos=%A]......(total %d items)\n" 
                    head.edtype head.c head.pos len
            // printfn "edLink\t: %A\n" e.Value.edlink
            )


let reconstruct (first:string) (verLink:VersionLink2D) (vernum:int) =
    let subvers = verLink
                    |> Seq.filter (fun e -> e.Key <= vernum)
    // subvers|> Seq.iteri (fun idx m -> printfn "idx\t: %d\nvernum\t: v%A\n" idx m.Value.vernum)

    let u = new StringBuilder() 
    u.Append(first) |> ignore
    for KeyValue(k,v) in subvers do
        //  printfn "k\t: %d\nvernum\t: ver %A\nfile\t: %A\n" k v.vernum u
            let res = rebuild (u.ToString()) v.edlink
            u.Clear()|>ignore
            u.Append(res)|>ignore
    u
    
let rollback (cur_content:string) (verlink:VersionLink2D) (step_back_n:int) =
    let sort_verlink = verlink |> Seq.sortByDescending (fun e -> e.Value.vernum) // 按照 新->旧 版本号排序
    let newest_ver_num = sort_verlink|> Seq.length // 有5个节点,表示最新版本号为5
    let expect_ver_num = newest_ver_num - (step_back_n - 1) //回退1步,表示取ver=5的diff节点
    let subver_collections = sort_verlink |> Seq.filter (fun e ->  e.Value.vernum >= expect_ver_num)
    printfn "当前回退 %d 步, 从 v%d 回退到 v%d" step_back_n newest_ver_num (newest_ver_num - 1)
    subver_collections |> Seq.iter (fun e -> 
                    printfn "key\t\t: %d\neditorId\t: %u\neditorName\t: %s\neditdate\t: %A\nvernum\t\t: %A\n"
                        e.Key e.Value.editorID e.Value.editorName e.Value.editdate e.Value.vernum
                    )

    let u = new StringBuilder() 
    u.Append(cur_content) |> ignore 
    for KeyValue(k,v) in subver_collections do
        //  printfn "k\t: %d\nvernum\t: ver %A\nfile\t: %A\n" k v.vernum u
            let prev_content = rebuild (u.ToString()) v.edlink
            u.Clear()|>ignore
            u.Append(prev_content)|>ignore
    u
    
type VerLink =
    {
        uid:int64
        vernum: int
        vdate: int64
        content:string
    }

let listVersion (verlink:VersionLink2D) (top_c:string) =
    let sort_verlink = verlink |> Seq.sortByDescending (fun e -> e.Value.vernum) // 按照 新->旧 版本号排序
    let length = sort_verlink|> Seq.length 
    let u = new ResizeArray<VerLink>()
    let mutable cur= ""
    cur <- top_c

    sort_verlink |> Seq.iteri (fun k v ->
            u.Add{  uid = v.Value.editorID
                    vernum = v.Value.vernum
                    vdate = v.Value.editdate |> Util.Time.wintime__unixtime
                    content = cur }
            if k<length-1 then
                cur <- rebuild (u[k].content) v.Value.edlink
            )
    // printfn "--------------------------------------------------------------------------------"
    // u |> Seq.iteri (fun k v ->
    //      printfn "k\t: %d\nvernum\t: ver %A\ndate\t: %A\nstr\t: %A\n" k v.vernum v.vdate v.content
    //  )
    u.ToArray()


[<Literal>]
let DiffStore = "datadiff"

let mutable bb = new BytesBuilder()
let mutable st = new VersionLink2D()
let editNode__bin (bb:BytesBuilder) (edit:Edit) =
    edit.edtype |> EnumToValue |> int32__bin bb
    edit.c |> char__bin bb
    edit.pos |> int32__bin bb
    
let editLink__bin (bb:BytesBuilder) (ed:EdLink) =
    ed.editorID |> int64__bin bb
    ed.editorName |> str__bin bb
    ed.editdate |> DateTime__bin bb
    ed.vernum |> int32__bin bb
    ed.edlink |> array__bin editNode__bin bb

let bin__editNode bi =
    {
        edtype = bi |> bin__int32 |> EnumOfValue
        c = bin__char bi
        pos = bin__int32 bi
    }
    
let bin__editLink bi = {
    editorID = bin__int64 bi
    editorName = bin__str bi
    editdate = bin__DateTime bi
    vernum = bin__int32 bi
    edlink = bin__array bin__editNode bi
    }

let diff__bin (bb:BytesBuilder) (st:VersionLink2D) : unit = 
    st.Count |> int32__bin bb
    st |> Seq.iter (fun e -> 
        e.Key |> int32__bin bb
        editLink__bin bb e.Value
        )
let bin__diff (bs:byte[]) (st:VersionLink2D) : unit = 
    let index = ref 0
    let bi = (bs , index)
    let count = bin__int32 bi
    for d=0 to count-1 do
        let key = bin__int32 bi
        let value = bi |> bin__editLink
        printfn $"d={d}\nkey={key}"
        st.Add (key,  value)
    
let getfullname filename = 
    let dataDir = Path.Combine(Directory.GetCurrentDirectory(),DiffStore)
    if not (Directory.Exists(dataDir)) then
        Directory.CreateDirectory(dataDir) |> ignore
    Path.Combine(dataDir,filename)
let write_diff (st:VersionLink2D) (filename:string) : unit = 
    use cw = new CodeWrapper("Diff.write_diff")
    // let fullname = getfullname filename
    let fullname = filename
    try
        use stream = File.Open(fullname, FileMode.OpenOrCreate, FileAccess.Write)
        use bw = new BinaryWriter(stream)
        let bb = new BytesBuilder()
        diff__bin bb st
        bb.bytes() |> bw.Write
        bw.Close()
        stream.Close()
    with ex ->
        [|  "Fullname = " + fullname
            ", Bin = " + bb.length.ToString()
            ", Ex = " + ex.ToString() |]
        |> linesConcat
        // SrOrm.Logger 没有导出到 namespace 导致无法找到 exn__logNoSql
        // 暂时先用 printfn 输出调试信息
        |> printfn "%s"

let read_diff (st:VersionLink2D) (filename:string) : unit = 
    use cw = new CodeWrapper("Diff.read_diff")

    // let fullname = getfullname filename
    let fullname=filename

    try
        use stream = new FileStream(fullname, FileMode.Open)
        use br = new BinaryReader(stream)
        let bin = br.ReadBytes(stream.Length |> int)
        br.Close()
        stream.Close()
        if bin.Length>0 then bin__diff bin st
    with ex ->
        [|  "Fullname = " + fullname
            ", Bin = " + bb.length.ToString()
            ", Ex = " + ex.ToString() |]
        |> linesConcat
        // SrOrm.Logger 没有导出到 namespace 导致无法找到 exn__logNoSql
        // 暂时先用 printfn 输出调试信息
        |> printfn "%s"

// user_comment_in_DB: lastest user comment which read from Social_Comment.content field
// newest_user_comment : the newest user comment come from client. 
//      user_comment_in_DB is 'OLD' version
//      newest_user_comment is 'NEW' version
// However, it is a trick when call 'difftwo' function. Actually,
//      user_comment_in_DB is 'dst' parameter  
//      newest_user_comment is 'src' parameter  
// Because the version history is a reverse diff. we only save the newest comment in DB.
// It is easy to trace back to older comment from newest comment, 
//    or we must trace version from the first comment.
// org_verlnk : the binary diff linked structure read from Social_Comment.verTraceLink
//
// Operation:
// 1. 'org_verlnk' add a new node in the array tail.
// 2. updated binary 'org_verlnk' write back to Social_Comment.verTraceLink field.
//
let add_new_node_in_version_trace_link
        (user_comment_in_DB:string)
        (newest_user_comment:string) 
        editorID
        (org_verlnk:VersionLink2D) : BytesBuilder=

    use cw = new CodeWrapper("Diff.add_new_node_in_version_trace_link")
    // Notice: By computing the difference between the souce and destination string,
    //         I switch the 'SRC' and 'DST' string parameters' position
    //         to trace the history version from the newest_user_comment.
    //         It is tricky.
    difftwo newest_user_comment user_comment_in_DB editorID org_verlnk
    // prt_difftwo org_verlnk
        
    // convert org_verlnk to bianry format
    let bb=new BytesBuilder()
    diff__bin bb org_verlnk

    // Should write it to DB with the newest user comment
    // after return binary diff structure
    bb

// write_diff "1.txt" bb 
// let readbytes=read_diff getfile st

let firstVersion editorID : string= 
    let vt = new VersionLink2D()
    let bb_diff = add_new_node_in_version_trace_link "" "firstVersion" editorID vt
    bb_diff.bytes() |> bytes__hex    

[<Literal>]
let MODULO = 1024L 
type Markdown_Dir_Struct =
    {   root:string
        folder:string
        id:int64
    } with
    member this.get_diff_fname : string =
        let residual = (this.id % MODULO).ToString()
        let dir = System.IO.Path.Combine([| this.root; this.folder; residual |])
        if not (Directory.Exists dir) then Directory.CreateDirectory dir |> ignore
        let file = System.IO.Path.Combine([| dir; this.id.ToString() + ".diff"|])
        if not (File.Exists file) then 
            let fs = File.Create(file) 
            fs.Close()
        file
