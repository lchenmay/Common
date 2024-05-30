module Util.Time 

open System.IO
open System.Text

open Util
open Util.Text

open System

let utc_starting = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)

let wintime__unixtime(dt:DateTime) =
    dt.Subtract(utc_starting).TotalMilliseconds |> int64

let wintime__unixtimestring(dt:DateTime) =
    (dt.Subtract(utc_starting).TotalMilliseconds |> int64).ToString()

let unixtime__wintime (timestamp: int64) =
    match timestamp.ToString().Length with
    | 10 -> utc_starting.AddSeconds(float timestamp) 
    | 13 -> utc_starting.AddMilliseconds(float timestamp) 
    | 16 -> utc_starting.AddTicks(timestamp * 10L) 
    | 19 -> utc_starting.AddTicks(timestamp / 100L) 
    | _ -> DateTime.MinValue

let get_utc_time(time:string) =
    let second = Int64.Parse(time) - 1447369457L
    if second > 0L then
        utc_starting.AddSeconds(float second)
    else
        DateTime.MinValue

let ofweek (year:int,month:int,day:int)=
    let Dayweek= DateTime(year,month,day).DayOfWeek
    match Dayweek with
    | DayOfWeek.Sunday -> "日"
    | DayOfWeek.Monday -> "一"
    | DayOfWeek.Tuesday -> "二"
    | DayOfWeek.Wednesday -> "三"
    | DayOfWeek.Thursday -> "四"
    | DayOfWeek.Friday -> "五"
    | _ -> "六"

let dt__str(dt:DateTime) = 
    [|  dt.Year.ToString("0000");
        "-";
        dt.Month.ToString("00");
        "-";
        dt.Day.ToString("00");
        " ";
        dt.Hour.ToString("00");
        ":";
        dt.Minute.ToString("00");
        ":";
        dt.Second.ToString("00") |]
    |> linesConcat

//2022-04-18T11:15:04+08:00
let r1 = 
    [|  @"(?<YYYY>\d\d\d\d)";
        "-(?<MO>\d\d)";
        "-(?<DD>\d\d)";
        ".(?<HH>\d\d)";
        ":(?<MM>\d\d)";
        ":(?<SS>\d\d)";
        "(?<Sign>.)";
        "(?<SH>\d\d)";
        ":(?<SM>\d\d)" |]
    |> linesConcat
    |> string__regex

//2022-04-18T05:40:00.000Z  Z:零时区
let r2 = 
    [|  @"(?<YYYY>\d\d\d\d)";
        "-(?<MO>\d\d)";
        "-(?<DD>\d\d)";
        ".(?<HH>\d\d)";
        ":(?<MM>\d\d)";
        ":(?<SS>\d\d)";
        ".(?<MIL>\d\d\d)" |]
    |> linesConcat
    |> string__regex

//2022-04-17 19:49:08Z      Z:零时区
let r3 = 
    [|  @"(?<YYYY>\d\d\d\d)";
        "-(?<MO>\d\d)";
        "-(?<DD>\d\d)";
        ".(?<HH>\d\d)";
        ":(?<MM>\d\d)";
        ":(?<SS>\d\d)" |]
    |> linesConcat
    |> string__regex

//202204171949
let r5 = 
    [|  @"(?<YYYY>\d\d\d\d)";
        "(?<MO>\d\d)";
        "(?<DD>\d\d)";
        "(?<HH>\d\d)";
        "(?<MM>\d\d)" |]
    |> linesConcat
    |> string__regex

//20220417
let r6 = 
    [|  @"(?<YYYY>\d\d\d\d)";
        "(?<MO>\d\d)";
        "(?<DD>\d\d)" |]
    |> linesConcat
    |> string__regex

//Mon, 18 Apr 2022 23:19:06 GMT
let r7 = 
    [|  @"(?<DD>\d+)";
        " (?<MO>\D+)";
        " (?<YYYY>\d\d\d\d)";
        " (?<HH>\d\d)";
        ":(?<MM>\d\d)";
        ":(?<SS>\d\d)" |]
    |> linesConcat
    |> string__regex

// Sat Apr 23 21:14:58 HKT 2022
let r8 = 
    [|  @"(?<MO>\w+)";
        " (?<DD>\d+)";
        " (?<HH>\d\d)";
        ":(?<MM>\d\d)";
        ":(?<SS>\d\d)";
        " (?<ZONE>\D+)";
        " (?<YYYY>\d\d\d\d)" |]
    |> linesConcat
    |> string__regex

let parse_MM(str:string) = 
    match str.ToUpper().Substring(0,3) with
    | "JAN" -> 1
    | "FEB" -> 2
    | "MAR" -> 3
    | "APR" -> 4
    | "MAY" -> 5
    | "JUN" -> 6
    | "JUL" -> 7
    | "AUG" -> 8
    | "SEP" -> 9
    | "OCT" -> 10
    | "NOV" -> 11
    | "DEC" -> 12
    | _ -> 1

// offseto 主要用于s为不带时区信息的时间字符串.
// 添加TimeSpan(-8,0,0), 将字符串"2024-02-20 02:05:48Z"转换为-8的UTC时间
// 添加offseto最终得到utc时间
let parse offseto (s:string) = 

    let s = s.Trim()
    let mutable res = DateTime.MinValue
    let mutable groups = s |> str__groups(r1)

    if(res.Ticks = DateTime.MinValue.Ticks) then
        if(groups.Length = 10 && s.Contains(groups.[0])) then
            let shift = 
                if(groups.[8] = "-") then
                    new TimeSpan(-parse_int32(groups.[8]),-parse_int32(groups.[9]),0)
                else
                    new TimeSpan(parse_int32(groups.[8]),parse_int32(groups.[9]),0)
            let dt = new DateTime(
                parse_int32(groups.[1]),
                parse_int32(groups.[2]),
                parse_int32(groups.[3]),
                parse_int32(groups.[4]),
                parse_int32(groups.[5]),
                parse_int32(groups.[6]),
                DateTimeKind.Utc)
            res <- dt.Subtract(shift)

    if(res.Ticks = DateTime.MinValue.Ticks) then

        groups <- s |> str__groups(r2)
        if(groups.Length = 8 && s.Contains(groups.[0])) then
            res <- new DateTime(
                parse_int32(groups.[1]),
                parse_int32(groups.[2]),
                parse_int32(groups.[3]),
                parse_int32(groups.[4]),
                parse_int32(groups.[5]),
                parse_int32(groups.[6]),
                parse_int32(groups.[7]),
                DateTimeKind.Utc)

    if(res.Ticks = DateTime.MinValue.Ticks) then

        groups <- s |> str__groups(r3)
        if(groups.Length = 7 && s.Contains(groups.[0])) then
            res <- new DateTime(
                parse_int32(groups.[1]),
                parse_int32(groups.[2]),
                parse_int32(groups.[3]),
                parse_int32(groups.[4]),
                parse_int32(groups.[5]),
                parse_int32(groups.[6]),
                DateTimeKind.Utc)

    if(res.Ticks = DateTime.MinValue.Ticks) then
        groups <-
            s
            |> regex_matches(string__regex(@"\d+"))
            |> linesConcat
            |> str__groups(r5)
        if(groups.Length = 6 && s.Contains(groups.[0])) then
            res <- new DateTime(
                parse_int32(groups.[1]),
                parse_int32(groups.[2]),
                parse_int32(groups.[3]),
                parse_int32(groups.[4]),
                parse_int32(groups.[5]),
                0,
                DateTimeKind.Utc)

    if(res.Ticks = DateTime.MinValue.Ticks) then
        groups <-
            s
            |> regex_matches(string__regex(@"\d+"))
            |> linesConcat
            |> str__groups(r6)
        if(groups.Length = 4 && s.Contains(groups.[0])) then
            res <- new DateTime(
                parse_int32(groups.[1]),
                parse_int32(groups.[2]),
                parse_int32(groups.[3]))

    if(res.Ticks = DateTime.MinValue.Ticks) then
        groups <- s |> str__groups(r7)
        if(groups.Length = 7 && s.Contains(groups.[0])) then
            res <- new DateTime(
                parse_int32(groups.[3]),
                groups.[2] |> parse_MM,
                parse_int32(groups.[1]),
                parse_int32(groups.[4]),
                parse_int32(groups.[5]),
                parse_int32(groups.[6]),
                DateTimeKind.Utc)

    if(res.Ticks = DateTime.MinValue.Ticks) then
        groups <- s |> str__groups(r8)
        if(groups.Length = 8 && s.Contains(groups.[0])) then
            let zone = groups.[6]
            res <- new DateTime(
                parse_int32(groups.[7]),
                groups.[1] |> parse_MM,
                parse_int32(groups.[2]),
                parse_int32(groups.[3]),
                parse_int32(groups.[4]),
                parse_int32(groups.[5]),
                DateTimeKind.Utc)

    if(res.Ticks = DateTime.MinValue.Ticks) then
        match(try_parse_int64(s)) with
        | Some(v) -> res <- v |> unixtime__wintime
        | None -> ()

    if(res.Ticks = DateTime.MinValue.Ticks) then
        let dt = ref (new DateTime())
        if(DateTime.TryParse(s,dt)) then
            res <- dt.Value

    match offseto with
    | Some(v) ->
        if(res.Ticks > DateTime.MinValue.Ticks) then
            res <- res.Add(v)
    | None -> ()

    res        

let string__windate(v) =
    if v = "" then
        0L
    else
        let y,m,d =
            let a = v.Split "-"
            a.[0],a.[1],a.[2]

        DateTime(y|>int, m|>int, d|>int, 0, 0, 0, DateTimeKind.Utc).Ticks


let string__timestamp(v) =
    if v = "" then
        DateTime(2000, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    else
        let ts = [|0;0;0;0;0;0;|]
        let ss = v.Split "-"
        let mutable i = 0
        let tss =
            ts |> Array.map(fun x ->
                i <- i + 1
                if i < ss.Length+1 then
                    ss.[i-1] |> parse_int32
                else
                    x
            )
        DateTime(tss.[0], tss.[1], tss.[2], tss.[3], tss.[4], tss.[5], DateTimeKind.Utc)
