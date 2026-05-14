module UtilAvalonia.Comp.TextConsole

open LanguagePrimitives

open System
open System.IO
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives   // 提供 ScrollBarVisibility
open Avalonia.Input.Platform        // 提供 IClipboard 的 SetTextAsync 方法
open Avalonia.Layout
open Avalonia.Threading
open Avalonia.Platform.Storage
open UtilAvalonia.UiUtil

type ColoredSegment = { Text: string; Color: BuildInColor }

type TextConsole(?log: string -> unit) as this =
    inherit UserControl()
    let log = defaultArg log ignore
    let mutable bufferSize = 1000
    let buffer = List<string>()
    let mutable dirty = false
    let mutable timer: DispatcherTimer = null
    let scrollViewer = ScrollViewer()
    let itemsControl = ItemsControl()
    let lines = ObservableCollection<TextBlock>()
    let buttonPanel = StackPanel()
    let clearBtn = txt__Button "清空"
    let copyBtn = txt__Button "复制"
    let saveBtn = txt__Button "保存"
    let saveAsBtn = txt__Button "另存"
    let defaultSavePath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "console.txt")

    let getTopLevel () = TopLevel.GetTopLevel(this)

    let copyToClipboard text =
        task {
            match getTopLevel() with
            | null -> ()
            | tl ->
                match tl.Clipboard with
                | null -> ()
                | cb -> do! cb.SetTextAsync(text)   // 现在可以正确解析
        } |> ignore

    let saveToFile path =
        task {
            try
                log $"Console文本保存至 {path}"
                File.WriteAllText(path, String.Join(Environment.NewLine, buffer))
            with ex -> log $"保存失败: {ex.Message}"
        }

    let saveToFileWithPicker () =
        task {
            match getTopLevel() with
            | null -> ()
            | tl ->
                match tl.StorageProvider with
                | null -> ()
                | sp ->
                    let options = FilePickerSaveOptions(
                        Title = "保存控制台内容",
                        DefaultExtension = "txt",
                        SuggestedFileName = "console_log.txt",
                        FileTypeChoices = [| FilePickerFileType("文本文件", Patterns = [| "*.txt" |]) |]
                    )
                    let! file = sp.SaveFilePickerAsync(options)
                    if file <> null then
                        do! saveToFile file.Path.LocalPath
        } |> ignore

    let initUI () =
        buttonPanel.Orientation <- Orientation.Vertical
        buttonPanel.Margin <- Thickness(5.0)
        buttonPanel.Spacing <- 5.0
        buttonPanel.VerticalAlignment <- VerticalAlignment.Top
        clearBtn.Click.Add(fun _ -> this.Clear())
        copyBtn.Click.Add(fun _ -> copyToClipboard (String.Join(Environment.NewLine, buffer)))
        saveBtn.Click.Add(fun _ -> saveToFile defaultSavePath |> ignore)
        saveAsBtn.Click.Add(fun _ -> saveToFileWithPicker())
        [ clearBtn; copyBtn; saveBtn; saveAsBtn ] |> List.iter buttonPanel.Children.Add

        itemsControl.ItemsSource <- lines
        itemsControl.Background <- color__Brush BuildInColor.DefaultBackground
        itemsControl.Margin <- Thickness(0.0, 0.0, 5.0, 0.0)
        scrollViewer.Content <- itemsControl
        scrollViewer.HorizontalScrollBarVisibility <- ScrollBarVisibility.Disabled
        scrollViewer.VerticalScrollBarVisibility <- ScrollBarVisibility.Auto

        let dockPanel = DockPanel()
        dockPanel.Children.Add buttonPanel |> ignore
        DockPanel.SetDock(buttonPanel, Dock.Right)
        dockPanel.Children.Add scrollViewer |> ignore
        this.Content <- dockPanel |> control__withBorder

    let refreshDisplay () =
        if dirty then
            dirty <- false
            let ary = lock buffer (fun () -> buffer.ToArray())
            lines.Clear()
            ary |> Array.iter (txt__TextBlock >> lines.Add)
            scrollViewer.ScrollToEnd()

    let onTimerTick _ = if dirty then refreshDisplay()

    do
        initUI()
        this.Loaded.Add(fun _ ->
            timer <- DispatcherTimer(Interval = TimeSpan.FromMilliseconds 200.0)
            timer.Tick.Add onTimerTick
            timer.Start()
        )

    member _.BufferSize
        with get() = bufferSize
        and set v =
            bufferSize <- v
            if buffer.Count > bufferSize then
                buffer.RemoveRange(0, buffer.Count - bufferSize)
                dirty <- true

    // 不使用预览语法 buffer[^1]，改用显式计算索引
    member _.Write text =
        if buffer.Count = 0 then
            buffer.Add text
        else
            let lastIdx = buffer.Count - 1
            buffer.[lastIdx] <- buffer.[lastIdx] + text
        dirty <- true

    member _.WriteLine text = buffer.Add text; dirty <- true

    member _.WriteColor ccolor text =
        sprintf "\u001b[%d]%s" (EnumToValue ccolor) text |> this.Write

    member _.NewLineColor ccolor text =
        sprintf "\u001b[%d]%s" (EnumToValue ccolor) text |> buffer.Add
        dirty <- true

    member _.WriteSegments segments =
        segments
        |> Seq.map (fun seg -> sprintf "\u001b[%d]%s" (EnumToValue seg.Color) seg.Text)
        |> String.concat ""
        |> this.Write

    member _.WriteLineSegments segments =
        segments
        |> Seq.map (fun seg -> sprintf "\u001b[%d]%s" (EnumToValue seg.Color) seg.Text)
        |> String.concat ""
        |> this.WriteLine

    member _.Clear() = buffer.Clear(); dirty <- true
    member _.Flush() = refreshDisplay()
    member _.Stop() = if timer <> null then timer.Stop(); timer <- null

    interface IDisposable with
        member this.Dispose() = this.Stop()