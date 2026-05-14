module UtilAvalonia.Comp.MainStatus

open System
open Avalonia               // 提供 Thickness
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Layout
open Avalonia.Threading
open UtilAvalonia.UiUtil

type MainStatus(?log: string -> unit) as this =   // 添加 as this 使 this 可用
    let log = defaultArg log ignore
    let statusPanel = Border()
    let statusText = TextBlock()
    let progressBar = ProgressBar()
    let clockText = TextBlock()
    let mutable timer: DispatcherTimer = null
    let statusChanged = Event<string>()

    let updateClock () = clockText.Text <- DateTime.Now.ToString("HH:mm:ss")
    let startClock () = 
        updateClock()
        timer <- new DispatcherTimer(Interval = TimeSpan.FromSeconds 1.0)
        timer.Tick.Add(fun _ -> updateClock())
        timer.Start()

    do
        statusPanel.Background <- BuildInColor.StatusPanelBackground |> color__Brush
        statusPanel.Height <- 28.0
        let dockPanel = DockPanel()
        statusText.Text <- "就绪"
        statusText.Foreground <- BuildInColor.StatusPanelForeground |> color__Brush
        statusText.VerticalAlignment <- VerticalAlignment.Center
        statusText.Margin <- Thickness(10.0, 0.0, 10.0, 0.0)
        progressBar.Width <- 150.0
        progressBar.Height <- 16.0
        progressBar.VerticalAlignment <- VerticalAlignment.Center
        clockText.Text <- DateTime.Now.ToString("HH:mm:ss")
        clockText.Foreground <- SolidColorBrush(Color.Parse("#E6E6E3"))
        clockText.VerticalAlignment <- VerticalAlignment.Center
        clockText.Margin <- Thickness(10.0, 0.0, 10.0, 0.0)
        dockPanel.Children.Add statusText |> ignore
        DockPanel.SetDock(statusText, Dock.Left)
        dockPanel.Children.Add progressBar |> ignore
        DockPanel.SetDock(progressBar, Dock.Right)
        dockPanel.Children.Add clockText |> ignore
        DockPanel.SetDock(clockText, Dock.Right)
        statusPanel.Child <- dockPanel
        startClock()

    member _.StatusBar = statusPanel
    member _.SetStatus(text: string) = 
        statusText.Text <- text
        log text
        statusChanged.Trigger(text)
    member _.StatusChanged = statusChanged.Publish
    member _.SetProgress(value, max) = 
        let percent = if max > 0.0 then value / max * 100.0 else 0.0
        progressBar.Value <- percent
        progressBar.Maximum <- 100.0
    member _.SetProgressIndeterminate() = progressBar.IsIndeterminate <- true
    member _.ClearProgress() = 
        progressBar.IsIndeterminate <- false
        progressBar.Value <- 0.0
    member this.Stop() = 
        if timer <> null then 
            timer.Stop()
            timer <- null

    interface IDisposable with
        member this.Dispose() = this.Stop()