namespace UtilAvalonia

open Avalonia.Controls
open Avalonia.Media

module Style = 

    type WinControl = {
        mutable fontFamily: FontFamily
        mutable fontSizeTab: float
        mutable colorTabForeground: IBrush
        mutable colorTabBackground: IBrush
        mutable tabHeader:string }

    let empty__WinControl () = {
        fontFamily = new FontFamily("Fira Code, 'JetBrains Mono', 'Cascadia Code', 'Consolas', monospace")
        fontSizeTab = 10.0
        colorTabForeground = Brushes.Yellow
        colorTabBackground = Brushes.DarkSlateGray
        tabHeader = "" }

    let mutable winControl = empty__WinControl()

    let txt__TextBlockTab txt = 
        let res = new TextBlock()
        res.Text <- txt 
        res.FontFamily <- winControl.fontFamily
        res.FontSize <- winControl.fontSizeTab
        res.Foreground <- winControl.colorTabForeground
        res.Background <- winControl.colorTabBackground
        res.FontWeight <- FontWeight.SemiBold
        res