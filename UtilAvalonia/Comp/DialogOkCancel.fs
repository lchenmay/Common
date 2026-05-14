module UtilAvalonia.Comp.DialogOkCancel

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open UtilAvalonia.UiUtil

/// 创建一个带确定/取消按钮的通用对话框
/// title: 窗口标题
/// content: 对话框主要内容（任意控件）
/// 返回: (对话框窗口, 结果引用 bool ref)，确定时结果为 true，取消为 false
let createOkCancelDialog title (content: Control) =
    let confirmed = ref false
    let dialog = Window()
    dialog.Title <- title
    dialog.Width <- 450.0
    dialog.Height <- 200.0
    dialog.WindowStartupLocation <- WindowStartupLocation.CenterOwner

    let layout = StackPanel(Margin = Thickness 10.0, Spacing = 10.0)

    layout.Children.Add content

    let btnPanel = StackPanel()
    btnPanel.Orientation <- Orientation.Horizontal
    btnPanel.HorizontalAlignment <- HorizontalAlignment.Right
    btnPanel.Spacing <- 10.0

    let okBtn = Button()
    okBtn.Content <- "确定"
    okBtn.Click.Add(fun _ ->
        confirmed.Value <- true
        dialog.Close()
    )

    let cancelBtn = Button()
    cancelBtn.Content <- "取消"
    cancelBtn.Click.Add(fun _ ->
        confirmed.Value <- false
        dialog.Close()
    )

    btnPanel.Children.Add okBtn
    btnPanel.Children.Add cancelBtn
    layout.Children.Add btnPanel

    dialog.Content <- layout
    dialog, confirmed

