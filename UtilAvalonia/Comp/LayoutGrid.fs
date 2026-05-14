module UtilAvalonia.Comp.LayoutGrid

open System
open System.Collections.Generic
open Avalonia
open Avalonia.Controls
open Avalonia.Layout

type LayoutGrid(rows: float[], cols: float[]) as this =
    inherit Grid()

    let children = List<Control>()

    do
        // 定义行
        for height in rows do
            this.RowDefinitions.Add(RowDefinition(Height = GridLength(height, GridUnitType.Star)))

        // 定义列
        for width in cols do
            this.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength(width, GridUnitType.Star)))

    member this.AddChild
        (row,col) child =

        Grid.SetRow(child, row)
        Grid.SetColumn(child, col)

        this.Children.Add child
        children.Add child

    member this.AddChildSpan
        (row,col) (rowSpan,colSpan)
        child =

        Grid.SetRow(child, row)
        Grid.SetColumn(child, col)
        Grid.SetRowSpan(child, rowSpan)
        Grid.SetColumnSpan(child, colSpan)

        this.Children.Add child
        children.Add child

    member this.Clear() =
        for child in children do
            this.Children.Remove(child) |> ignore
        children.Clear()