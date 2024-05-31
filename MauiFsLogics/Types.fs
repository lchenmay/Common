module MauiFsLogics.Types

open System
open System.Collections.Generic

open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls
open Microsoft.Maui.Layouts

type Runtime = {
mutable layout: AbsoluteLayout }

let runtime = {
    layout = new AbsoluteLayout() }



