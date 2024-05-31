module UtilMaui.SPA

open System
open System.Collections.Generic

open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls
open Microsoft.Maui.Layouts

type SpaRuntime = {
mutable page: ContentPage }

let runtime = {
    page = new ContentPage() }
