module AioLogics.Types

open System
open System.Collections.Generic

open Microsoft.Maui
open Microsoft.Maui.Graphics
open Microsoft.Maui.Controls
open Microsoft.Maui.Layouts

type Runtime = {
mutable spa: ContentPage }

let runtime = {
    spa = new ContentPage() }

