module projectSchedule

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open System

type Workitem = {
    Name : string
    StartDate : DateTime
    InternalPreview : DateTime
    ExternalPreview : DateTime
    GeneralAvailability : DateTime
    EndDate : DateTime
}

type Dependency =
    | Dependency of Workitem * Dependent:Workitem

module FillColor =
    let Yellow (ctx:Browser.CanvasRenderingContext2D) = ctx.fillStyle <- !^"rgb(254, 249, 52)"
    let Orange (ctx:Browser.CanvasRenderingContext2D) = ctx.fillStyle <-  !^"rgb(254, 177, 40)"
    let LightBlue (ctx:Browser.CanvasRenderingContext2D) = ctx.fillStyle <-  !^"rgb(187, 239, 239)"
    let LightGreen (ctx:Browser.CanvasRenderingContext2D) = ctx.fillStyle <-  !^"rgb(168, 247, 171)"
    let LightGrey (ctx:Browser.CanvasRenderingContext2D) = ctx.fillStyle <-  !^"rgb(237, 240, 247)"
    let MediumGrey (ctx:Browser.CanvasRenderingContext2D) = ctx.fillStyle <-  !^"rgb(220, 227, 239)"
    let Black (ctx:Browser.CanvasRenderingContext2D) = ctx.fillStyle <-  !^"rgb(0, 0, 0)"


let timeline =
    [
        {
            Name = "Authorization Services"
            StartDate = DateTime (2017, 12, 1)
            InternalPreview = DateTime (2018, 2, 1)
            ExternalPreview = DateTime (2018, 3, 15)
            GeneralAvailability = DateTime (2018, 5, 1)
            EndDate = DateTime (2018, 7, 1)
        }
        {
            Name = "Log Aggregation and Query"
            StartDate = DateTime(2017, 8, 1)
            InternalPreview = DateTime(2018, 2, 1)
            ExternalPreview = DateTime (2018, 3, 15)
            GeneralAvailability = DateTime (2018, 5, 1)
            EndDate = DateTime (2018, 7, 1)
        }
        {
            Name = "System Dashboard Service"
            StartDate = DateTime(2018, 1, 1)
            InternalPreview = DateTime(2018, 2, 1)
            ExternalPreview = DateTime (2018, 3, 15)
            GeneralAvailability = DateTime (2018, 5, 1)
            EndDate = DateTime (2018, 7, 1)
        }
        {
            Name = "System Management UI"
            StartDate = DateTime(2018, 2, 1)
            InternalPreview = DateTime(2018, 3, 1)
            ExternalPreview = DateTime (2018, 4, 1)
            GeneralAvailability = DateTime (2018, 5, 1)
            EndDate = DateTime (2018, 7, 1)
        }
    ]
    |> List.sortBy (fun wi -> wi.StartDate)

type Point = { X: float; Y:float } 

let months (startDate:DateTime) (endDate:DateTime) =
    (endDate.Month - startDate.Month) + 12 * (endDate.Year - startDate.Year) |> float

let drawWorkitem (ctx:Browser.CanvasRenderingContext2D) (monthWidth:float) (start:Point) (workitem:Workitem) =
    let implementationMonths = months workitem.StartDate workitem.InternalPreview |> float
    let internalPreviewMonths = months workitem.InternalPreview workitem.ExternalPreview |> float
    let externalPreviewMonths = months workitem.ExternalPreview workitem.GeneralAvailability |> float
    let generalAvailabilityMonths = months workitem.GeneralAvailability workitem.EndDate |> float
    let totalMonths = (workitem.EndDate.Month - workitem.StartDate.Month) + 12 * (workitem.EndDate.Year - workitem.StartDate.Year) |> float
    ctx |> FillColor.Black
    ctx.fillRect (start.X, start.Y, totalMonths * monthWidth + 2., 32.)
    ctx |> FillColor.Yellow
    ctx.fillRect(start.X + 1., start.Y + 1., implementationMonths * monthWidth, 30.)
    ctx |> FillColor.Orange
    ctx.fillRect(start.X + 1. + (implementationMonths * monthWidth), start.Y + 1., internalPreviewMonths * monthWidth, 30.)
    ctx |> FillColor.LightBlue
    ctx.fillRect(start.X + 1. + (implementationMonths + internalPreviewMonths) * monthWidth, start.Y + 1., externalPreviewMonths * monthWidth, 30.)
    ctx |> FillColor.LightGreen
    ctx.fillRect(start.X + 1. + (implementationMonths + internalPreviewMonths + externalPreviewMonths) * monthWidth, start.Y + 1., generalAvailabilityMonths * monthWidth, 30.)
    // Add label
    ctx.font <- "Arial"
    ctx |> FillColor.Black
    ctx.textAlign <- "center"
    ctx.fillText (workitem.Name, start.X + (totalMonths * monthWidth / 2.), start.Y + 20.)
    
let init() =
    let startDate = timeline |> List.minBy (fun wi -> wi.StartDate) |> fun wi -> wi.StartDate
    let endDate = timeline |> List.maxBy (fun wi -> wi.EndDate) |> fun wi -> wi.EndDate
    let mainHeader = Browser.document.getElementsByTagName_h1().[0]
    mainHeader.textContent <- sprintf "Project Schedule: %s - %s" (startDate.ToShortDateString()) (endDate.ToShortDateString())
    mainHeader.style.fontFamily <- "Arial"
    mainHeader.style.textAlign <- "center"
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    canvas.width <- Browser.window.innerWidth - 20. // 1000.
    canvas.height <- 800.
    let totalMonths = (endDate.Month - startDate.Month) + 12 * (endDate.Year - startDate.Year)
    let totalWidth = canvas.width - 2.
    let monthWidth = totalWidth / float(totalMonths)
    Browser.console.log (sprintf "Total months: %i" totalMonths)
    let ctx = canvas.getContext_2d()
    // Overall timeline
    ctx.fillStyle <- !^"rgb(200,200,200)"
    ctx.fillRect (0.,0.,canvas.width,100.)
    ctx |> FillColor.Black
    ctx.textAlign <- "center"
    ctx.fillText ("TIMELINE TO GO HERE", canvas.width/2., 50., canvas.width)
    // Each item
    timeline |> List.iteri (fun idx wi ->
        let delayStart = months startDate wi.StartDate * monthWidth
        let startingPoint = { X = delayStart; Y = float (34 * idx) + 102. }
        drawWorkitem ctx monthWidth startingPoint wi
    )


init()