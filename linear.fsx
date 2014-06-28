#load "packages/FSharp.Charting.0.82/FSharp.Charting.fsx"

open System
open Microsoft.FSharp.Collections
open FSharp.Charting
open System.IO

let data = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + """\transistors.csv""").[1..] 
    |> Array.map (fun line -> 
        let values = line.Split(',')
        (int values.[1]-1970, int64 values.[0]/1000L))

//Chart.Point(data,Title="Transistor Count by Year (000's)", XTitle="Years since 1970", YTitle="Transistors")

// functions for getting values from Theta
let intercept (x,_) = x
let slope (_,x) = x

// functions for getting values from the x,y values of the data points
let year (x,_) = x
let transistorCount (_,x) = x

let prediction theta x =
    (theta |> intercept) + (float x) * (theta |> slope)

// generates the line as given by the parameters theta (y-intercept, slope) with x ranging from 0 to x_size
let line theta x_size =
    let y_intercept = theta |> intercept
    let slope' = theta |> slope
    let point1 = (0, y_intercept)
    let point2 = x_size, (prediction theta x_size)
    Chart.Line([point1;point2])

// calculates the cost of the parameters theta (y-intercept, slope) in terms of mean squared distance from the data points
let cost theta =
    data
    |> Array.fold (fun total dataPoint -> 
        let predicted = prediction theta (dataPoint|> year)
        let difference = abs (int64 predicted - (dataPoint |> transistorCount))
        total + (difference * difference)) 0L
    |> (fun total -> total / int64 data.Length)
    
// find costs for various different slopes (keeping the y-intercept at 0 to make it easier to plot)
let costs = 
    [1000.0..1000.0..50000.0]
    |> List.map (fun x -> x, cost(0.0,x))

let minCost = costs |> List.minBy snd |> snd

Chart.Line(costs, Title="Cost of single-variable Theta", XTitle="Slope", YTitle="Cost").WithYAxis(Min=float minCost)