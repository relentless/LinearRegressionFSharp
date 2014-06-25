#load "packages/FSharp.Charting.0.82/FSharp.Charting.fsx"

open System
open Microsoft.FSharp.Collections
open FSharp.Charting
open System.IO

let data = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + """\transistors.csv""").[1..] 
    |> Array.map (fun line -> 
        let values = line.Split(',')
        (int values.[1], int64 values.[0]))

Chart.Point(data,Title="Transistor Count by Year", XTitle="Year", YTitle="Transistors")

let intercept (x,_) = x
let slope (_,x) = x

let prediction theta x =
    (theta |> intercept) + x * (theta |> slope)

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
    |> Array.fold (fun agg el -> 
        let predicted = prediction theta (fst el)
        let difference = abs (int64 predicted - snd el)
        agg + difference) 0L // TODO: square (might overflow)

cost (0,1)