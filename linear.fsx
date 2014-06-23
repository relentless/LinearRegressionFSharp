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

// generates the line as given by the parameters theta (y-intercept, slope) with x ranging from 0 to x_size
let line theta x_size =
    let y_intercept = fst theta
    let slope = snd theta
    let point1 = (0, y_intercept)
    let point2 = (x_size, slope*x_size+y_intercept)
    Chart.Line([point1;point2])

// calculates the cost of the parameters theta (y-intercept, slope) in terms of mean squared distance from the data points
let cost theta =
    