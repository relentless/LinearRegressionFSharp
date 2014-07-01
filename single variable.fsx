#load "packages/FSharp.Charting.0.82/FSharp.Charting.fsx"

open System
open Microsoft.FSharp.Collections
open FSharp.Charting
open System.IO
open Checked

let data = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + """\brains.csv""").[1..] 
    |> Array.map (fun line -> 
        let values = line.Split(',')
        (float (values.[1].Trim()), float (values.[0].Trim())))

Chart.Point(data,Title="Mammal Body vs Brain Weight", XTitle="Body Weight (Kg)", YTitle="Brain Weight (g)")

let examples = data.Length

// functions for getting values from Theta
let intercept (x,_) = x
let slope (_,x) = x

// functions for getting values from the x,y values of the data points
let body (x,_) = x
let brain (_,x) = x

// the prediction for a given body size based on parameters theta
let prediction theta x =
    (theta |> intercept) + (float x) * (theta |> slope)

// generates the line as given by the parameters theta (y-intercept, slope) with x ranging from 0 to x_size
let line theta x_size =
    let y_intercept = theta |> intercept
    let slope' = theta |> slope
    let point1 = (0.0, y_intercept)
    let point2 = x_size, (prediction theta x_size)
    Chart.Line([point1;point2])

// calculates the cost of the parameters theta (y-intercept, slope) in terms of mean squared distance from the data points
let cost theta =
    data
    |> Array.fold (fun total dataPoint -> 
        let predicted = prediction theta (dataPoint|> body)
        let difference = float predicted - (dataPoint |> brain)
        total + (difference * difference)) 0.0
    |> (fun total -> float(total / float examples)*2.0)
    
// find costs for various different slopes (keeping the y-intercept at 0 to make it easier to plot)
let costs = 
    [0.1..0.05..1.0]
    |> List.map (fun x -> x, cost(0.0,x))

Chart.Line(costs, Title="Cost of single-variable Theta", XTitle="Slope", YTitle="Cost")

// minimise cost using gradient descent search
let gradientDescent initialTheta learningRate maxIterations convergencePct =

    let withinPercent x y pct =
        abs(y-x)/y*100.0 < pct

    let derivCost theta findx =
        data
        |> Array.fold (fun total dataPoint -> 
            let predicted = prediction theta (dataPoint|> body)
            let difference = float predicted - (dataPoint |> brain)
            total + (difference * float (findx (dataPoint|> body)))) 0.0
        |> (fun total -> float(total / float examples))

    let rec descend theta iteration = 
        match iteration < maxIterations with
        | false -> theta
        | _ -> 
            let t0, t1 = theta
            printfn "iteration: %i, Theta:%A, cost:%f" iteration theta (cost theta)
            let newTheta0 = t0 - learningRate * derivCost theta (fun _ -> 1.0)
            let newTheta1 = t1 - learningRate * derivCost theta id
            match withinPercent t0 newTheta0 convergencePct, withinPercent t0 newTheta0 convergencePct with
            | true, true -> (newTheta0,newTheta1)
            | _ -> descend (newTheta0,newTheta1) (iteration+1)

    descend initialTheta 0
    

// check gradient descent is working
let gradCosts = 
    [1..10]
    |> List.map (fun x -> x, cost(gradientDescent (0.0,0.0) 0.00001 x 1.0))

Chart.Line(gradCosts, Title="Cost of Theta given by Gradient Descent", XTitle="Iterations", YTitle="Cost")

let finalTheta = gradientDescent (0.0,0.0) 0.00001 10 1.0

// see our line

let maxBody = data |> Array.maxBy body |> body

Chart.Combine [
    Chart.Point(data,Title="Mammal Body vs Brain Weight", XTitle="Body Weight (Kg)", YTitle="Brain Weight (g)")
    Chart.Line([0.0,prediction finalTheta 0.0; maxBody, prediction finalTheta maxBody])
    ]
    