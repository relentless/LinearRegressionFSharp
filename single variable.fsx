#load "packages/FSharp.Charting.0.82/FSharp.Charting.fsx"

open System
open FSharp.Charting
open System.IO
open Checked

type Parameters = { Intercept:float; Slope:float }

// load the dataset
let data = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + """\brains.csv""").[1..] 
    |> Array.map (fun line -> 
        let values = line.Split(',')
        (float (values.[1].Trim()), float (values.[0].Trim())))

let exampleCount = float data.Length

Chart.Point(data,Title="Mammal Body vs Brain Weight", XTitle="Body Weight (kg)", YTitle="Brain Weight (g)")

// the prediction for a given body size based on parameters theta
let prediction theta bodyWeight =
    theta.Intercept + bodyWeight * theta.Slope

// calculates the cost of the parameters theta (y-intercept, slope) in terms of the mean squared distance from 
// the predictions they give and the actual data points
let cost theta =
    data
    |> Array.averageBy (fun (body,brain) ->
        let difference = (prediction theta body) - brain
        difference * difference)
    
// find costs for various different slopes (keeping the y-intercept at 0 to make it easier to plot)
let costs = 
    [0.1..0.05..1.0]
    |> List.map (fun x -> x, cost {Intercept=0.0; Slope=x})

Chart.Line(costs, Title="Cost of single-variable Theta", XTitle="Slope", YTitle="Cost")

// minimise cost using gradient descent search
let gradientDescent learningRate maxIterations =

    let derivativeCost theta getMultiplier =
        data
        |> Array.averageBy (fun (body,brain) ->
            let difference = (prediction theta body) - brain
            difference * getMultiplier body)

    let rec descentSearch theta iteration = 
        match iteration < maxIterations with
        | false -> theta
        | _ -> 
            //printfn "iteration: %i, Theta:%A, cost:%f" iteration theta (cost theta)
            let x0 = theta.Intercept - learningRate * derivativeCost theta (fun _ -> 1.0)
            let x1 = theta.Slope - learningRate * derivativeCost theta (fun x -> x)
            descentSearch {Intercept=x0; Slope=x1} (iteration+1)

    descentSearch {Intercept=0.0; Slope=0.0} 0
    
// check gradient descent is working
let gradCosts = 
    [1..10]
    |> List.map (fun x -> x, cost(gradientDescent 0.00001 x))

Chart.Line(gradCosts, Title="Cost of Theta given by Gradient Descent", XTitle="Iterations", YTitle="Cost")

// calculate the parameters theta
let finalTheta = gradientDescent 0.00001 10

// see our line against the data
let maxBody = data |> Array.maxBy fst |> fst

Chart.Combine [
    Chart.Point(data,Title="Mammal Body vs Brain Weight", XTitle="Body Weight (kg)", YTitle="Brain Weight (g)")
    Chart.Line([0.0,prediction finalTheta 0.0; maxBody, prediction finalTheta maxBody])
    ]

printfn "Prediction of brain size for a mammal with a body weighing 2000kg: %fg" (prediction finalTheta 2000.0)
    