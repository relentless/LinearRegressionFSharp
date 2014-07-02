#load "packages/FSharp.Charting.0.82/FSharp.Charting.fsx"

open System
open FSharp.Charting
open System.IO
open Checked

// load the dataset
let data = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + """\murders.csv""").[1..] 
    |> Array.map (fun line -> 
        line.Split(',')
        |> Array.map (fun item ->
            (float (item.Trim()))))

// split the data from the number we're trying to find (which we assume is the last one)
let data' = 
    data
    |> Array.map (fun record ->
        record.[0..record.Length-2])

let answers = 
    data
    |> Array.map (fun record ->
        record.[record.Length-1])

let exampleCount = float data.Length

// the prediction for a given set of values based on parameters theta
let prediction theta values =
    Array.map2 (fun t v ->
        t*v) theta values
    |> Array.sum

// calculates the cost of the parameters theta  in terms of the mean squared distance from 
// the predictions they give and the actual data points
let cost theta =
    Array.zip data' answers
    |> Array.averageBy (fun (dataRecord, answer) ->
        let difference = (prediction theta dataRecord) - answer
        difference * difference)

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
    Chart.Point(data,Title="Mammal Body vs Brain Weight", XTitle="Body Weight (Kg)", YTitle="Brain Weight (g)")
    Chart.Line([0.0,prediction finalTheta 0.0; maxBody, prediction finalTheta maxBody])
    ]
    
printfn "Prediction of brain size for a mammal with a body weighing 2000kg: %fg" (prediction finalTheta 2000.0)