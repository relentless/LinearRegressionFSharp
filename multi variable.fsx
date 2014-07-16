#load "packages/FSharp.Charting.0.82/FSharp.Charting.fsx"

open System
open FSharp.Charting
open System.IO
open Checked

// load the dataset into an array of arrays of values, and an array of answers
// (we assume the answer is the last column)
let data, answers = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + """\murders.csv""").[1..] 
    |> Array.map (fun line -> 
        line.Split(',')
        |> Array.map (fun item ->
            (float (item.Trim()))))
    |> Array.map (fun record ->
        record.[0..record.Length-2],record.[record.Length-1])
    |> Array.unzip

let exampleCount = float data.Length
let featureCount = data.[0].Length

// the prediction for a given set of values based on parameters theta
let prediction theta values =
    Array.map2 (fun t v ->
        t*v) theta values
    |> Array.sum

// calculates the cost of the parameters theta  in terms of the mean squared distance from 
// the predictions they give and the actual data points
let cost theta =
    Array.zip data answers
    |> Array.averageBy (fun (dataRecord, answer) ->
        let difference = (prediction theta dataRecord) - answer
        difference * difference)

// Create an array for parameters theta.  The first will always be 0, which we use to make
// calculations more consistent
let blankTheta = 
    let theta = Array.create featureCount 0.0
    theta.[0] = 1.0 |> ignore
    theta

// minimise cost using gradient descent search
let gradientDescent learningRate maxIterations =

// TODO : finish?
    let derivativeTheta theta parameter =
        Array.zip data answers
        |> Array.sumBy (fun (dataRecord,answer) ->
            ((prediction theta dataRecord) - answer) * parameter)

    let rec descentSearch theta iteration = 
        match iteration < maxIterations with
        | false -> theta
        | _ -> 
            //printfn "iteration: %i, Theta:%A, cost:%f" iteration theta (cost theta)
            let newTheta = 
                theta
                |> Array.map (fun parameter -> 
                    parameter - learningRate * derivativeTheta theta parameter)

            descentSearch newTheta (iteration+1)

    descentSearch blankTheta 0
    
// check gradient descent is working
let gradCosts = 
    [1..10]
    |> List.map (fun x -> x, cost(gradientDescent 0.01 x))

Chart.Line(gradCosts, Title="Cost of Theta given by Gradient Descent", XTitle="Iterations", YTitle="Cost")

// calculate the parameters theta
let finalTheta = gradientDescent 0.00001 10
    
printfn "Prediction of brain size for a mammal with a body weighing 2000kg: %fg" (prediction finalTheta [|2000.0;10.0|])