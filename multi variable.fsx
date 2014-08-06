#load "packages/FSharp.Charting.0.82/FSharp.Charting.fsx"

open System
open FSharp.Charting
open System.IO
open Checked

// load the dataset into an array of arrays of values, and an array of answers
// (we assume the answer is the last column)

// city size, unemployment, annual murders per 100k citizens
let originaldata, answers = 
    File.ReadAllLines(__SOURCE_DIRECTORY__ + """\murders.csv""").[1..] 
    |> Array.map (fun line -> 
        line.Split(',')
        |> Array.map (fun item ->
            (float (item.Trim()))))
    |> Array.map (fun record ->
        record.[0..record.Length-2],record.[record.Length-1])
    |> Array.unzip

// TOTO: feature scaling (make all values roughly -1 <= x <= 1)

let exampleCount = float originaldata.Length

// subtract average so mean is 0
let averages = 
    originaldata 
    |> Array.reduce (fun totals record -> Array.map2 (+) totals record)
    |> Array.map (fun total -> total/exampleCount)

let meanZeroData = 
    originaldata |> Array.map (fun record -> Array.map2 (-) record averages)

// divide each feature by the maximum to make it -1.0 <= x <= 1.0

let maximums = 
    meanZeroData 
    |> Array.reduce (fun maximums record -> 
        Array.map2 (fun x y -> max (abs x) (abs y)) maximums record)
        
let scaledDataOriginal = 
    meanZeroData |> Array.map (fun record -> Array.map2 (/) record maximums)

// Add a first field with value 1.0 to all records, to make later derivative calculations simpler
let data = scaledDataOriginal |> Array.map (fun x -> Array.append [|1.0|] x)
//let data = originaldata |> Array.map (fun x -> Array.append [|1.0|] x)

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

// Create an array for parameters theta.  The first will always be 1.0, which we use to make
// calculations more consistent.  Others are set to random values, because if they are the same,
// gradient descent will keep them the same
let blankTheta = 
    let rand = Random()
    let featureCount = data.[0].Length
    Array.init featureCount (fun x -> float (rand.Next(100)) / 100.0) 

// minimise cost using gradient descent search
let gradientDescent learningRate maxIterations =

    let derivativeTheta theta parameter =
        Array.zip data answers
        |> Array.sumBy (fun (dataRecord,answer) ->
            ((prediction theta dataRecord) - answer) * parameter)

    let rec descentSearch theta iteration = 
        match iteration < maxIterations with
        | false -> theta
        | _ -> 
            let newTheta = 
                theta
                |> Array.map (fun parameter -> 
                    parameter - learningRate * derivativeTheta theta parameter)

            descentSearch newTheta (iteration+1)

    descentSearch blankTheta 0
    
// check gradient descent is working
let gradCosts = 
    [1..10]
    //|> List.map (fun x -> x, cost(gradientDescent 0.00000001 x)) // with unscaled data, takes > 200 iterations
    |> List.map (fun x -> x, cost(gradientDescent 0.003 x)) // with scaled data, takes 5 iterations

Chart.Line(gradCosts, Title="Cost of Theta given by Gradient Descent", XTitle="Iterations", YTitle="Cost")

// calculate the parameters theta
let finalTheta = gradientDescent 0.003 10

let scale data =
    let meanData = Array.map2 (/) data averages
    let scaledData = Array.map2 (/) meanData maximums
    scaledData |> Array.append [|1.0|]

let predict theta data =
    data
    |> scale
    |> prediction theta

printfn "Prediction of annual murder rate with a city size of %i and unemployment rate of %f: %f per 100k" 100000 5.0 (predict finalTheta [|100000.0;10.0|])

// TODO: unscale data to make predictions
