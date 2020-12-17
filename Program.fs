// Learn more about F# at http://fsharp.org

open System

// Easy helper to let us confirm that something is an int
let (|Int|_|) str =
    match System.Int32.TryParse(str:string) with
    | (true,int) -> Some(int)
    | _ -> None

let (|Int64|_|) str =
    match System.Int64.TryParse(str:string) with
    | (true,int) -> Some(int)
    | _ -> None

// Helper to use matching on prefix of string via https://stackoverflow.com/a/3722671
let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let (|Suffix|_|) (p:string) (s:string) =
    if s.EndsWith(p) then
        Some(s.[.. s.Length - p.Length - 1])
    else
        None

type SearchType<'a> = Min of 'a | Max of 'a | Any

let comboSearch searchType condition lists =
    let pickChild searchType children =
        let existingChildren = children |> List.choose id
        match existingChildren, searchType with
        | [], _ -> None
        | _, (Min _ | Any) -> Some (List.minBy (fst) existingChildren)
        | _, (Max _) -> Some (List.maxBy (fst) existingChildren)

    let rec findSelections searchType condition selections listOfLists =
        match listOfLists with
        | head :: tail ->
            head
            |> List.map (fun curr -> findSelections searchType condition (curr :: selections) tail)
            |> pickChild searchType

        | [] ->
            if condition selections
            then
                match searchType with
                | Max f | Min f -> Some (f selections, selections)
                | Any -> Some (0, selections)
            else
                None

    findSelections searchType condition [] lists

let printExecutionTime f =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let result = f ()
    stopWatch.Stop()
    printfn "Execution took %f" stopWatch.Elapsed.TotalMilliseconds
    result


module Day1 =
    let inputList =
        IO.File.ReadAllLines "day1.txt"
        |> Array.map int
        |> List.ofArray
        |> List.sort

    let rec sumIntWithList (num: int) numList sumTo =
        match numList with
        | first :: rest ->
            match num + first with
            | x when x = sumTo ->
                Some (num * first)
            | x when x > sumTo ->
                None
            | _ ->
                sumIntWithList num rest sumTo
        | [] ->
            None

    let rec sumTwoNumsInList firstList secondList sumTo =
        match firstList with
        | first :: rest ->
            match sumIntWithList first secondList sumTo with
            | Some x -> Some x
            | None -> sumTwoNumsInList rest secondList sumTo
        | [] ->
            None

    let rec sumThreeNumsInList firstList secondList thirdList sumTo =
        match firstList with
        | first :: rest ->
            match sumTwoNumsInList secondList thirdList (sumTo - first) with
            | Some x -> Some (x * first)
            | None -> sumThreeNumsInList rest secondList thirdList sumTo
        | [] ->
            None

    let runner () =
        sumTwoNumsInList inputList inputList 2020,
        sumThreeNumsInList inputList inputList inputList 2020

module Day2 =
    type PasswordSpec = {
        Minimum: int
        Maximum: int
        Letter: char
        Password: string
    }

    // This is called an Active Pattern, it allows matching in a match clause
    // and this tells it how to know whether it's a match or whether it should fall through
    let (|PasswordSpec|) (str: string) =
        // Just doing the replacements so that a simple .Split on a space will
        // break up the input into its components
        match str.Replace("-", " ").Replace(": ", " ").Split(" ") with
        | [|minimum; maximum; letter; password|] ->
            {
                Minimum = minimum |> int
                Maximum = maximum |> int
                Letter = letter |> char
                Password = password
            }
        | _ ->
            failwith("Failed to parse password spec")

    let checkSpec spec =
        let occurrences =
            spec.Password |> Seq.where ((=) spec.Letter) |> Seq.length
        spec.Minimum <= occurrences && spec.Maximum >= occurrences

    let checkSpec2 spec =
        (spec.Password.[spec.Minimum - 1] = spec.Letter)
            <> (spec.Password.[spec.Maximum - 1] = spec.Letter)

    // for part 2, just change checkSpec to checkSpec2
    let runner() =
        IO.File.ReadAllLines "day2.txt"
        |> Array.where (function
            | PasswordSpec spec -> checkSpec spec)
        |> Array.length

module Day3 =
    let input =
        IO.File.ReadAllLines "day3.txt"
        |> List.ofArray

    let rec treeCheck (treeMap : string list) xSlope ySlope xPosn (currCount: int64) =
        match treeMap with
        | currLine :: _ ->
            let nextTreeMap =
                if (List.length treeMap) >= ySlope
                then treeMap |> List.skip ySlope
                else []
            let newCount =
                match currLine.[xPosn % 31] with
                | '#' -> currCount + 1L
                | _ -> currCount
            treeCheck nextTreeMap xSlope ySlope (xPosn + xSlope) newCount
        | [] ->
            currCount

    // Just comment out all but the 3 1 one for part 1
    let runner () =
        (treeCheck input 1 1 0 0L)
        * (treeCheck input 3 1 0 0L)
        * (treeCheck input 5 1 0 0L)
        * (treeCheck input 7 1 0 0L)
        * (treeCheck input 1 2 0 0L)

module Day4 =
    let mandatoryFieldsPresent (passport: string) =
        let passportElements = passport.Replace("\n", " ").Split(" ")
        ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
        |> List.where (fun field ->
            (passportElements |> Array.exists (fun (x: string) -> x.StartsWith(field)))
        )
        |> (List.length >> (<=) 7)


    let (|IntInRange|_|) minYear maxYear (s: string) =
        match s with
        | Int i when i >= minYear && i <= maxYear ->
            Some ()
        | _ ->
            None

    let (|HexColor|_|) (str: string) =
        if
            str.Length = 7
                && str.[0] = '#'
                && str.[1..6] |> Seq.forall (fun x -> (x >= '0' && x <= '9') || (x >= 'a' && x <= 'f'))
        then Some ()
        else None

    let (|Byr|_|) (str: string) =
        match str with
        | Prefix "byr:" (IntInRange 1920 2002) -> // the start must be "byr" and the rest is an Int
            Some ()
        | _ -> None

    let (|Iyr|_|) (str: string) =
        match str with
        | Prefix "iyr:" (IntInRange 2010 2020) ->
            Some ()
        | _ -> None

    let (|Eyr|_|) (str: string) =
        match str with
        | Prefix "eyr:" (IntInRange 2020 2030) ->
            Some ()
        | _ -> None

    let (|Hgt|_|) (str: string) =
        match str with
        | Prefix "hgt:"
            (Suffix "cm" (IntInRange 150 193)
            | Suffix "in" (IntInRange 59 76)) ->
            Some ()
        | _ ->
            None

    let (|Hcl|_|) (str: string) =
        match str with
        | Prefix "hcl:" (HexColor) ->
            Some ()
        | _ ->
            None

    let (|Ecl|_|) (str: string) =
        match str with
        | Prefix "ecl:" ("amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth") ->
            Some ()
        | _ -> None

    // Check to make sure everything is a number and that the string representation has a length of 9
    let (|Pid|_|) (str: string) =
        match str with
        | Prefix "pid:" rest ->
            match rest, rest.Length with
            | (Int _), 9 ->
                Some ()
            | _ ->
                None
        | _ ->
            None

    let (|Cid|_|) (str: string) =
        match str with
        | Prefix "cid:" _ ->
            Some ()
        | _ ->
            None

    let validField field =
        match field with
        | Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid ->
            true
        | _ ->
            false

    let allFieldsValid (line: string) =
        line.Replace("\n", " ").Split(" ")
        |> Array.forall validField

    let runner () =
        (IO.File.ReadAllText "day4.txt").Split("\n\n")
        |> Array.where mandatoryFieldsPresent
        // Comment out below line to just get part 1
        |> Array.where allFieldsValid
        |> Array.length

module Day5 =
    type SplitType = Upper | Lower

    let splitSeats minSeat maxSeat splitType =
        let middle = (minSeat + maxSeat) / 2
        match splitType with
        | Upper ->
            middle, maxSeat
        | Lower ->
            minSeat, middle

    let rec findColumn (minSeat, maxSeat) letters =
        match letters with
        | 'R' :: rest ->
            findColumn (splitSeats minSeat maxSeat Upper) rest
        | 'L' :: rest ->
            findColumn (splitSeats minSeat maxSeat Lower) rest
        | [] ->
            minSeat
        | _ ->
            failwith("Invalid column seat pattern")


    let rec findSeatPosition (minSeat, maxSeat) letters =
        printfn "%A" (minSeat, maxSeat)
        match letters with
        | [colLetter1; colLetter2; colLetter3] ->
            minSeat, (findColumn (0, 8) [colLetter1; colLetter2; colLetter3])
        | 'F' :: rest ->
            findSeatPosition (splitSeats minSeat maxSeat Lower) rest
        | 'B' :: rest ->
            findSeatPosition (splitSeats minSeat maxSeat Upper) rest
        | _ ->
            failwith("Invalid seat pattern")

    let getId (row, column) =
        row * 8 + column

    let runner1 () =
        IO.File.ReadAllLines "day5.txt"
        |> Array.map (Seq.toList >> findSeatPosition (0, 128) >> getId)
        |> Array.max

    let runner2 () =
        let seatsOnList =
            IO.File.ReadAllLines "day5.txt"
            |> Array.map (Seq.toList >> findSeatPosition (0, 128) >> getId)
            |> Set.ofArray

        seq { 0 .. (127 * 8 + 7)}
        |> Seq.where (fun n ->
            seatsOnList.Contains(n - 1)
            && seatsOnList.Contains(n + 1)
            && not (seatsOnList.Contains(n))
        )
        |> Seq.exactlyOne

module Day6 =
    let countGroupAnswers (group: string) =
        group.Replace("\n", "")
        |> Set.ofSeq
        |> Set.count

    let countGroupAnswers2 (group: string) =
        group.Split("\n")
        |> Array.map Set.ofSeq
        |> Set.intersectMany
        |> Set.count

    let runner () =
        (IO.File.ReadAllText "day6.txt").Split("\n\n")
        // change this to countGroupAnswers2 for part 2
        |> Array.sumBy countGroupAnswers

module Day7 =
    type BagRule = string * ((int * string) Set)

    let (|BagName|) (str: string) =
        match str.Replace(".", "") with
        | Suffix " bags" rest -> rest
        | Suffix " bag" rest -> rest
        | _ ->
            failwithf("Unable to parse bag name")

    let (|NumberedBagName|) (str: string) =
        let number = str.Split(" ").[0] |> int
        match str.[(str.IndexOf(" ") + 1) ..] with
        | BagName b -> (number, b)

    let (|BagRule|) (str: string) =
        match str.Split(" contain ") with
        | [|(BagName container); "no other bags."|] ->
            (container, Set.empty)
        | [|(BagName container); rest|] ->
            let contents =
                rest.Split(", ")
                |> Set.ofArray
                |> Set.map (function | NumberedBagName n -> n)
            (container, contents)
        | _ -> failwith("Could not parse bag rule")

    let parents (bagTree: BagRule Set) (color: string) =
        bagTree
        |> Set.filter (fun r ->
            (snd r) |> Set.exists (snd >> ((=) color))
        )
        |> Set.map fst

    let rec bagsPossiblyContaining (bagTree: BagRule Set) (previouslyFoundBags: string Set) (retiredBags: string Set) =
        let newlyFoundBags =
            previouslyFoundBags
            |> Set.map (parents bagTree)
            |> Set.unionMany

        let newlyRetired = Set.union previouslyFoundBags retiredBags

        if Set.isEmpty newlyFoundBags
        then newlyRetired |> Set.count
        else
            bagsPossiblyContaining bagTree newlyFoundBags newlyRetired

    let rec bagsContained (bagTree: BagRule Set) (color: string) =
        let immediateChildren =
            bagTree
            |> Set.filter (fst >> ((=) color))
            |> Set.map snd
            |> Set.unionMany
            |> Set.toList

        match immediateChildren with
        | [] -> 0
        | _ ->
            (+)
                (immediateChildren
                |> List.sumBy fst)

                (immediateChildren
                |> List.sumBy (fun (n, c) -> n * (bagsContained bagTree c)))


    let runner1 color =
        let bagTree =
            (IO.File.ReadAllLines "day7.txt")
            |> Array.map (function | BagRule r -> r)
            |> Set.ofArray
        bagsPossiblyContaining bagTree (parents bagTree color) Set.empty

    let runner2 color =
        let bagTree =
            (IO.File.ReadAllLines "day7.txt")
            |> Array.map (function | BagRule r -> r)
            |> Set.ofArray
        bagsContained bagTree color

module Day8 =

    type ExitStatus = InfiniteLoop of int | RunToEnd of int | Error

    let rec runCode (instructionList: string list) accumulator currIndex indexesRun =
        let numInstructions = List.length instructionList
        if Set.contains currIndex indexesRun
            then InfiniteLoop accumulator
        elif currIndex < 0 || currIndex > numInstructions
            then Error
        elif currIndex = numInstructions
            then RunToEnd accumulator
        else
            let newIndexesRun = Set.add currIndex indexesRun
            match instructionList.[currIndex] with
            | Prefix "acc " (Int i) ->
                runCode instructionList (accumulator + i) (currIndex + 1) newIndexesRun
            | Prefix "jmp " (Int i) ->
                runCode instructionList accumulator (currIndex + i) newIndexesRun
            | Prefix "nop " (Int _) ->
                runCode instructionList accumulator (currIndex + 1) newIndexesRun
            | _ -> failwith("Invalid instruction")

    let flipInstruction (instructions: string list) n =
        let newInstruction =
            match instructions.[n] with
            | Prefix "jmp " rest -> "nop " + rest
            | Prefix "nop " rest -> "jmp " + rest
            | other -> other
        List.append instructions.[.. (n - 1)] (newInstruction :: instructions.[(n + 1)..])


    let runner () =
        let input =
            (IO.File.ReadAllLines "day8.txt")
            |> List.ofArray

        let part1 = runCode input 0 0 Set.empty

        let part2 =
            [0 .. (List.length input) - 1]
            |> List.choose (fun n ->
                    match runCode (flipInstruction input n) 0 0 Set.empty with
                    | RunToEnd i -> Some i
                    | _ -> None)

        part1, part2

module Day9 =

    let rec part1 (numbers: int64 list) =
        let possibleSummers = numbers.[0 .. 24]
        let currNumber = numbers.[25]

        match comboSearch Any (fun n ->
            n = List.distinct n && List.sum n = currNumber
            ) [possibleSummers; possibleSummers] with
        | Some _ ->
            part1 numbers.[1..]
        | None ->
            currNumber

    let rec part2 (numbers: int64 list) numberToMatch =
        let numRange = [0 .. (List.length numbers)]

        match comboSearch Any (fun n ->
            n.[0] < n.[1] && (List.sum numbers.[n.[0] .. n.[1]]) = numberToMatch
            ) [numRange; numRange] with
        | Some n ->
            let contiguous = numbers.[(snd n).[0] .. (snd n).[1]]
            (List.min contiguous + List.max contiguous)
        | None ->
            failwith("No solution found")

    let runner () =
        let input =
            (IO.File.ReadAllLines "day9.txt")
            |> Array.map int64
            |> List.ofArray

        let firstAnswer = part1 input
        let secondAnswer = part2 input firstAnswer

        firstAnswer, secondAnswer

module Day10 =

    let part1 ratings =
        0 :: (List.append ratings [ratings.[(List.length ratings) - 1] + 3])
        |> List.pairwise
        |> List.map (fun (a, b) -> b - a)
        |> List.countBy id

    let numWaysToGetTo ratings (waysToGetToArr: int64 list) n =
        match n, List.tryFindIndex ((=) n) ratings with
        | 0, _ -> 1L
        | _, Some index -> waysToGetToArr.[index]
        | _, None -> 0L

    let rec part2 ratings waysToGetToArr currIndex =
        if currIndex = List.length ratings
        then List.last waysToGetToArr
        else
            let waysToGetHere =
                [1; 2; 3]
                |> List.sumBy (fun n -> numWaysToGetTo ratings waysToGetToArr (ratings.[currIndex] - n))
            part2 ratings (List.append waysToGetToArr [waysToGetHere]) (currIndex + 1)


    let runner () =
        (IO.File.ReadAllLines "day10.txt")
        |> Array.map int
        |> Array.sort
        |> List.ofArray
        |> part1

    let runner2 () =
        let input =
            (IO.File.ReadAllLines "day10.txt")
            |> Array.map int
            |> Array.sort
            |> List.ofArray

        part2 input [] 0

module Day11 =

    let rec occupiedAt seats seatRow seatCol rowFun colFun keepGoing =
        match rowFun seatRow, colFun seatCol with
        | r, c when r >= 0 && r < (Array.length seats) && c >= 0 && c < (String.length seats.[0]) ->
            match seats.[r].[c] with
            | '#' -> true
            | '.' -> if keepGoing then occupiedAt seats r c rowFun colFun keepGoing else false
            | _ -> false
        | _ -> false

    let newSeatState seats keepGoing neighborThreshold (seatRow, seatCol) =
        let numNeighbors =
            [|
                ((+)  1), ((+)  0)
                ((+) -1), ((+)  0)
                ((+)  0), ((+)  1)
                ((+)  0), ((+) -1)
                ((+)  1), ((+)  1)
                ((+) -1), ((+) -1)
                ((+)  1), ((+) -1)
                ((+) -1), ((+)  1)
            |]
            |> Array.filter (fun (rowFun, colFun) ->
                occupiedAt seats seatRow seatCol rowFun colFun keepGoing)
            |> Array.length

        match seats.[seatRow].[seatCol], numNeighbors with
        | 'L', 0 -> '#'
        | '#', n when n >= neighborThreshold -> 'L'
        | s, _ -> s

    let rec occupiedAfterEquilibrium keepGoing neighborThreshold seats =
        let newSeats =
            seats
            |> Array.mapi (fun r row ->
                row
                |> Seq.mapi (fun c _ -> (newSeatState seats keepGoing neighborThreshold (r, c)) |> string)
                |> String.concat ""
            )

        if (seats = newSeats)
        then seats |> Array.sumBy (fun l -> l |> Seq.filter ((=) '#') |> Seq.length)
        else occupiedAfterEquilibrium keepGoing neighborThreshold newSeats

    let runner () =
        (IO.File.ReadAllLines "day11.txt")
        |> occupiedAfterEquilibrium false 4,

        (IO.File.ReadAllLines "day11.txt")
        |> occupiedAfterEquilibrium true 5

module Day12 =

    type Direction = North | South | East | West

    let rec rotateShipRight currDirection degrees =
        if degrees = 0
        then currDirection
        else
            match currDirection with
            | North -> rotateShipRight East (degrees - 90)
            | East -> rotateShipRight South (degrees - 90)
            | South -> rotateShipRight West (degrees - 90)
            | West -> rotateShipRight North (degrees - 90)

    let rec finalShipPosition northPos eastPos direction instructions =
        match instructions with
        instruction :: rest ->
            let mutable newNorth, newEast, newDirection = northPos, eastPos, direction
            match instruction with
            | Prefix "N" (Int amount) ->
                newNorth <- northPos + amount
            | Prefix "S" (Int amount) ->
                newNorth <- northPos - amount
            | Prefix "E" (Int amount) ->
                newEast <- eastPos + amount
            | Prefix "W" (Int amount) ->
                newEast <- eastPos - amount
            | Prefix "L" (Int amount) ->
                newDirection <- rotateShipRight direction (-amount + 360)
            | Prefix "R" (Int amount) ->
                newDirection <- rotateShipRight direction amount
            | Prefix "F" (Int amount) ->
                match direction with
                | North ->
                    newNorth <- northPos + amount
                | South ->
                    newNorth <- northPos - amount
                | East ->
                    newEast <- eastPos + amount
                | West ->
                    newEast <- eastPos - amount
            | _ ->
                failwith "No such instruction"

            finalShipPosition newNorth newEast newDirection rest
        | [] ->
            Math.Abs(northPos) + Math.Abs(eastPos)

    let rec moveWaypointBy north east waypointPos =
        let waypointNorth, waypointEast = waypointPos
        waypointNorth + north, waypointEast + east

    let rec rotateWaypoint (shipNorth, shipEast) (waypointNorth, waypointEast) degrees =
        if degrees = 0
        then (waypointNorth, waypointEast)
        else
            let (relativeNorth, relativeEast) = (waypointNorth - shipNorth, waypointEast - shipEast)
            rotateWaypoint (shipNorth, shipEast) (-relativeEast + shipNorth, relativeNorth + shipEast) (degrees - 90)

    let rec finalShipPosition2 (shipPos: int * int) (waypointPos: int * int) instructions =
        printfn "Ship: %A, Waypoint: %A" shipPos waypointPos
        match instructions with
        | instruction :: rest ->
            let mutable (newShipPos, newWaypointPos) = (shipPos, waypointPos)
            match instruction with
            | Prefix "N" (Int amount) ->
                newWaypointPos <- moveWaypointBy amount 0 waypointPos
            | Prefix "S" (Int amount) ->
                newWaypointPos <- moveWaypointBy -amount 0 waypointPos
            | Prefix "E" (Int amount) ->
                newWaypointPos <- moveWaypointBy 0 amount waypointPos
            | Prefix "W" (Int amount) ->
                newWaypointPos <- moveWaypointBy 0 -amount waypointPos
            | Prefix "L" (Int amount) ->
                newWaypointPos <- (rotateWaypoint shipPos waypointPos (-amount + 360))
            | Prefix "R" (Int amount) ->
                newWaypointPos <- (rotateWaypoint shipPos waypointPos amount)
            | Prefix "F" (Int amount) ->
                let (shipNorth, shipEast), (waypointNorth, waypointEast) = (shipPos, waypointPos)
                let northToMove = (waypointNorth - shipNorth) * amount
                let eastToMove = (waypointEast - shipEast) * amount
                newShipPos <- (shipNorth + northToMove, shipEast + eastToMove)
                newWaypointPos <- (waypointNorth + northToMove, waypointEast + eastToMove)
            | _ ->
                failwith "No such instruction"
            finalShipPosition2 newShipPos newWaypointPos rest
        | [] ->
            let shipNorth, shipEast = shipPos
            Math.Abs(shipNorth) + Math.Abs(shipEast)

    let runner () =
        IO.File.ReadAllLines "day12.txt"
        |> List.ofArray
        |> finalShipPosition 0 0 East,

        IO.File.ReadAllLines "day12.txt"
        |> List.ofArray
        |> finalShipPosition2 (0, 0) (1, 10)

module Day13 =

    let rec findNearestGreaterMultiple n multipleOf =
        if n % multipleOf = 0
        then n
        else findNearestGreaterMultiple (n + 1) multipleOf

    let rec findNearestGreaterMultipleLong n multipleOf =
        if n % multipleOf = 0L
        then n
        else findNearestGreaterMultipleLong (n + 1L) multipleOf

    let part1 (busses: string) departureTime =
        (Array.map ((fun b -> Int32.Parse(b)) >> (fun bus ->
            bus, (findNearestGreaterMultiple departureTime bus) - departureTime)) (busses.Split(",")
        |> Array.filter ((<>) "x")))
        |> Array.minBy snd


    let extendedEuclid a b =
        let mutable mA, mB = a, b
        let mutable x0, x1, y0, y1 = 0L, 1L, 1L, 0L
        while mA <> 0L do
            let q = mB / mA
            let newMa = mB % mA
            let newMb = mA
            let newY0, newY1 = y1, y0 - q * y1
            let newX0, newX1 = x1, x0 - q * x1
            y0 <- newY0
            y1 <- newY1
            x0 <- newX0
            x1 <- newX1
            mA <- newMa
            mB <- newMb
        b, x0, y0

    let modInverse (a, b) =
        let g, x, _ = extendedEuclid (a % b) b
        if g <> 1L
        then (x + b) % b
        else failwith "Could not compute modular inverse"

    let part2 (busses: string) =
        let busInfo =
            busses.Split(",")
            |> Array.indexed
            |> Array.choose (fun (index, busStr) ->
                match busStr with
                | Int64 busNum -> Some (index |> int64, busNum)
                | _ -> None

            )

        let ais = busInfo |> Array.map (fun (a, n) -> (n - a) % n)
        let nis = busInfo |> Array.map snd

        let bigN =
            nis
            |> Array.reduce ( * )

        let yis = nis |> Array.map (fun ni -> bigN / ni)
        let zis =
            Array.zip yis nis
            |> Array.map modInverse

        let x =
            Array.zip3 ais yis zis
            |> Array.sumBy (fun (ai, yi, zi) -> ai * yi * zi)

        (x % bigN)

    let runner () =
        let input = IO.File.ReadAllLines "day13.txt"
        part1 (input.[1]) (Int32.Parse(input.[0])),
        part2 (input.[1])

module Day14 =
    let getAllAddresses address (mask: string) =
        let mutable xLocations = []
        let baseAddress =
            [|0 .. 35|]
            |> Array.sumBy (fun i ->
                match mask.[i] with
                | 'X' ->
                    xLocations <- i :: xLocations
                    0L
                | '1' ->
                    1L <<< i
                | '0' ->
                    address &&& (1L <<< i)
                | _ ->
                    failwith "Could not parse mask"
            )

        [|0 .. (1 <<< List.length xLocations) - 1|]
        |> Array.map (fun i ->
            xLocations
            |> List.indexed
            |> List.sumBy (fun (index, location) ->
                if ((1 <<< index) &&& i) > 0
                then (1L <<< location)
                else 0L)
            |> (+) baseAddress
        )

    let applyValue memoryAddress value (mask: string) memorySpace =
        let mutable result = 0L
        [|0 .. 35|]
        |> Array.iter (fun i ->
            let valueToAdd =
                match mask.[i] with
                | 'X' ->
                    value &&& (1L <<< i)
                | '1' ->
                    1L <<< i
                | '0' ->
                    0L
                | _ ->
                    failwith "Could not parse mask"
            result <- result + valueToAdd
        )

        Map.add memoryAddress result memorySpace

    let v2ApplyValue memoryAddress value (mask: string) memorySpace =
        (memorySpace, getAllAddresses memoryAddress mask)
        ||> Array.fold (fun map addr -> Map.add addr value map)

    let (|MemoryAssignment|) (str: string) =
        let firstBracket, secondBracket =
            str.IndexOf("["), str.IndexOf("]")
        let memoryAddress = Int64.Parse(str.Substring(firstBracket + 1, secondBracket - firstBracket - 1))

        let value = Int64.Parse(str.Substring(str.IndexOf("=") + 1))

        memoryAddress, value

    let rec processInstructions instructions mask (memorySpace: Map<int64, int64>) applyValueFun =
        match instructions with
        | instruction :: rest ->
            match instruction with
            | Prefix "mask = " newMask ->
                processInstructions rest (Seq.rev newMask |> Seq.map string |> String.concat "") memorySpace applyValueFun
            | MemoryAssignment (address, value) ->
                let newMemorySpace = applyValueFun address value mask memorySpace
                processInstructions rest mask newMemorySpace applyValueFun
        | [] ->
            memorySpace
            |> Map.toArray
            |> Array.sumBy snd

    let runner () =
        processInstructions (IO.File.ReadAllLines "day14.txt" |> List.ofArray) "" Map.empty applyValue,
        processInstructions (IO.File.ReadAllLines "day14.txt" |> List.ofArray) "" Map.empty v2ApplyValue

module Day15 =

    let rec nthInSequence (numberSet: Map<int, int>) previousNum turnNum maxTurns =
        if turnNum = maxTurns - 1
        then
            previousNum
        else
            let nextNumber =
                let lastSpoken =
                    numberSet |> Map.tryFind previousNum
                match lastSpoken with
                | Some t -> turnNum - t
                | None -> 0

            nthInSequence (Map.add previousNum (turnNum) numberSet) nextNumber (turnNum + 1) maxTurns


    let runner () =
        let input =
            (IO.File.ReadAllText "day15.txt").Split(",")
            |> List.ofArray
            |> List.map int
        let inputSet =
            input
            |> List.mapi (fun i n -> n, i)
            |> Map.ofList

        nthInSequence inputSet 0 (List.length input) 2020,
        nthInSequence inputSet 0 (List.length input) 30000000

module Day16 =

    let (|NumberRange|) (str: string) =
        let first = str.Split("-").[0] |> int
        let second = str.Split("-").[1] |> int
        first, second

    let (|FieldRule|) (str: string) =
        let name = str.Split(": ").[0]
        let values = str.Split(": ").[1].Split(" or ")
        let firstRange, secondRange =
            match values.[0], values.[1] with
            | NumberRange first, NumberRange second ->
                first, second

        name, firstRange, secondRange

    let inRange (rangeMin, rangeMax) number =
        number >= rangeMin && number <= rangeMax

    let validForRule number (_, firstRange, secondRange) =
        (inRange firstRange number) || (inRange secondRange number)


    let getValidTickets rules (tickets: int array[]) =

        tickets
        |> Array.where(fun t ->
            t
            |> Array.exists (fun n ->
                rules
                |> Array.forall (fun r -> not (validForRule n r)))
            |> not)

    let part1 rules (nearbyTickets: int array[]) =
        let allNums = Array.collect id nearbyTickets

        allNums
        |> Array.where (fun n ->
            rules
            |> Array.forall (fun r -> not (validForRule n r)))
        |> Array.sum

    let ruleInPosition rule position tickets =
        let values = Array.collect (fun (t: int array) -> [|t.[position]|]) tickets
        // printfn "RIP for %d: nums are %A" position values
        values
        |> Array.forall (fun n -> validForRule n rule)

    let rec assignPositions rulesWithPossiblePositions (currAssignments: string list) =
        // printfn "Trying %A" currAssignments
        let currPosition = (List.length currAssignments)
        // printfn "cp is %d, rwpp is %d" currPosition (Array.length rulesWithPossiblePositions)
        if Array.isEmpty rulesWithPossiblePositions
        then
            printfn "heyya"
            Some(currAssignments)
        else
            let candidates =
                rulesWithPossiblePositions
                |> Array.filter (fun (_, positions) ->
                    Set.contains currPosition positions
                )

            candidates
            |> Array.tryPick (fun (candidateName, _) ->
                let remainingRules =
                    rulesWithPossiblePositions |> Array.filter (fun (name, _) -> name <> candidateName)
                let result = assignPositions remainingRules (candidateName :: currAssignments)
                result
            )

    let part2 rules tickets =
        let rulesAndPossiblePositions =
            rules
            |> Array.map (fun r ->
                let possibleIndexes =
                    [0 .. (Array.length rules) - 1]
                    |> List.where (fun i -> ruleInPosition r i tickets)
                    |> Set.ofList
                let name, _, _ = r
                name, possibleIndexes
            )
        printfn "RAPP is %A" rulesAndPossiblePositions
        match assignPositions rulesAndPossiblePositions [] with
        | Some x -> x |> List.rev
        | None -> []

    let runner () =
        let inputGroups = (IO.File.ReadAllText "day16.txt").Split("\n\n")
        let rules =
            inputGroups.[0].Split("\n")
            |> Array.map (fun l -> match l with | FieldRule r -> r)
        let myTicket =
            inputGroups.[1].Split("\n").[1].Split(",") |> Array.map int

        let nearbyTickets =
            inputGroups.[2].Split("\n").[1 ..] |> Array.map (fun l ->
                l.Split(",") |> Array.map int
            )
        let validTickets = getValidTickets rules (Array.append nearbyTickets [|myTicket|])
        part2 rules validTickets

module Day17 =

    let get1dNeighborsAndSelf (x) =
        [x; x + 1; x - 1]

    let get2dNeighborsAndSelf (x, y) =
        get1dNeighborsAndSelf x
        |> List.collect (fun (nx) -> [nx, y; nx, y + 1; nx, y - 1])

    let get3dNeighborsAndSelf (x, y, z) =
        get2dNeighborsAndSelf (x, y)
        |> List.collect (fun (nx, ny) -> [nx, ny, z; nx, ny, z + 1; nx, ny, z - 1])

    let get4dNeighborsAndSelf (x, y, z, w) =
        get3dNeighborsAndSelf (x, y, z)
        |> List.collect (fun (nx, ny, nz) -> [nx, ny, nz, w; nx, ny, nz, w + 1; nx, ny, nz, w - 1])


    let cubeIsActiveNextCycle gridState neighborFun cube =
        let cubeCurrentlyActive = Set.contains cube gridState
        let activeNeighborsAndSelf =
            neighborFun cube
            |> List.filter (fun n -> Set.contains n gridState)
            |> List.length

        match cubeCurrentlyActive, activeNeighborsAndSelf with
        | true, (3 | 4) // 3 and 4 because the (active) cube itself is included in the count
        | false, 3 -> true
        | _, _ -> false


    let rec performCycle numCycles neighborFun gridState =
        if numCycles = 0
        then gridState
        else
            let cubesAndTheirNeighbors =
                Set.unionMany(
                    gridState
                    |> Set.map (neighborFun >> Set.ofList))

            let nextGrid =
                cubesAndTheirNeighbors
                |> Set.filter (cubeIsActiveNextCycle gridState neighborFun)

            performCycle (numCycles - 1) neighborFun nextGrid


    let inputToGridState input =
        input
        |> Array.indexed
        |> Array.collect (fun (x, rowContents) ->
            rowContents
            |> Seq.indexed
            |> Seq.choose (fun (y, c) ->
                match c with
                | '#' -> Some (x, y)
                | '.' -> None
                | _ -> failwith "Unknown character in input")
            |> Array.ofSeq)
        |> Set.ofArray

    let convert2dTo3dGrid grid =
        grid
        |> Set.map (fun (x, y) -> x, y, 0)

    let convert2dTo4dGrid grid =
        grid
        |> Set.map (fun (x, y) -> x, y, 0, 0)

    let runner () =
        (IO.File.ReadAllLines "day17.txt")
        |> inputToGridState
        |> convert2dTo3dGrid
        |> performCycle 6 get3dNeighborsAndSelf
        |> Set.count,

        (IO.File.ReadAllLines "day17.txt")
        |> inputToGridState
        |> convert2dTo4dGrid
        |> performCycle 6 get4dNeighborsAndSelf
        |> Set.count

[<EntryPoint>]
let main argv =
    printfn "%A" (Day17.runner ())
    0