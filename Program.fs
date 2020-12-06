// Learn more about F# at http://fsharp.org

open System

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

    // Easy helper to let us confirm that something is an int
    let (|Int|_|) str =
       match System.Int32.TryParse(str:string) with
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

[<EntryPoint>]
let main argv =
    printfn "%A" (Day2.runner())
    0