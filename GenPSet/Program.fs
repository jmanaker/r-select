[<System.Runtime.InteropServices.DllImport("kernel32.dll", SetLastError=true)>]
extern uint32 GetConsoleProcessList(uint32[] processes, uint32 count)

let inSuiGenerisConsole=
    let retval=GetConsoleProcessList([|0u|],1u)
    if 0u=retval then
        System.Runtime.InteropServices.Marshal.GetHRForLastWin32Error() |> System.Runtime.InteropServices.Marshal.ThrowExceptionForHR
    1u=retval

let gen = new System.Random()

let rec GetUnique times lst max = 
    match times with
    | 0 -> lst
    | t when t > max -> invalidArg "max" "requested more problems than available"
    | t when t < 0 -> invalidArg "times" "requested negative number of problems"
    | t -> 
        let candidate = gen.Next max
        match (=) candidate |> List.tryFind <| lst with
        | Some _ -> lst |> GetUnique t <| max
        | None -> candidate::lst |> GetUnique (t-1) <| max

let prompt cue = printfn cue; System.Console.ReadLine()

let map (f, g) (x, y) = (f x, g y)

[<EntryPoint>]
let main argv = 
    let (num, max) = (
        match argv.LongLength with
        | 2L -> argv.[0], argv.[1]
        | _ -> prompt "How many problems do you want?", prompt "How many problems are available?") |> map (int, int)
    GetUnique num [] max |> List.sort |> printfn "%A"
    0
    