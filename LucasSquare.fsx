#time "on"
// #load "Bootstrap.fsx"
#r @"bin/Debug/netcoreapp3.1/FSharp.Core.dll"
#r @"bin/Debug/netcoreapp3.1/Akka.dll"
#r @"bin/Debug/netcoreapp3.1/Akka.FSharp.dll"


open System
open Akka
open System.Collections.Generic
open Akka.Actor
open Akka.Configuration
open Akka.FSharp

type commands = 
    | Divide of string
    | Multsum of string
    | Done of string

let args : string array = fsi.CommandLineArgs |> Array.tail
let input = args.[0] |>int64|> bigint
let window_Size = args.[1] |> int64 |>  bigint
let mutable flag = false
let mutable actors_used = 0
let worker1(mailbox: Actor<_>)=
    let rec loop() = actor {
        let! msg = mailbox.Receive()
        let sender = mailbox.Sender()
        match msg with 
        | Multsum message ->
            let numbers = message.Split ' '
            let mutable result = 0 |>bigint
            let start = numbers.[0] |>int
            let last = numbers.[1] |>int
            let one = 1|>bigint
            let mutable temp = 1 |> bigint
            let mutable st = 1|> bigint
            let mutable flag = false
            for i = start|>bigint to last|>bigint do
                let startPlusWindow = (i + (window_Size)- (one))
                for j = i to startPlusWindow do 
                    temp <- ((j*j))  
                    result <- result + temp

                let sqRt = result |> double |>sqrt |>uint64
                if result|>uint64 = sqRt * sqRt then
                    printf "%A\n" i
                   
                result <- 0|>bigint
            mailbox.Sender() <! Done "done" 
        return! loop()        
    }
    loop()

let system = System.create "system" (Configuration.defaultConfig())

let boss (mailbox: Actor<_>)=
    let rec loop() = actor {
        let! msg = mailbox.Receive()  
        
        match msg with
        | Divide command_line_ip ->
            let com = command_line_ip.Split ' ' 
            let input = com.[0] |>int 
            let window_size = com.[1]|>int |>bigint
            let number_of_actors = 10
            let part = input/number_of_actors
            let mutable resultlist = []
            for i = 1 to 9 do
                resultlist  <- [i*part] |> List.append resultlist
            
            let message1 = "1 " + (resultlist.[0] |>string)
            spawn system "0" worker1 <! Multsum message1
            for i = 1 to 8 do
                let message = ((resultlist.[i-1] + 1) |>string) + " "+ ((resultlist.[i])|>string)
                spawn system (sprintf "%i" i) worker1 <! Multsum message
            
            let message10 = ((resultlist.[8] + 1) |>string) + " "+ (input|>string)
            spawn system (sprintf "%s" "10") worker1 <! Multsum message10 
            
        | Done meaningful ->
            actors_used <- actors_used + 1
            if(actors_used = 10) then
                flag <- true    
        return! loop()
        }
    loop()    
        
let message_string = (input |> string)+ " " + (window_Size|>string)
if(window_Size > (0|>bigint)) then
    let boss_caller = spawn system "boss" boss 
    boss_caller<! Divide message_string
else
    flag <- true

let mutable Break = false
while not Break do 
      if (flag) then  
        Break <- true
done
        
    
