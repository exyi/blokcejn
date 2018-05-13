namespace Blokčejn

open System
open System.Collections.Generic
open System.IO
open System.Linq
open System.Threading.Tasks
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Logging

module Program =
    open Model

    let exitCode = 0

    let BuildWebHost args =
        WebHost
            .CreateDefaultBuilder(args)
            .UseStartup<Startup>()
            .UseUrls("http://*:12345")
            .Build()

    [<EntryPoint>]
    let main args =
        match Environment.GetEnvironmentVariable "blokcejn_savefile" with
        | null | "" -> ()
        | f -> StateManager.reload (Some f)
        // StateManager.processUpdate (UpdateMsg.ProposeTransaction { TransactionModel.Id = Guid.NewGuid(); From = None; To = "debili"; Assets = [| { AssetModel.Count = int64 232; Name = "uhli" } |]; Notes = "" })
        BuildWebHost(args).Run()

        exitCode
