namespace Views.Dashboard
open System.ComponentModel.DataAnnotations
open DotVVM.Framework.Hosting
open System.Threading.Tasks
open DotVVM.Framework.ViewModel
open DotVVM.Framework.ViewModel.Validation
open Helpers
open Helpers
open Model
open Model
open Newtonsoft.Json
open System.Linq.Expressions
open System

type MineStateViewModel = {
    Name: string
    Resource: string
    NextYield: int
    NextCoords: int[]
    Power: int
}

module Helpers =
    open Microsoft.AspNetCore.Http

    let authCheck (context: HttpContext) =
        let name = context.Request.Cookies.Item "name"
        let password = context.Request.Cookies.Item "password"
        if StateManager.getState().Credentials |> Array.contains { TeamCredentials.Name = name; Password = password } then
            Some name
        else
            None

    let login name password (context: HttpContext) =
        if StateManager.getState().Credentials |> Array.contains { TeamCredentials.Name = name; Password = password } then
            context.Response.Cookies.Delete "name"
            context.Response.Cookies.Delete "password"
            context.Response.Cookies.Append ("name", name)
            context.Response.Cookies.Append ("password", password)
            true
        else
            false
    let getTeamState (teamName) =
        let state = StateManager.getState()
        let teamData = state.Teams |> Array.find (fun t -> t.Name = teamName)
        (state, teamData)

    let isGodMode (context: HttpContext) = context.Request.Cookies.Item "magic_secret" = "ffsahflksafhsajfhgds"

type TransactionViewModel = {
    [<RequiredAttribute>]
    To: string
    Assets: AssetModel []
    Notes: string
}

type AccountDataViewModel = {
    AssetName: string
    Spendable: int64
    TransactionLocked: int64
}

type ViewModel() =
    inherit DotvvmViewModelBase()

    let emptyTransactionModal teamData =
        { TransactionViewModel.To = null; Assets = teamData.Account.Assets |> Array.map (fun a -> { a with Count = int64 0 }); Notes = "" }

    let spendableAmount teamData =
        let spendableAssets = teamData.Account.Assets |> Seq.map(fun a -> a.Name, a.Count) |> Map.ofSeq
        fun name -> Map.tryFind name spendableAssets |> Option.defaultValue (int64 0)

    override x.Load() =
        x.IsGodMode <- Helpers.isGodMode (x.Context.GetAspNetCoreContext())
        if not x.IsGodMode then
            x.TeamName <-
                      match Helpers.authCheck (x.Context.GetAspNetCoreContext()) with
                      | Some name -> name
                      | None -> x.Context.RedirectToRoute("login"); failwith ""

        Task.CompletedTask

    override x.PreRender() =
        let state = StateManager.getState()
        x.OtherTeamNames <- state.Teams |> Array.map (fun t -> t.Name)
        if String.IsNullOrEmpty x.TeamName then x.TeamName <- Array.head x.OtherTeamNames
        x.ComingTransactions <- state.Transactions |> Array.filter (fun t -> t.To = x.TeamName)
        let pendingTransations = state.Transactions |> Array.filter (fun t -> t.From = Some x.TeamName)
        x.MyPendingTransactions <- pendingTransations

        let (_, teamData) = Helpers.getTeamState x.TeamName

        x.AccountData <-
            teamData.Account.Assets
            |> Array.map (fun (a: AssetModel) ->
                let locked = pendingTransations |> Seq.sumBy (fun t -> t.Assets |> Seq.tryFind (fun aa -> aa.Name = a.Name) |> Option.map (fun a -> a.Count) |> Option.defaultValue (int64 0))
                { AccountDataViewModel.AssetName = a.Name; Spendable = a.Count; TransactionLocked = locked }
            )
        x.Mines <- teamData.Mines |> Array.map (fun (i, pow, m) ->
                let mine = state.Mines |> Array.find (fun (mm:MineModel) -> mm.Name = m)
                { MineStateViewModel.Name = mine.Name; Power = pow; NextYield = int (mine.Yield * float mine.Resources * float pow); NextCoords = Hasher.getCoords m x.TeamName (Seq.toList mine.Dimensions) i; Resource = mine.ResourceName }
            )
        x.MineCodes <- Array.append x.MineCodes (Array.create (x.Mines.Length - x.MineCodes.Length) "")

        if not x.Context.IsPostBack then
            x.TransactionModal <- emptyTransactionModal teamData
        else if x.TransactionModal.Assets.Length <> x.AccountData.Length then
            x.TransactionModal <- { x.TransactionModal with Assets = Seq.append x.TransactionModal.Assets (teamData.Account.Assets |> Seq.skip x.TransactionModal.Assets.Length |> Seq.map (fun a -> { a with Count = int64 0 })) |> Seq.toArray}

        Task.CompletedTask

    member val TeamName : string = "" with get, set
    member val TransactionModalOpen = false with get, set
    [<BindAttribute(Direction.ServerToClient)>]
    member val AccountData: AccountDataViewModel[] = [||] with get, set
    [<JsonConverterAttribute(typeof<HackDefaultJsonConverter>)>]
    member val TransactionModal = Unchecked.defaultof<TransactionViewModel> with get, set
    [<JsonConverterAttribute(typeof<HackDefaultJsonConverter>)>]
    [<ProtectAttribute(ProtectMode.SignData)>]
    member val ComingTransactions : TransactionModel[] = [||] with get, set

    [<JsonConverterAttribute(typeof<HackDefaultJsonConverter>)>]
    [<ProtectAttribute(ProtectMode.SignData)>]
    member val MyPendingTransactions : TransactionModel[] = [||] with get, set
    member val OtherTeamNames : string[] = [||] with get, set

    [<JsonConverterAttribute(typeof<HackDefaultJsonConverter>)>]
    [<ProtectAttribute(ProtectMode.SignData)>]
    member val Mines : MineStateViewModel[] = [||] with get, set
    member val MineCodes: string[] = [||] with get, set

    member val MineUpgrade = 0 with get, set

    member val UpdateMsgSerialized = "" with get, set
    member val IsGodMode = false with get, set
    member val GodModeTeamSwitch = "" with get, set

    member x.ChangeTeam() =
        let (_, teamData) = Helpers.getTeamState x.TeamName
        x.MineCodes <- [||]
        x.TransactionModal <- emptyTransactionModal teamData

    member x.SubmitTransaction() =
        lock StateManager.locker (fun _ ->
            let (state, teamData) = Helpers.getTeamState x.TeamName
            let transaction = { TransactionModel.Assets = x.TransactionModal.Assets |> Array.filter (fun a -> a.Count <> int64 0); Id = Guid.NewGuid(); From = Some x.TeamName; To = x.TransactionModal.To; Notes = x.TransactionModal.Notes }

            let spendable = spendableAmount teamData
            for (i, asset) in Array.indexed transaction.Assets do
                if spendable asset.Name < asset.Count then
                    x.Context.ModelState.Errors.Add(
                        let m = ViewModelValidationError()
                        m.PropertyPath <- sprintf "Assets()[%d]().Count" i
                        m.ErrorMessage <- sprintf "%s: Máš jenom %d" asset.Name (spendable asset.Name)
                        m)
            if isNull transaction.To then
                x.Context.ModelState.Errors.Add(
                        let m = ViewModelValidationError()
                        m.PropertyPath <- "To"
                        m.ErrorMessage <- "Je potřeba vyplnit příjemce."
                        m)

            x.Context.FailOnInvalidModelState()

            StateManager.processUpdate (UpdateMsg.ProposeTransaction transaction)

            x.TransactionModal <- emptyTransactionModal teamData
        )
        x.TransactionModalOpen <- false

        ()

    member x.AcceptTransaction (transaction:TransactionModel) =
        let (state, teamData) = Helpers.getTeamState x.TeamName
        let spendable = spendableAmount teamData
        for i in transaction.Assets |> Array.filter (fun a -> spendable a.Name + a.Count < int64 0) do
            x.Context.ModelState.Errors.Add(
                                    let m = ViewModelValidationError()
                                    m.PropertyPath <- "Assets"
                                    m.ErrorMessage <- sprintf "%s: Máš jenom %d" i.Name (spendable i.Name)
                                    m)
        x.Context.FailOnInvalidModelState()

        StateManager.processUpdate (UpdateMsg.AcceptTransaction transaction.Id)
    member x.CancelTransaction (tid: Guid) =
        StateManager.processUpdate (UpdateMsg.CancelTransaction tid)

    member val NewMineName = "" with get, set
    member x.DiscoverMine() =
        let (state, teamData) = Helpers.getTeamState x.TeamName
        if Array.exists (fun (m: MineModel) -> m.Name = x.NewMineName) state.Mines |> not then
            x.AddModelError(Expr.Quote(fun x -> x.NewMineName), "Takový důl není") |> ignore
        if Array.exists (fun (_, _, n) -> n = x.NewMineName) teamData.Mines then
            x.AddModelError(Expr.Quote(fun x -> x.NewMineName), "Tento důl už máš objevený") |> ignore
        x.Context.FailOnInvalidModelState()
        StateManager.processUpdate (UpdateMsg.DiscoverMine (x.TeamName, x.NewMineName))


    member x.Mine index =
        let code = x.MineCodes.[index]
        let mineVM = x.Mines.[index]
        let correctCode = Hasher.mineEntry mineVM.Name (Seq.toList mineVM.NextCoords)
        if String.Equals(correctCode, code, StringComparison.InvariantCultureIgnoreCase) |> not then
            x.Context.ModelState.Errors.Add(
                            let m = ViewModelValidationError()
                            m.PropertyPath <- sprintf "$data.MineCodes()[%d]" index
                            m.ErrorMessage <- "špatný kód"
                            m)
        x.Context.FailOnInvalidModelState()
        x.MineCodes.[index] <- ""
        StateManager.processUpdate (UpdateMsg.MineItem (x.TeamName, mineVM.Name, mineVM.Power))

    member x.PushMsg () =
        if not x.IsGodMode then failwith "go away"
        try
            let msgstring = x.UpdateMsgSerialized
            let msg = StateManager.serializer.UnPickleOfString<UpdateMsg> msgstring
            StateManager.processUpdate msg
        with e ->
            // ta zkurvená validace tu nefunguje, tak se prostě můžeš podívat do stdout...
            printfn "push msg error: %O" e
            x.AddModelError(Expr.Quote(fun x -> x.UpdateMsgSerialized), e.ToString()) |> ignore
            x.Context.FailOnInvalidModelState()

    member x.UpgradeMine (mineName) =
        if not x.IsGodMode then failwith "go away"
        StateManager.processUpdate (UpdateMsg.UpgradeMine (x.TeamName, mineName, x.MineUpgrade))
        x.MineUpgrade <- 0



