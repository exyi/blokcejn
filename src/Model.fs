namespace Model
open System

type AssetModel = {
    Name: string
    Count: int64
}
with
    static member IsPositive asset = asset.Count > int64 0

type AccountModel = {
    Assets: AssetModel []
}

type TeamModel = {
    Name: string
    Account: AccountModel
    Mines: (int * int * string) []
}

type TeamCredentials = {
    Name: string
    Password: string
}

type TransactionModel = {
    Id: Guid
    From: string option
    To: string
    Assets: AssetModel []
    Notes: string
}

type MineModel = {
    Name: string
    MineKey: string
    Dimensions: int []
    Resources: int64
    ResourceName: string
    Yield: float
}

type GameState = {
    Teams: TeamModel[]
    Transactions: TransactionModel []
    Credentials: TeamCredentials []
    Mines: MineModel[]
}

type UpdateMsg =
    | AddTeam of name: string * password: string * account: AccountModel
    | ProposeTransaction of TransactionModel
    | AcceptTransaction of Guid
    | CancelTransaction of Guid
    | NewMine of MineModel
    | DiscoverMine of teamName: string * mineName: string
    | MineItem of teamName: string * mineName: string * power: int
    | UpgradeMine of teamName: string * mineName: string * levels: int

module StateManager =
    open System.Collections.Generic
    open MBrace.FsPickler.Json

    let emptyState = { Teams = [||]; Transactions = [||]; Credentials = [||]; Mines = [||] }
    let serializer = FsPickler.CreateJsonSerializer(indent = false)
    let mutable private savefile : string option = None
    let mutable private globalState : GameState = emptyState
    let locker = obj()
    let mutable private notifications : Action<UpdateMsg> = Action<_>(ignore)
    let private txLog = ResizeArray()
    let getTxLog () = txLog :> _ seq

    let addNofificationTarget (a: Action<UpdateMsg>) = lock locker (fun _ ->
            notifications <- Action<_>.Combine(notifications, a) :?> Action<_>
        )

    let rmNofificationTarget (a: Action<UpdateMsg>) = lock locker (fun _ ->
            notifications <- Action<_>.Remove(notifications, a) :?> Action<_>
        )

    let private validateState state =
        let req a = if not a then failwithf "Invalid state"
        let isGroupOfOne t = t |> Seq.forall (fun (k, g) -> Seq.length g = 1) |> req
        state.Teams |> Seq.groupBy (fun t -> t.Name) |> isGroupOfOne
        state.Mines |> Seq.groupBy (fun t -> t.Name) |> isGroupOfOne
        state.Mines |> Seq.forall(fun m -> m.Resources >= int64 0) |> req
        state.Teams |> Seq.iter(fun t -> t.Mines |> Seq.groupBy (fun (_, _, a) -> a) |> isGroupOfOne)
        state.Teams |> Seq.forall (fun t -> t.Account.Assets |> Seq.forall (fun a -> a.Count >= int64 0)) |> req

    let private editTeams teamPredicate teamSelect state =
        { state with
            Teams = Array.map (fun (x: TeamModel) ->
                        match x with
                        | team when teamPredicate team -> teamSelect team
                        | a -> a
                    ) state.Teams
        }

    let private addAssets teamPredicate (assets: AssetModel[]) (multiplier:int64) =
        editTeams teamPredicate (fun team ->
            { team with
                Account =
                { team.Account with
                    Assets = team.Account.Assets
                             |> Array.map(fun a ->
                                      { a with Count = a.Count + multiplier * (assets |> Seq.filter (fun aa -> aa.Name = a.Name) |> Seq.map (fun a -> a.Count) |> Seq.sum)})
                             |> fun x -> Array.append x (assets |> Array.filter (fun ta -> Array.exists (fun (a: AssetModel) -> a.Name = ta.Name) team.Account.Assets |> not))
                }
            })

    let processUpdate msg =
        lock locker (fun _ ->
            let s = globalState
            let newState =
                match msg with
                | AddTeam (name, password, account) ->
                    { s with Teams = Array.append s.Teams [| { TeamModel.Name = name; Account = account; Mines = [||] } |]; Credentials = Array.append s.Credentials [| { TeamCredentials.Name = name; Password = password } |] }
                | ProposeTransaction t ->
                    let s = addAssets (fun team -> Some team.Name = t.From) (t.Assets |> Array.filter AssetModel.IsPositive) (int64 -1) s
                    { s with Transactions = s.Transactions |> Array.append [| t |] }
                | AcceptTransaction tid ->
                    let t = s.Transactions |> Array.find (fun t -> t.Id = tid)
                    let s = addAssets (fun team -> team.Name = t.To) t.Assets (int64 1) s
                    let s = addAssets (fun team -> Some team.Name = t.From) (t.Assets |> Array.filter (AssetModel.IsPositive >> not)) (int64 -1) s
                    { s with Transactions = s.Transactions |> Array.except [| t |] }
                | CancelTransaction tid ->
                    let t = s.Transactions |> Array.find (fun t -> t.Id = tid)
                    let s = addAssets (fun team -> Some team.Name = t.From) (t.Assets |> Array.filter AssetModel.IsPositive) (int64 1) s
                    { s with Transactions = s.Transactions |> Array.except [| t |] }
                | NewMine mine ->
                    { s with Mines = Array.append s.Mines [|mine|] }
                | DiscoverMine (team, mine) ->
                    editTeams (fun t -> t.Name = team) (fun t -> { t with Mines = Array.append t.Mines [|0, 1, mine|] }) s
                | MineItem (team, mineName, power) ->
                    let mine = s.Mines |> Array.find (fun m -> m.Name = mineName)
                    let mined = int64 (mine.Yield * float power * float mine.Resources)
                    let mined = if mine.Resources - mined >= int64 0 then mined else mine.Resources
                    let s = addAssets (fun t -> t.Name = team) [| { AssetModel.Name = mine.ResourceName; Count = mined } |] (int64 1) s
                    let s = editTeams (fun t -> t.Name = team) (fun team ->
                               { team with Mines = team.Mines |> Array.map (fun (i, pow, n) ->
                                        if n = mineName then (i + 1, pow, n)
                                        else (i, pow, n))
                               } ) s
                    { s with
                        Mines = s.Mines |> Array.map (fun m ->
                            match m with
                            | m when m = mine -> { m with Resources = m.Resources - mined }
                            | a -> a)
                    }
                | UpgradeMine (team, mine, levels) ->
                    editTeams (fun t -> t.Name = team) (fun t ->
                        { t with Mines = t.Mines |> Array.map (fun (i, p, name) ->
                                            if name = mine then
                                                (i, p + levels, name)
                                            else
                                                (i, p, name)
                                 )
                        }) s

            validateState newState

            notifications.Invoke msg

            globalState <- newState

            txLog.Add msg
            savefile |> Option.map (fun f -> IO.File.AppendAllLines(f, [ serializer.PickleToString msg ])) |> ignore

            ()
        )
    let getState () = globalState

    let reload file =
        lock locker (fun _ ->
            globalState <- emptyState
            savefile <- None
            match file with
            | Some file ->

                IO.File.ReadLines file |> Seq.map (fun l -> serializer.UnPickleOfString<UpdateMsg> l) |> Seq.iter processUpdate
                ()
            | _ -> ()
            savefile <- file
            ()
        )