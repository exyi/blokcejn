module WebSocketUpdate
open System.Net.WebSockets
open FSharp.Control.Tasks
open Model
open System
open System.Threading
open System.Threading.Tasks

let handleWS (connection: WebSocket) teamName =
    task {
        printfn "WS: connection from %s" teamName
        let isMyTransaction id =
            let t = (StateManager.getState().Transactions |> Array.find(fun t -> t.Id = id))
            t.From = Some teamName || t.To = teamName
        let hander = Action<UpdateMsg>(fun msg ->
            let shouldUpdate =
                match msg with
                | UpdateMsg.AddTeam _ -> true
                | UpdateMsg.AcceptTransaction tid -> isMyTransaction tid
                | UpdateMsg.CancelTransaction tid -> isMyTransaction tid
                | UpdateMsg.ProposeTransaction t -> t.From = Some teamName || t.To = teamName
                | UpdateMsg.DiscoverMine (team, _) -> team = teamName
                | UpdateMsg.MineItem (team, _, _) -> team = teamName
                | UpdateMsg.NewMine _ -> false
            if shouldUpdate || String.IsNullOrEmpty teamName then
                printfn "Sending update to %s" teamName
                connection.SendAsync(ArraySegment.Empty, WebSocketMessageType.Text, true, CancellationToken.None) |> ignore
        )
        StateManager.addNofificationTarget hander
        while not connection.CloseStatus.HasValue do
            let buffer = Array.create (4 * 1024) (byte 0)
            let! msg = connection.ReceiveAsync(ArraySegment(buffer, 0, buffer.Length), CancellationToken.None)
            ()
        StateManager.rmNofificationTarget hander
        printfn "WS: closed from %s" teamName
        ()
    }