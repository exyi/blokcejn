namespace Blokčejn
open DotVVM.Framework.Compilation.Styles
open DotVVM.Framework.Configuration
open DotVVM.Framework.Configuration
open DotVVM.Framework.Controls
open DotVVM.Framework.ResourceManagement
open FSharp.Control.Tasks
open Helpers
open System
open System.IO
open System.Reflection.Emit
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Model
open MBrace.FsPickler.Json
open Model
open Views.Dashboard


type DotvvmStartup() =
    interface IDotvvmStartup with
        member x.Configure(config, appPath) =
            config.RouteTable.Add("board", "", "Views/dashboard.html")
            config.RouteTable.Add("login", "login", "Views/login.html")
            config.Resources.Register("bulma.css", StylesheetResource(FileResourceLocation("Resources/bulma.min.css")))
            config.Styles.Register<Button>().SetDotvvmProperty(Button.ButtonTagNameProperty, ButtonTagName.button, StyleOverrideOptions.Ignore) |> ignore
            config.Styles.Register<Validator>().SetDotvvmProperty(Validator.ShowErrorMessageTextProperty, true).SetAttribute("class", "is-danger", StyleOverrideOptions.Append) |> ignore
            config.ClientSideValidation <- false
            ()

type Startup() =

    member this.ConfigureServices(services: IServiceCollection) =
        services.AddDotVVM() |> ignore
        ()

    member this.Configure(app: IApplicationBuilder, env: IHostingEnvironment) =
        if env.IsDevelopment() then app.UseDeveloperExceptionPage() |> ignore

        app.UseWebSockets() |> ignore

        app.UseDotVVM<DotvvmStartup>() |> ignore
        app.Run(fun context ->
            if context.WebSockets.IsWebSocketRequest then
                task {
                    let! ws = context.WebSockets.AcceptWebSocketAsync()
                    do! WebSocketUpdate.handleWS ws context.Request.Cookies.["name"]
                } :> Task
            else if Helpers.isGodMode context then
                if context.Request.Path.Value = "/dump" then
                    let s = FsPickler.CreateJsonSerializer(indent = true)
                    context.Response.WriteAsync(s.PickleToString (StateManager.getState()))
                else if context.Request.Path.Value = "/dump_mine" then
                    let name = context.Request.Query.["name"].ToArray() |> Array.head
                    use str = new StringWriter()

                    let rec printTable getCode =
                        function
                        | [] -> ()
                        | [ num ] ->
                            str.WriteLine(String.Join("\t", [0..(num-1)] |> Seq.map (sprintf "[%d]")))
                            str.WriteLine(String.Join("\t", [0..(num-1)] |> Seq.map (fun i -> getCode [i])))
                        | [ rows; cols ] ->
                            str.WriteLine("\t" + String.Join("\t", [0..(cols-1)] |> Seq.map (sprintf "[%d]")))
                            str.WriteLine()

                            for i in 0..(rows-1) do
                                str.WriteLine((sprintf "[%d]\t" i) + String.Join("\t", [0..(cols-1)] |> Seq.map (fun j -> getCode [i; j])))
                        | num :: coords ->
                            for i in 0..(num-1) do
                                fprintfn str "P %d:\n" i
                                printTable (fun c -> getCode <| i :: c) coords
                                fprintfn str "\n"



                    printTable (fun c -> Hasher.mineEntry name c) ((StateManager.getState().Mines |> Array.find (fun m -> m.Name = name)).Dimensions |> Array.toList)

                    let wordWidth = str.ToString().Split([| '\t'; '\n'|] ) |> Seq.map (fun s -> s.Length) |> Seq.max
                    let lines = str.ToString().Split('\n') |> Seq.map (fun line ->
                        let result = new StringWriter()
                        for w in line.Split('\t') do
                            result.Write w
                            String(' ', wordWidth - w.Length + 2) |> result.Write
                        result.ToString()
                    )

                    context.Response.WriteAsync (String.Join ('\n', lines))


                else Task.CompletedTask
            else
                Task.CompletedTask
            )

        ()
