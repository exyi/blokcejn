namespace Views.Login
open System.Linq.Expressions
open DotVVM.Framework.Hosting
open System.Threading.Tasks
open DotVVM.Framework.ViewModel
open DotVVM.Framework.ViewModel.Validation
open Helpers
open Model
open Views.Dashboard

type ViewModel() =
    inherit DotvvmViewModelBase()

    override this.Load() =
        this.IsLogged <- Helpers.authCheck (this.Context.GetAspNetCoreContext ()) |> Option.isSome

        Task.CompletedTask

    member val IsLogged = false with get, set
    member val Name = "" with get, set
    member val Password = "" with get, set

    member x.Login () =
        // let c = x.Context.HttpContext
        let c = x.Context.GetAspNetCoreContext()
        if Helpers.login x.Name x.Password c then
            x.Context.RedirectToRoute "board"
        else
            x.AddModelError(Expr.Quote(fun (x : ViewModel) -> x.Name), "Neplatny login.") |> ignore
            x.Context.FailOnInvalidModelState()
