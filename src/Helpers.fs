namespace Helpers
open System.Linq.Expressions
open Newtonsoft.Json

type HackDefaultJsonConverter() =
    inherit JsonConverter()

    override x.CanRead with get () = true
    override x.CanWrite with get () = true

    override x.CanConvert(t) = true
    override x.ReadJson(r, t, o, s) =
        let ss = JsonSerializer()
        ss.Deserialize(r, t)
    override x.WriteJson(w, v, s) =
        let ss = JsonSerializer()
        ss.Serialize(w, v)


type Expr =
  static member Quote<'a, 'b>(e:Expression<System.Func<'a, 'b>>) = e

module Hasher =
    open System.Globalization
    open System
    open System.Text
    open System.Security.Cryptography

    let korpus1 = lazy (IO.File.ReadAllLines("korpus1"))
    let korpus2 = lazy (IO.File.ReadAllLines("korpus2"))
    let korpus_hesel = lazy (IO.File.ReadAllLines("korpus_hesel"))

    let stripString (str: string) = (str.Normalize(NormalizationForm.FormD) |> Seq.filter (fun c -> Char.GetUnicodeCategory c <> UnicodeCategory.NonSpacingMark) |> Seq.toArray |> String).Normalize(NormalizationForm.FormC)

    let mineEntry mineName (indices: int list) =
        use sha = SHA1.Create()
        let data = sha.ComputeHash(Encoding.UTF8.GetBytes(sprintf "%s|%A" mineName indices))
        let number = BitConverter.ToUInt64 (data, 0)

        match mineName with
        | "internet" | "autobus" ->
            korpus_hesel.Value.[abs (int number) % korpus_hesel.Value.Length]
        | "zvonicka" | "les" | "kiruna" ->
            korpus1.Value.[abs (int number) % korpus1.Value.Length] |> stripString
        | "hasici" | "ali" ->
            korpus2.Value.[abs (int number) % korpus2.Value.Length] |> stripString
        | "drevnik" ->
            [| "hroch"; "zuzka"; "dalibor"; "lucka"; "tomas"; "lucka"; "janek"; "petr"; "vojta"; "jirka"; "janek"; "marek"; "risa"; "honza"; "majda"; "peta"; "jirka"; "terka"; "standa"; "ondra"; "honza"; "adam"; "honza"; "karel"; "ondra"; "honza"; "sejsel"; "dominik"; "anicka"; "david"; "filip"; "dan"; "maruska"; "jenda"; "petr" |].[indices |> Seq.head]
        | _ ->
            let len = if mineName = "pole" then 6 else 3
            Seq.unfold (fun (n: uint64) -> Some (('A' + char (n % uint64 26)), (n / uint64 26))) number |> Seq.take len |> Seq.toArray |> String

    let getCoords mineName teamName (indices: int list) index =
        use sha = SHA1.Create()
        let data = sha.ComputeHash(Encoding.UTF8.GetBytes(sprintf "%s|%s|%A|%d" mineName teamName indices index))
        let number = BitConverter.ToUInt64 (data, 0)


        indices |> Seq.mapFold (fun state item -> (number % uint64 item),(number / uint64 item)) number |> fst |> Seq.map int |> Seq.toArray
