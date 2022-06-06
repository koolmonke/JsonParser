module JsonParser.Adapter

open JsonParser
open FParsec
open CommonTypes

type JsonAdapter(jsonObject: Result<JsonValue, string>) =
    member _.JsonObject = jsonObject

    new(jsonRawString) =
        JsonAdapter(
            match run Parser.jsonValue jsonRawString with
            | Success (value, _, _) -> Core.Ok value
            | Failure (err, _, _) -> Core.Error err
        )

    member this.Item
        with get (index: JsonIndex) =
            match (this.JsonObject, index) with
            | Core.Ok (JsonList lst), ArrayIndex idx when lst.Length > idx -> JsonAdapter(Core.Ok lst[idx])
            | Core.Ok (JsonList _), ArrayIndex _ -> JsonAdapter(Core.Error "Index out of bounds")
            | Core.Ok (JsonObject o), ObjectIndex idx when o.ContainsKey(idx) -> JsonAdapter(Core.Ok o[idx])
            | Core.Ok (JsonObject _), ObjectIndex _ -> JsonAdapter(Core.Error "Key in object not found")
            | Core.Error msg, _ -> JsonAdapter(Core.Error msg)
            | _ -> JsonAdapter(Core.Error "Index was missmatched")

    member this.Item
        with get (index) = this[ArrayIndex index]

    member this.Item
        with get (index) = this[ObjectIndex index]

    override this.ToString() =
        sprintf "JsonAdapter(%A)" this.JsonObject
