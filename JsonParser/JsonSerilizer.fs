module JsonParser.Serializer

open JsonParser.CommonTypes
open FSharp.Core.Operators


let rec serialize (tree: JsonValue) =
    let concatRecords = String.concat ","

    match tree with
    | JsonNull -> "null"
    | JsonBool true -> "true"
    | JsonBool false -> "false"
    | JsonFloat fl -> string fl
    | JsonString str ->
        "\""
        + (String.collect
            (function
            | '\n' -> "\\n"
            | '\t' -> "\\t"
            | '\b' -> "\\b"
            | '\r' -> "\\r"
            | '\012' -> "\\f"
            | c -> string c)
            str)
        + "\""
    | JsonList lst ->
        "["
        + (lst |> Seq.map serialize |> concatRecords)
        + "]"
    | JsonObject o ->
        let stringedObjectContents =
            o
            |> Seq.map (fun (KeyValue (k, v)) -> $"\"{k}\": {serialize v}")
            |> concatRecords

        $"{{{stringedObjectContents}}}"
