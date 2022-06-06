module JsonParser.CommonTypes

type JsonValue =
    | JsonNull
    | JsonBool of bool
    | JsonFloat of float
    | JsonString of string
    | JsonList of JsonValue list
    | JsonObject of Map<string, JsonValue>

type JsonIndex =
    | ArrayIndex of int
    | ObjectIndex of string
