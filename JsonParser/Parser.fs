module JsonParser.Parser

open FParsec
open CommonTypes

let private stringLiteral =
    let escape =
        anyOf "\"\\/bfnrt"
        |>> function
            | 'b' -> "\b"
            | 'f' -> "\u000C"
            | 'n' -> "\n"
            | 'r' -> "\r"
            | 't' -> "\t"
            | c -> string c

    let unicodeEscape =
        let hex2int c = (int c &&& 15) + (int c >>> 6) * 9

        pstring "u"
        >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3) * 0x1000
            + (hex2int h2) * 0x100
            + (hex2int h1) * 0x10
            + hex2int h0
            |> char
            |> string)

    let escapedCharSnippet = pstring @"\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (pstring "\"") (pstring "\"") (stringsSepBy normalCharSnippet escapedCharSnippet)

let private strWs s = pstring s .>> spaces

let private collection openBracker closeBracket itemParser =
    strWs openBracker
    >>. sepBy (itemParser .>> spaces) (strWs ",")
    .>> strWs closeBracket

let jsonNull: Parser<_, unit> = stringReturn @"null" JsonNull

let jsonBool =
    let jsonTrue = stringReturn @"true" (JsonBool true)
    let jsonFalse = stringReturn @"false" (JsonBool false)
    jsonTrue <|> jsonFalse

let jsonFloat: Parser<_, unit> = pfloat |>> JsonFloat

let jsonString = stringLiteral |>> JsonString

let jsonValue, jsonValueRef = createParserForwardedToRef<JsonValue, _> ()

let jsonList = collection "[" "]" jsonValue |>> JsonList

let jsonObject =
    collection
        "{"
        "}"
        (stringLiteral
         .>>. (spaces >>. pstring ":" >>. spaces >>. jsonValue))
    |>> Map.ofList
    |>> JsonObject

jsonValueRef.Value <-
    jsonObject
    <|> jsonList
    <|> jsonString
    <|> jsonFloat
    <|> jsonBool
    <|> jsonNull
