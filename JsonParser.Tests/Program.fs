open JsonParser.Adapter


let text = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\text.json")

let t = JsonAdapter(text)


printfn "%A" t
