open JsonParser.Adapter
open JsonParser


let text = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"\text.json")

let (Ok tree1) = JsonAdapter(text).JsonObject

let strFromT1 = Serializer.serialize (tree1)

let (Ok tree2) = JsonAdapter(strFromT1).JsonObject

printfn "%A" (tree1 = tree2)