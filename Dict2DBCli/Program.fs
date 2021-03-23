// Learn more about F# at http://fsharp.org

open System
open Argu
open Dict2DB.Conversion
open Dict2DB.MongoConvert
open MongoDB.Driver
open MongoDB.Bson.Serialization
open DictionaryTypes

type DictionaryType =
    | Translation
    | Definition


let stringToDictionaryType (stringType:string) = 
    match (stringType.ToLowerInvariant()) with
    | "translation" -> Translation
    | "trans" -> Translation
    | "tr" -> Translation
    | "definition" -> Definition
    | "def" -> Definition
    | _ -> raise (ArgumentOutOfRangeException(nameof stringType))

type CliArguments =
    | [<Unique>][<AltCommandLine("-c")>] ConnectionString of string
    | [<Unique>][<AltCommandLine("-db")>] DatabaseName of string
    | [<Unique>][<AltCommandLine("-f")>] FilePath of path:string
    | [<Unique>][<AltCommandLine("-t")>] Type of string

    interface IArgParserTemplate with  
        member s.Usage = 
            match s with
            | ConnectionString _ -> $"specify connection string (default: 'mongodb://localhost')"
            | FilePath _ -> "specify path to TEI file"
            | DatabaseName _ -> "specify database name (default: 'dictionary')"
            | Type _ -> "specify dictionary type [ translation | definition ] (default: 'translation')"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>(programName = "Dict2DBCLI");
    try
        let parseResults = parser.Parse(argv)
        let connectionString = parseResults.GetResult(ConnectionString, "mongodb://localhost")
        let databaseName = parseResults.GetResult(DatabaseName, "dictionary")
        let dictionaryType = parseResults.GetResult(Type, "translation") |> stringToDictionaryType
        BsonClassMap.RegisterClassMap<Article>(
            fun cm -> 
                cm.AutoMap()
                cm.GetMemberMap(fun article -> article.PartOfSpeech).SetSerializer(PartOfSpeechToBsonConverter()).SetIgnoreIfNull(true) |> ignore
                cm.GetMemberMap(fun article -> article.Examples).SetIgnoreIfNull(true) |> ignore
                cm.GetMemberMap(fun article -> article.Synonyms).SetIgnoreIfNull(true) |> ignore
                ) 
        |> ignore

        let sequentially x = Async.Parallel (x, 1)
        let convertTeiFunction = 
            match dictionaryType with
            | Translation -> convertTeiXmlToTranslationDictionary
            | Definition -> convertTeiXmlToDefinitionDictionary

        async {
            let! dictXml = 
                parseResults.GetResult FilePath
                |> IO.File.ReadAllTextAsync
                |> Async.AwaitTask
            
            let! updateResults =
                dictXml
                |> convertTeiFunction
                |> dictionaryToDBEntry
                |> addToDatabase (MongoClient(connectionString).GetDatabase(databaseName))
                |> sequentially

            updateResults
            |> Array.filter (fun ur -> not ur.IsAcknowledged)
            |> Array.map (fun ur -> printf "%s was not acknowledged" ur.UpsertedId.AsString)
            |> ignore
        }
        |> Async.RunSynchronously
    with e ->
        printf "%s" e.Message
    0
