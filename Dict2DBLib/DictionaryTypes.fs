module DictionaryTypes

open System
open Newtonsoft.Json
open MongoDB.Bson.Serialization

type Dictionary = 
    {
        FromLanguage: string
        ToLanguage: string
        Author: string option
        Title: string
        Entries: Entry list
    }

and Entry =
    {
        Keyword: string
        Pronunciations: string list
        Articles: Article list
    }

and Article =
    {
        Text: string
        PartOfSpeech: PartOfSpeech option
        Synonyms: string list option
        Examples: string list option
    }
    
and PartOfSpeech = 
    | Noun
    | Pronoun
    | Verb
    | Adjective
    | Adverb
    | Preposition
    | Conjunction
    | Interjunction
    | Other

type PartOfSpeechToBsonConverter() = 
    interface IBsonSerializer with

        member this.Serialize (context, _, value) =
            let optPos: PartOfSpeech option = downcast value
            match optPos with
            | Some pos -> context.Writer.WriteString <| pos.ToString()
            | None -> context.Writer.WriteString <| null

        member this.Deserialize (context, _) = 
            let stringValue = context.Reader.ReadString()
            let posOptResult = 
                match stringValue with
                | "Noun"          -> Some Noun
                | "Pronoun"       -> Some Pronoun
                | "Verb"          -> Some Verb
                | "Adjective"     -> Some Adjective
                | "Adverb"        -> Some Adverb
                | "Preposition"   -> Some Preposition
                | "Conjunction"   -> Some Conjunction
                | "Interjunction" -> Some Interjunction
                | "Other"         -> Some Other
                | _               -> None
            upcast posOptResult

        member this.ValueType = typeof<PartOfSpeech option>


type PartOfSpeechToStringJsonConverter() =
    inherit JsonConverter()

    override this.CanConvert objectType = objectType = typeof<PartOfSpeech> ;
    
    override this.WriteJson (writer: JsonWriter, value: obj, serializer: JsonSerializer): unit = 
        writer.WriteValue(value.ToString())

    override this.CanRead = false

    override this.ReadJson (reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) : obj =
        raise <| NotImplementedException();
