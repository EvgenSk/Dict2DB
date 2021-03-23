namespace Dict2DB

open DictionaryTypes

module ConversionFunctions =
    let posFromString s =
        match s with
        | Some "n" -> Some PartOfSpeech.Noun
        | Some "v" -> Some PartOfSpeech.Verb
        | Some "adj" -> Some PartOfSpeech.Adjective
        | Some "adv" -> Some PartOfSpeech.Adverb
        | Some "preposition" -> Some PartOfSpeech.Preposition
        | Some "conjunction" -> Some PartOfSpeech.Conjunction
        | Some "interjunction" -> Some PartOfSpeech.Interjunction
        | Some _ -> Some PartOfSpeech.Other
        | None -> None

    let fuseEntries (entries: Entry[]) =
        match entries with
        | [||] -> None
        | [|e|] -> Some e
        | _ -> Some
                {
                    Keyword = entries.[0].Keyword
                    Pronunciations = entries 
                                        |> Array.toList
                                        |> List.collect (fun e -> e.Pronunciations)
                                        |> List.distinct
                    Articles = entries 
                                |> Array.toList
                                |> List.collect (fun e -> e.Articles) 
                }
