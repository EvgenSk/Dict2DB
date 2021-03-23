namespace Dict2DB

open FSharp.Data
open DictionaryTypes
open ConversionFunctions
open System

module ConversionEnRu = 

    type private TeiDict = XmlProvider<"res/eng-rus.tei">

    let convertEntryTranslation (teiEntry: TeiDict.Entry) =
        { 
            Keyword = 
                match teiEntry.Form.Orth.String with
                | Some orth -> orth
                | None -> String.Empty
            // Pronunciations = teiEntry.Form.Pron |> Option.map (fun x -> [x]) 
            Pronunciations = 
                match teiEntry.Form.Pron with
                | Some pron -> [pron]
                | None -> List.Empty
            Articles = teiEntry.Senses
                        |> Array.collect (fun sense -> sense.Cits)
                        |> Array.filter (fun cit -> cit.Type = "trans") 
                        |> Array.map (fun cit -> 
                                            { 
                                                Text = cit.Quote 
                                                PartOfSpeech = None
                                                Synonyms = None
                                                Examples = None
                                            })
                        |> Array.toList
        }


    let convertTeiEntries convertEntry (teiEntries: TeiDict.Entry []) =
        teiEntries
        |> Array.map convertEntry
        |> Array.groupBy (fun entry -> entry.Keyword)
        |> Array.map (fun (_, group) -> group |> fuseEntries)
        |> Array.choose id

    let convertTeiDictionary convertEntry (teiDictionary: TeiDict.Tei) =
        let fromLang = teiDictionary.Text.Body.Lang
        let toLang = teiDictionary.Text.Body.Entries.[0].Senses.[0].Cits.[0].Lang
        {
            Author = Some teiDictionary.TeiHeader.FileDesc.PublicationStmt.Publisher
            Title = teiDictionary.TeiHeader.FileDesc.TitleStmt.Title
            FromLanguage = fromLang
            ToLanguage = toLang
            Entries = convertTeiEntries convertEntry teiDictionary.Text.Body.Entries |> Array.toList
        }

    let convertTeiTranslationDictionary = 
        convertTeiDictionary convertEntryTranslation

    let convertTeiXmlToTranslationDictionary = 
        TeiDict.Parse
        >> convertTeiTranslationDictionary
