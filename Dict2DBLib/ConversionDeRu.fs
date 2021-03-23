namespace Dict2DB

open FSharp.Data
open DictionaryTypes
open ConversionFunctions

module ConversionDeRu = 

    type private TeiDict = XmlProvider<"res/deu-rus.tei">

    let convertEntryTranslation (teiEntry: TeiDict.Entry) =
        { 
            Keyword = teiEntry.Form.Orth
            Pronunciations = teiEntry.Form.Prons |> Array.toList
            Articles = teiEntry.Senses 
                        |> Array.filter (fun sense -> sense.Cit.Type = "trans") 
                        |> Array.collect (fun sense -> sense.Cit.Quotes)
                        |> Array.map (fun q -> 
                                            { 
                                                Text = q 
                                                PartOfSpeech = if teiEntry.GramGrp.IsSome then posFromString teiEntry.GramGrp.Value.Pos else None
                                                Synonyms = None
                                                Examples = None
                                            })
                        |> Array.toList
        }

    let convertEntryDefinition (teiEntry: TeiDict.Entry) =
        { 
            Keyword = teiEntry.Form.Orth
            Pronunciations = teiEntry.Form.Prons |> Array.toList
            Articles = teiEntry.Senses 
                        |> Array.filter (fun sense -> sense.Senses.Length > 0)
                        |> Array.collect (fun sense -> sense.Senses)
                        |> Array.map (fun sense -> 
                                            { 
                                                Text = sense.Def
                                                PartOfSpeech = if teiEntry.GramGrp.IsSome then posFromString teiEntry.GramGrp.Value.Pos else None
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

    let convertTeiDictionary convertEntry getTargetLanguage (teiDictionary: TeiDict.Tei) =
        let fromLang = teiDictionary.Text.Body.Lang
        let toLang = getTargetLanguage teiDictionary
        {
            Author = Some teiDictionary.TeiHeader.FileDesc.PublicationStmt.Publisher
            Title = teiDictionary.TeiHeader.FileDesc.TitleStmt.Title
            FromLanguage = fromLang
            ToLanguage = toLang
            Entries = convertTeiEntries convertEntry teiDictionary.Text.Body.Entries |> Array.toList
        }

    // TODO: invent more robust getTargetLanguage function
    let convertTeiTranslationDictionary = 
        convertTeiDictionary convertEntryTranslation (fun teiDict -> teiDict.Text.Body.Entries.[0].Senses.[0].Cit.Lang)

    let convertTeiDefinitionDictionary =
        convertTeiDictionary convertEntryDefinition (fun teiDict -> teiDict.Text.Body.Lang)

    let convertTeiXmlToTranslationDictionary = 
        TeiDict.Parse 
        >> convertTeiTranslationDictionary

    let convertTeiXmlToDefinitionDictionary = 
        TeiDict.Parse 
        >> convertTeiDefinitionDictionary
