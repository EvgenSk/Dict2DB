namespace Dict2DB

module Conversion =
    let tryParse parseFun dict =
        try
            Some <| parseFun dict
        with
        | _ -> None

    let tryParseOneByOne parsers dict =
        parsers
        |> List.map (fun parseFun -> tryParse parseFun dict)
        |> List.choose id
        |> List.head


    let convertTeiXmlToTranslationDictionary =
        [ConversionDeRu.convertTeiXmlToTranslationDictionary; ConversionEnRu.convertTeiXmlToTranslationDictionary]
        |> tryParseOneByOne

    let convertTeiXmlToDefinitionDictionary =
        [ConversionDeRu.convertTeiXmlToDefinitionDictionary]
        |> tryParseOneByOne
        