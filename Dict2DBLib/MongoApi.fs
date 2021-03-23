namespace Dict2DB

open MongoConvert

module MongoApi =

    let AddToDatabaseFromTeiXml (database, dictionaryXml) =
        if isNull database then nullArg <| nameof database
        if isNull dictionaryXml then nullArg <| nameof dictionaryXml
        
        addToDatabaseFromTeiXml database dictionaryXml
        |> Async.Parallel
        |> Async.StartAsTask