namespace Dict2DB

open DictionaryTypes
open MongoDB.Driver
open MongoDB.Bson.Serialization.Attributes
open Dict2DB.Conversion

module MongoConvert =

    type DBEntry =
        {
            CollectionName: string
            Words: Word list
        }
    and Word =
        {
            [<BsonId>] 
            TheWord: string
            Dictionaries: DictEntry seq
        }
    and DictEntry =
        {
            [<BsonId>]
            DictionaryName: string
            Pronunciations: string list
            Articles: Article list
        }

    let addToDatabase (database: IMongoDatabase) (dbEntry: DBEntry) =
        let collection = database.GetCollection(dbEntry.CollectionName)
        dbEntry.Words
        |> List.map (fun word -> 
            collection.UpdateOneAsync(
                Builders<Word>.Filter.Eq((fun e -> e.TheWord), word.TheWord),
                Builders<Word>.Update.PushEach((fun e -> e.Dictionaries), word.Dictionaries),
                UpdateOptions(IsUpsert = true))
                |> Async.AwaitTask)

    let dictionaryToDBEntry (dictionary: Dictionary) =
        let dictionaryName = 
            match (dictionary.Author, dictionary.Title) with
            | (Some author, title) -> $"{author}-{title}"
            | (None, title) -> title
        {
            CollectionName = $"{dictionary.FromLanguage}-{dictionary.ToLanguage}"
            Words = 
                dictionary.Entries 
                |> List.map (fun e -> {
                    TheWord = e.Keyword.ToLowerInvariant() 
                    Dictionaries = [{
                        DictionaryName = dictionaryName
                        Pronunciations = e.Pronunciations
                        Articles = e.Articles
                    }]
                })
        }

    let addToDatabaseFromTeiXml (database: IMongoDatabase) (dictionaryXml: string) = 
        dictionaryXml
        |> convertTeiXmlToTranslationDictionary
        |> dictionaryToDBEntry
        |> addToDatabase database

    let getWord (db: IMongoDatabase) (langFrom: string) (langTo: string) (word: string) =
        let collectionName = $"{langFrom}-{langTo}"
        let filter = Builders<Word>.Filter.Eq((fun f -> f.TheWord), word.ToLower())
        async {
            let! cursor = 
                db.GetCollection(collectionName).FindAsync<Word>(filter) //XXX: it cannot to deserialize F# collections
                |> Async.AwaitTask

            let! list =
                cursor.ToListAsync()
                |> Async.AwaitTask

            return list.[0]
        }

