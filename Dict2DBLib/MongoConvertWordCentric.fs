namespace Dict2DB

open DictionaryTypes
open MongoDB.Driver
open MongoDB.Bson.Serialization.Attributes
open Dict2DB.Conversion
open MongoDB.Bson

module MongoConvertWordCentric =

    let [<Literal>]dictionariesCollectionName = "dictionaries"

    type DictionaryEntry =
        {
            [<BsonId>] Id: ObjectId
            Title: string
            Author: string
            FromLanguage: string
            ToLanguage: string
        }
    
    type DBEntry =
        {
            CollectionName: string
            Words: Word list
        }
    and Word =
        {
            [<BsonId>] 
            TheWord: string
            Pronunciations: PronunciationEntry seq
            Articles: ArticleEntry seq
        }
    and PronunciationEntry =
        {
            Pronunciation: string
            DictionaryId: ObjectId
        }
    and ArticleEntry =
        {
            Article: Article
            DictionaryId: ObjectId
        }

    let updateWord (collection: IMongoCollection<Word>) (word: Word) =
        seq{
            yield 
                collection.UpdateOneAsync(
                    Builders<Word>.Filter.Eq((fun e -> e.TheWord), word.TheWord),
                    Builders<Word>.Update.PushEach((fun e -> e.Articles), word.Articles),
                    UpdateOptions(IsUpsert = true))
                |> Async.AwaitTask
            yield
                collection.UpdateOneAsync(
                    Builders<Word>.Filter.Eq((fun e -> e.TheWord), word.TheWord),
                    Builders<Word>.Update.PushEach((fun e -> e.Pronunciations), word.Pronunciations),
                    UpdateOptions(IsUpsert = true))
                |> Async.AwaitTask
        }
    
    let addToDatabase (database: IMongoDatabase) (dbEntry: DBEntry) =
        let collection = database.GetCollection(dbEntry.CollectionName)
        dbEntry.Words
        |> Seq.collect (fun word -> updateWord collection word)


    let dictionaryToDBEntry (dictionary: Dictionary, id: ObjectId) =
        {
            CollectionName = $"{dictionary.FromLanguage}-{dictionary.ToLanguage}"
            Words = 
                dictionary.Entries 
                |> List.map (fun e -> {
                    TheWord = e.Keyword.ToLowerInvariant() 
                    Articles = e.Articles 
                                |> List.map (fun a -> 
                                                { 
                                                    Article = a
                                                    DictionaryId = id
                                                })
                    Pronunciations = e.Pronunciations 
                                        |> List.map (fun p -> 
                                                        {
                                                            Pronunciation = p
                                                            DictionaryId = id
                                                        })
                })
        }

    let addDictionaryInfoToDatabase (database: IMongoDatabase) (dictionary: Dictionary) =
        let newId = ObjectId.GenerateNewId(System.DateTime.Now)
        async{
            database
                .GetCollection<DictionaryEntry>(dictionariesCollectionName)
                .InsertOneAsync({
                    Id = newId
                    Title = dictionary.Title
                    Author = 
                        match dictionary.Author with 
                        | Some author -> author
                        | None -> System.String.Empty
                    FromLanguage = dictionary.FromLanguage
                    ToLanguage = dictionary.ToLanguage
                })
            |> Async.AwaitTask
            |> Async.Start

            return newId
        }
        

    let addToDatabaseFromTeiXml convertFunction (database: IMongoDatabase) (dictionaryXml: string) = 
        let dictionary = dictionaryXml |> convertFunction
        async {
            let! newId = 
                dictionary
                |> addDictionaryInfoToDatabase database

            return
                dictionary
                |> fun d -> dictionaryToDBEntry (d, newId)
                |> addToDatabase database
        }
        |> Async.RunSynchronously

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

