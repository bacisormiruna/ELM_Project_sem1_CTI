module Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), applyChanges, defaultConfig, filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Html.Attributes exposing (scope)
import Model.Post exposing (Post)
import Time
import List exposing (take, sort)


type SortBy
    = Score
    | Title
    | Posted
    | None


sortOptions : List SortBy
sortOptions =
    [ Score, Title, Posted, None ]


sortToString : SortBy -> String
sortToString sort =
    case sort of
        Score ->
            "Score"

        Title ->
            "Title"

        Posted ->
            "Posted"   

        None ->
            "None"

{-|

    sortFromString "Score" --> Just Score

    sortFromString "Invalid" --> Nothing

    sortFromString "Title" --> Just Title

-}
sortFromString : String -> Maybe SortBy   --dupa ce criteriu se sorteaza
sortFromString sort =
    case sort of
        "Score" -> Just Score
        "Title" -> Just Title
        "Date" -> Just Posted
        "Unsorted" -> Just None
        _ -> Nothing


sortToCompareFn : SortBy -> (Post -> Post -> Order)
sortToCompareFn sort =
    case sort of
        Score ->
            \postA postB -> compare postB.score postA.score

        Title ->
            \postA postB -> compare postA.title postB.title

        Posted ->
            \postA postB -> compare (Time.posixToMillis postB.time) (Time.posixToMillis postA.time)

        None ->
            \_ _ -> EQ


type alias PostsConfig =
    { postsToFetch : Int
    , postsToShow : Int
    , sortBy : SortBy
    , showJobs : Bool
    , showTextOnly : Bool
    }


defaultConfig : PostsConfig
defaultConfig =
    PostsConfig 50 10 None False True


{-| A type that describes what option changed and how
-}
type Change
    = ConfigChanged PostsConfig --s-a facut o modificare in structura
    | ChangePostsPerPage Int --cate rezultate afisez pe pagina
    | ChangeSortBy SortBy --dupa ce criteriu se sorteaza
    | ChangeShowJobPosts Bool --pentru bifarea optiunii de show job posts
    | ChangeShowTextOnlyPosts Bool --pentru bifarea
    


{-| Given a change and the current configuration, return a new configuration with the changes applied
-}
applyChanges : Change -> PostsConfig -> PostsConfig --aplicarea schimbarilor in pagina in functie de ce se schimba ca si optiuni
applyChanges changes config =
    case changes of --am folosit aceasta structura pentru ca in momentul in care am modificari sa se faca astfel incat sa nu afecteze structura care nu se modifica
        ChangePostsPerPage count ->
            { config | postsToShow = count }

        ChangeSortBy sort ->
            { config | sortBy = sort} 

        ChangeShowJobPosts showJobs ->
            { config | showJobs = showJobs }

        ChangeShowTextOnlyPosts showTextOnly ->
            { config | showTextOnly = showTextOnly }
            
        ConfigChanged newConfig -> --noua configurare cu schimbarile structurii
            newConfig


{-| Given the configuration and a list of posts, return the relevant subset of posts according to the configuration

Relevant local functions:

  - sortToCompareFn

Relevant library functions:

  - List.sortWith

-}
--Scop: Funcția filtrează, sortează și limitează numărul de postări bazându-se pe configurația din config.
filterPosts : PostsConfig -> List Post -> List Post --aici am folosit pipeline pentru a filtra informatiile si a trece pe nivele rezultatele
filterPosts config posts =
    let 
        postsFiltered =
            posts
                |> List.filter (\post -> 
                    if config.showTextOnly then 
                        True 
                    else 
                        case post.url of
                            Just _ -> True      --toate postările sunt păstrate la True si la False păstrează doar postările care au un URL 
                            Nothing -> False) 

                |> List.filter (\post ->
                    if config.showJobs then 
                        True 
                    else post.type_ /= "job")   --Dacă showJobs este True, toate postările sunt păstrate, dacă showJobs este False, elimină postările de tip "job".
                
                |> List.take config.postsToShow --Dacă lista conține mai multe postări decât valoarea postsToShow, taie restul, iar dacă lista conține mai puține, rămâne întreaga listă.
                
                |> List.sortWith (sortToCompareFn config.sortBy) --Dacă config.sortBy = "score", lista se sorteaza dupa scor, etc.
    in
    postsFiltered


