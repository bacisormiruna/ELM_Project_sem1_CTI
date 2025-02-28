module View.Posts exposing (..)

import Html exposing (Html, button, div, text, select, option, text, label, input)
import Html.Attributes exposing (href, id, value, attribute, type_, checked, selected)
import Html.Events exposing (onClick, onInput, onCheck)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}

postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config time posts =
    Html.table [ Html.Attributes.class "posts-table" ]
        [ Html.thead []
            [ Html.tr []
                (List.map (\header -> Html.th [] [ text header ]) 
                    [ "Score", "Title", "Type", "Posted Date", "Link" ]
                )
            ]
        , Html.tbody []
            (List.map (postRow time) (filterPosts config posts)) --popularea tabelului cu randuri si filtrarea configuratiei
        ]

postRow : Time.Posix -> Post -> Html Msg --populam tabelul cu randuri
postRow time post =
    let
        duration = Util.Time.durationBetween post.time time --cate zile ore secunde etc exista intre timpul curent de acum si timpul generat de utc

        format =
            case duration of
                Just durata ->
                    " (" ++ Util.Time.formatDuration durata ++ ")"
                Nothing -> " (Invalid time)"
    in
    Html.tr [] 
        [ Html.td [ Html.Attributes.class "post-score" ]  --desi poate este redundanta e mai usor de citit, de inteles si de extins
            [ text (String.fromInt post.score) ]
        , Html.td [ Html.Attributes.class "post-title" ] 
            [ text post.title ]
        , Html.td [ Html.Attributes.class "post-type" ] 
            [ text post.type_ ]
        , Html.td [ Html.Attributes.class "post-time" ] 
            [ text (Util.Time.formatTime Time.utc post.time)
            , text format
            ]
        , Html.td [ Html.Attributes.class "post-url" ] 
            [ Html.a [ Html.Attributes.href (Maybe.withDefault "#" post.url) ] 
                [ text "Link" ] 
            ]
        ]


{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}

postsConfigView : PostsConfig -> Html Msg  --configuration options related to the posts table.
postsConfigView postsConfig =
    div []
        [ div []
            [ text "Select the number of posts to show: " -- cate rezultate sa se afiseze pe pagina
            , select 
                [ id "select-posts-per-page" --aici folosesc expresie lambda
                , onInput (\postsPerPage -> Model.ConfigChanged (ChangePostsPerPage (Maybe.withDefault 10 (String.toInt postsPerPage)))) --valoarea default care va fi afisata la rularea aplicatiei
                ]
                [ option [ value "10", selected (postsConfig.postsToShow == 10) ] [ text "10" ] --select pentru ce s-a selectat ca sa se faca update instant
                , option [ value "25", selected (postsConfig.postsToShow == 25) ] [ text "25" ]
                , option [ value "50", selected (postsConfig.postsToShow == 50) ] [ text "50" ]
                ]
            ]
        , div []
            [ text "Sort by: " --am folosit pentru select onInput si pentru id function composition ca trec de la Change la Msg
            , select [ id "select-sort-by"
            , onInput (Model.ConfigChanged << ChangeSortBy << Maybe.withDefault None << sortFromString) ]
                [ option [ value "Score", selected (postsConfig.sortBy == Score)] [ text "Score" ]
                , option [ value "Title", selected (postsConfig.sortBy == Title) ] [ text "Title" ]-- in plus folosesc selected pentru update
                , option [ value "Posted", selected (postsConfig.sortBy == Posted) ] [ text "Posted" ]
                , option [ value "None", selected (postsConfig.sortBy == None) ] [ text "None" ]
                ]
            ]
        , div []
            [ text "Show job posts: " --pentru input folosesc onCheck sa verific daca s-a bifat sau nu
            , input [ type_ "checkbox"
                    , id "checkbox-show-job-posts"
                    , checked postsConfig.showJobs, onCheck (Model.ConfigChanged << ChangeShowJobPosts) ]       
                    []
            ]
        , div []
            [ text "Show text-only posts: "--la fel ca si la job posts
            , input [ type_ "checkbox", id "checkbox-show-text-only-posts", checked postsConfig.showTextOnly, onCheck (Model.ConfigChanged << ChangeShowTextOnlyPosts) ] []
            ]
        ]
