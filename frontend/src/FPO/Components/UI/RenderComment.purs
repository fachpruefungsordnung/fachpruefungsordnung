module FPO.Components.UI.RenderComment
  ( renderComment
  , renderFirstComment
  ) where

import Prelude

import Data.Formatter.DateTime (Formatter, format)
import Data.Maybe (Maybe, maybe)
import FPO.Translations.Labels (Labels)
import FPO.Types (Comment, FirstComment)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (Translator, label, translate)

renderFirstComment
  :: forall action slots m
   . Translator Labels
  -> Maybe Formatter
  -> Boolean
  -> action
  -> FirstComment
  -> H.ComponentHTML action slots m
renderFirstComment translator mTimeFormatter clickable clickAction firstComment =
  HH.div
    ( [ HP.classes
          [ HB.p2
          , HB.mb2
          , HB.border
          , HB.rounded
          , HB.shadowSm
          , HB.dFlex
          , HB.flexColumn
          ]
      , HP.style
          ( "background-color:"
              <>
                ( if firstComment.resolved then "rgba(66, 250, 0, 0.9)"
                  else if firstComment.hasProblem then "rgb(253, 126, 20)"
                  else "rgba(246, 250, 0, 0.9)"
                )
              <> ";"
          )
      ]
        <>
          if clickable then [ HE.onClick \_ -> clickAction ]
          else []
    )
    [ HH.div_
        [ HH.div
            [ HP.classes [ HB.dFlex, HB.alignItemsCenter ]
            , HP.style "font-weight: 500; font-size: 1rem;"
            ]
            ( [ HH.span_ [ HH.text firstComment.comment.author ] ]
                <>
                  if firstComment.resolved then
                    [ HH.i
                        [ HP.classes
                            [ HB.bi
                            , H.ClassName "bi-check-circle-fill"
                            , HB.msAuto
                            , H.ClassName "fs-4"
                            ]
                        ]
                        []
                    ]
                  else if firstComment.hasProblem then
                    [ HH.i
                        [ HP.classes
                            [ HB.bi
                            , H.ClassName "bi-exclamation-circle-fill"
                            , HB.msAuto
                            , H.ClassName "fs-4"
                            ]
                        ]
                        []
                    ]
                  else []
            )
        , HH.div
            [ HP.classes [ HB.mt1 ]
            , HP.style "font-size: 1rem;"
            ]
            [ HH.text firstComment.comment.content ]
        ]
    , HH.div
        [ HP.classes [ HB.mt2 ]
        , HP.style "align-self: flex-end; font-size: 0.75rem; color: #555;"
        ]
        [ HH.text $ maybe
            (translate (label :: _ "comment_no_timestamp") translator)
            (\formatter -> format formatter firstComment.comment.timestamp)
            mTimeFormatter
        ]
    ]

renderComment
  :: Translator Labels
  -> Maybe Formatter
  -> Comment
  -> forall action slots m
   . H.ComponentHTML action slots m
renderComment translator mTimeFormatter c =
  HH.div
    [ HP.classes
        [ HB.p1
        , HB.mb1
        , HB.mx2
        , HB.border
        , HB.rounded
        , HB.shadowSm
        , HB.dFlex
        , HB.flexColumn
        ]
    , HP.style "background-color: #fff9c4;"
    ]
    [ HH.div_
        [ HH.div
            [ HP.style "font-weight: 500; font-size: 1rem;" ]
            [ HH.text c.author ]
        , HH.div
            [ HP.classes [ HB.mt1 ]
            , HP.style "font-size: 0.875rem;"
            ]
            [ HH.text c.content ]
        ]
    , HH.div
        [ HP.classes [ HB.mt2 ]
        , HP.style "align-self: flex-end; font-size: 0.75rem; color: #555;"
        ]
        [ HH.text $ maybe
            (translate (label :: _ "comment_no_timestamp") translator)
            (\formatter -> format formatter c.timestamp)
            mTimeFormatter
        ]
    ]