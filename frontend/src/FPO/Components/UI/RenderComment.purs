module FPO.Components.UI.RenderComment
  ( renderComment
  , renderFirstComment
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Formatter.DateTime (Formatter, format)
import Data.Maybe (Maybe(..), maybe)
import Data.String (take, toUpper)
import FPO.Data.Time (formatRelativeTime)
import FPO.Translations.Labels (Labels)
import FPO.Types (Comment, FirstComment)
import FPO.UI.Css as HB
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.I18n.Translator (Translator, label, translate)

-- | Renders a "first comment" (the root of a comment thread) as a modern card
-- | with avatar, status accent border, and optional status icon.
renderFirstComment
  :: forall action slots m
   . Translator Labels
  -> Maybe Formatter
  -> Maybe DateTime
  -> Boolean
  -> Boolean
  -> action
  -> FirstComment
  -> H.ComponentHTML action slots m
renderFirstComment
  translator
  mTimeFormatter
  mCurrentTime
  inLatest
  clickable
  clickAction
  firstComment =
  HH.div
    ( [ HP.classes $
          [ H.ClassName "fpo-comment" ]
            <> if clickable then [ H.ClassName "fpo-comment--clickable" ] else []
      ]
        <>
          ( if clickable then
              [ HE.onClick \_ -> clickAction ]
            else
              [ HP.style
                  ( "border-left-color:"
                      <>
                        ( case
                            firstComment.resolved,
                            firstComment.hasProblem,
                            inLatest
                            of
                            true, _, _ -> "var(--comment-accent-resolved);"
                            _, true, true -> "var(--comment-accent-problem);"
                            _, true, false -> "var(--comment-accent-outdated);"
                            _, _, _ -> "var(--comment-accent-default);"
                        )
                  )
              ]
          )
    )
    [ -- Header row: avatar + author + time + status icon
      HH.div [ HP.classes [ H.ClassName "fpo-comment__header" ] ]
        [ -- Avatar
          HH.div [ HP.classes [ H.ClassName "fpo-comment__avatar" ] ]
            [ HH.text $ toUpper $ take 1 firstComment.comment.author ]
        , -- Author + time
          HH.div [ HP.classes [ H.ClassName "fpo-comment__meta" ] ]
            [ HH.span [ HP.classes [ H.ClassName "fpo-comment__author" ] ]
                [ HH.text firstComment.comment.author ]
            , HH.span [ HP.classes [ H.ClassName "fpo-comment__time" ] ]
                [ HH.text $ formatCommentTime translator mTimeFormatter mCurrentTime
                    firstComment.comment.timestamp
                ]
            ]
        , -- Status icon (top-right)
          case firstComment.resolved, firstComment.hasProblem, inLatest of
            true, _, _ ->
              HH.div
                [ HP.classes
                    [ H.ClassName "fpo-comment__status"
                    , H.ClassName "fpo-comment__status--resolved"
                    ]
                ]
                [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-check-circle-fill" ] ] []
                ]
            _, true, true ->
              HH.div
                [ HP.classes
                    [ H.ClassName "fpo-comment__status"
                    , H.ClassName "fpo-comment__status--problem"
                    ]
                ]
                [ HH.i
                    [ HP.classes [ HB.bi, H.ClassName "bi-exclamation-circle-fill" ] ]
                    []
                ]
            _, true, false ->
              HH.div
                [ HP.classes
                    [ H.ClassName "fpo-comment__status"
                    , H.ClassName "fpo-comment__status--outdated"
                    ]
                ]
                [ HH.i [ HP.classes [ HB.bi, H.ClassName "bi-clock-history" ] ] [] ]
            _, _, _ ->
              HH.text ""
        ]
    , -- Content
      HH.div [ HP.classes [ H.ClassName "fpo-comment__body" ] ]
        [ HH.text firstComment.comment.content ]
    ]

-- | Renders a reply comment (child in a thread).
renderComment
  :: Translator Labels
  -> Maybe Formatter
  -> Maybe DateTime
  -> Comment
  -> forall action slots m
   . H.ComponentHTML action slots m
renderComment translator mTimeFormatter mCurrentTime c =
  HH.div
    [ HP.classes [ H.ClassName "fpo-comment", H.ClassName "fpo-comment--reply" ] ]
    [ -- Header row: avatar + author + time
      HH.div [ HP.classes [ H.ClassName "fpo-comment__header" ] ]
        [ HH.div
            [ HP.classes
                [ H.ClassName "fpo-comment__avatar"
                , H.ClassName "fpo-comment__avatar--sm"
                ]
            ]
            [ HH.text $ toUpper $ take 1 c.author ]
        , HH.div [ HP.classes [ H.ClassName "fpo-comment__meta" ] ]
            [ HH.span [ HP.classes [ H.ClassName "fpo-comment__author" ] ]
                [ HH.text c.author ]
            , HH.span [ HP.classes [ H.ClassName "fpo-comment__time" ] ]
                [ HH.text $ formatCommentTime translator mTimeFormatter mCurrentTime
                    c.timestamp
                ]
            ]
        ]
    , -- Content
      HH.div [ HP.classes [ H.ClassName "fpo-comment__body" ] ]
        [ HH.text c.content ]
    ]

-- | Formats a comment timestamp: uses relative time if current time is available,
-- | otherwise falls back to the formatter, otherwise shows a "no timestamp" message.
formatCommentTime
  :: Translator Labels
  -> Maybe Formatter
  -> Maybe DateTime
  -> DateTime
  -> String
formatCommentTime translator mTimeFormatter mCurrentTime timestamp =
  case mCurrentTime of
    Just _ -> formatRelativeTime mCurrentTime timestamp
    Nothing -> maybe
      (translate (label :: _ "comment_no_timestamp") translator)
      (\formatter -> format formatter timestamp)
      mTimeFormatter
