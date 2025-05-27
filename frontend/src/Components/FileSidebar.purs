module FPO.Components.FileSidebar where

import Prelude

import Data.Array (delete, head, reverse, tail)
import Data.Maybe (Maybe(..))
import Data.String (length, singleton, take, toCodePointArray)
import Data.String.CodePoints (CodePoint)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)
import Web.File.File (File)
import Web.File.File as F
import Web.File.Url (createObjectURL)

data Output = SendPDF (Maybe String)

type Input = Unit

data Query a = LoadPDF File a

data PDFFile = PDFFile { file :: File, url :: String }

instance eqPDFFile :: Eq PDFFile where
  eq (PDFFile a) (PDFFile b) = F.name a.file == F.name b.file

type State =
  { files :: Array PDFFile
  , sidebarOpen :: Boolean
  }

data Action
  = ToggleSidebar
  | FileUploaded (Maybe File)
  | DeleteFile PDFFile
  | RaisePDF (Maybe String)

fileSidebar :: forall m. MonadEffect m => H.Component Query Input Output m
fileSidebar =
  H.mkComponent
    { initialState: \_ -> { files: [], sidebarOpen: false }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.button
          [ HE.onClick \_ -> ToggleSidebar ]
          [ HH.text (if state.sidebarOpen then "⬅️ Schließen" else "📂 Dateien") ]
      -- , HH.input
      --     [ HP.type_ HP.InputFile
      --     , HE.onFileUpload FileUploaded
      --     ]

      , if state.sidebarOpen then
          HH.div
            [ HP.style
                "position: fixed; \
                \top: 100; \
                \left: 0; \
                \width: 500px; \
                \height: 100vh; \
                \background: white; \
                \box-shadow: 2px 0 8px rgba(0,0,0,0.2); \
                \padding: 1rem; \
                \z-index: 1000;"
            ]
            [ HH.h3_ [ HH.text "Hochgeladene Dateien" ]
            , HH.input
                [ HP.type_ HP.InputFile
                , HE.onFileUpload FileUploaded
                , HP.style "margin-bottom: 0.5rem;"
                ]
            , HH.div_
                ( state.files <#> \(PDFFile entry) ->
                    HH.div_
                      [ HH.text ((abrFileName (F.name entry.file)) <> " ")
                      -- Show button
                      , HH.button
                          [ HE.onClick \_ -> RaisePDF (Just entry.url)
                          , HP.title "Anzeigen"
                          , HP.style "margin-left: 0.5rem;"
                          ]
                          [ HH.text "👁️" ]
                      -- Download button
                      , HH.a
                          [ HP.href entry.url
                          , HP.download (F.name entry.file)
                          , HP.title "Herunterladen"
                          , HP.style "margin-left: 0.5rem;"
                          ]
                          [ HH.text "⬇️" ]
                      -- Delete button
                      , HH.button
                          [ HE.onClick \_ -> DeleteFile (PDFFile entry)
                          , HP.title "Löschen"
                          , HP.style "margin-left: 0.5rem;"
                          ]
                          [ HH.text "🗑️" ]
                      ]
                )
            ]
        else HH.text ""
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of

    ToggleSidebar ->
      H.modify_ \st -> st { sidebarOpen = not st.sidebarOpen }

    FileUploaded Nothing -> pure unit

    FileUploaded (Just file) -> do
      url <- liftEffect $ createObjectURL (unsafeCoerce file)
      H.modify_ \st -> st { files = st.files <> [ PDFFile { file, url } ] }

    DeleteFile pdfFile -> do
      H.modify_ \st -> st { files = delete pdfFile st.files }

    RaisePDF mURL -> H.raise (SendPDF mURL)

  abrFileName :: String -> String
  abrFileName name =
    if length name > 28 then
      case getEnding $ reverse $ toCodePointArray name of
        Nothing -> name
        Just end -> take 22 name <> "..." <> end
    else name
    where
    getEnding :: Array CodePoint -> Maybe String
    getEnding [] = Just ""
    getEnding name' = do
      h <- head name'
      t <- tail name'
      let cp = singleton h
      if cp == "." then Just "."
      else do
        ts <- getEnding t
        pure (ts <> cp)
