module Sisku.Search (search) where

import Control.Lens ((^.), (^?))
import Data.Aeson
import Data.Aeson.Lens
import Network.HTTP.Req
import Relude

search :: Text -> Int -> Text -> IO ()
search esFqdn esPort query = do
  runReq defaultHttpConfig $ do
    let payload =
          object
            [ "size" .= (10000 :: Int),
              "query"
                .= object
                  [ "match"
                      .= object
                        [ "hover.contents.value"
                            .= object
                              [ "query" .= query,
                                "fuzziness" .= ("AUTO" :: Text)
                              ]
                        ]
                  ]
            ]
    r <- req POST (http esFqdn /: "hovercraft" /: "_search") (ReqBodyJson payload) jsonResponse (port esPort)
    let response = responseBody r :: Value
    let hits = toList $ response ^. key "hits" . key "hits" . _Array
    let hovercrafts = mapMaybe (^? key "_source") hits
    for_ hovercrafts $ \hovercraft -> do
      case hovercraft ^? key "hover" . key "contents" . key "value" . _String of
        Nothing -> pure ()
        Just contents -> putTextLn contents
