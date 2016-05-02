module Main where

import Lib
import Model

main :: IO ()
main = listPlaylistItems apiKey uploadsPlaylistId >>= prettyPrintPlaylistContent

-- Configuration
-- Should be read from environment variables
apiKey = "AIzaSyCec4oVXaalTcu5qC7JrAPAGzyaHtwRojU"
uploadsPlaylistId = "UUHmNTOzvZhZwaRJoioK0Mqw"
-- ftvChannelId = "UCHmNTOzvZhZwaRJoioK0Mqw"
