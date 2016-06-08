module PlaylistManager
    ( Position
    , findInsertPositions
    ) where

import Helpers   (decorate)
import YouTube   (Video(..))

import Data.List ((\\))

type Position = Int

findInsertPositions :: [Video] -> [Video] -> [(Video, Position)]
findInsertPositions oldVideos videos =
  decorate (findInsertPosition oldVideos) newVideos
    where newVideos = reverse $ videos \\ oldVideos

findInsertPosition :: [Video] -> Video -> Position
findInsertPosition vs v = length vs - (length $ dropWhile (`before` v) vs)
  where v1 `before` v2 = videoPublishDate v1 < videoPublishDate v2
