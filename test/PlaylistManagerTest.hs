module PlaylistManagerTest (suite) where

import PlaylistManager
import YouTube (Video(..), YouTubeId)

import Data.Time.Clock (UTCTime(..), DiffTime(..), secondsToDiffTime)
import Data.Time.Calendar (Day, fromGregorian)
import Data.List (sortOn)

import Test.QuickCheck

import Test.Tasty
import Test.Tasty.QuickCheck as QC

year  = choose (2012, 2016)
month = choose (1, 12)
day   = choose (1, 28)

instance Arbitrary Day where
  arbitrary = fromGregorian <$> year <*> month <*> day

secondsOfDay = choose (0, 60 * 60 * 24)

instance Arbitrary DiffTime where
  arbitrary = secondsToDiffTime <$> secondsOfDay

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> arbitrary

youtubeIdChars :: [Char]
youtubeIdChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['+', '-', '_', '=']

youtubeId :: Gen String
youtubeId = vectorOf 11 (elements youtubeIdChars)

mkVideo :: YouTubeId -> UTCTime -> Video
mkVideo vid publishDate = Video "" vid "" [] "" publishDate

instance Arbitrary Video where
  arbitrary = mkVideo <$> youtubeId <*> arbitrary

newtype SortedVideos = SortedVideos [Video] deriving (Show)

instance Arbitrary SortedVideos where
  arbitrary = do
    v1 <- arbitrary
    v2 <- arbitrary
    vs <- listOf arbitrary
    let videos = sortOn videoPublishDate $ v1 : v2 : vs
    return $ SortedVideos videos

prop_emptyPlaylist :: Video -> Bool
prop_emptyPlaylist v = findInsertPositions [] [v] == [(v, 0)]

prop_avoidDoublons :: [Video] -> Bool
prop_avoidDoublons vs = findInsertPositions vs vs == []

prop_previousVideoWillBeFirst :: SortedVideos -> Bool
prop_previousVideoWillBeFirst (SortedVideos vs) =
  findInsertPositions vs' [v] == [(v, 0)]
    where vs' = tail vs
          v   = head vs

prop_previous2VideoWillBeFirst :: SortedVideos -> Bool
prop_previous2VideoWillBeFirst (SortedVideos vs) =
  findInsertPositions vs' [v1, v2] == [(v2, 0), (v1, 0)]
    where vs'      = drop 2 vs
          [v1, v2] = take 2 vs

prop_nextVideoWillBeLast :: SortedVideos -> Bool
prop_nextVideoWillBeLast (SortedVideos vs) =
  findInsertPositions vs' [v] == [(v, length vs')]
    where vs' = init vs
          v   = last vs

prop_next2VideoWillBeLast :: SortedVideos -> Bool
prop_next2VideoWillBeLast (SortedVideos vs) =
  findInsertPositions vs' [v1, v2] == [(v2, length vs'), (v1, length vs')]
    where vs' = init $ init vs
          v1  = last $ init vs
          v2  = last vs

prop_insertionShouldReversed :: SortedVideos -> Bool
prop_insertionShouldReversed (SortedVideos vs) =
  findInsertPositions [] vs == zip (reverse vs) (repeat 0)

suite :: TestTree
suite = testGroup "PlaylistManager"
  [ QC.testProperty "empty playlist" prop_emptyPlaylist
  , QC.testProperty "doublons" prop_avoidDoublons
  , QC.testProperty "previous will be first" prop_previousVideoWillBeFirst
  , QC.testProperty "2 previous will be first" prop_previous2VideoWillBeFirst
  , QC.testProperty "next will be last" prop_nextVideoWillBeLast
  , QC.testProperty "2 next will be last" prop_next2VideoWillBeLast
  , QC.testProperty "insertion backward" prop_insertionShouldReversed
  ]
