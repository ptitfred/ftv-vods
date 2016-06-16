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

youtubeIdChars :: String
youtubeIdChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['+', '-', '_', '=']

youtubeId :: Gen String
youtubeId = vectorOf 11 (elements youtubeIdChars)

mkVideo :: YouTubeId -> UTCTime -> Video
mkVideo vid = Video "" vid "" [] ""

instance Arbitrary Video where
  arbitrary = mkVideo <$> youtubeId <*> arbitrary

sortedVideos :: Gen [Video]
sortedVideos = do
    v1 <- arbitrary
    v2 <- arbitrary
    vs <- listOf arbitrary
    return $ sortOn videoPublishDate $ v1 : v2 : vs

prop_emptyPlaylist :: Video -> Bool
prop_emptyPlaylist v = findInsertPositions [] [v] == [(v, 0)]

prop_avoidDoublons :: [Video] -> Bool
prop_avoidDoublons vs = null (findInsertPositions vs vs)

prop_previousVideoWillBeFirst :: Property
prop_previousVideoWillBeFirst = forAll sortedVideos prop
  where prop (v:vs) = findInsertPositions vs [v] == [(v, 0)]

prop_previous2VideoWillBeFirst :: Property
prop_previous2VideoWillBeFirst = forAll sortedVideos prop
  where prop (v1:v2:vs) = findInsertPositions vs [v1, v2] == [(v2, 0), (v1, 0)]

prop_nextVideoWillBeLast :: Property
prop_nextVideoWillBeLast = forAll sortedVideos prop
  where prop vs = findInsertPositions (init vs) [last vs] == [(last vs, length $ init vs)]

prop_next2VideoWillBeLast :: Property
prop_next2VideoWillBeLast = forAll sortedVideos (\vs ->
         let vs' = init $ init vs
             v1  = last $ init vs
             v2  = last vs
         in findInsertPositions vs' [v1, v2] == [(v2, length vs'), (v1, length vs')]
        )

prop_insertionShouldReversed :: Property
prop_insertionShouldReversed = forAll sortedVideos prop
  where prop vs = findInsertPositions [] vs == zip (reverse vs) (repeat 0)

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
