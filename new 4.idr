module Radar

import Data.List
import Data.Maybe
import Data.Stream
import Data.String
import System
import System.File

interface Bounded a where
  minBound : a
  maxBound : a

interface Enum a where
  succ : a -> a
  succ = toEnum . (+ 1) . fromEnum

  pred : a -> a
  pred = toEnum . (\x => x - 1) . fromEnum
  
  enumFrom : a -> Stream a
  enumFrom x = map toEnum [fromEnum x ..]

  toEnum : Int -> a
  fromEnum : a -> Int

interface (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred : a -> a
  csucc : a -> a

data Direction = North | East | South | West

data Turn = TNone | TLeft | TRight | TAround

FilePath : Type
FilePath = String

Bounded Direction where
  minBound = North
  maxBound = West

Bounded Turn where
  minBound = TNone
  maxBound = TAround

Enum Direction where
  toEnum 0 = North
  toEnum 1 = East
  toEnum 2 = South
  toEnum 3 = West
  toEnum n = toEnum (n `mod` 4)

  fromEnum North = 0
  fromEnum East  = 1
  fromEnum South = 2
  fromEnum West  = 3

Enum Turn where
  toEnum 0 = TNone
  toEnum 1 = TLeft
  toEnum 2 = TRight
  toEnum 3 = TAround
  toEnum n = toEnum (n `mod` 4)

  fromEnum TNone = 0
  fromEnum TLeft = 1
  fromEnum TRight = 2
  fromEnum TAround = 3

Eq Direction where
  North == North = True
  East  == East  = True
  South == South = True
  West  == West  = True

  North == _ = False
  East  == _ = False
  South == _ = False
  West  == _ = False

  a /= b = not (a == b)

Eq Turn where
  TNone   == TNone   = True
  TLeft   == TLeft   = True
  TRight  == TRight  = True
  TAround == TAround = True

  TNone   == _ = False
  TLeft   == _ = False
  TRight  == _ = False
  TAround == _ = False

  a /= b = not (a == b)

CyclicEnum Direction where
  cpred d = if d == minBound then maxBound else pred d
  csucc d = if d == maxBound then minBound else succ d

CyclicEnum Turn where
  cpred d = if d == minBound then maxBound else pred d
  csucc d = if d == maxBound then minBound else succ d

Show Direction where
  show North = "North"
  show East = "East"
  show South = "South"
  show West = "West"

Show Turn where
  show TNone = "TNone"
  show TLeft = "TLeft"
  show TRight = "TRight"
  show TAround = "TAround"

Semigroup Turn where
  TNone   <+> turn    = turn
  TLeft   <+> TLeft   = TAround
  TLeft   <+> TRight  = TNone
  TLeft   <+> TAround = TRight
  TRight  <+> TRight  = TAround
  TRight  <+> TAround = TLeft
  TAround <+> TAround = TNone
  turn1   <+> turn2   = turn2 <+> turn1

Monoid Turn where
  -- mempty
  neutral = TNone

mappend : Monoid a => a -> a -> a
mappend = (<+>)

mconcat : Monoid m => List m -> m
mconcat = foldr mappend neutral

every : (Enum a, Bounded a) => Stream a
every = enumFrom minBound

||| Usage:
||| move to direction given turn
|||
||| rotate TLeft East
||| North
rotate : Turn -> Direction -> Direction
rotate TNone = id              -- id
rotate TLeft = cpred           -- (-)
rotate TRight = csucc          -- (+)
rotate TAround = cpred . cpred -- (--)

turns : Stream Turn
turns = every

||| Usage
||| find when d1 & d2 == t
|||
||| findTurn North Sourh [] TAround
||| [TAround]
||| findTurn North East [] TRight
||| [TRight]
findTurn : Direction -> Direction -> List Turn -> Turn -> List Turn
findTurn d1 d2 acc t = if rotate t d1 == d2 then t :: acc else acc

||| Usage: 
||| Compute turn give direction a & b
|||
||| orient North South
||| Just TAround
||| orient North East
||| Just TRight
orient : Direction -> Direction -> Maybe Turn
orient d1 d2 =
  join $
    map head' $
      find isCons $ -- isCons == not isNil
        take 10 $
          scanl (findTurn d1 d2) [] turns

||| Usage:
||| Compute final direction give a list of turn exclude steps
||| 
||| Radar> rotateMany North [TRight, TRight, TRight]
||| West
rotateMany : Direction -> List Turn -> Direction
rotateMany = foldl $ flip rotate

||| Usage:
||| Compute final direction give a list of turn exclude steps
||| 
||| Radar> rotateMany North [TRight, TRight, TRight]
||| West
rotateMany' : Direction -> List Turn -> Direction
rotateMany' dir ts = rotate (mconcat ts) dir

||| Usage:
||| Compute final direction give a list of turn include steps
||| 
||| Radar> rotateManySteps North [TAround, TAround]
||| [North, South, North]
||| Radar> rotateManySteps North [TRight, TRight, TRight]
||| [North, East, South, West]
rotateManySteps : Direction -> List Turn -> List Direction
rotateManySteps = scanl' $ flip rotate
  where
    scanl' : (b -> a -> b) -> b -> List a -> List b
    scanl' f x [] = x :: []
    scanl' f x (y :: xs) = x :: scanl' f (f x y) xs

||| Usage:
||| Compute turn for each direction
||| 
||| Radar> orientMany [North, East, South, West]
||| [Just TRight, Just TRight, Just TRight]
orientMany : List Direction -> List (Maybe Turn)
orientMany ds@(_::_::_) = zipWith orient ds (tail ds)
orientMany _ = []

parseDirection : String -> Maybe Direction
parseDirection "North" = Just North
parseDirection "East"  = Just East
parseDirection "South" = Just South
parseDirection "West"  = Just West
parseDirection _       = Nothing

parseTurn : String -> Maybe Turn
parseTurn "TNone"   = Just TNone
parseTurn "TLeft"   = Just TLeft
parseTurn "TRight"  = Just TRight
parseTurn "TAround" = Just TAround
parseTurn _         = Nothing

shortFormDir : Direction -> String
shortFormDir North = "N"
shortFormDir East  = "E"
shortFormDir South = "S"
shortFormDir West  = "W"

shortFormTurn : Turn -> String
shortFormTurn TNone   = "--"
shortFormTurn TLeft   = "<-"
shortFormTurn TRight  = "->"
shortFormTurn TAround = "||"

rotateFromFile : Direction -> FilePath -> IO ()
rotateFromFile dir fname = do Right ok <- readFile fname
                                | Left err => printLn err
                              let turns    = map (fromMaybe TNone . parseTurn) $ lines ok
                                  finalDir = rotateMany' dir turns
                                  dirs     = rotateManySteps dir turns
                              putStrLn $ "Final direction: " ++ show finalDir
                              putStrLn $ "Intermediate directions: " ++ (unwords $ map shortFormDir dirs)

orientFromFile : FilePath -> IO ()
orientFromFile fname = do Right ok <- readFile fname
                            | Left err => printLn err
                          let turns = map (fromMaybe TNone . parseTurn) $ lines ok
                          putStrLn $ "All turns: " ++ (unwords $ map shortFormTurn turns)

public export
prog : IO ()
prog = do args <- getArgs
          case args of
               (_ :: "-r" :: fname :: dir :: []) => rotateFromFile (fromMaybe North . parseDirection $ dir) fname
               (_ :: "-o" :: fname :: []) => orientFromFile fname
               _ => printLn $ "Usage: locator -o filename\n" ++ "locator -r filename direction"