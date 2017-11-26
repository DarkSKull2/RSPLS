module Main where

import System.Random(randomRIO)
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import System.Environment (getArgs)
import Data.List(find)
import qualified Data.Map.Strict as Map

data Move = Rock | Scissors | Paper | Lizard | Spock
  deriving (Show,Read,Eq)

data Result = P1Win | Tie | P2Win
  deriving (Show,Eq)

class Player p where
  getName :: p -> String
  getMove :: p -> IO(Move)

data RPlayer = RPlayer { name :: String }
  deriving (Show,Eq)

instance Player RPlayer where
  getName RPlayer{name=n} = n
  getMove _ = getMove'
   where
     getMove' = let moveOptions = [Rock , Scissors , Paper , Lizard , Spock]
               in randomRIO(0,length(moveOptions)-1) >>= ( \idx -> pure $ (!!) moveOptions idx )


data ScoreTracker p = ScoreTracker { player :: p, score :: Int}
  deriving (Show,Eq)


beat :: Move -> Move -> Result
beat Scissors Paper = P1Win
beat Paper Rock = P1Win
beat Rock Lizard = P1Win
beat Lizard Spock = P1Win
beat Spock Scissors = P1Win
beat Scissors Lizard = P1Win
beat Lizard Paper = P1Win
beat Paper Spock = P1Win
beat Spock Rock = P1Win
beat Rock Scissors = P1Win
beat m1 m2 | m1 == m2 = Tie
           | otherwise = P2Win

desc :: Move -> Move -> Maybe String
desc Scissors Paper = Just "Scissors cuts Paper"
desc Paper Rock = Just "Paper covers Rock"
desc Rock Lizard = Just "Rock crushes Lizard"
desc Lizard Spock = Just "Lizard poisons Spock"
desc Spock Scissors = Just "Spock smashes Scissors"
desc Scissors Lizard = Just "Scissors decapitates Lizard"
desc Lizard Paper = Just "Lizard eats Paper"
desc Paper Spock = Just "Paper disproves Spock"
desc Spock Rock = Just "Spock vaporizes Rock"
desc Rock Scissors = Just "Rock crushes Scissors"
desc m1 m2 | m1 == m2 = Nothing
           | otherwise = desc m2 m1

handPlay :: Player p => p -> p -> IO Result
handPlay p1 p2 = do
                  p1m <- getMove p1
                  p2m <- getMove p2
                  putStr ((getName p1) ++ " picked " ++ (show p1m) ++ ", ")
                  putStr ((getName p2) ++ " picked " ++ (show p2m))
                  putStrLn (fromMaybe' ", " (desc p1m p2m))
                  pure (beat p1m p2m)
                    where
                      fromMaybe' _ Nothing = ""
                      fromMaybe' x (Just y) = x++y

gamePlay :: Player p => ScoreTracker p -> ScoreTracker p -> Int -> IO (ScoreTracker p, ScoreTracker p)
gamePlay sc1 sc2 bo = do
                        res <-  handPlay (player sc1) (player sc2)
                        let scorep1 = if(res==P1Win) then ((score sc1)+1) else (score sc1)
                        let scorep2 = if(res==P2Win) then ((score sc2)+1) else (score sc2)
                        let nsc1 = sc1 {score= scorep1}
                        let nsc2 = sc2 {score= scorep2}
                        let target = quot bo 2
                        if(scorep1>target || scorep2>target)
                          then pure (nsc1,nsc2)
                          else gamePlay nsc1 nsc2 bo

gameStart :: String -> String -> Int -> IO (ScoreTracker RPlayer, ScoreTracker RPlayer)
gameStart p1 p2 bo = putStrLn (p1++" VS "++p2) >> gamePlay (initRPlayerScoreTracker p1) (initRPlayerScoreTracker p2) bo

initRPlayerScoreTracker :: String -> ScoreTracker RPlayer
initRPlayerScoreTracker n = ScoreTracker{score=0,player=RPlayer{name=n}}


arrangeGames :: [String] -> [(String,String)]
arrangeGames pl = [(x,y) | x<-pl , y<-pl , x /= y ]




data Flag = Players [String] | BestOf Int
   deriving Show

options :: [OptDescr Flag]
options = [
            Option ['b'] ["best-of"] (ReqArg (\x -> BestOf (read x) ) "BESTOF") "Set the Best Of value" ,
            Option ['p'] ["players"] (ReqArg (\x -> Players (read x) ) "PLAYERLIST") "List of Players"
          ]

compilerOpts :: [String] -> IO ([Flag])
compilerOpts argv =
  case getOpt RequireOrder options argv of
     (o,_,[]  ) -> return o
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where header = "Usage: "

doWork = do
          args <- getArgs
          x <- compilerOpts args
          let bo = flagToBestOf (fromMaybe (BestOf 1) (find findBestOf x ) )
          let pl = flagToPlayers (fromMaybe (Players []) (find findPlayers x ))
          let matches = arrangeGames pl
          gameResults <- mapM (\(p1,p2) -> gameStart p1 p2 bo) matches
          let x = foldl f Map.empty gameResults
          putStrLn "\n\nResults:"
          putStrLn (prettyPrint x)
          putStrLn "DONE"
          where
            findBestOf (BestOf _) = True
            findBestOf _ = False
            flagToBestOf (BestOf z) = z
            findPlayers (Players _) = True
            findPlayers _ = False
            flagToPlayers (Players y) = y
            f :: Map.Map String (Int,Int) -> (ScoreTracker RPlayer, ScoreTracker RPlayer) -> Map.Map String (Int,Int)
            f m (ScoreTracker{player=RPlayer{name=n1},score=s1},ScoreTracker{player=RPlayer{name=n2},score=s2}) =
              let
                p1v = if(s1>s2) then 1 else 0
                p2v = if(s2>s1) then 1 else 0
              in Map.insertWith (\(a,b) (c,d) -> (a+c,b+d)) n2 (p2v,s2) (Map.insertWith (\(a,b) (c,d) -> (a+c,b+d)) n1 (p1v,s1) m)

prettyPrint :: Map.Map String (Int,Int) -> String
prettyPrint = Map.foldlWithKey f ("| " ++ (pad "name" " " 20) ++" | "++(pad "games" " " 5)++" | "++(pad "hands" " " 5)++" |\n" ++ "| " ++ (pad "" "-" 20) ++" | "++(pad "" "-" 5)++" | "++(pad "" "-" 5)++" |\n")
  where
    f prev k (g,h) = prev ++ "| " ++ (pad k " " 20)++ " | " ++ (pad (show g) " " 5) ++ " | " ++ (pad (show h) " " 5) ++ " |\n"
    pad s c sz = if length(s)<sz then pad (s++c) c sz else s

main :: IO ()
main = doWork
