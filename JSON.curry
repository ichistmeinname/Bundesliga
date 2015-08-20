{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module JSON where

import FPParser
import Maybe (catMaybes)
import Read (readInt)

type JSONParser = Parser JSON

data JSON = Object [(Key,JSON)]
          | Array [JSON]
          | ValString String
          | ValInt Int
          | ValBool Bool
          | NULL
  deriving Show

type Key = String

infixl 5 .:

(.:) :: FromJSON a => Key -> JSON -> Maybe a
(.:) key json = maybe Nothing (Just . fromJSON) (key !$ json)

(!$) :: Key -> JSON -> Maybe JSON
(!$) key json = case json of
                    Object pairs -> lookup key pairs
                    _            -> Nothing

(.:!) :: FromJSON a => Key -> Maybe JSON -> Maybe a
(.:!) key (Just json) = key .: json
(.:!) _   Nothing     = Nothing

(!$!) :: Key -> Maybe JSON -> Maybe JSON
(!$!) key (Just json) = key !$ json
(!$!) _   Nothing     = Nothing

(<!$>) :: FromJSON a => Key -> JSON -> Maybe [a]
(<!$>) k (Array xs) = case catMaybes (map (k .:) xs) of
                           [] -> Nothing
                           xs -> Just xs

openQ :: Parser ()
openQ = word "\"" *> yield ()

closeQ :: Parser ()
closeQ = word "\":" *> yield ()

openO :: Parser ()
openO = word "{" *> yield ()

closeO :: Parser ()
closeO = word "}" *> yield ()

openA :: Parser ()
openA = word "[" *> yield ()

closeA :: Parser ()
closeA = word "]" *> yield ()

comma :: Parser ()
comma = word "," *> yield ()

null :: Parser JSON
null = word "null" *> yield NULL

bool :: Parser JSON
bool = (word "true" *> yield (ValBool True)) <|> (word "false" *> yield (ValBool False))

keyword :: Key -> Parser ()
keyword k = between openQ closeQ (word k) *> yield ()

key :: Parser String
key = between openQ closeQ alpha

array :: Parser JSON -> Parser JSON
array pJ = Array <$> between openA closeA (sepBy1 pJ comma)

mapWithSep :: Parser c -> ((Key,Parser a) -> Parser b) -> [(Key,Parser a)] -> Parser [b]
mapWithSep _   f []     = yield []
mapWithSep _   f [x]    = (:[]) <$> f x
mapWithSep sep f (x:y:xs) = (:) <$> (f x <* sep) <*> (mapWithSep sep f (y:xs))

objectWithKeys :: [(Key,Parser JSON)] -> Parser JSON
objectWithKeys keysAndParser = Object <$>
  (between openO closeO
    (mapWithSep comma
               (\ (k,p) -> mkPair k <$> (keyword k *> p))
               keysAndParser))
 where
  mkPair :: Key -> JSON -> (Key,JSON)
  mkPair k p = (k,p)

string :: Parser JSON
string = ValString <$> between openQ openQ (some (charPred ( \c -> c /= '\"')))

int :: Parser JSON
int = ValInt <$> number

links = objectWithKeys [("self", string), ("soccerseason", string)]
linksTeam = objectWithKeys [("team", objectWithKeys [("href",string)])]
standing = array (objectWithKeys
  [ ("_links", linksTeam)
  , ("position", int)
  , ("teamName", string)
  , ("playedGames", int)
  , ("points", int)
  , ("goals", int)
  , ("goalsAgainst", int)
  , ("goalDifference", int)
  ])

leagueTable = objectWithKeys
  [ ("_links", links)
  , ("leagueCaption", string)
  , ("matchday", int)
  , ("standing", standing)
  ]

class FromJSON a where
  fromJSON :: JSON -> a
  fromJSONList :: JSON -> [a]
  fromJSONList = (: []) . fromJSON
  read :: String -> a
  readList :: String -> [a]
  readList = map (read . (: []))

teamExample = Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/5")])]),("position",ValInt 1),("teamName",ValString "FC Bayern M\252nchen"),("playedGames",ValInt 1),("points",ValInt 3),("goals",ValInt 5),("goalsAgainst",ValInt 0),("goalDifference",ValInt 5)]

example = Object [("_links",Object [("self",ValString "http://api.football-data.org/alpha/soccerseasons/394/leagueTable/?matchday=1"),("soccerseason",ValString "http://api.football-data.org/alpha/soccerseasons/394")]),("leagueCaption",ValString "1. Bundesliga 2015/16"),("matchday",ValInt 1),("standing",Array [Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/5")])]),("position",ValInt 1),("teamName",ValString "FC Bayern M\252nchen"),("playedGames",ValInt 1),("points",ValInt 3),("goals",ValInt 5),("goalsAgainst",ValInt 0),("goalDifference",ValInt 5)],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/4")])]),("position",ValInt 2),("teamName",ValString "Borussia Dortmund"),("playedGames",ValInt 1),("points",ValInt 3),("goals",ValInt 4),("goalsAgainst",ValInt 0),("goalDifference",ValInt 4)],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/6")])]),("position",ValInt 3),("teamName",ValString "FC Schalke 04"),("playedGames",ValInt 1),("points",ValInt 3),("goals",ValInt 3),("goalsAgainst",ValInt 0),("goalDifference",ValInt 3)],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/1")])]),("position",ValInt 4),("teamName",ValString "1. FC K\246ln"),("playedGames",ValInt 1),("points",ValInt 3),("goals",ValInt 3),("goalsAgainst",ValInt 1),("goalDifference",ValInt 2)],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/3")])]),("position",ValInt 5),("teamName",ValString "Bayer Leverkusen"),("playedGames",ValInt 1),("points",ValInt 3),("goals",ValInt 2),("goalsAgainst",ValInt 1),("goalDifference",ValInt 1)],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/11")])]),("position",ValInt 5),("teamName",ValString "VfL Wolfsburg"),("playedGames",ValInt 1),("points",ValInt 3),("goals",ValInt 2),("goalsAgainst",ValInt 1),("goalDifference",ValInt 1)],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/9")])]),("position",ValInt 7),("teamName",ValString "Hertha BSC"),("playedGames",ValInt 1),("points",ValInt 3),("goals",ValInt 1),("goalsAgainst",ValInt 0),("goalDifference",ValInt 1)],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/31")])]),("position",ValInt 7),("teamName",ValString "FC Ingolstadt 04"),("playedGames",ValInt 1),("points",ValInt 3),("goals",ValInt 1),("goalsAgainst",ValInt 0),("goalDifference",ValInt 1)],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/55")])]),("position",ValInt 9),("teamName",ValString "SV Darmstadt 98"),("playedGames",ValInt 1),("points",ValInt 1),("goals",ValInt 2),("goalsAgainst",ValInt 2),("goalDifference",ValInt 0)],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/8")])]),("position",ValInt 9),("teamName",ValString "Hannover 96"),("playedGames",ValInt 1),("points",ValInt 1),("goals",ValInt 2),("goalsAgainst",ValInt 2),("goalDifference",ValInt 0)],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/2")])]),("position",ValInt 11),("teamName",ValString "TSG 1899 Hoffenheim"),("playedGames",ValInt 1),("points",ValInt 0),("goals",ValInt 1),("goalsAgainst",ValInt 2),("goalDifference",ValInt (-1))],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/19")])]),("position",ValInt 11),("teamName",ValString "Eintracht Frankfurt"),("playedGames",ValInt 1),("points",ValInt 0),("goals",ValInt 1),("goalsAgainst",ValInt 2),("goalDifference",ValInt (-1))],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/16")])]),("position",ValInt 13),("teamName",ValString "FC Augsburg"),("playedGames",ValInt 1),("points",ValInt 0),("goals",ValInt 0),("goalsAgainst",ValInt 1),("goalDifference",ValInt (-1))],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/15")])]),("position",ValInt 13),("teamName",ValString "1. FSV Mainz 05"),("playedGames",ValInt 1),("points",ValInt 0),("goals",ValInt 0),("goalsAgainst",ValInt 1),("goalDifference",ValInt (-1))],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/10")])]),("position",ValInt 15),("teamName",ValString "VfB Stuttgart"),("playedGames",ValInt 1),("points",ValInt 0),("goals",ValInt 1),("goalsAgainst",ValInt 3),("goalDifference",ValInt (-2))],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/12")])]),("position",ValInt 16),("teamName",ValString "Werder Bremen"),("playedGames",ValInt 1),("points",ValInt 0),("goals",ValInt 0),("goalsAgainst",ValInt 3),("goalDifference",ValInt (-3))],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/18")])]),("position",ValInt 17),("teamName",ValString "Bor. M\246nchengladbach"),("playedGames",ValInt 1),("points",ValInt 0),("goals",ValInt 0),("goalsAgainst",ValInt 4),("goalDifference",ValInt (-4))],Object [("_links",Object [("team",Object [("href",ValString "http://api.football-data.org/alpha/teams/7")])]),("position",ValInt 18),("teamName",ValString "Hamburger SV"),("playedGames",ValInt 1),("points",ValInt 0),("goals",ValInt 0),("goalsAgainst",ValInt 5),("goalDifference",ValInt (-5))]])]