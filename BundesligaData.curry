{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module BundesligaData where
import JSON

apiCall :: Maybe Int -> String
apiCall mMatchday = "http://api.football-data.org/alpha/soccerseasons/394/leagueTable" ++
  (case mMatchday of
       Just day -> "?matchday=" ++ show mMatchday
       Nothing  -> "")

data Team = Bayern | Leverkusen | Dortmund
          | Wolfsburg | Mainz | Schalke
          | Gladbach | Hannover | Hoffenheim
          | Stuttgart | Hamburg | Bremen
          | Augsburg | Frankfurt | Koeln
          | Berlin | Darmstadt | Ingolstadt
 deriving (Show,Eq)

-- instance Show Team where
--   show Bayern = "FC Bayern Muenchen"

data Table = Table String Int [TeamInfo]
  deriving Show

type Pos = Int

tableName :: Table -> String
tableName (Table name _ _) = name

matchday :: Table -> Int
matchday (Table _ md _) = md

standing :: Table -> [TeamInfo]
standing (Table _ _ ts) = ts

instance FromJSON Table where
  fromJSON doc = case table of
                     Nothing -> error "fromJust: not a Table"
                     Just x  -> x
   where table = Table <$>
          "leagueCaption" .: doc <*>
          "matchday" .: doc <*>
          "standing" .: doc

data TeamInfo = TeamInfo URL Int Team Int Int Int Int Int
  deriving Show

type URL = String

teamLink :: TeamInfo -> URL
teamLink (TeamInfo url _ _ _ _ _ _ _) = url

position :: TeamInfo -> Int
position (TeamInfo _ pos _ _ _ _ _ _) = pos

teamName :: TeamInfo -> Team
teamName (TeamInfo _ _ name _ _ _ _ _) = name

playedGames :: TeamInfo -> Int
playedGames (TeamInfo _ _ _ games _ _ _ _) = games

points :: TeamInfo -> Int
points (TeamInfo _ _ _ _ points _ _ _) = points

goals :: TeamInfo -> Int
goals (TeamInfo _ _ _ _ _ goals _ _) = goals

goalsAgainst :: TeamInfo -> Int
goalsAgainst (TeamInfo _ _ _ _ _ _ against _) = against

goalsDifference :: TeamInfo -> Int
goalsDifference (TeamInfo _ _ _ _ _ _ _ diff) = diff

instance FromJSON TeamInfo where
  fromJSONList (Array xs) = map fromJSON xs
  fromJSON doc = case info of
                       Nothing -> error "fromJust: not a TeamInfo"
                       Just x  -> x
   where info = TeamInfo <$>
          "href" .:! ("team" !$!  ("_links" !$ doc)) <*>
          "position" .: doc <*>
          "teamName" .: doc <*>
          "playedGames" .: doc <*>
          "points" .: doc <*>
          "goals" .: doc <*>
          "goalsAgainst" .: doc <*>
          "goalDifference" .: doc

instance FromJSON Int where
  fromJSON json = case json of
                      ValInt i -> i
                      _        -> error "fromJSON: not a ValInt"

instance FromJSON Char where
  fromJSON json = case json of
                      ValString str -> head str
                      _             -> error "fromJSON: not a ValString"
  fromJSONList json = case json of
                          ValString str -> str
                          _             -> error "fromJSON: not a ValString"

instance FromJSON a => FromJSON [a] where
  fromJSON = fromJSONList


instance FromJSON Team where
  read str = case str of
                 "FC Bayern M\252nchen"    -> Bayern
                 "Borussia Dortmund"       -> Dortmund
                 "FC Schalke 04"           -> Schalke
                 "1. FC K\246ln"           -> Koeln
                 "Bayer Leverkusen"        -> Leverkusen
                 "VfL Wolfsburg"           -> Wolfsburg
                 "Hertha BSC"              -> Berlin
                 "FC Ingolstadt 04"        -> Ingolstadt
                 "SV Darmstadt 98"         -> Darmstadt
                 "Hannover 96"             -> Hannover
                 "TSG 1899 Hoffenheim"     -> Hoffenheim
                 "Eintracht Frankfurt"     -> Frankfurt
                 "FC Augsburg"             -> Augsburg
                 "1. FSV Mainz 05"         -> Mainz
                 "VfB Stuttgart"           -> Stuttgart
                 "Werder Bremen"           -> Bremen
                 "Bor. M\246nchengladbach" -> Gladbach
                 "Hamburger SV"            -> Hamburg
  fromJSON json = case json of
                      ValString str -> read str
                      _             -> error "fromJSON: not a ValString"
infixl 4 <*>, <$>

(<$>) :: (a -> b) -> Maybe a -> Maybe b
_ <$> Nothing  = Nothing
f <$> (Just x) = Just (f x)

(<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
mF <*> mA = maybe Nothing ((<$> mA)) mF
