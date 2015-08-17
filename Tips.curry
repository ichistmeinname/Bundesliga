{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Tips where

import HTML (HtmlExp(..), headedTable)

data Team = Bayern | Leverkusen | Dortmund
          | Wolfsburg | Mainz | Schalke
          | Gladbach | Hannover | Hoffenheim
          | Stuttgart | Hamburg | Bremen
          | Augsburg | Frankfurt | Koeln
          | Berlin | Darmstadt | Ingolstadt
 deriving (Show,Eq)

-- instance Show Team where
--   show Bayern = "FC Bayern Muenchen"

data Player = JanWulf | Jan | JR | Christoph
            | Johannes | Julia | Daniel | Mirko
            | Ulf | Sandra
  deriving (Eq, Show, Enum, Bounded)

allPlayers = [minBound .. maxBound]

type Tip = [(Team,Pos)]
type Pos = Int

tipsForPlayer :: Player -> Tip
tipsForPlayer name = zip (go name) [1..]
 where
  go JanWulf =
   [ Bayern, Leverkusen, Dortmund
   , Wolfsburg, Mainz, Schalke
   , Gladbach, Hannover, Hoffenheim
   , Stuttgart, Hamburg, Bremen
   , Augsburg, Frankfurt, Koeln
   , Berlin, Darmstadt, Ingolstadt ]
  go Christoph =
    [ Bayern, Gladbach, Wolfsburg
    , Leverkusen, Schalke, Dortmund
    , Hoffenheim, Augsburg, Bremen
    , Mainz, Frankfurt, Koeln
    , Hannover, Berlin, Ingolstadt
    , Hamburg, Darmstadt, Stuttgart ]
  go Johannes =
    [ Bayern, Dortmund, Wolfsburg
    , Leverkusen, Gladbach, Schalke
    , Mainz, Augsburg, Hoffenheim
    , Bremen, Frankfurt, Stuttgart
    , Koeln, Hamburg, Berlin
    , Hannover, Darmstadt, Ingolstadt ]
  go Jan =
    [ Bayern, Gladbach, Dortmund
    , Wolfsburg, Leverkusen, Stuttgart
    , Augsburg, Koeln, Schalke
    , Bremen, Mainz, Hamburg
    , Hoffenheim, Ingolstadt, Frankfurt
    , Berlin, Hannover, Darmstadt ]
  go JR =
    [ Bayern, Gladbach, Wolfsburg
    , Schalke, Leverkusen, Dortmund
    , Mainz, Stuttgart, Berlin
    , Hoffenheim, Augsburg, Koeln
    , Bremen, Hamburg, Ingolstadt
    , Hannover, Frankfurt, Darmstadt ]
  go Ulf =
    [ Bayern, Wolfsburg, Dortmund
    , Leverkusen, Gladbach, Schalke
    , Augsburg, Hoffenheim, Mainz
    , Bremen, Stuttgart, Frankfurt
    , Hannover, Koeln, Hamburg
    , Berlin , Ingolstadt, Darmstadt ]
  go Mirko =
    [ Wolfsburg, Bayern, Dortmund
    , Leverkusen, Gladbach, Schalke
    , Hamburg, Augsburg, Bremen
    , Berlin, Mainz, Stuttgart
    , Hoffenheim, Ingolstadt, Koeln
    , Hannover, Frankfurt, Darmstadt ]
  go Julia =
    [ Bayern, Dortmund, Wolfsburg
    , Leverkusen, Gladbach, Schalke
    , Frankfurt, Augsburg, Hannover
    , Mainz, Darmstadt, Stuttgart
    , Hamburg, Hoffenheim, Bremen
    , Berlin, Koeln, Ingolstadt ]
  go Daniel =
    [ Dortmund, Bayern, Wolfsburg
    , Gladbach, Frankfurt, Leverkusen
    , Augsburg, Schalke, Bremen
    , Hannover, Hoffenheim, Mainz
    , Koeln, Stuttgart, Hamburg
    , Berlin, Darmstadt, Ingolstadt ]
   
tips :: [(Player, Tip)]
tips = map (\name -> (name,tipsForPlayer name)) allPlayers

mkTable (name,tip) = headedTable ([[HtmlText name]] : posAndTeams)
 where
  posAndTeams = zipWith (\team pos -> [[HtmlText (show pos),HtmlText (show team)]])
                        tip
                        [1..]
apiCall matchday =
  "http://api.football-data.org/alpha/soccerseasons/394/leagueTable?matchday="
    ++ show matchday

main = writeFile "index.html" (showHtmlExps (map mkTable tips))