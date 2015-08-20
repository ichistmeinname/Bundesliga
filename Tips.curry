{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Tips where

import Crawler
import BundesligaData
import FPParser (parse)
import JSON (FromJSON(..), leagueTable)
import Metric

import Maybe (fromJust)
import List (sortBy, sum)
import HTML ( HtmlExp(..), HtmlPage(..), PageParam(..)
            , headedTable, showHtmlPage, addClass, blockstyle, h1 )

data Player = JanWulf | Jan | JR | Christoph
            | Johannes | Julia | Daniel | Mirko
            | Ulf | Sandra
  deriving (Eq, Enum, Bounded)

instance Show Player where
  show JanWulf = "Jan W."
  show Jan = "Jan"
  show JR = "JR"
  show Christoph = "Christoph"
  show Johannes = "Johannes"
  show Julia = "Julia"
  show Daniel = "Daniel"
  show Mirko = "Mirko"
  show Ulf = "Ulf"
  show Sandra = "Sandra"

allPlayers = [minBound .. maxBound]

type Tip = [(Team,Pos)]

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
  go Sandra =
    [ Gladbach, Bayern, Wolfsburg
    , Dortmund, Leverkusen, Hoffenheim
    , Bremen, Augsburg, Schalke
    , Frankfurt, Mainz, Hamburg
    , Ingolstadt, Stuttgart, Koeln
    , Berlin, Hannover, Darmstadt ]
   
tips :: [(Player, Tip)]
tips = map (\name -> (name,tipsForPlayer name)) allPlayers


-- Metric stuff

type Points = Int

distance :: Table -> Tip -> [Points]
distance table tips =
  map (\ (team,pos) -> euklid (currentPos team) pos) tips
 where
  currentPos t = fromJust (lookup t (tableToTip table))

tableToTip :: Table -> Tip
tableToTip table = map (\tInfo -> (teamName tInfo,position tInfo)) tInfos
 where
  tInfos = standing table

-- HTML stuff

mkTipsTable :: (Player,[((Team,Pos),Points)]) -> HtmlExp
mkTipsTable (name,tipAndPoints) =
  blockstyle "bs-example" [tableWithCaption ["#","Verein", "Punkte"] (show name) posAndTeams]
 where
  posAndTeams = map (\ ((team,pos),points) -> [[HtmlText (show pos)]
                                             ,[HtmlText (show team)]
                                             ,[HtmlText (show points)]])
                    tipAndPoints

mkPlayerTable :: [(Player,Points)] -> [HtmlExp]
mkPlayerTable pair =
  [blockstyle "bs-example cur-table" [tableWithCaption ["#","Tipper", "Punkte"] "Tabellenstand" table]]
     ++ [blockstyle "clear" []]
 where
  table = map (\ ((name,points),pos) -> [[HtmlText (show pos)]
                                        ,[HtmlText (show name)]
                                        ,[HtmlText (show points)]])
              (zip pair [1..])

tableWithCaption :: [String] -> String -> [[[HtmlExp]]] -> HtmlExp
tableWithCaption header caption items = HtmlStruct "table" [("class","table")]
 (HtmlStruct "caption" [] [HtmlText caption] :
 (HtmlStruct "thead" []
   [HtmlStruct "tr" [] (map (\text -> HtmlStruct "th" [] [HtmlText text]) header)]) :
 [HtmlStruct "tbody" [] (map (\row-> HtmlStruct "tr" []
                              (map (\item -> HtmlStruct "td" [] item) row)) items)])

mkPage :: [(Player,[((Team,Pos),Points)])] -> [(Player,Points)] -> HtmlPage
mkPage info table = HtmlPage "Saison Spektakel 15/16" cssParams [content]
 where
  content = blockstyle "content" ([title] ++ mkPlayerTable table ++ map mkTipsTable info ++ [blockstyle "clear" []])
  cssParams = [ PageCSS "bootstrap-3.3.5/css/bootstrap.min.css"
             , PageCSS "style.css"]
  title = h1 [HtmlText "Saison Spektakel 2015/16"] `addClass` "jumbotron"

main :: IO ()
main = do
  putStrLn "Crawling Bundesliga table ..."
  jsonData <- getContentsOfUrl (apiCall Nothing)
  case parse leagueTable jsonData of
      Nothing     -> putStrLn "Could not parse Bundesliga table"
      Just jTable -> do
        let table = fromJSON jTable
            playerInfos :: [(Player,[((Team,Pos),Points)])]
            playerInfos  = map (\ (player,ts) -> (player,zip ts (distance table ts)))
                               tips
            playerAndPoints = map (\ (player,tipAndPoints) -> (player,sum (map snd tipAndPoints)))
                                  playerInfos
            playerTable = sortBy (\p1 p2 -> snd p1 < snd p2) playerAndPoints
        putStrLn "Write index.html ..."
        writeFile "index.html" (showHtmlPage (mkPage playerInfos playerTable))