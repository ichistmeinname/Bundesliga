------------------------------------------------------------------------------
--- Library for crawling web sites.
---
--- @author Sandra Dylus
--- @version August 2015
--- @category web
------------------------------------------------------------------------------

module Crawler (getContentsOfUrl) where

import System
import IOExts (readCompleteFile)

--- Reads the contents of a document located by a URL.
--- This action requires that the program "curl" is in your path,
--- otherwise the implementation must be adapted to the local
--- installation.

getContentsOfUrl :: String -> IO String
getContentsOfUrl url = do
  pid <- getPID
  let tmpfile = "curl-"++show pid
  system ("curl -o "++tmpfile++" \""++url++"\"")
  cont <- readCompleteFile tmpfile
  system ("rm -f "++tmpfile)
  return cont