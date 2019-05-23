{-# language OverloadedStrings, FlexibleContexts, TupleSections #-}
module Snap.Site where

import Text.Blaze.Load(contentToHtml,Macro)
import Snap.Core(route,
                 MonadSnap,
                 dir,
                 pathArg,
                 pass,
                 finishWith,
                 setResponseBody,
                 setContentType,
                 setResponseStatus,
                 emptyResponse)
import Control.Applicative(Alternative(..))
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString(ByteString)
import Text.Blaze.Html(Html)
import Text.XML.Light.Types(Line,Element(..),QName(..),Content(..),Attr(..))
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class(MonadIO(..))
import Text.XML.Light.Input(parseXMLDoc)  
import Data.Maybe(listToMaybe)
import Text.Show.Functions()
import qualified System.IO.Streams as Streams
import Text.Blaze.Html.Renderer.Utf8(renderHtmlBuilder)

data Site m =
  Site {
    siteVars :: Map String String
  , siteContent :: Dir m
  , notFound :: ([String],Site m) -> m Html } deriving Show

data Entry m =
  Entry {
    path :: String
  , vars :: Map String String
  , content :: Either (Dir m) (([String],Site m) -> m Html) } deriving Show

data Dir m =
  Dir {
    root :: ([String],Site m) -> m Html
  , entries :: [Entry m] } deriving Show

snapSite :: MonadSnap m => Site m -> m Html
snapSite site@(Site _ top _) = snapDir [] site top

snapDir :: MonadSnap m => [String] -> Site m -> Dir m -> m Html
snapDir path site sdir = (dir "" $ (root sdir) (path,site)) <|> (route $ map (buildRoute path site) $ entries sdir) <|> (((notFound site) (path,site)) >>= e404)

e404 :: MonadSnap m
     => Html
     -> m a
e404 c =
  finishWith $ setResponseBody build $
  setContentType "text/html; charset=UTF-8" $
  setResponseStatus 404 "Not Found" emptyResponse
  where build out =
          do
            Streams.write  (Just $ renderHtmlBuilder c) out
            return out

findVars :: [String] -> Site m -> Maybe (Map String String)
findVars [] site = Just $ siteVars site
findVars spath site = listToMaybe $ mapMaybe (go spath) $ entries $ siteContent site
  where go [] _ = Nothing
        go [last] ent | last == path ent = Just $ vars ent
                      | otherwise = Nothing
        go (h:t) ent | h == path ent = either (listToMaybe . mapMaybe (go t) . entries) (const Nothing) $ content ent
                     | otherwise = Nothing

buildRoute :: MonadSnap m
           => [String]
           -> Site m
           -> Entry m
           -> (ByteString,m Html)
buildRoute path site (Entry  s _ ent) = (Char8.pack s,go ent)
  where go (Left dr) = snapDir path' site dr
        go (Right act) = act (path',site)
        path' = path ++ [s]

showLine :: Maybe Line -> String
showLine Nothing = ""
showLine (Just l) = " at line " ++ show l

loadSite :: Monad m
         => Map String (Macro ([String],Site m) m)
         -> FilePath
         -> IO (Site m)
loadSite ms fn =
  do
    txt <- liftIO $ TIO.readFile fn
    case parseXMLDoc txt of
      Nothing -> error $ "No valid xml in " ++ show fn
      Just xml -> either error return $ xmlToSite ms xml

mkAttrs :: [Attr] -> Map String String
mkAttrs = foldl go mempty
  where go mp (Attr (QName n _ _) v) = Map.insert n v mp 

xmlToSite :: Monad m => Map String (Macro ([String],Site m) m) -> Element -> Either String (Site m)
xmlToSite ms (Element (QName "site" _ _) attrs cont line) =
  do
    c <- dirContents ms cont line
    nf <- getNF line ms cont
    return $ Site (mkAttrs attrs) c nf
xmlToSite _ (Element qn _ _ line) =
  Left $ "Unexpected name " ++ show qn ++ showLine line 

dirContents :: Monad m
            => Map String (Macro ([String],Site m) m)
            -> [Content]
            -> Maybe Line
            -> Either String (Dir m)
dirContents ms cont line =
  do
    r <- getRoot line ms cont
    e <- getEnts line ms cont
    return $ Dir r e

findEl :: String -> [Content] -> Maybe ([Attr],[Content])
findEl str [] = Nothing
findEl str (h:t) = matchH h `mappend` (findEl str t)
  where matchH (Elem (Element (QName str' _ _) attr cont _)) | str' == str = Just (attr,cont)
                                                             | otherwise = Nothing 
        matchH _ = Nothing

findAttr :: String -> [Attr] -> Maybe String
findAttr str [] = Nothing
findAttr str ((Attr (QName k _ _) v):t) | k == str = Just v
                                        | otherwise = findAttr str t

getRoot :: Monad m
        => Maybe Line
        -> Map String (Macro ([String],Site m) m)
        -> [Content]
        -> Either String (([String],Site m) -> m Html)
getRoot line ms = maybe err go . findEl "root"
  where err = Left $ "No root node for dir element" ++ showLine line
        go (_,cont) = Right $ \e -> fmap mconcat $ mapM (contentToHtml ms e) cont

getNF :: Monad m
      => Maybe Line
      -> Map String (Macro ([String],Site m) m)
      -> [Content]
      -> Either String (([String],Site m) -> m Html)
getNF line ms = maybe err go . findEl "notfound"
  where err = Left $ "No not found node for dir element" ++ showLine line
        go (_,cont) = Right $ \e -> fmap mconcat $ mapM (contentToHtml ms e) cont

getEnts :: Monad m
        => Maybe Line
        -> Map String (Macro ([String],Site m) m)
        -> [Content]
        -> Either String [Entry m]
getEnts line ms = maybe err go . findEl "entries"
  where err = Left $ "No entries node for dir element" ++ showLine line
        go (_,cont) = mapM (getEnt ms) $ mapMaybe getElement cont

getElement :: Content -> Maybe Element
getElement (Elem e) = Just e
getElement _ = Nothing

getEnt :: Monad m
       => Map String (Macro ([String],Site m) m)
       -> Element
       -> Either String (Entry m)
getEnt ms (Element (QName "dir" _ _) attrs cont line) =
  case findAttr "path" attrs of
    Just p -> fmap (Entry p (mkAttrs attrs)) $ fmap Left $ dirContents ms cont line
    Nothing -> Left $ "dir needs path and name" ++ showLine line
getEnt ms (Element (QName "page" _ _) attrs cont line) =
  case findAttr "path" attrs of
    Just p -> Right (Entry p (mkAttrs attrs) $ Right $ \e -> fmap mconcat $ mapM (contentToHtml ms e) cont)
    Nothing -> Left $ "Can't find name for page" ++ showLine line
getEnt _ e@(Element qn _ _ line) =
  Left $ "Expected dir or page, found " ++ show qn ++ showLine line
