-- |Utility functions for easier handling of YAML files.
module Data.Yaml.SyckUtils (
    fromStringNode,fromMapNode,
    emapEntryVal,
) where

import qualified Control.Monad
import qualified Data.List as List
import Data.Yaml.Syck

-- |Convert a YamlNode containing a String into a normal String
fromStringNode :: YamlNode -> Maybe String
fromStringNode n = case (n_elem n) of
    EStr s -> Just (unpackBuf s)
    _ -> Nothing

-- |Convert a YamlNode containing a map into a array with (key,value)-tuples
fromMapNode :: YamlNode -> Maybe [(YamlNode, YamlNode)]
fromMapNode n = case (n_elem n) of
    EMap m -> Just m
    _ -> Nothing

-- |Get a entry with a given key out of a EMap
emapEntryVal :: YamlNode -> String -> Maybe YamlNode
emapEntryVal n mkey = Control.Monad.liftM snd ((fromMapNode n) >>= (List.find keyMatch))
    where keyMatch (k, v) = maybe False (\x -> x == mkey) (fromStringNode k)

