module Common.Utils (safeHead, safeTail) where

safeHead :: [a] -> a
safeHead [] = error "empty list"
safeHead (x : _) = x

safeTail :: [a] -> [a]
safeTail = drop 1
