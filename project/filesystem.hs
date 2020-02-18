{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
import Data.List

data File = Directory {name :: String, children :: [File]}
            | TextFile {name:: String, content :: String, children :: [File]}
            deriving (Show)

type Path = [String]

-- util fucntion to parse file paths
-- takes only the head of the args
-- ["/asd/asd2", ...] -> ["/", "asd", "asd2"]
splitParts :: [String] -> [String]
splitParts [] = []
splitParts (s:xs) = if (head s) == '/'
                    then "/" : splitted
                    else splitted
                  where splitted = words (map (\c -> if c == '/' then ' ' else c) s)

splitPartsMultiple :: [String] -> [[String]]
splitPartsMultiple [] = []
splitPartsMultiple (x:xs) = splitParts (x:[]) : splitPartsMultiple xs

main = do executor ((Directory "/"
                                [(Directory "dir1"
                                            [(Directory "dir3"
                                                        []),
                                             (TextFile "b" "I am B" [])]),
                                (Directory "dir2"
                                            [(TextFile "a" "I am A" [])])]),
                    ["/"])

executor :: (File, Path) -> IO ()
executor (filesystem, currentDir) = do
    putStr "$ "
    fullCommand <- getLine
    let cmd = head $ words fullCommand
    let args = tail $ words fullCommand

    case cmd of
         "pwd" -> print currentDir
         "ls" -> print $ listChildren filesystem currentDir $ splitParts args
         "cd" -> executor (filesystem, (changeCurrentDir filesystem currentDir $ splitParts args))
         "cat" -> if (1 == length args )
                  then print (getFileContent filesystem currentDir (splitParts args))
                  else print (catFiles filesystem currentDir (splitPartsMultiple args))
         "exit" -> return ()
         _ -> putStrLn ("Unrecognised command " ++ cmd)
    executor (filesystem, currentDir)

-- checks if the given target path is relative or full
-- if it is relative, adds the current directory in the beggining
getFullPath :: Path -> Path -> Path
getFullPath _ t@("/":xs) = t
getFullPath curr tgt = curr ++ tgt

-- checks if the File has a child with the given name
containsFisrtLevel :: String -> File -> Bool
containsFisrtLevel childName = elem childName . map name . children

getChildByName :: String -> File -> Maybe File
getChildByName searched file = find (\f -> (name f) == searched) (children file)

-- Returns the File object in the filesystem with the given FULL PATH if it is a directory
goToPath :: Maybe File -> Path -> Maybe File
goToPath _ [] = Nothing
goToPath Nothing _ = Nothing
goToPath (Just (TextFile _ _ _)) _ = Nothing
goToPath (Just filesystem) [x] = if x == (name filesystem) then Just filesystem else Nothing
goToPath (Just filesystem) (x:x2:xs) = if x == (name filesystem)
                                  then goToPath (getChildByName x2 filesystem) (x2:xs)
                                  else Nothing

-- Returns the File object in the filesystem with the given FULL PATH if it is a TextFile
goToFile :: File -> Path -> Maybe File
gotToFile _ [] = Nothing
goToFile filesystem path = case parentPath of
                           Just p -> case (getChildByName (last path) p) of
                                     Nothing -> Nothing
                                     Just (Directory _ _) -> Nothing
                                     Just a -> Just a
                           Nothing -> Nothing
                       where parentPath = goToPath (Just filesystem) (init path)

-- ls
listChildren :: File -> Path -> [String] -> String
listChildren filesystem currentDir [] = case file of
                                        Just a -> unwords $ map name $ children a
                                        Nothing -> []
                                     where file = goToPath (Just filesystem) currentDir
listChildren filesystem currentDir targetPath = case (goToPath (Just filesystem) fullPath) of
                                                Just a -> unwords $ map name $ children a
                                                Nothing -> []
                                             where fullPath = getFullPath currentDir targetPath
-- cd
changeCurrentDir :: File -> Path -> [String] -> [String]
changeCurrentDir _ currentDir [] = currentDir
changeCurrentDir filesystem currentDir targetPath = case (goToPath (Just filesystem) fullPath) of
                                                    Just a -> targetPath --we have such a directory, return the path
                                                    Nothing -> currentDir -- we have no such directory, stay at old one
                                                 where fullPath = getFullPath currentDir targetPath


--cat
getFileContent :: File -> Path -> Path -> String
getFileContent filesystem currentDir targetPath = case (goToFile filesystem fullPath) of
                                                  Just a -> content a --we have such a directory, return the path
                                                  Nothing -> [] -- we have no such directory, stay at old one
                                              where fullPath = getFullPath currentDir targetPath


catFiles :: File -> Path -> [Path] -> String
catFiles filesystem currentDir paths = concat ((map $ getFileContent filesystem currentDir) (init paths))
