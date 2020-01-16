module ErrorMsg where

import qualified Data.Map as M

-- map element sampling with a more readable error message
-- requires keys and values to be showable
(!) :: (Show k, Show a, Ord k) => M.Map k a -> k -> a
(!) xs x = if M.member x xs then xs M.! x else error $ error_mapElem x xs --(M.keys xs)

-- if the data is showable, let us show it in the error
at1 :: (Show a) => [a] -> Int -> a
at1 xs x = if x < length xs then xs !! x else error $ error_arrElem x xs

-- otherwise, let us just say that "something is wrong"
at2 :: [a] -> Int -> a
at2 xs x = if x < length xs then xs !! x else error $ error_arrElem x "(an unshowable array)"

equal :: (Ord a) => a -> a -> Bool
equal x y = (x == y)

notequal :: (Ord a) => a -> a -> Bool
notequal x y = (x /= y)

divide :: (Fractional a) => a -> a -> a
divide x y = x / y

--------------------------------------------------------
errorTag   = "ERROR:"
inErrTag   = "INTERNAL ERROR:"
warningTag = "WARNING:"


-- the term t contains the error message generated inside megaparsec
error_parseProgram s t = errorTag ++ " Could not parse the datalog program from file " ++ show s ++ "\nError details: " ++ t

-- TODO we could probably track the error location better...
error_nonGroundTerm x  = errorTag ++ "ERROR! a free variable " ++ show x ++ " in a term used in ABB operation."

-- internall errors that may come due to bugs
error_mapElem x xs     = inErrTag ++ " Element " ++ show x ++ " is not in the map " ++ show xs
error_arrElem x xs     = inErrTag ++ " Index " ++ show x ++ " is out of bounds in array " ++ show xs
