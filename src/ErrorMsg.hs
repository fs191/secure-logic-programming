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

-- type errors
error_typeNum  x    = errorTag ++ "Expected num in expr evaluation, but got " ++ show x ++ "."
error_typeBool x    = errorTag ++ "Expected bool in expr evaluation, but got " ++ show x ++ "."
error_typeOp op x y = errorTag ++ "Cannot apply op " ++ show op ++ " to types " ++ show x ++ " and " ++ show y ++ "."

-- csv import errors
error_unknownColumn x y        = errorTag ++ "Could not find column " ++ show x ++ " of table " ++ show y ++ "."
error_unsupportedColumnType x  = errorTag ++ "Column type " ++ show x ++ " is not supported."

error_dbFileLengthsTooMany x ys = errorTag ++ "Excessive row records " ++ show ys ++ " in the table " ++ show x ++ "."
error_dbFileLengthsTooFew x ys = errorTag ++ "Insufficient row records for attributes " ++ show ys ++ " in the table " ++ show x ++ "."

-- transformation errors
-- TODO we could probably track the error location better...
error_nonGroundTerm x  = errorTag ++ "A non-ground term " ++ show x ++ " used in ABB operation."
error_nonConstantTerm x  = errorTag ++ "A non-constant term " ++ show x ++ " was tried to be evaluated."
error_nonGroundTableVar t x i  = inErrTag ++ "Table " ++ show t ++ " is called with a free variable " ++ show x ++ " on argument " ++ show i ++ " after unfolding."

-- secrec code generation errors
error_complexExpression x  = errorTag ++ "Could not process " ++ show x ++ ", olny variables are supported as predicate arguments."
error_tableArgNotFound t x i = inErrTag ++ show i ++ "-th arg of " ++ show t ++ ", " ++ show x ++ " has not been included into cross product table."
error_argNotFound x          = inErrTag ++ " var " ++ show x ++ " has no matching in the argument list."

-- internall errors that may come due to bugs
error_mapElem x xs     = inErrTag ++ " Element " ++ show x ++ " is not in the map " ++ show xs
error_arrElem x xs     = inErrTag ++ " Index " ++ show x ++ " is out of bounds in array " ++ show xs
