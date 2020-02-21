import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Data.Int
import Data.Word (Word8)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
} deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ 
                             show w ++ "x" ++ show h ++ " " ++ show m

-- all thins functions returns result and rest string
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num,rest)
                 | num <= 0    -> Nothing
                 | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count           = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) ->
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (Greymap width height maxGrey bitmap, s6)

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s      >>?
    \s -> skipSpace ((), s)           >>?
    (getNat . snd)                    >>?
    skipSpace                         >>?
    \(width, s) ->   getNat s         >>?
    skipSpace                         >>?
    \(height, s) ->  getNat s         >>?
    \(maxGrey, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s) -- get all variables from scope

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

-- current string and offset in parsed string
data ParseState = ParseState {
      currentString :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)

{- get state and return value + new state. like in parseP5_take2
New value is parsed value of ParseState.currentString
-}
simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

-- parse or error
betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

{- Declare type of objects that can contain functions. functional object or etc.
We make function behaves as object for instantiate in future 'Functor' and get ability 
for nested function to use 'fmap', '<$>'
runParse, applyend on ParseState (previous) returns Exception (Left String) 
or parsed result + new ParseState (a, ParseState)
-}
newtype ParseFunc parseResT = ParseFunc {
  runParse :: ParseState -> Either String (parseResT, ParseState)
}

{- Simple parsing function that do nothing.
its get some value and return ParseFunc object, that can be 
runned with 'runParse' on some parse state 's', and we get back (a, s)
It's didn't touch state and put 'a' as parse result
-}
identity :: parseResT -> ParseFunc parseResT
identity a = ParseFunc (\s -> Right (a, s))

{- ParseFunc usages. We get parser function that has result of type 'a'
and source string - initial state. Then we run this function
-}
parse :: ParseFunc a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result -- drop state

{- Record syntax for modifiing state.
  p = ParseState (L8.pack "A") 23
  b = p { offset = 3, currentString = L8.pack "B"}
b now is ParseState "B" 3 
-}

getState :: ParseFunc ParseState
getState = ParseFunc (\s -> Right (s, s))

putState :: ParseState -> ParseFunc ()
putState s = ParseFunc (\_ -> Right ((), s))

bail :: String -> ParseFunc a
bail err = ParseFunc $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: ParseFunc a -> (a -> ParseFunc b) -> ParseFunc b
firstParser ==> secondParser  =  ParseFunc chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState

parseByte :: ParseFunc Word8
parseByte =
    getState ==> \initState ->
      case L.uncons (currentString initState) of
        Nothing -> 
          bail "no more input"
        Just (byte,remainder) ->
          putState newState ==> \_ ->
          identity byte
            where newState = initState { currentString = remainder,
                                         offset = newOffset }
                  newOffset = offset initState + 1