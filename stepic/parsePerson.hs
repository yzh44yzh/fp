newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = undefined
    mappend = undefined

import Data.Char (isDigit, isSpace)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

type PersonData = (Maybe String, Maybe String, Maybe Int)


parsePerson :: String -> Either Error Person
parsePerson str =
    case pData of
      Left error -> Left error
      Right (Just fn, Just ln, Just a) -> Right Person { firstName = fn, lastName = ln, age = a }
      Right (_, _, _) -> Left IncompleteDataError
    where
      acc = Right (Nothing, Nothing, Nothing)
      pData = foldl parsePersonData acc $ lines str



parsePersonData :: Either Error PersonData -> String -> Either Error PersonData
parsePersonData (Left e) _ = Left e
parsePersonData (Right (fn, ln, a)) line =
    case parseTuple line of
      Just ("firstName", val) -> Right (Just val, ln, a)
      Just ("lastName", val) -> Right (fn, Just val, a)
      Just ("age", val) ->
          case parseAge val of
            Left error -> Left error
            Right age -> Right (fn, ln, Just age)
      Just (_, _) -> Right (fn, ln, a)
      Nothing -> Left ParsingError


parseTuple :: String -> Maybe (String, String)
parseTuple str =
    case span (/= '=') str of
      (k, ('=':v)) -> Just (trim k, trim v)
      _ -> Nothing


parseAge :: String -> Either Error Int
parseAge str | all isDigit str = Right (read str :: Int)
             | otherwise = Left (IncorrectDataError str)


trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . trimLeft. reverse

trim :: String -> String
trim = trimLeft . trimRight


test1 = parsePerson "firstName = John\nlastName = Connor\nage = 30"
test2 = parsePerson "firstName = John\nlastName = Connor\nage = 30\nhello"
test3 = parsePerson "firstName\nlastName = Connor\nage = 30"
test4 = parsePerson "firstName = John\nage = 30"
test5 = parsePerson "firstName = John\nlastName = Connor\nage = haha"
