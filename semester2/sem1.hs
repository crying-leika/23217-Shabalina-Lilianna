-- 1
import Data.Maybe
import Control.Applicative

data Person = Person {
 name :: String,
 surname :: String,
 father :: Maybe Person,
 mother :: Maybe Person
} deriving Show

jane = Person "Jane" "Smith" Nothing Nothing
russ = Person "Russ" "Cox" Nothing Nothing
miranda = Person "Miranda" "Lee" Nothing Nothing
alice = Person "Alice" "Cox" (Just russ) (Just jane)
john = Person "John" "Lee" Nothing (Just miranda)
david = Person "David" "Lee" (Just john) (Just alice)

mothersFather :: Person -> Maybe Person
mothersFather p = case mother p of 
    Nothing -> Nothing 
    Just mama -> case father mama of 
        Nothing -> Nothing
        Just granddad -> Just granddad

mothersFather' :: Person -> Maybe Person
mothersFather' p = do
  mother <- mother p
  father <- father mother
  return father

-- 2 

hasAllGrands :: Person -> Maybe Person
hasAllGrands p = do
  m <- mother p
  let motherMother = mother m
  let motherFather = father m
  f <- father p
  let fatherMother = mother f
  let fatherFather = father f
  motherFather <|> motherMother <|> fatherFather <|> fatherMother

--3
 
sumTwoInts :: IO ()
sumTwoInts =
  readLn >>= (\x ->
    readLn >>= (\y ->
      print (x + y)))
