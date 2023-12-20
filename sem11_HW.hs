import System.IO (hFlush, stdout, hSetBuffering, BufferMode (NoBuffering))
import System.Environment (getArgs)
import Data.List (elemIndex)
import Data.Maybe

alphabet :: [Char]
alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

shiftChar :: Char -> Int -> Char
shiftChar char shift =
  case elemIndex char alphabet of
    Just index -> alphabet !! ((index + shift) `mod` length alphabet)
    Nothing    -> char

vigenere :: String -> String -> String
vigenere text key = zipWith shiftChar text (cycle ( map (subtract ( ord 'a') key)))

devigenere :: String -> String -> String
devigenere text key = zipWith shiftChar text (cycle ( map (negate . subtract ( ord 'a') key)))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  [mode, key, filePath] <- getArgs
  text <- readFile filePath
  let res = case mode of
              "e" -> vigenere text key
              "d" -> devigenere text key
              _   -> error "We don`t Know what is it takoe"
  writeFile "out.txt" res


  -- https://www.youtube.com/watch?v=T7yQjo1V4es&ab_channel=Душкинобъяснит
  -- вообще про zipWith говорили еще на лекциях в сентябре-октябре, но мне только видео ролик помог 
  -- вообще Душкин объяснит крутой канал по хаскеллю
  -- благодаря нему я избавила вас от 1001 своего вопроса 
