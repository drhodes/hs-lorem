{-# LANGUAGE OverloadedStrings #-}

module Text.Lorem 
    ( host 
    , url
    , email
    , natural_word
    , natural_sentence
    , paragraph
    , word
    ) where

import qualified Control.Monad.Random as CMR
import qualified Text.Lorem.Words as TLW
import qualified Data.Text as DT
import qualified Data.List as DL

-- | A lookup table from probability to word frequency
gen_word_len :: Int -> Int
gen_word_len f
    -- a table of word lengths and their frequencies.
    | f < 2   = 1
    | f < 20  = 2
    | f < 38  = 3
    | f < 50  = 4
    | f < 61  = 5
    | f < 70  = 6
    | f < 78  = 7
    | f < 85  = 8
    | f < 90  = 9
    | f < 95  = 10
    | f < 97  = 11
    | f < 98  = 12
    | f < 100 = 13
    -- shouldn't get here
    | f >= 100 = 2 
gen_word_len _ = 2

range :: Int -> Int -> IO Int
range m n = CMR.evalRandIO $ CMR.getRandomR (m,n)

-- | Fetch a word with length n
fetch_word_len :: Int -> IO DT.Text
fetch_word_len n = do 
  let ws = [w | w <- TLW.words, DT.length w == n]
  idx <- range 0 $ (length ws) - 1
  return $ ws !! idx

-- | Generate a word with a natural length, 
natural_word :: IO DT.Text
natural_word = do 
  n <- range 0 100
  fetch_word_len $ gen_word_len n

-- | Generate one word with length n
one_word :: Int -> IO DT.Text
one_word n = do  
  let m = min 16 n          
  fetch_word_len $ m

-- | Generate a word in a specfied range of letters.
word :: Int -> Int -> IO DT.Text
word m n = range m n >>= one_word

ends_with_comma :: DT.Text -> Bool
ends_with_comma x = DT.last x == ','

insert_commas :: [DT.Text] -> Int -> IO [DT.Text]
insert_commas xs depth = do 
  {
  ; if depth <= 0 
    then return xs
    else do { 
         ; n <- range 1 $ (length xs) - 2
         ; let front = take n xs 
         ; let back = drop n xs
         ; let last' = last front
         ; let last'' = if ends_with_comma last'
                        then last'
                        else DT.concat [last', ","]
         ; let front' = if length front == 0
                        then [last'']
                        else (init front) ++ [last'']
         ; let result = front' ++ back
         ; insert_commas result (depth - 1)
         }
}

-- | Generate a sentence with a specified range of words.
sentence :: Int -> Int -> IO DT.Text
sentence m n = do   
  let minwords = max m 5
  let maxwords = max n 5
  numwords <- range minwords maxwords
  numcomma <- range 0 2
  ws <- sequence $ take numwords $ repeat natural_word
  withcommas <- insert_commas ws numcomma
  let connected = DT.concat $ DL.intersperse (" ") withcommas
  let first = DT.take 1 connected
  let rest = DT.drop 1 connected
  return $ DT.concat [DT.toUpper first, rest, "."]

-- | Generate a natural appearing sentence
natural_sentence :: IO DT.Text
natural_sentence = do n <- range 5 22
                      sentence n n

-- | Generate a random paragraph with a number of sentences numbering from m to n
paragraph :: Int -> Int -> IO DT.Text
paragraph m n 
    = do numsents <- range m n
         sents <- sequence $ take numsents $ repeat natural_sentence
         return $ DT.concat $ DL.intersperse (" ") sents

-- | Generate a random URL
url :: IO DT.Text
url = do n <- range 0 2
         h <- host
         let base = DT.concat ["http://.", h]
         w1 <- word 2 8
         w2 <- word 2 8
         return $ case n of 
                    0 -> base 
                    1 -> DT.concat [base, "/", w1]
                    2 -> DT.concat [base, "/", w1, "/", w2, ".html"]
                    _ -> error "the Text.Lorem.range function is malfunctioning"

-- | Generate a random host
host :: IO DT.Text
host = do n <- range 0 2
          let tld = case n of 
                      0 -> ".com"
                      1 -> ".net"
                      2 -> ".org"
                      _ -> error "the Text.Lorem.range function is malfunctioning"
          w1 <- word 2 8
          w2 <- word 2 8
          return $ DT.concat [w1, w2, tld]

-- | Generate a random email address
email :: IO DT.Text
email = do w <- word 4 10 
           h <- host
           return $ DT.concat [w, "@", h]
