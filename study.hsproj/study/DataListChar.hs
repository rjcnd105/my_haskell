module DataListChar where
  
import Data.List
import Data.Char

 
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   