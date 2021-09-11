module Uri.Util

import Data.Buffer
import Debug.Trace 

public export
hexDigitToDigit : Char -> Int 
hexDigitToDigit c =
         if c >= '0' && c <= '9' then (cast c) - (cast '0')
    else if c >= 'A' && c <= 'F' then (cast c) - (cast 'A') + 10
    else if c >= 'a' && c <= 'f' then (cast c) - (cast 'a') + 10
    else 0

public export 
hexToStringIO : List Int -> IO String  
hexToStringIO list = do 
        let size = length list 
        res <- newBuffer (cast $ size)
        case res of 
            Just buf => getString (hexToStr buf 0 list) 0 (cast size)
            Nothing => pure ""
    where
        hexToStr : Buffer -> Int -> List Int -> Buffer 
        hexToStr buf idx (h :: tl) = 
            let () = unsafePerformIO (setByte buf idx h) in
            hexToStr buf (idx+1) tl 
        hexToStr buf idx [] = buf

%inline
public export 
hexToStr : List Int -> String 
hexToStr = unsafePerformIO . hexToStringIO