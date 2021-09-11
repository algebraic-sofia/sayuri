module Uri.Core

import Data.String

public export
oneOf : Char -> String -> Bool
oneOf valid str = oneOf' 0 valid str
    where 
        oneOf' : Nat -> Char -> String -> Bool 
        oneOf' idx valid input = 
            if (cast idx) >= length input then False
            else if assert_total $ strIndex input (cast idx) == valid then True 
            else oneOf' (idx+1) valid input 

public export
data Parser tok = MkParser ((Int, String) -> Either Int (tok, Int))

public export
run : Parser tok -> (Int, String) -> Either Int (tok, Int)
run (MkParser parserA) input = parserA input

public export 
fail : Parser a 
fail = MkParser $ \(pos, input) => Left pos 

public export
Semigroup tok => Semigroup (Parser tok) where 
    (<+>) (MkParser funA) (MkParser funB) = MkParser $ \(pos, input) => do 
        (res1, pos')  <- funA (pos, input)
        (res2, pos'') <- funB (pos', input)
        Right (res1 <+> res2, pos'')

public export
Functor (Parser) where 
    map func parser = MkParser $ \(pos, input) =>
        case run parser (pos, input) of 
            Right (tok, newPos) => Right (func tok, newPos)
            Left err => Left err

public export
Applicative Parser where 
    pure a = MkParser $ \(pos,input) => Right (a, pos)
    (MkParser fun) <*> (MkParser entry) = MkParser $ \(pos, input) => do 
        (func, pos')  <- fun   (pos, input)
        (res, pos'') <- entry (pos', input)
        Right (func res, pos'')


public export
Alternative Parser where 
    empty = ?not_use_it
    (MkParser parserA) <|> (MkParser parserB) = MkParser $ \(pos, input) => 
        case parserA (pos, input) of 
            Left err => parserB (pos, input)
            Right (tok, pos') => Right (tok, pos')

public export
Monad Parser where 
    join (MkParser res) = MkParser $ \(pos, input) => 
        case res (pos, input) of 
            Left err => Left err 
            Right (tok, pos') => run tok (pos', input)


-- Cuts 

public export
data Cut = MkCut Int Int 
         | MkLit String
         | Join Cut Cut

Show Cut where 
    show (MkCut a b) = concat ["(", show a, "--", show b ,")"]
    show (MkLit a) = concat ["(", a,")"]
    show (Join a b) = concat ["(", show a, " ", show b,")"]

public export
Semigroup Cut where 
    (<+>) (MkCut sc ec) (MkCut sc' ec') = (MkCut sc ec')
    (<+>) (MkLit a) (MkLit b) = (MkLit (a ++ b))
    (<+>) a b = Join a b

Monoid Cut where 
    neutral = (MkCut 0 0)


public export
(|>>) : Parser a -> Parser b -> Parser b
(|>>) parserA parserB = do 
    _ <- parserA 
    parserB 

public export
(>>|) : Parser a -> Parser b -> Parser a
(>>|) parserA parserB = do 
    res <- parserA 
    _  <- parserB 
    pure res 

infixl 6 |>>
infixl 6 >>|

public export
option : Parser a -> Parser (Maybe a)
option (MkParser parserA) = MkParser $ \(pos, inp) => 
    case parserA (pos, inp) of 
        Left err => Right (Nothing, pos)
        Right (tok, pos') => Right (Just tok, pos')


-- I trust you. I know that you will NEVER make an invalid 
-- range that will make assert_total just panics.... you, Chiyoku,
-- have a great power and you'll have to use this power in the future
-- to cut strings.

cutBy : Cut -> String -> String 
cutBy (MkCut sc ec) inp = assert_total (strSubstr sc (ec - sc) inp)
cutBy (MkLit a) inp = a
cutBy (Join a b) inp = (cutBy a inp) ++ (cutBy b inp) 


public export
cut : Parser Cut -> Parser String 
cut (MkParser parserCut) = MkParser $ \(pos, inp) => 
    case parserCut (pos, inp) of 
        Left err => Left err 
        Right (MkCut sc ec, pos') => Right (assert_total (strSubstr sc (ec - sc) inp), pos')
        Right (MkLit a, pos')     => Right (a, pos')
        Right (Join  a b, pos')   => Right ((cutBy a inp) ++ (cutBy b inp), pos')
         
-- Lexer rules

public export
is : (Char -> Bool) -> Parser Cut 
is valid = MkParser $ \(pos, inp) =>
    if cast pos >= length inp 
        then Left pos 
        else if valid (assert_total $ strIndex inp pos)
                then Right (MkCut pos (pos+1), pos+1)
                else Left pos

%inline
public export
chr : Char -> Parser Cut 
chr c = is (== c)

public export
match : (Char -> Bool) -> Parser Cut 
match valid = MkParser 
    $ \(pos, input) =>  
        case match' valid (input, pos) of 
            Left err   => Left err 
            Right pos' => Right (MkCut pos pos', pos')
    where 
        match' : (Char -> Bool) -> (String, Int) -> Either Int Int 
        match' valid (input, inpPos) = 
                 if (cast inpPos >= length input) 
                    then Right inpPos
                    else if valid (assert_total $ strIndex input inpPos)
                            then match' valid (input, inpPos+1)
                            else Right inpPos

public export
matchPlus : (Char -> Bool) -> Parser Cut 
matchPlus valid = do
    res <- match valid 
    case res of 
        (MkCut start end) => 
            if start == end then fail 
                            else pure (MkCut start end)
        res => pure res

public export
exact : String -> Parser Cut 
exact valid = MkParser 
    $ \(pos, input) =>  
        case exact' (valid, 0) (input, pos) of 
            Left err   => Left err 
            Right pos' => Right (MkCut pos pos', pos')
    where 
        exact' : (String, Int) -> (String, Int) -> Either Int Int 
        exact' (valid, valPos) (input, inpPos) = 
                 case (cast valPos >= length valid, cast inpPos >= length input) of 
                    (True, _) => Right inpPos -- End of the valid string (probably matched everything)
                    (_, True) => Left  inpPos -- End of the input string gives us an error
                    (False, False) =>  
                        let valChar = assert_total $ strIndex valid valPos
                            inpChar = assert_total $ strIndex input inpPos
                        in if valChar /= inpChar 
                            then Left inpPos 
                            else exact' (valid, valPos+1) (input, inpPos+1)

public export
some : Show a => Parser a -> Parser (List a)
some (MkParser parserA) = MkParser $ \(pos, input) => 
    case parserA (pos, input) of 
        Left err => Left err 
        Right (tok, pos') => 
            if pos' == pos then Left pos' 
            else case run (some (MkParser parserA)) (pos', input) of 
                    Left err => Right ([tok], pos') 
                    Right (tokens, pos'') => Right (tok :: tokens, pos'')

public export
cutters : Parser Cut -> Parser Cut
cutters parserA = do 
    fst <- parserA
    res <- option (cutters parserA)
    case res of 
        Just end => pure (fst <+> end)
        Nothing => pure fst

public export 
eof : Parser ()
eof = MkParser $ \(pos, input) => 
    if (cast pos) >= (length input)
        then Right ((), pos)
        else Left pos