module Uri.Parser

import public Uri.Core
import public Uri.Types
import Data.String
import Debug.Trace
import Uri.Util

%inline 
valAlphaNum : Char -> Bool 
valAlphaNum c = isAlpha c || isDigit c

%inline 
valMark : Char -> Bool 
valMark = (`oneOf` "-_.!~*'()")

%inline
valUnreserved : Char -> Bool 
valUnreserved c = valAlphaNum c || valMark c 

%inline
valReserved : Char -> Bool
valReserved = (`oneOf` ";/?:@&=+$,")

-- Not Escaped
%inline
valUric : Char -> Bool 
valUric c = valReserved c || valUnreserved c || (c `oneOf` ";/?:@&=+$,")

-- Not Escaped
%inline
valPchar : Char -> Bool 
valPchar c = valUnreserved c || (c `oneOf` ":@&=+$,")

%inline
valPcharColonSlash : Char -> Bool 
valPcharColonSlash c = valUnreserved c || c == '/' || c == ';' || (c `oneOf` ":@&=+$,")

%inline 
valScheme : Char -> Bool 
valScheme c = isAlpha c || isDigit c || (c `oneOf` "+-.")

-- Not Escaped
%inline 
valRegNameNoAtColon : Char -> Bool 
valRegNameNoAtColon c = valUnreserved c || (c `oneOf` "$,;&=+")

%inline 
valUserInfo : Char -> Bool 
valUserInfo c = valUnreserved c || (c `oneOf` ";:&=+$,")

%inline
valUricNoSlash : Char -> Bool 
valUricNoSlash c = valUnreserved c || (c `oneOf` ";?:@&=+$,")

%inline
valRelSegment : Char -> Bool 
valRelSegment c = valUnreserved c || (c `oneOf` ";@&=+$,")

-- Parsers 

parseSingleEncode : Parser Int 
parseSingleEncode = do 
    _     <- chr '%' 
    start <- (\str => assert_total (strIndex str 0)) <$> (cut $ is isHexDigit)
    end   <- (\str => assert_total (strIndex str 0)) <$> (cut $ is isHexDigit)
    pure $ (hexDigitToDigit start * 16) + (hexDigitToDigit end)

parseEncode : Parser Cut 
parseEncode = (MkLit . hexToStr) <$> (some parseSingleEncode)

parseUricNoSlash : Parser Cut
parseUricNoSlash = cutters ((matchPlus valUricNoSlash) <|> parseEncode)

parseUric : Parser Cut 
parseUric = cutters $ (matchPlus valUric) <|> parseEncode

%inline
parseFragment : Parser Cut 
parseFragment = match valUric

%inline
parseQuery : Parser Cut 
parseQuery = cutters $ (matchPlus valUric) <|> parseEncode

parseParam : Parser Cut 
parseParam = cutters $ (matchPlus valPchar) <|> parseEncode

parsePathSegment : Parser Cut 
parsePathSegment = (cutters $ (matchPlus valPcharColonSlash) <|> parseEncode) <|> pure (MkLit "")

parseOpaquePart : Parser Cut 
parseOpaquePart = is valUricNoSlash <+> match valUric

parseAbsPath : Parser Cut 
parseAbsPath = chr '/' <+> parsePathSegment

parsePath : Parser (Maybe Cut) 
parsePath = option (parseAbsPath <|> parseOpaquePart)

parseRelPath : Parser Cut
parseRelPath = do 
    res <- cutters (matchPlus valRelSegment <|> parseEncode)
    rest <- option parseAbsPath
    case rest of 
        Just n => pure (res <+> n)
        Nothing => pure res

parseHost : Parser Cut 
parseHost = matchPlus (\c => valAlphaNum c || c == '.')

unsafePort : String -> Int 
unsafePort str = 
    if str == "" 
        then 80 
        else assert_total (let (Just n) = parseInteger str in n)

public export
parseServer : Parser Authority
parseServer = do 
    userInfo <- option $ cut (match valUserInfo >>| chr '@')
    host     <- cut parseHost 
    port     <- option $ unsafePort <$> cut (chr ':' |>> match isDigit)
    pure (MkAuthority userInfo host port)

-- Not implemented "reg_name" rule
parseAuthority : Parser Authority
parseAuthority = parseServer

parseNetPathWithoutPath : Parser Authority 
parseNetPathWithoutPath = exact "//" |>> parseAuthority

parseHierPart : Parser (Maybe Authority, Maybe String, Maybe String)
parseHierPart = do
    netPath <- option parseNetPathWithoutPath
    case netPath of 
        Just _ => do 
            path <- option (cut parseAbsPath)
            query <- option $ cut (chr '?' |>> parseQuery)
            pure (netPath, path, query)
        Nothing => do 
            path <- (cut parseAbsPath)
            query <- option $ cut (chr '?' |>> parseQuery)
            pure (netPath, Just path, query)

public export
parseAbsoluteURI : Parser AbsoluteURI 
parseAbsoluteURI = do
        scheme <- (cut $ is isAlpha <+> match valScheme)
        _ <- chr ':'  
        (authority, path, query) <- (parseHierPart <|> parseOpaque)
        frag <- option $ cut (chr '#' |>> matchPlus valUric)
        pure $ MkAbsURI scheme authority path query frag
    where 
        parseOpaque : Parser (Maybe Authority, Maybe String, Maybe String)
        parseOpaque = (\x => (Just (MkAuthority Nothing x Nothing), Nothing, Nothing)) <$> cut parseOpaquePart

-- Will not parse net_path as relative.
public export
parseRelativeURI : Parser RelativeURI
parseRelativeURI = do 
    res <- cut (parseAbsPath <|> parseRelPath)
    query <- option $ cut (chr '?' |>> parseQuery)
    pure (MkRelURI res query)

public export
parseURI : Parser URI 
parseURI = 
        ((Absolute <$> parseAbsoluteURI)
    <|> (Relative <$> parseRelativeURI))
    >>| eof