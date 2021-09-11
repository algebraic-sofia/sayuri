module Uri.Types 

import Data.List

public export
record Authority where 
    constructor MkAuthority 
    userInfo : Maybe String 
    host : String 
    port : Maybe Int

public export
record AbsoluteURI where 
    constructor MkAbsURI 
    scheme : String 
    authority : Maybe Authority 
    path : Maybe String
    query : Maybe String 
    fragment : Maybe String

public export
record RelativeURI where 
    constructor MkRelURI
    path : String 
    query : Maybe String

public export
data URI
    = Absolute AbsoluteURI
    | Relative RelativeURI

par : String -> List String -> String
par name args = "(" ++ (concat $ intersperse " " (name :: args)) ++ ")"

before : Maybe String -> String -> String 
before (Just n) str = str ++ n
before Nothing str = ""

after : Maybe String -> String -> String 
after (Just n) str = n ++ str 
after Nothing str = ""

public export
interface Encode t where 
    encode : t -> String

public export
Encode Authority where 
    encode (MkAuthority userinfo host port) = 
        (after (userinfo) "@")
     ++ host 
     ++ (before (show <$> port) ":") 

public export
Encode AbsoluteURI where 
    encode (MkAbsURI scheme authority path query fragment) = 
        scheme ++ ":" ++ (before (encode <$> authority) "//") ++ (before path "") ++ (before query "?") ++ (before fragment "#")
 
public export
Encode RelativeURI where 
    encode (MkRelURI path query) = par "RelativeURI" [show path, show query]

public export
Encode URI where 
    encode (Absolute uri) = encode uri
    encode (Relative uri) = encode uri

-- Show

public export
Show Authority where 
    show (MkAuthority userinfo host path) = 
        par "Authority" [show userinfo, show host, show path]

public export
Show AbsoluteURI where 
    show (MkAbsURI scheme authority path query frag) = 
        par "AbsUri" [show scheme, show authority, show path, show query, show frag]
 
public export
Show RelativeURI where 
    show (MkRelURI path query) = par "RelativeURI" [show path, show query]

public export
Show URI where 
    show (Absolute uri) = par "Absolute" [show uri]
    show (Relative uri) = par "Relative" [show uri]