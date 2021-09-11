module Uri

import Uri.Parser
import Uri.Types
import Debug.Time

public export
decodeRelativeURI : String -> Maybe RelativeURI
decodeRelativeURI str = 
    case run parseRelativeURI (0, str) of 
        Right (uri, pos) => Just uri 
        Left err => Nothing

public export
decodeURI : String -> Maybe URI 
decodeURI str = 
    case run parseURI (0, str) of 
        Right (uri, pos) => Just uri 
        Left err => Nothing