module Main 

import Test.Golden

prefixFolder : String -> List String -> List String 
prefixFolder pref paths = map ((pref ++ "/") ++) paths

tests : TestPool
tests = MkTestPool "Tests" [] Nothing ["uri"]

main : IO ()
main = runner [ tests ]