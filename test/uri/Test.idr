module Main

import Uri

lul : List String 
lul = [
    "https://www.youtube.com/watch?v=YgmFIVOR1-I&list=RD2By3feOVw20&index=9&ab_channel=%E3%81%9A%E3%81%A3%E3%81%A8%E7%9C%9F%E5%A4%9C%E4%B8%AD%E3%81%A7%E3%81%84%E3%81%84%E3%81%AE%E3%81%AB%E3%80%82ZUTOMAYO",
    "ftp://ftp.is.co.za/rfc/rfc1808.txt",
    "gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles",
    "http://www.math.uio.no/faq/compression-faq/part1.html",
    "mailto:mduerst@ifi.unizh.ch",
    "news:comp.infosystems.www.servers.unix",
    "telnet://melvyl.ucop.edu/",
    "http://192.168.0:8080",
    "//www.cwi.nl:80/%7Eguido/Python.html",
    "www.cwi.nl/%7Eguido/Python.html",
    "help/Python.html",
    "http://www.cwi.nl/%7Eguido/Python.html",
    "example.com",
    "user:password@example.com:8080",
    "http://user:password@example.com:8080"
]

main : IO ()
main = 
    case (traverse decodeURI lul) of 
        Right res => 
            for_ res (\x => putStrLn (show res))
        Left err => putStrLn "Err"