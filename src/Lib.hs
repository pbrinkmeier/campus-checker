{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

{- Mnemonics used in this module:
 - res: HTTP response
 - doc: parsed HTML document
 -}

import Data.Aeson
import Data.Map hiding ((!))
import GHC.Generics
import Network.HTTP.Req
import Network.HTTP.Client (CookieJar, destroyCookieJar, cookie_name, cookie_domain, cookie_value)
import Text.HandsomeSoup
import Text.XML.HXT.Core
import qualified Codec.Binary.UTF8.Generic as UTF8

-- Hash for SS 2021
currentTerm = "0xD0E9BBD669AD49178175FD393C64FB97"

printCookies :: CookieJar -> IO ()
printCookies = mapM_ printCookie . destroyCookieJar
  where
    printCookie cookie = putStrLn $ domain ++ "/" ++ name ++ " =\n  " ++ value
      where
        domain = UTF8.toString $ cookie_domain cookie
        name   = UTF8.toString $ cookie_name cookie
        value  = UTF8.toString $ cookie_value cookie

-- Establish a session with campus.studium.kit.edu through the Shibboleth login form
establishSession :: String -> String -> IO CookieJar
establishSession username password = do
  lfParams <- getLoginFormParameters
  -- password <- readFile passfile
  redirectParams <- sendLoginForm lfParams username password
  cookies <- completeRedirect redirectParams
  putStrLn $ "Authentication for " ++ username ++ " succeeded."
  return cookies

getLoginFormParameters = do
  putStrLn "Fetching campus Shibboleth login form parameters..."
  res <- runReq defaultHttpConfig request

  let doc = parseHtml $ UTF8.toString $ responseBody res

  [formAction] <- runX $ doc >>> css "form[action|=/idp/profile]" ! "action"
  [csrfToken]  <- runX $ doc >>> css "input[name=csrf_token]" ! "value"

  return (responseCookieJar res, csrfToken, getExecutionParam formAction)
  where
    request = req GET url NoReqBody bsResponse query

    url = https "campus.studium.kit.edu" /: "Shibboleth.sso" /: "Login"
    query = "target" =: ("https://campus.studium.kit.edu/?login=1" :: String)

    -- TODO: Hacky solution that assumes the "execution" query param is the last one.
    getExecutionParam = tail . dropWhile (/= '=')

sendLoginForm (cookies, csrfToken, execution) username password = do
  putStrLn "Sending Shibboleth login form..."
  res <- runReq defaultHttpConfig request

  let doc = parseHtml $ UTF8.toString $ responseBody res
  [relayState]   <- runX $ doc >>> css "input[name=RelayState]" ! "value"
  [samlResponse] <- runX $ doc >>> css "input[name=SAMLResponse]" ! "value"

  return (responseCookieJar res, relayState, samlResponse)
  where
    request = req POST url (ReqBodyUrlEnc formValues) bsResponse (query <> cookieJar cookies)

    url = https "idp.scc.kit.edu" /: "idp" /: "profile" /: "SAML2" /: "Redirect" /: "SSO"
    query = "execution" =: execution
    formValues = mconcat
      [ "csrf_token"       =: csrfToken
      , "j_username"       =: username
      , "j_password"       =: password
      , "_eventId_proceed" =: ("" :: String)
      ]

completeRedirect (cookies, relayState, samlResponse) = do
  putStrLn "Redirecting back to campus..."
  res <- runReq defaultHttpConfig request

  return $ responseCookieJar res
  where
    request = req POST url (ReqBodyUrlEnc formValues) ignoreResponse (cookieJar cookies)

    url = https "campus.studium.kit.edu" /: "Shibboleth.sso" /: "SAML2" /: "POST"
    formValues = mconcat
      [ "RelayState"   =: relayState
      , "SAMLResponse" =: samlResponse
      ]

fetchContractView cookies = do
  putStrLn "Acquiring login token..."
  token <- getToken cookies
  (cookies, prods) <- getProducts cookies token
  putStrLn "Selecting first product..."
  let product = head $ keys prods
  cvParams <- getContractView cookies product currentTerm
  (cookies, body) <- completeCvRedirect cvParams
  return body


data TokenResponse = TokenResponse
  { username :: String
  , firstname :: String
  , lastname :: String
  , matriculationNumber :: String
  , timestamp :: Integer
  , tokenA :: String
  , tokenB :: String
  } deriving (Show, Generic)

instance FromJSON TokenResponse

getToken :: CookieJar -> IO TokenResponse
getToken cookies = responseBody <$> runReq defaultHttpConfig request
  where
    request = req GET url NoReqBody jsonResponse (cookieJar cookies)

    url = https "campus.studium.kit.edu" /: "token.php"

data Product = Product
  { namede :: String
  } deriving (Show, Generic)

instance FromJSON Product

-- "product" refers to the degree program, e.g. Bachelor of X
getProducts :: CookieJar -> TokenResponse -> IO (CookieJar, Map String Product)
getProducts cookies token = do
  putStrLn "Fetching products..."
  res <- runReq defaultHttpConfig request
  return (responseCookieJar res, responseBody res)
  where
    request = req GET url NoReqBody jsonResponse (cookieJar cookies <> query)
    
    url = https "campus.kit.edu" /: "sp" /: "server" /: "services" /: "kit" /: "products.asp"
    query = "token" =: tokenA token

getContractView cookies product term = do
  putStrLn "Expecting contract view authentication form..."
  res <- runReq defaultHttpConfig request

  let doc = parseHtml $ UTF8.toString $ responseBody res
  [relayState]   <- runX $ doc >>> css "input[name=RelayState]" ! "value"
  [samlResponse] <- runX $ doc >>> css "input[name=SAMLResponse]" ! "value"

  return (responseCookieJar res, relayState, samlResponse)
  where
    request = req GET url NoReqBody bsResponse (cookieJar cookies <> query)

    url = https "campus.studium.kit.edu" /: "redirect.php"
    query = mconcat
      [ "system" =: ("campus" :: String)
      , "page"   =: ("/exams/registration.php" :: String)
      , "lang"   =: ("de" :: String)
      , "url"    =: (redirectionTarget :: String)
      ]
    redirectionTarget = "https://campus.kit.edu/sp/campus/student/contractview.asp?gguid=" ++ product ++ "&tguid=" ++ term ++ "&pguid=" ++ product ++ "&lang=de"

completeCvRedirect :: (CookieJar, String, String) -> IO (CookieJar, String)
completeCvRedirect (cookies, relayState, samlResponse) = do
  putStrLn "Completing contract view auth..."
  res <- runReq defaultHttpConfig request
  return (responseCookieJar res, UTF8.toString $ responseBody res)
  where
    request = req POST url (ReqBodyUrlEnc formValues) bsResponse (cookieJar cookies)

    url = https "campus.kit.edu" /: "sp" /: "Shibboleth.sso" /: "SAML2" /: "POST"
    formValues = mconcat
      [ "RelayState"   =: relayState
      , "SAMLResponse" =: samlResponse
      ]

data Grade = NumberGrade Float | Passed | NotPassed | Missing deriving (Show)

decodeGrade "be" = Passed
decodeGrade "nb" = NotPassed
decodeGrade _ = error "not implemented"

extractAndPrintGrades :: String -> IO ()
extractAndPrintGrades body = do
  let doc = parseHtml body

  grades <- runX $ doc >>> gradesArrow

  mapM_ printGrade grades
  where
    printGrade (title, grade) = putStrLn $ maybe "Missing" id grade ++ "\t" ++ title

    gradesArrow = css "tr.brick" >>> (titleArrow &&& gradeArrow)
    titleArrow = css "a[title]" ! "title"
    -- gradeArrow = css "td.nowrap" >>> ifA (this /> getText) (this /> getText >>> arr decodeGrade) (arr $ const Missing)
    gradeArrow = css "td.nowrap" >>> ifA (this /> getText) (this /> getText >>> arr Just) (arr $ const Nothing)
