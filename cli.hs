#!/usr/bin/env stack
-- stack script --resolver lts-11.2 --package turtle
{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (FilePath)
import Turtle

main :: IO ()
main = do
    gitCheckout "master"
    scrapeFailures
    buildFrontend
    deployToGhPages


gitCheckout :: Text -> IO ()
gitCheckout branch =
    procs "git" ["checkout", branch] empty


gitAdd :: [FilePath] -> IO ()
gitAdd files =
    procs "git" ("add" : fmap (format fp) files) empty


buildFrontend :: IO ()
buildFrontend =
    with (pushd "frontend") $ \() -> do
        shells "elm make --yes --warn Main.elm --output=dist/js/elm.js" empty
        addGeneratedOnInfo
        minifyJs


addGeneratedOnInfo :: IO ()
addGeneratedOnInfo = do
    d <- today
    inplace (const d <$> text "GENERATED_ON_PLACEHOLDER") "dist/js/elm.js"


today :: IO Text
today = fmap lineToText . single $ inshell "date +%F" empty


minifyJs :: IO ()
minifyJs = do
    maybeUglifyPath <- which "uglifyjs"
    case maybeUglifyPath of
        Nothing -> die "uglifyjs is not installed. You can install it 'sudo npm -global install uglify-js'"
        Just uglifyPath -> shells "uglifyjs dist/js/elm.js --compress --mangle --output dist/js/elm.min.js" empty


scrapeFailures :: IO ()
scrapeFailures =
    with (pushd "scraper") $ \() -> do
        shells "mvn clean compile assembly:single" empty
        ensureChromedriverExists
        shells "java -jar target/scraper.jar" empty


ensureChromedriverExists :: IO ()
ensureChromedriverExists = do
    exists <- testfile "chromedriver"
    unless exists $ do
      shells "wget --quiet --show-progress https://chromedriver.storage.googleapis.com/2.37/chromedriver_linux64.zip" empty
      shells "unzip chromedriver_linux64.zip && rm chromedriver_linux64.zip" empty


deployToGhPages :: IO ()
deployToGhPages =
    with (mktempdir "/tmp" "random-failures") $ \tmpdir -> do
        cptree "frontend/dist" tmpdir
        gitCheckout "gh-pages"
        pwd >>= cptree tmpdir
        gitAdd ["failures.json", "js/elm.min.js"]