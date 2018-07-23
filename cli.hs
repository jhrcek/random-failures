#!/usr/bin/env stack
-- stack script --resolver lts-12.2 --package turtle
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
    when (maybeUglifyPath == Nothing) installUglifyjs
    shells "uglifyjs dist/js/elm.js --compress --mangle --output dist/js/elm.min.js" empty


installUglifyjs :: IO ()
installUglifyjs = do
    let command = "sudo npm --global install uglify-js"
    echo . unsafeTextToLine $ "uglifyjs is not installed. Installing it using: " <> command
    shells command empty


scrapeFailures :: IO ()
scrapeFailures =
    with (pushd "scraper") $ \() ->
        shells "stack build && stack exec scraper" empty


deployToGhPages :: IO ()
deployToGhPages =
    with (mktempdir "/tmp" "random-failures") $ \tmpdir -> do
        cptree "frontend/dist" tmpdir
        gitCheckout "gh-pages"
        pwd >>= cptree tmpdir
        gitAdd ["failures.json", "js/elm.min.js"]
        date <- today
        procs "git" ["commit", "-m", "random failures report " <> date] empty
        shells "git push origin gh-pages" empty
        gitCheckout "master"
