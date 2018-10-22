#!/usr/bin/env stack
-- stack script --resolver lts-12.14 --package turtle
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


buildFrontend :: IO ()
buildFrontend =
    with (pushd "frontend") $ \() -> do
        shells "elm make --optimize src/Main.elm --output=dist/js/elm.js" empty
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
    shells "uglifyjs dist/js/elm.js --compress 'pure_funcs=\"F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9\",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=dist/js/elm.js" empty

installUglifyjs :: IO ()
installUglifyjs = do
    let command = "sudo npm install uglify-js --global"
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
        shells "git clean -df" empty
        pwd >>= cptree tmpdir
        date <- today
        procs "git" ["commit", "--all", "-m", "random failures report " <> date] empty
        shells "git push origin gh-pages" empty
        gitCheckout "master"
