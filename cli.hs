#!/usr/bin/env stack
-- stack script --resolver lts-13.5 --package turtle
{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (isNothing)
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
        minifyJs


today :: IO Text
today = fmap lineToText . single $ inshell "date +%F" empty


minifyJs :: IO ()
minifyJs = do
    maybeUglifyPath <- which "uglifyjs"
    when (isNothing maybeUglifyPath) installUglifyjs
    shells "uglifyjs dist/js/elm.js --compress 'pure_funcs=\"F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9\",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=dist/js/elm.js" empty

installUglifyjs :: IO ()
installUglifyjs = do
    let command = "sudo npm install uglify-js --global"
    echo . unsafeTextToLine $ "uglifyjs is not installed. Installing it using: " <> command
    shells command empty


scrapeFailures :: IO ()
scrapeFailures =
    with (pushd "scraper") $ \() -> do
        shells "stack build" empty
        shells "stack exec scraper -- scrape --jenkins-directory=https://rhba-jenkins.rhev-ci-vms.eng.rdu2.redhat.com/job/KIE/job/master/job/pullrequest --output=new-failures.json" empty
        shells "git checkout gh-pages ../failures.json" empty
        shells "stack exec scraper -- merge  --kiegroup-dir=/home/jhrcek/Devel/github.com/jhrcek/kiegroup-all --archived-report=../failures.json --new-report=new-failures.json --output=../frontend/dist/failures.json" empty


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
