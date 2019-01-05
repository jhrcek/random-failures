{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Commands.Merge (mergeReports)
import Commands.Scrape (scrapeFailures)
import Config (Command (..), parseCommand)

main :: IO ()
main =
    parseCommand >>= executeCommand

executeCommand :: Command -> IO ()
executeCommand command = case command of
    Scrape {jenkinsJobsFolder, outputReport} ->
        scrapeFailures jenkinsJobsFolder outputReport
    Merge {archivedReport, newReport, outputReport, kiegroupDir} ->
        mergeReports archivedReport newReport outputReport kiegroupDir
