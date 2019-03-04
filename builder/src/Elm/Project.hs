{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Elm.Project
  ( getRoot
  , getRootWithReplFallback
  , compile
  , compileForRepl
  , generateDocs
  )
  where


import qualified Data.ByteString as BS
import Data.Map ((!))
import System.FilePath ((</>))

import qualified Elm.Compiler as Compiler
import qualified Elm.Docs as Docs
import qualified Elm.Name as N
import qualified Elm.Project.Json as Project
import qualified Elm.Project.Root as Root
import qualified Elm.Project.Summary as Summary
import Elm.Project.Summary (Summary)
import qualified File.Args as Args
import qualified File.Artifacts as Artifacts
import qualified File.Compile as Compile
import qualified File.Crawl as Crawl
import qualified File.Plan as Plan
import qualified Generate.Output as Output
import qualified Reporting.Render.Type.Localizer as L
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Path

import Debug.Trace
-- import qualified Deps.Verify as Verify
-- import qualified Elm.Compiler.Objects as Obj


-- import qualified Data.List as List
-- import qualified Data.Map as Map
-- import qualified AST.Optimized as Opt

-- GET ROOT


getRoot :: Task.Task Summary
getRoot =
  Root.get


getRootWithReplFallback :: IO FilePath
getRootWithReplFallback =
  Root.getWithReplFallback


-- -- Srinath

-- addGraph :: Compiler.Artifacts -> Obj.Graph -> Obj.Graph
-- addGraph (Compiler.Artifacts _ elmo _) graph =
--   Obj.union elmo graph


--   --
--   -- Srinath
--   --
--   -- This has to be called for our native kernel
--   -- this seems to actually contain the javascript implementation
--   --
--   -- This is called from install.hs which writes out the required
--   -- binary data like ifaces.dat and objs.dat


-- objGraphFromKernels :: Crawl.Result -> Obj.Graph
-- objGraphFromKernels (Crawl.Graph _ _ kernels _ _) =
--   Obj.fromKernels kernels


-- addKernels :: Crawl.Result -> Map Module.Raw Compiler.Artifacts -> Crawl.Result
-- addKernels results graph =
--   Map.foldr addGraph (objGraphFromKernels graph) results


-- -- End Srinath


-- COMPILE


-- dumpGraph graph =
--   List.intercalate "\n" (map Opt.toString (Map.keys graph))
  -- let
  --   dump =

  -- in
  -- trace ("dump: \n" ++ dump) $
--
-- Srinath
-- The Artifacts.write step is writting out elmi and elmo which seem to be valid
-- The emlo object has a reference to "elm kernel Hello $" which looks like what
-- we want. Should that read "author project Hello $"? But then the graph dump
-- didn't have any reference to that as well
--
-- Is the elmo object being read at all?? Does the kernel $ (whatever it means)
-- being read for local packages

compile
  :: Output.Mode
  -> Output.Target
  -> Maybe Output.Output
  -> Maybe FilePath
  -> Summary
  -> [FilePath]
  -> Task.Task ()
compile mode target maybeOutput docs summary@(Summary.Summary root project _ _ _) paths =
  do  Project.check project
      args <- Args.fromPaths summary paths
      graph <- trace "Crawl.crawl" $ Crawl.crawl summary args
      (dirty, ifaces) <- trace ("Plan.plan: \n\n" ++ Crawl.dumpGraph graph ++ "\n\n") $ Plan.plan docs summary graph
      answers <- trace "Compile.compile" $ Compile.compile project docs ifaces dirty
      results <- trace "Artifacts.write" $ Artifacts.write root answers
      _ <- traverse (Artifacts.writeDocs results) docs
      -- _ <- Verify.writeObjects graph results
      trace "Output.generate" $ Output.generate mode target maybeOutput summary graph results
      -- trace "Output.generate" $ Output.generate mode target maybeOutput summary (Verify.addKernels graph results) results



-- COMPILE FOR REPL


compileForRepl :: Bool -> L.Localizer -> BS.ByteString -> Maybe N.Name -> Task.Task (Maybe FilePath)
compileForRepl noColors localizer source maybeName =
  do  summary@(Summary.Summary root project _ _ _) <- getRoot
      Project.check project
      graph <- Crawl.crawlFromSource summary source
      (dirty, ifaces) <- Plan.plan Nothing summary graph
      answers <- Compile.compile project Nothing ifaces dirty
      results <- Artifacts.write root answers
      let (Compiler.Artifacts elmi _ _) = results ! N.replModule
      traverse (Output.generateReplFile noColors localizer summary graph elmi) maybeName



-- GENERATE DOCS


generateDocs :: Summary.Summary -> Task.Task Docs.Documentation
generateDocs summary@(Summary.Summary root project _ _ _) =
  do  let docsPath = root </> Path.docs
      args <- Args.fromSummary summary
      graph <- Crawl.crawl summary args
      (dirty, ifaces) <- Plan.plan (Just docsPath) summary graph
      answers <- Compile.compile project (Just docsPath) ifaces dirty
      results <- Artifacts.write root answers
      Output.noDebugUsesInPackage summary graph
      Artifacts.writeDocs results docsPath
