module Main where

import RIO
import Test.Hspec

import Core
import qualified Docker
import qualified Runner

import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.ByteString as ByteString
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial

import qualified Data.Yaml as Yaml
import qualified System.Process.Typed as Process

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker

  beforeAll cleanupDocker $ describe "Quad CI" do
    it "should run a build (success)" do
      testRunSuccess runner
    it "should run a build (failure)" do
      testRunFailure runner
    it "should share workspace between steps" do
      testSharedWorkspace runner
    it "should collect logs" do
      testLogCollection runner
    it "should pull images" do
      testImagePull runner
    it "should decode pipelines" do
      testYamlDecoding runner

cleanupDocker :: IO ()
cleanupDocker = void do
  Process.readProcessStdout "docker container ls -aq --filter \"label=quad\" | xargs docker rm -f"
  Process.readProcessStdout "docker volume ls --filter \"label=quad\" | xargs docker volume rm -f"

-- Helpers
makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands
  = Step
      { name = StepName name
      , image = Docker.Image { name = image, tag = "latest" }
      , commands = NonEmpty.Partial.fromList commands
      }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline { steps = NonEmpty.Partial.fromList steps }

emptyHooks :: Runner.Hooks
emptyHooks =
  Runner.Hooks
    { logCollected = \_ -> pure ()
    }

-- First test
testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <- runner.prepareBuild $ makePipeline
             [ makeStep "First step" "ubuntu" ["date"]
             , makeStep "Second step" "ubuntu" ["uname -r"]
             ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <- runner.prepareBuild $ makePipeline
             [ makeStep "Should fail" "ubuntu" ["exit 1"]
             ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe` [StepFailed (Docker.ContainerExitCode 1)]

testSharedWorkspace :: Runner.Service -> IO ()
testSharedWorkspace runner = do
  build <- runner.prepareBuild $ makePipeline
             [ makeStep "Create file" "ubuntu" ["echo hello > test"]
             , makeStep "Read file" "ubuntu" ["cat test"]
             ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do
  expected <- newMVar $ Set.fromList ["hello", "world", "Linux"]
  let onLog :: Log -> IO ()
      onLog log = do
        remaining <- readMVar expected
        forM_ remaining $ \word -> do
          case ByteString.breakSubstring word log.output of
            (_, "") -> pure ()
            _ -> modifyMVar_ expected (pure . Set.delete word)

  let hooks = Runner.Hooks { logCollected = onLog }

  build <- runner.prepareBuild $ makePipeline
             [ makeStep "Long step" "ubuntu" ["echo hello", "sleep 2", "echo world"]
             , makeStep "Echo Linux" "ubuntu" ["uname -s"]
             ]
  result <- runner.runBuild hooks build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

  readMVar expected >>= \logs -> logs `shouldBe` Set.empty

testImagePull :: Runner.Service -> IO ()
testImagePull runner = do
  Process.readProcessStdout "docker image rm -f busybox"

  build <- runner.prepareBuild $ makePipeline
             [ makeStep "First step" "busybox" ["date"]
             ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded]

testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
  pipeline <- Yaml.decodeFileThrow "test/pipeline.sample.yml"
  build <- runner.prepareBuild pipeline
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` BuildFinished BuildSucceeded
