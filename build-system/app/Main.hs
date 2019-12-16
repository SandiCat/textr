module Main (main) where

import Development.Shake
import Development.Shake.FilePath

buildDir = "_build"
binaryDir = buildDir </> "bin"
exeName = "server"
finalExe = binaryDir </> exeName
backendDir = ".." </> "timesafe-backend"
buildSrcDir = "build-src"

projectName = "timesafe"
dockerImageName = projectName <> "-image"

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles="_build"} $ do
    -- want [finalExe]
    
    phony "run-docker-container" $ do
        need ["docker-image"]
        command_ [] "docker" ["run", "--rm", "-p", "8532:80", dockerImageName]

    phony "docker-image" $ do
        need [finalExe]
        command_ [] "docker" ["build", "-f", buildSrcDir </> "run.Dockerfile", "-t", dockerImageName, buildDir]

    finalExe %> \out -> do
        sourceFiles <- getDirectoryFiles "" [backendDir </> "//*.hs", backendDir </> "//*.yaml"]
        need sourceFiles
        command_ [Cwd backendDir] "stack" [{-"--docker",-} "build"]
        putQuiet "done building backend"
        Stdout stackLocalInstallOut <-
            command [Cwd backendDir] "stack" [{-"--docker",-} "path", "--local-install-root"]
        let stackLocalInstall = lines stackLocalInstallOut !! 0
        command_ [] "cp" [stackLocalInstall </> "bin" </> exeName, binaryDir]
