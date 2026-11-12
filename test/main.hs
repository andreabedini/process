{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad (guard, unless, void, when)
import System.Exit
import System.IO.Error
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Process
import System.Process.Internals (withForkWait, ignoreSigPipe, translate)
import System.Process.CommunicationHandle
import qualified System.Process.CommunicationHandle.OsString as OC
import qualified System.Process.Internals.OsString as OI
import qualified System.Process.OsString as O
import Control.Concurrent
import Control.DeepSeq
import Data.Char (isDigit)
import Data.IORef
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Data.Word (Word8)
import System.IO (hClose, hFlush, openBinaryTempFile, hGetContents, hPutStr)
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as S8
import System.Directory (getTemporaryDirectory, removeFile, exeExtension)
import System.FilePath ((<.>))
import System.OsPath (OsString)
import qualified System.OsPath as OsPath
import GHC.Conc.Sync (getUncaughtExceptionHandler, setUncaughtExceptionHandler)

#if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem ((<!>))
#endif
#if !defined(mingw32_HOST_OS)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Array (peekArray0)
import Foreign.Ptr (nullPtr)
#endif

import Test.Paths ( processInternalExes )

ifWindows :: IO () -> IO ()
ifWindows action
  | not isWindows = return ()
  | otherwise     = action

isWindows :: Bool
#if defined(mingw32_HOST_OS)
isWindows = True
#else
isWindows = False
#endif

main :: IO ()
main = do
    testDoesNotExist
    testModifiers
    testSubdirectories
    testBinaryHandles
    testMultithreadedWait
    testInterruptMaskedWait
    testGetPid
    testReadProcess
    testInterruptWith
    testDoubleWait
    testKillDoubleWait
    testCreateProcess
    testAddChdir
    testAddChdir2
    testCommunicationHandle False
#if defined(__IO_MANAGER_WINIO__)
    -- With WinIO, also run the test with the child process using WinIO
    testCommunicationHandle True
#endif
    testOsStringReadProcess
    testOsStringShell
    testOsStringDoesNotExist
    testOsStringCallProcessFailure
    testOsStringShowCommandForUser
    testOsStringCwdAndEnv
    testOsStringBytes
    testOsStringOldFunctions
    testOsStringInternals
    testOsStringCommunicationHandle
    putStrLn ">>> Tests passed successfully"

run :: String -> IO () -> IO ()
run label test = do
    putStrLn $ ">>> Running: " ++ label
    test

testDoesNotExist :: IO ()
testDoesNotExist = run "non-existent executable" $ do
    res <- handle (return . Left . isDoesNotExistError) $ do
        (_, _, _, ph) <- createProcess (proc "definitelydoesnotexist" [])
            { close_fds = True
            }
        fmap Right $ waitForProcess ph
    case res of
        Left True -> return ()
        _ -> error $ show res

testModifiers :: IO ()
testModifiers = do
    let test name modifier = run ("modifier " ++ name) $ do
            (_, _, _, ph) <- createProcess
                $ modifier $ proc "echo" ["hello", "world"]
            ec <- waitForProcess ph
            unless (ec == ExitSuccess)
                $ error $ "echo returned: " ++ show ec

    test "vanilla" id

    -- FIXME need to debug this in the future on Windows
    unless isWindows $ test "detach_console" $ \cp -> cp { detach_console = True }

    test "create_new_console" $ \cp -> cp { create_new_console = True }
    test "new_session" $ \cp -> cp { new_session = True }

testSubdirectories :: IO ()
testSubdirectories = ifWindows $ run "subdirectories" $ do
    withCurrentDirectory "exes" $ do
      res1 <- readCreateProcess (proc "./echo.bat" []) ""
      unless ("parent" `isInfixOf` res1 && not ("child" `isInfixOf` res1)) $ error $
        "echo.bat with cwd failed: " ++ show res1

      res2 <- readCreateProcess (proc "./echo.bat" []) { cwd = Just "subdir" } ""
      unless ("child" `isInfixOf` res2 && not ("parent" `isInfixOf` res2)) $ error $
        "echo.bat with cwd failed: " ++ show res2

testBinaryHandles :: IO ()
testBinaryHandles = run "binary handles" $ do
    tmpDir <- getTemporaryDirectory
    bracket
      (openBinaryTempFile tmpDir "process-binary-test.bin")
      (\(fp, h) -> hClose h `finally` removeFile fp)
      $ \(fp, h) -> do
        let bs = S8.pack "hello\nthere\r\nworld\0"
        SBS.hPut h bs
        hClose h

        (Nothing, Just out, Nothing, ph) <- createProcess (proc "cat" [fp])
            { std_out = CreatePipe
            }
        res' <- SBS.hGetContents out
        hClose out
        ec <- waitForProcess ph
        unless (ec == ExitSuccess)
            $ error $ "Unexpected exit code " ++ show ec
        unless (bs == res')
            $ error $ "Unexpected result: " ++ show res'

testMultithreadedWait :: IO ()
testMultithreadedWait = run "multithreaded wait" $ do
    (_, _, _, p) <- createProcess (proc "sleep" ["0.1"])
    me1 <- newEmptyMVar
    _ <- forkIO . void $ waitForProcess p >>= putMVar me1
    -- check for race / deadlock between waitForProcess and getProcessExitCode
    e3 <- getProcessExitCode p
    e2 <- waitForProcess p
    e1 <- readMVar me1
    unless (isNothing e3)
          $ error $ "unexpected exit " ++ show e3
    unless (e1 == ExitSuccess && e2 == ExitSuccess)
          $ error "sleep exited with non-zero exit code!"

testInterruptMaskedWait :: IO ()
testInterruptMaskedWait = run "interrupt masked wait" $ do
    (_, _, _, p) <- createProcess (proc "sleep" ["1.0"])
    mec <- newEmptyMVar
    tid <- mask_ . forkIO $
        (waitForProcess p >>= putMVar mec . Just)
            `catchThreadKilled` putMVar mec Nothing
    killThread tid
    eec <- takeMVar mec
    case eec of
      Nothing -> return ()
      Just ec ->
        if isWindows
          then putStrLn "FIXME ignoring known failure on Windows"
          else error $ "waitForProcess not interrupted: sleep exited with " ++ show ec

testGetPid :: IO ()
testGetPid = run "getPid" $ do
    (_, Just out, _, p) <-
      if isWindows
        then createProcess $ (proc "sh" ["-c", "z=$$; cat /proc/$z/winpid"]) {std_out = CreatePipe}
        else createProcess $ (proc "sh" ["-c", "echo $$"]) {std_out = CreatePipe}
    pid <- getPid p
    line <- hGetContents out
    putStrLn $ " queried PID: " ++ show pid
    putStrLn $ " PID reported by stdout: " ++ show line
    _ <- waitForProcess p
    hClose out
    let numStdoutPid = read (takeWhile isDigit line) :: Pid
    unless (Just numStdoutPid == pid) $
      if isWindows
        then putStrLn "FIXME ignoring known failure on Windows"
        else error "subprocess reported unexpected PID"

testReadProcess :: IO ()
testReadProcess = run "readProcess" $ do
    output <- readProcess "echo" ["hello", "world"] ""
    unless (output == "hello world\n") $
        error $ "unexpected output, got: " ++ output

-- | Test that withCreateProcess doesn't throw exceptions besides
-- the expected UserInterrupt when the child process is interrupted
-- by Ctrl-C.
testInterruptWith :: IO ()
testInterruptWith = unless isWindows $ run "interrupt withCreateProcess" $ do
    mpid <- newEmptyMVar
    forkIO $ do
        pid <- takeMVar mpid
        void $ readProcess "kill" ["-INT", show pid] ""

    -- collect unhandled exceptions in any threads (specifically
    -- the asynchronous 'waitForProcess' call from 'cleanupProcess')
    es <- collectExceptions $ do
        let sleep = (proc "sleep" ["10"]) { delegate_ctlc = True }
        res <- try $ withCreateProcess sleep $ \_ _ _ p -> do
            Just pid <- getPid p
            putMVar mpid pid
            waitForProcess p
        unless (res == Left UserInterrupt) $
            error $ "expected UserInterrupt, got " ++ show res

    unless (null es) $
        error $ "uncaught exceptions: " ++ show es

  where
    collectExceptions action = do
        oldHandler <- getUncaughtExceptionHandler
        flip finally (setUncaughtExceptionHandler oldHandler) $ do
            exceptions <- newIORef ([] :: [SomeException])
            setUncaughtExceptionHandler (\e -> atomicModifyIORef exceptions $ \es -> (e:es, ()))
            action
            threadDelay 1000 -- give some time for threads to finish
            readIORef exceptions

-- Test that we can wait without exception twice, if the process exited on its own.
testDoubleWait :: IO ()
testDoubleWait = run "run process, then wait twice" $ do
    let sleep = (proc "sleep" ["0"])
    (_, _, _, p) <- createProcess sleep
    res <- try $ waitForProcess p
    case res of
        Left e -> error $ "waitForProcess threw: " ++ show (e :: SomeException)
        Right ExitSuccess -> return ()
        Right exitCode -> error $ "unexpected exit code: " ++ show exitCode

    res2 <- try $ waitForProcess p
    case res2 of
        Left e -> error $ "second waitForProcess threw: " ++ show (e :: SomeException)
        Right ExitSuccess -> return ()
        Right exitCode -> error $ "unexpected exit code: " ++ show exitCode

-- Test that we can wait without exception twice, if the process was killed.
testKillDoubleWait :: IO ()
testKillDoubleWait = unless isWindows $ do
    run "terminate process, then wait twice (delegate_ctlc = False)" $ runTest "TERM" False
    run "terminate process, then wait twice (delegate_ctlc = True)" $ runTest "TERM" True
    run "interrupt process, then wait twice (delegate_ctlc = False)" $ runTest "INT" False
    run "interrupt process, then wait twice (delegate_ctlc = True)" $ runTest "INT" True
  where
    runTest sig delegate = do
        let sleep = (proc "sleep" ["10"])
        (_, _, _, p) <- createProcess sleep { delegate_ctlc = delegate }
        Just pid <- getPid p
        void $ readProcess "kill" ["-" ++ sig, show pid] ""

        res <- try $ waitForProcess p
        checkFirst sig delegate res

        res' <- try $ waitForProcess p
        checkSecond sig delegate res'

    checkFirst :: String -> Bool -> Either SomeException ExitCode -> IO ()
    checkFirst sig delegate res = case (sig, delegate) of
        ("INT", True) -> case res of
            Left e -> case fromException e of
                Just UserInterrupt -> putStrLn "result ok"
                Nothing -> error $ "expected UserInterrupt, got  " ++ show e
            Right _ -> error $ "expected exception, got " ++ show res
        _ -> case res of
            Left e -> error $ "waitForProcess threw: " ++ show e
            Right ExitSuccess -> error "expected failure"
            _ -> putStrLn "result ok"

    checkSecond :: String -> Bool -> Either SomeException ExitCode -> IO ()
    checkSecond sig delegate res = case (sig, delegate) of
        ("INT", True) -> checkFirst "INT" False res
        _ -> checkFirst sig delegate res

-- Test that createProcess doesn't segfault on Mac with a cwd of Nothing
testCreateProcess :: IO ()
testCreateProcess = run "createProcess with cwd = Nothing" $ do
    let env = CreateProcess
            { child_group = Nothing
            , child_user = Nothing
            , close_fds = False
            , cmdspec = RawCommand "env" []
            , create_group = True
            , create_new_console = False
            , cwd = Nothing
            , delegate_ctlc = False
            , detach_console = False
            , env = Just [("PATH", "/bin:/usr/bin")]
            , new_session = False
            , std_err = Inherit
            , std_in = Inherit
            , std_out = Inherit
            , use_process_jobs = False
            }
    (_, _, _, p) <- createProcess env
    res <- try $ waitForProcess p
    case res of
        Left e -> error $ "waitForProcess threw: " ++ show (e :: SomeException)
        Right ExitSuccess -> return ()
        Right exitCode -> error $ "unexpected exit code: " ++ show exitCode

testCommunicationHandle :: Bool -> IO ()
testCommunicationHandle childUsesWinIO = do
  parentUsesWinIO <-
    return False
#if defined(__IO_MANAGER_WINIO__)
      <!> return True
#endif
  putStr $ unlines
    [ "testCommunicationHandle {"
    , "parentUsesWinIO: " ++ show parentUsesWinIO
    ]
  -- Workaround for Cabal bug #9854 (cli-child executable not in PATH).
  let cliChild =
        case lookup "cli-child" processInternalExes of
          Just cliChildPath -> cliChildPath
          Nothing -> "cli-child" <.> exeExtension
  (ex, output) <-
    readCreateProcessWithExitCodeCommunicationHandle
      (\(chTheyRead, chTheyWrite) ->
        let args = [show chTheyRead, show chTheyWrite]
                    ++ if childUsesWinIO
                       then ["+RTS", "--io-manager=native", "-RTS"]
                       else []
        in proc cliChild args)
      hGetContents
      (`hPutStr` "hello")
  case ex of
    ExitSuccess ->
      if output == "olleh123"
      then return ()
      else error $ "testCommunicationHandle: unexpected output " ++ show output
    ExitFailure {} ->
      error $ "testCommunicationHandle: child exited with exception " ++ show ex
  putStrLn "testCommunicationHandle }"

testAddChdir :: IO ()
testAddChdir = run "testAddChdir" $ handleIOErr $ do
  (_, _, _, commhand) <-
      runInteractiveProcess "true" [] (Just "/no/such/dir") Nothing
  exitCode <- waitForProcess commhand
  print exitCode
  where handleIOErr a = a `catchIOError` \e -> putStrLn ("Exc: " ++ show (ioeGetErrorType e))

testAddChdir2 :: IO ()
testAddChdir2 = run "testAddChdir2" $ handleIOErr $ do
  commhand <- runProcess "true" [] (Just "/no/such/dir") Nothing
                Nothing Nothing Nothing
  exitCode <- waitForProcess commhand
  print exitCode
  where handleIOErr a = a `catchIOError` \e -> putStrLn ("Exc: " ++ show (ioeGetErrorType e))

-- | An 'OsString' from a string literal that has only ASCII characters.
ascii :: String -> OsString
ascii = OsPath.pack . map OsPath.unsafeFromChar

testOsStringReadProcess :: IO ()
testOsStringReadProcess = run "OsString: readProcess" $ do
    output <- O.readProcess (ascii "echo") [ascii "hello", ascii "world"] ""
    unless (output == "hello world\n") $
        error $ "unexpected output, got: " ++ output

    (ec, out, err) <-
      O.readProcessWithExitCode (ascii "echo") [ascii "hello", ascii "world"] ""
    unless ((ec, out, err) == (ExitSuccess, "hello world\n", "")) $
        error $ "unexpected result, got: " ++ show (ec, out, err)

testOsStringShell :: IO ()
testOsStringShell = run "OsString: shell" $ do
    output <- O.readCreateProcess (O.shell (ascii "echo hello")) ""
    -- The shell of Windows ends the line in a different way.
    unless (takeWhile (\c -> c /= '\r' && c /= '\n') output == "hello") $
        error $ "unexpected output, got: " ++ show output

testOsStringDoesNotExist :: IO ()
testOsStringDoesNotExist = run "OsString: non-existent executable" $ do
    res <- try $ do
        (_, _, _, ph) <- O.createProcess (O.proc (ascii "definitelydoesnotexist") [])
            { O.close_fds = True
            }
        waitForProcess ph
    case res of
        Left e
          | isDoesNotExistError e
          , ioeGetFileName e == Just "definitelydoesnotexist" -> return ()
        _ -> error $ show res

testOsStringCallProcessFailure :: IO ()
testOsStringCallProcessFailure = run "OsString: callProcess failure" $ do
    res <- try $ O.callProcess (ascii "sh") [ascii "-c", ascii "exit 3"]
    case res of
        Left e
          | ioeGetLocation e == "callProcess: sh \"-c\" \"exit 3\" (exit 3)" -> return ()
        _ -> error $ show res

testOsStringShowCommandForUser :: IO ()
testOsStringShowCommandForUser = run "OsString: showCommandForUser" $ do
    let cmd = "echo"
        args = ["hello world", "it's", ""]
    -- The result is the one of System.Process, encoded.
    expected <- OsPath.encodeUtf (showCommandForUser cmd args)
    let actual = O.showCommandForUser (ascii cmd) (map ascii args)
    unless (actual == expected) $
        error $ "unexpected result, got: " ++ show actual

testOsStringCwdAndEnv :: IO ()
testOsStringCwdAndEnv = unless isWindows $ run "OsString: cwd and env" $ do
    output <- O.readCreateProcess
        (O.proc (ascii "/bin/sh") [ascii "-c", ascii "pwd; printf '%s' \"$PROCESS_TEST_VAR\""])
            { O.cwd = Just (ascii "/")
            , O.env = Just [(ascii "PROCESS_TEST_VAR", ascii "hello")]
            }
        ""
    unless (output == "/\nhello") $
        error $ "unexpected output, got: " ++ show output

-- | Test that on POSIX the new process gets the bytes of an 'OsString' as they
-- are, even when they are not valid in the encoding of the locale.
testOsStringBytes :: IO ()
testOsStringBytes = unless isWindows $ run "OsString: bytes that are not valid text" $ do
    let bytes :: [Word8]
        bytes = [0x66, 0xff, 0xfe, 0x20, 0xc3, 0x28, 0x6f]
        raw = OsPath.pack (map (OsPath.unsafeFromChar . toEnum . fromIntegral) bytes)
        expected = SBS.pack bytes

        readBytes cp = do
            (_, Just out, _, ph) <- O.createProcess cp { O.std_out = CreatePipe }
            res <- SBS.hGetContents out
            hClose out
            ec <- waitForProcess ph
            unless (ec == ExitSuccess)
                $ error $ "Unexpected exit code " ++ show ec
            return res

        check what res = unless (res == expected) $
            error $ what ++ ": unexpected output, got: " ++ show res

    -- as an argument
    readBytes (O.proc (ascii "/bin/sh") [ascii "-c", ascii "printf '%s' \"$1\"", ascii "sh", raw])
      >>= check "argument"

    -- in the environment
    readBytes (O.proc (ascii "/bin/sh") [ascii "-c", ascii "printf '%s' \"$PROCESS_TEST_VAR\""])
                { O.env = Just [(ascii "PROCESS_TEST_VAR", raw)] }
      >>= check "environment"

    -- in a shell command, quoted by showCommandForUser
    readBytes (O.shell (O.showCommandForUser (ascii "printf") [ascii "%s", raw]))
      >>= check "shell command"

testOsStringOldFunctions :: IO ()
testOsStringOldFunctions = do
    run "OsString: rawSystem" $ do
        ec <- O.rawSystem (ascii "echo") [ascii "hello", ascii "world"]
        expectExit ExitSuccess ec

    run "OsString: system" $ do
        ec <- O.system (ascii "exit 3")
        expectExit (ExitFailure 3) ec

        res <- try $ O.system mempty
        case res of
            Left e | show (e :: IOError) == "system: invalid argument (null command)" -> return ()
            _ -> error $ show res

    run "OsString: runCommand" $ do
        ec <- O.runCommand (ascii "exit 0") >>= waitForProcess
        expectExit ExitSuccess ec

    run "OsString: runProcess" $ do
        ec <- O.runProcess (ascii "echo") [ascii "hello", ascii "world"]
                  Nothing Nothing Nothing Nothing Nothing
                >>= waitForProcess
        expectExit ExitSuccess ec

    run "OsString: runInteractiveProcess" $ do
        r <- O.runInteractiveProcess (ascii "echo") [ascii "hello", ascii "world"]
                 Nothing Nothing
        output <- interact' r
        unless (output == "hello world\n") $
            error $ "unexpected output, got: " ++ show output

    run "OsString: runInteractiveCommand" $ do
        r <- O.runInteractiveCommand (ascii "echo hello")
        output <- interact' r
        -- The shell of Windows ends the line in a different way.
        unless (takeWhile (\c -> c /= '\r' && c /= '\n') output == "hello") $
            error $ "unexpected output, got: " ++ show output
  where
    expectExit expected ec = unless (ec == expected) $
        error $ "unexpected exit code: " ++ show ec

    interact' (inp, out, err, ph) = do
        hClose inp
        output <- hGetContents out
        evaluate (rnf output)
        ec <- waitForProcess ph
        hClose out
        hClose err
        expectExit ExitSuccess ec
        return output

testOsStringInternals :: IO ()
testOsStringInternals = do
    run "OsString: Internals.translate" $ do
        let arg = "hello world"
        -- The result is the one of System.Process.Internals, encoded.
        expected <- OsPath.encodeUtf (translate arg)
        let actual = OI.translate (ascii arg)
        unless (actual == expected) $
            error $ "unexpected result, got: " ++ show actual

    run "OsString: Internals.withFilePathException" $ do
        res <- try $ OI.withFilePathException (ascii "some-file") $
                   ioError (userError "failed")
        case res of
            Left e | ioeGetFileName e == Just "some-file" -> return ()
            _ -> error $ show (res :: Either IOError ())

    run "OsString: Internals.createProcess_" $ do
        (_, _, _, ph) <- OI.createProcess_ "test" (O.proc (ascii "echo") [ascii "hello"])
        ec <- waitForProcess ph
        unless (ec == ExitSuccess) $
            error $ "unexpected exit code: " ++ show ec

#if !defined(mingw32_HOST_OS)
    -- On Windows the environment is one block of wide characters.
    run "OsString: Internals.withCEnvironment" $ do
        vars <- OI.withCEnvironment [(ascii "A", ascii "b"), (ascii "C", mempty)] $ \p ->
                    peekArray0 nullPtr p >>= mapM peekCString
        unless (vars == ["A=b", "C="]) $
            error $ "unexpected result, got: " ++ show vars
#endif

testOsStringCommunicationHandle :: IO ()
testOsStringCommunicationHandle = run "OsString: CommunicationHandle" $ do
  -- Workaround for Cabal bug #9854 (cli-child executable not in PATH).
  cliChild <- OsPath.encodeFS $
        case lookup "cli-child" processInternalExes of
          Just cliChildPath -> cliChildPath
          Nothing -> "cli-child" <.> exeExtension
  (ex, output) <-
    OC.readCreateProcessWithExitCodeCommunicationHandle
      (\(chTheyRead, chTheyWrite) ->
        O.proc cliChild [ascii (show chTheyRead), ascii (show chTheyWrite)])
      hGetContents
      (`hPutStr` "hello")
  case ex of
    ExitSuccess ->
      if output == "olleh123"
      then return ()
      else error $ "unexpected output " ++ show output
    ExitFailure {} ->
      error $ "child exited with exception " ++ show ex

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory new inner = do
  orig <- getCurrentDirectory
  bracket_
    (setCurrentDirectory new)
    (setCurrentDirectory orig)
    inner

catchThreadKilled :: IO a -> IO a -> IO a
catchThreadKilled f g = catchJust (\e -> guard (e == ThreadKilled)) f (\() -> g)
