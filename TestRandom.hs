{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

#define CRYPTO

import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (Exception, SomeException, handle, catch, throwIO, try)
import Data.ByteString (ByteString)
import Data.List (intercalate, unfoldr)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Numeric (showInt, showHex)
import System.IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as WB
import qualified Data.Map as Map

#ifndef linux_HOST_OS
import Hypervisor.Console (initXenConsole)
#endif

#ifdef CRYPTO
import Crypto.Random (getRandomBytes)
#endif

showSize :: Integral n => n
showSize = 128

sampleSize :: Integral n => n
sampleSize = round $ 2^10

main :: IO ()
main = do
#ifndef linux_HOST_OS
    initXenConsole
#endif
    putStrLn "Domain started"
    res1 <- uRandomBytes sampleSize
    putStrLn $ case res1 of
        Left  e -> "Reading /dev/urandom failed: " ++ e
        Right b -> "Successfully read from /dev/urandom:\n"
                ++ prettyHex (B.take showSize b) ++ "\n"
                ++ "Entropy: " ++ show (entropy b) ++ " / 8\n"
#ifdef CRYPTO
    res2 <- cryptoRandomBytes sampleSize
    putStrLn $ case res2 of
        Left  e -> "Getting random bytes from cryptonite failed: " ++ e
        Right b -> "Successfully got random bytes from cryptonite:\n"
                ++ prettyHex (B.take showSize b) ++ "\n"
                ++ "Entropy: " ++ show (entropy b) ++ " / 8\n"
#endif
    tryIOFunctions
#ifdef THREADED
    threadedAccess 100
#endif
    threadDelay (1000000 * 20)

uRandomBytes :: Int -> IO (Either String ByteString)
uRandomBytes n = handle showE $
    withBinaryFile "/dev/urandom" ReadMode $ \h -> Right <$> B.hGetSome h n

tryIOFunctions :: IO ()
tryIOFunctions = do
    withAction "openBinaryFile" (openBinaryFile "/dev/urandom" ReadMode) $ \h -> do
        tryAction' "B.hGet"            $ B.hGet h 10
        tryAction' "B.hGetNonBlocking" $ B.hGetNonBlocking h 10
        tryAction' "B.hGetSome"        $ B.hGetSome h 10
        tryAction' "hFileSize"         $ hFileSize h
        tryAction' "hLookAhead"        $ hLookAhead h
        tryAction' "hGetChar"          $ hGetChar h
        tryAction' "hGetEcho"          $ hGetEcho h
        tryAction' "hGetEncoding"      $ hGetEncoding h
        tryAction' "hGetPosn"          $ hGetPosn h
        tryAction' "hIsClosed"         $ hIsClosed h
        tryAction' "hIsEOF"            $ hIsEOF h
        tryAction' "hIsOpen"           $ hIsOpen h
        tryAction' "hIsReadable"       $ hIsReadable h
        tryAction' "hIsSeekable"       $ hIsSeekable h
        tryAction' "hIsTerminalDevice" $ hIsTerminalDevice h
        tryAction' "hIsWritable"       $ hIsWritable h
        tryAction' "hReady"            $ hReady h
        tryAction' "hShow"             $ hShow h
        tryAction  "hClose"            $ hClose h
    where
    showFailure  s e   = putStrLn $ s ++ ": " ++ show (e::SomeException)
    showSuccess  s _   = putStrLn $ s ++ " succeeded!"
    showSuccess' s r   = putStrLn (s ++ " succeeded! --> " ++ show r)
    tryAction    s m   = try m >>= either (showFailure s) (showSuccess s)
    tryAction'   s m   = try m >>= either (showFailure s) (showSuccess' s)
    withAction   s m f = try m >>= either (showFailure s) (\r -> showSuccess s r >> f r)

threadedAccess :: Int -> IO ()
threadedAccess n = flip mapM_ [1..n]
    $ \n -> forkIO . withBinaryFile "/dev/urandom" ReadMode
        $ \h -> flip mapM_ [1..10] $ \_ -> do
            e <- uRandomBytes 65
            case e of
                Right bs | B.length bs == 65 -> do
                    putStrLn $ "Success! Thread " ++ show n ++ " got "
                                                  ++ show (B.length bs) ++ " bytes: "
                                                  ++ show (B.take 10 bs) ++ "..."
                    threadDelay . fromIntegral $ WB.head bs
                Right bs -> do
                    putStrLn $ "Problem! Thread " ++ show n ++ " got "
                                                  ++ show (B.length bs) ++ " bytes"
                    threadDelay $ if not (B.null bs)
                        then fromIntegral $ WB.head bs
                        else 500000
                Left s -> do
                    putStrLn $ "Problem! " ++ s
                    threadDelay 500000

#ifdef CRYPTO
cryptoRandomBytes :: Int -> IO (Either String ByteString)
cryptoRandomBytes n = handle showE $ Right <$> getRandomBytes n
#endif

showE :: SomeException -> IO (Either String a)
showE = return . Left . show

chunksOf :: Integral n => n -> [a] -> [[a]]
chunksOf n = unfoldr f
    where
    f [] = Nothing
    f l  = Just $ splitAt (fromIntegral n) l

prettyHex :: B.ByteString -> String
prettyHex = pp . chunksOf 4 . chunksOf 4 . WB.unpack
    where
    ph = (\s -> if length s == 1 then '0':s else s) . flip showHex ""
    pp = intercalate "\n" . fmap (intercalate " " . fmap (intercalate "" . fmap ph))

entropy :: B.ByteString -> Double
entropy bs = entropy' . Map.elems . counts $ WB.unpack bs
    where
    counts         = foldr incr Map.empty
    incr           = Map.alter incr'
    incr' Nothing  = Just 1
    incr' (Just n) = Just (n+1)
    h d n          = if n == 0 then 0 else (n/d) * logBase 2 (d/n)
    entropy' cs    = sum $ fmap (h $ sum cs) cs
