module Data.Dicom.Io
  (
    readDicomFile,
    readJpeg2000Fragment,
    readJpegFile
  ) where


import qualified Data.ByteString as BS
import Data.Binary.Strict.Get
import System.FilePath
import System.Directory (doesFileExist)

import Data.Dicom
import Data.Dicom.Jpeg2000

readDicomFile :: FilePath -> IO (Either String EncapDicomObject)
readDicomFile fileName = do
  validFile <- doesFileExist fileName
  if (not validFile)
    then return (Left $ "Non-existant file: " ++ fileName)
    else do
      fileBytes <- BS.readFile fileName
      let result = runGet getEncapDicomObject fileBytes
      case result of
        (Left errorMessage, _) -> return (Left errorMessage)
        (Right encap, _)       -> return (Right encap)

readJpeg2000Fragment :: BS.ByteString -> IO (Either String DicomJpeg2000)
readJpeg2000Fragment jpegBytes = do
  let result = runGet getJpeg2000 jpegBytes
  case result of
    (Left errorMessage, _) -> return (Left errorMessage)
    (Right jpeg2k, _)      -> return (Right jpeg2k)

readJpegFile :: FilePath -> IO (Either String DicomJpeg2000)
readJpegFile fileName = do
  validFile <- doesFileExist fileName
  if (not validFile)
    then return (Left $ "Non-existant file: " ++ fileName)
    else do
      fileBytes <- BS.readFile fileName
      let result = runGet getJpeg2000 fileBytes
      case result of
        (Left errorMessage, _) -> return (Left errorMessage)
        (Right j2k, _)         -> return (Right j2k)

        