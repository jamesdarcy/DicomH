module Data.Dicom.Io
  (
    readDicomFile
  ) where


import qualified Data.ByteString as BS
import Data.Binary.Strict.Get
import System.FilePath
import System.Directory (doesFileExist)

import Data.Dicom

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
