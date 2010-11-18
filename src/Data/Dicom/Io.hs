{-
Copyright James d'Arcy 2010

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of James d'Arcy nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

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

        