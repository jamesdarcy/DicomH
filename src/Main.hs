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

module Main where

import Data.Dicom
import Data.Dicom.Show
import Data.Dicom.Io

main :: IO ()
main = do
  putStrLn "*** DicomH v0.1 ***"
--  let fileName = "E:\\User\\Plootarg\\DicomData\\MR\\DicomH\\impvrle.dcm"
--  let fileName = "E:\\User\\Plootarg\\DicomData\\MR\\DicomH\\impvrle_nopreamble.dcm"
--  let fileName = "E:\\User\\Plootarg\\DicomData\\MR\\DicomH\\expvrle.dcm"
--  let fileName = "E:\\User\\Plootarg\\DicomData\\MR\\DicomH\\expvrbe.dcm"
--  let fileName = "E:\\User\\Plootarg\\DicomData\\MR\\DicomH\\multiframe.dcm"
--  let fileName = "E:\\User\\Plootarg\\DicomData\\Osirix\\CEREBRIX\\Neuro Crane\\_MPR Range[1]_ - 18\\IM-0001-0001.dcm"
  let fileName = "E:\\User\\Plootarg\\DicomData\\MR\\DicomH\\cerebrix.j2k"
--  let fileName = "D:\\DicomData\\MR\\DicomH\\impvrle.dcm"
--  let fileName = "D:\\DicomData\\MR\\DicomH\\impvrle_nopreamble.dcm"
--  let fileName = "D:\\DicomData\\MR\\DicomH\\expvrle.dcm"
--  let fileName = "D:\\DicomData\\MR\\DicomH\\expvrbe.dcm"
--  let fileName = "D:\\DicomData\\MR\\DicomH\\multiframe.dcm"
  putStrLn $ " - File: \"" ++ fileName ++ "\""

--  dicom <- readDicomFile fileName
--  print dicom

  j2k <- readJpegFile fileName
  print j2k 