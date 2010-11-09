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
  let fileName = "E:\\User\\Plootarg\\DicomData\\Osirix\\CEREBRIX\\Neuro Crane\\_MPR Range[1]_ - 18\\IM-0001-0001.dcm"
--  let fileName = "D:\\DicomData\\MR\\DicomH\\impvrle.dcm"
--  let fileName = "D:\\DicomData\\MR\\DicomH\\impvrle_nopreamble.dcm"
--  let fileName = "D:\\DicomData\\MR\\DicomH\\expvrle.dcm"
--  let fileName = "D:\\DicomData\\MR\\DicomH\\expvrbe.dcm"
--  let fileName = "D:\\DicomData\\MR\\DicomH\\multiframe.dcm"
  putStrLn $ " - File: \"" ++ fileName ++ "\""

  dicom <- readDicomFile fileName
  print dicom 