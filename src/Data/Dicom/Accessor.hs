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

module Data.Dicom.Accessor
  (
    -- * Accessors for 0x0002
    getTransferSyntaxUid,
    -- * Accessors for 0x0008
    getSopClassUid,
    getSopInstanceUid,
    getStudyDate,
    getSeriesDate,
    getModality,
    getStudyDescription,
    getSeriesDescription,
    -- * Accessors for 0x0010
    getPatientName,
    -- * Accessors for 0x0020
    getStudyInstanceUid,
    getSeriesInstanceUid,
    getStudyId,
    getSeriesNumber,
    -- * Accessors for 0x0028
    getNumberOfFrames,
    getRows,
    getColumns,
    -- * Accessors for 0x7fe0
    getPixelData
  ) where

import Data.Word
import Data.Int
import qualified Data.ByteString as B

import Data.Dicom
import Data.Dicom.Tag

-- | Group 0x0002 - Metadata
getTransferSyntaxUid :: DicomObject -> Maybe String
getTransferSyntaxUid = getString tRANSFER_SYNTAX_UID

-- | Group 0x0008
getSopClassUid :: DicomObject -> Maybe String
getSopClassUid = getString sOP_CLASS_UID

getSopInstanceUid :: DicomObject -> Maybe String
getSopInstanceUid = getString sOP_INSTANCE_UID

getStudyDate :: DicomObject -> Maybe String
getStudyDate = getString sTUDY_DATE

getSeriesDate :: DicomObject -> Maybe String
getSeriesDate = getString sERIES_DATE

getModality :: DicomObject -> Maybe String
getModality = getString mODALITY

getStudyDescription :: DicomObject -> Maybe String
getStudyDescription = getString sTUDY_DESCRIPTION

getSeriesDescription :: DicomObject -> Maybe String
getSeriesDescription = getString sERIES_DESCRIPTION

-- | Group 0x0010
getPatientName :: DicomObject -> Maybe String
getPatientName = getString pATIENT_NAME

-- | Group 0x0020
getStudyInstanceUid :: DicomObject -> Maybe String
getStudyInstanceUid = getString sTUDY_INSTANCE_UID

getSeriesInstanceUid :: DicomObject -> Maybe String
getSeriesInstanceUid = getString sERIES_INSTANCE_UID

getStudyId :: DicomObject -> Maybe String
getStudyId = getString sTUDY_ID

getSeriesNumber :: DicomObject -> Maybe String
getSeriesNumber = getString sERIES_NUMBER

-- | Group 0x0028
getNumberOfFrames :: DicomObject -> Maybe String
getNumberOfFrames = getString nUMBER_OF_FRAMES

getRows :: DicomObject -> Maybe Word16
getRows = getWord16 rOWS

getColumns :: DicomObject -> Maybe Word16
getColumns = getWord16 cOLUMNS

-- | Group 0x0028
getPixelData :: DicomObject -> Maybe B.ByteString
getPixelData = getBytes pIXEL_DATA

