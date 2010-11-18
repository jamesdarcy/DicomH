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

module Data.Dicom.Tag where

import Data.Word (Word32)

type DicomTag = Word32

-- | Group 0x0002 - Metadata
tRANSFER_SYNTAX_UID :: DicomTag
tRANSFER_SYNTAX_UID = 0x00020010

-- | Group 0x0008
sOP_CLASS_UID :: DicomTag
sOP_CLASS_UID = 0x00080016

sOP_INSTANCE_UID :: DicomTag
sOP_INSTANCE_UID = 0x00080018

sTUDY_DATE :: DicomTag
sTUDY_DATE = 0x00080020

sERIES_DATE :: DicomTag
sERIES_DATE = 0x00080021

mODALITY :: DicomTag
mODALITY = 0x00080060

sTUDY_DESCRIPTION :: DicomTag
sTUDY_DESCRIPTION = 0x00081030

sERIES_DESCRIPTION :: DicomTag
sERIES_DESCRIPTION = 0x0008103e

-- | Group 0x0010
pATIENT_NAME :: DicomTag
pATIENT_NAME = 0x00100010

-- | Group 0x0020
sTUDY_INSTANCE_UID :: DicomTag
sTUDY_INSTANCE_UID = 0x0020000d

sERIES_INSTANCE_UID :: DicomTag
sERIES_INSTANCE_UID = 0x0020000e

sTUDY_ID :: DicomTag
sTUDY_ID = 0x00200010

sERIES_NUMBER :: DicomTag
sERIES_NUMBER = 0x00200011

-- | Group 0x0028
nUMBER_OF_FRAMES :: DicomTag
nUMBER_OF_FRAMES = 0x00280008

rOWS :: DicomTag
rOWS = 0x00280010

cOLUMNS :: DicomTag
cOLUMNS = 0x00280011

