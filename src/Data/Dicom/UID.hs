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

module Data.Dicom.UID
  ( -- * Types
    UID,
    -- * Transfer Syntax
    iMPLICIT_VR_LE,
    eXPLICIT_VR_LE,
    eXPLICIT_VR_BE,
    jPEG_2000,
    -- * SOP Class
    eNHANCED_CT_IMAGE_STORAGE,
    eNHANCED_MR_IMAGE_STORAGE,
    eNHANCED_PET_IMAGE_STORAGE,
    cT_IMAGE_STORAGE,
    gRAYSCALE_SOFTCOPY_PRESENTATION_STATE_STORAGE,
    mR_IMAGE_STORAGE,
    pOSITRON_EMISSION_TOMOGRAPHY_IMAGE_STORAGE,
    rAW_DATA_STORAGE,
    uLTRASOUND_IMAGE_STORAGE,
    uLTRASOUND_MULTIFRAME_IMAGE_STORAGE
  ) where

-- | UID type
type UID = String

-- | Implicit VR Little Endian
iMPLICIT_VR_LE :: UID
iMPLICIT_VR_LE = "1.2.840.10008.1.2"

-- | Explicit VR Little Endian
eXPLICIT_VR_LE :: UID
eXPLICIT_VR_LE = "1.2.840.10008.1.2.1"

-- | Explicit VR Big Endian
eXPLICIT_VR_BE :: UID
eXPLICIT_VR_BE = "1.2.840.10008.1.2.2"

-- | JPEG2000
jPEG_2000 :: UID
jPEG_2000 = "1.2.840.10008.1.2.4.91"

-- | CT Image Storage
cT_IMAGE_STORAGE :: UID
cT_IMAGE_STORAGE = "1.2.840.10008.5.1.4.1.1.2"

-- | Enhanced CT Image Storage
eNHANCED_CT_IMAGE_STORAGE :: UID
eNHANCED_CT_IMAGE_STORAGE = "1.2.840.10008.5.1.4.1.1.2.1"

-- | Enhanced MR Image Storage
eNHANCED_MR_IMAGE_STORAGE :: UID
eNHANCED_MR_IMAGE_STORAGE = "1.2.840.10008.5.1.4.1.1.4.1"

-- | Enhanced PET Image Storage
eNHANCED_PET_IMAGE_STORAGE :: UID
eNHANCED_PET_IMAGE_STORAGE = "1.2.840.10008.5.1.4.1.1.130"

-- | Grayscale Softcopy Presentation State Storage
gRAYSCALE_SOFTCOPY_PRESENTATION_STATE_STORAGE :: UID
gRAYSCALE_SOFTCOPY_PRESENTATION_STATE_STORAGE = "1.2.840.10008.5.1.4.1.1.11.1"

-- | MR Image Storage
mR_IMAGE_STORAGE :: UID
mR_IMAGE_STORAGE = "1.2.840.10008.5.1.4.1.1.4"

-- | PET Image Storage
pOSITRON_EMISSION_TOMOGRAPHY_IMAGE_STORAGE :: UID
pOSITRON_EMISSION_TOMOGRAPHY_IMAGE_STORAGE = "1.2.840.10008.5.1.4.1.1.128"

-- | Raw Data Storage
rAW_DATA_STORAGE :: UID
rAW_DATA_STORAGE = "1.2.840.10008.5.1.4.1.1.66"

-- | US Image Storage
uLTRASOUND_IMAGE_STORAGE :: UID
uLTRASOUND_IMAGE_STORAGE = "1.2.840.10008.5.1.4.1.1.6.1"

-- | US Multiframe Image Storage
uLTRASOUND_MULTIFRAME_IMAGE_STORAGE :: UID
uLTRASOUND_MULTIFRAME_IMAGE_STORAGE = "1.2.840.10008.5.1.4.1.1.3.1"

