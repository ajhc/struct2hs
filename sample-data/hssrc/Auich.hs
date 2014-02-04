{-# LANGUAGE ForeignFunctionInterface #-}
module Auich where
import Foreign.C.Types
import Foreign.Ptr
import Bus
import Ac97var

newtype {-# CTYPE "struct auich_softc" #-} AuichSoftc = AuichSoftc ()
foreign import primitive "const.offsetof(struct auich_softc, codec_if)"
  offsetOf_AuichSoftc_codec_if :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_modem_offset)"
  offsetOf_AuichSoftc_sc_modem_offset :: Int
foreign import primitive "const.offsetof(struct auich_softc, aud_ioh)"
  offsetOf_AuichSoftc_aud_ioh :: Int
foreign import primitive "const.offsetof(struct auich_softc, aud_size)"
  offsetOf_AuichSoftc_aud_size :: Int

p_AuichSoftc_codec_if :: Ptr AuichSoftc -> IO (Ptr (Ptr Ac97CodecIf))
p_AuichSoftc_codec_if p = return $ plusPtr p offsetOf_AuichSoftc_codec_if
p_AuichSoftc_sc_modem_offset :: Ptr AuichSoftc -> IO (Ptr BusSizeT)
p_AuichSoftc_sc_modem_offset p = return $ plusPtr p offsetOf_AuichSoftc_sc_modem_offset
p_AuichSoftc_aud_ioh :: Ptr AuichSoftc -> IO (Ptr BusSpaceHandleT)
p_AuichSoftc_aud_ioh p = return $ plusPtr p offsetOf_AuichSoftc_aud_ioh
p_AuichSoftc_aud_size :: Ptr AuichSoftc -> IO (Ptr CSize)
p_AuichSoftc_aud_size p = return $ plusPtr p offsetOf_AuichSoftc_aud_size
