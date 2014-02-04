import Control.Monad
import Foreign.Ptr
import Foreign.Storable
import Bus
import Ac97var
import Auich

foreign import ccall "hs_extern.h dummy_func" c_dummy_func :: IO()

main :: IO ()
main = do
  c_dummy_func
  putStrLn "Haskell code start."

foreign export ccall "trueMain" trueMain :: Ptr AuichSoftc -> IO ()
trueMain :: Ptr AuichSoftc -> IO ()
trueMain sc = do
  putStrLn "Call trueMain at Haskell."
  test_auich_softc sc
  test_ac97_codec_if sc

test_auich_softc :: Ptr AuichSoftc -> IO ()
test_auich_softc sc = do
  modem <- peek =<< p_AuichSoftc_sc_modem_offset sc
  when (modem /= 8888) $ error "modem /= 8888"
  audIoh <- peek =<< p_AuichSoftc_aud_ioh sc
  when (audIoh /= 0xdeadbeef) $ error "audIoh /= 0xdeadbeef"
  audSize <- peek =<< p_AuichSoftc_aud_size sc
  when (audSize /= 256) $ error "audSize /= 256"
  return ()

test_ac97_codec_if :: Ptr AuichSoftc -> IO ()
test_ac97_codec_if sc = do
  codecIf <- peek =<< p_AuichSoftc_codec_if sc
  vtbl <- peek =<< p_Ac97CodecIf_vtbl codecIf
  var <- peek =<< p_Ac97CodecIfVtbl_var vtbl
  when (var /= 7) $ error "var /= 7"
  lock <- peek =<< p_Ac97CodecIfVtbl_lock vtbl
  call_Ac97CodecIfVtbl_lock lock codecIf
  return ()
