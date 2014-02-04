{-# LANGUAGE ForeignFunctionInterface #-}
module Ac97var where
import Foreign.Ptr

newtype {-# CTYPE "struct ac97_codec_if" #-} Ac97CodecIf = Ac97CodecIf ()
foreign import primitive "const.offsetof(struct ac97_codec_if, vtbl)"
  offsetOf_Ac97CodecIf_vtbl :: Int
newtype {-# CTYPE "struct ac97_codec_if_vtbl" #-} Ac97CodecIfVtbl = Ac97CodecIfVtbl ()
foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, lock)"
  offsetOf_Ac97CodecIfVtbl_lock :: Int
foreign import primitive "const.offsetof(struct ac97_codec_if_vtbl, var)"
  offsetOf_Ac97CodecIfVtbl_var :: Int
type Ac97CodecIfVtbl_lock = Ptr Ac97CodecIf -> IO ()
foreign import ccall "dynamic" call_Ac97CodecIfVtbl_lock :: FunPtr (Ac97CodecIfVtbl_lock) -> Ac97CodecIfVtbl_lock

p_Ac97CodecIf_vtbl :: Ptr Ac97CodecIf -> IO (Ptr (Ptr Ac97CodecIfVtbl))
p_Ac97CodecIf_vtbl p = return $ plusPtr p offsetOf_Ac97CodecIf_vtbl
p_Ac97CodecIfVtbl_lock :: Ptr Ac97CodecIfVtbl -> IO (Ptr (FunPtr Ac97CodecIfVtbl_lock))
p_Ac97CodecIfVtbl_lock p = return $ plusPtr p offsetOf_Ac97CodecIfVtbl_lock
p_Ac97CodecIfVtbl_var :: Ptr Ac97CodecIfVtbl -> IO (Ptr Int)
p_Ac97CodecIfVtbl_var p = return $ plusPtr p offsetOf_Ac97CodecIfVtbl_var
