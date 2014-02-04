# 例: C言語の構造体ツリーを局所的に辿る

Storableクラスを使わずにポインタのpeekだけを使って、
C言語の構造体ツリーを局所的に辿る例です。

## ソースコードツリー

~~~
sample_pointer_combinator
|-- Makefile
|-- README.md
|-- ac97var.h
|-- auich.h
|-- bus.h
|-- hs_extern.h
|-- hssrc
|   |-- Ac97var.hs
|   |-- Auich.hs
|   |-- Bus.hs
|   `-- Main.hs
`-- main.c
~~~

main.cはC言語のエントリポイントが入っているソースコードです。
ここから実行がはじまります。
main.cはHaskellのtrueMain関数を呼び出します。

trueMain関数はhssrc/Main.hsに格納されています。
この関数はmain.cで初期化された構造体が正しくHaskell側から読めるかテストします。

以下のファイルは人間による手書きで管理されます。

* ac97var.h
* auich.h
* bus.h
* hs_extern.h
* main.c
* hssrc/Main.hs

しかし、以下のHaskellソースコードはなんらかのツールで自動生成されるべきです。

* hssrc/Auich.hs (auich.hを元に自動生成)
* hssrc/Bus.hs (bus.hを元に自動生成)
* hssrc/Ac97var.hs (ac97var.hを元に自動生成)

## 自動生成のルール

### ファイルの命名

netbsd-arafura-s1/sys/dev/pci/pciide_pdc202xx_reg.hファイルでの定義は
netbsd-arafura-s1/metasepi/sys/hssrc/Dev/Pci/PciidePdc202xxReg.hsにHaskell定義が自動生成され、
その内容は以下のようになるべきです。

```
{-# LANGUAGE ForeignFunctionInterface #-}
module Dev.Pci.PciidePdc202xxReg where
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
```

NetBSDではC言語ファイル名を小文字と数字と'_'で主に表わします。
これはファイル名だけではなくディレクトリ名にもあてはまります。
そこで以下のルールでC言語のファイル名をHaskellのファイル名に変換します。

1. C言語のファイル名をアルファベットと数値のみをつかったグループに分ける
2. 1のグループの文字列先頭を大文字に変える
3. 全てのグループを文字列連結する

Haskellの関数で表わすと以下のようになります。

```haskell
ghci> concat $ fmap (\(x:xs) -> toUpper x:xs) $ filter (not . null) $ fmap (filter isAlphaNum) $ groupBy (\a b -> isAlphaNum a && isAlphaNum b) "pciide_sl82c105_reg"
"PciideSl82c105Reg"
```

例えば、ac97var.hはhssrc/Ac97var.hsというファイル名に変換されるべきです。

```haskell
> concat $ fmap (\(x:xs) -> toUpper x:xs) $ filter (not . null) $ fmap (filter isAlphaNum) $ groupBy (\a b -> isAlphaNum a && isAlphaNum b) "ac97var"
"Ac97var"
```

またこのファイル名にもとづいてHaskellのモジュール名も付記する必要があります。
FFIとForeign.Ptr,Foreign.Storable,Foreign.C.Typesはほぼ必須なので、一律に付けてしまっても良いでしょう

```
$ head -3 hssrc/Ac97var.hs
{-# LANGUAGE ForeignFunctionInterface #-}
module Ac97var where
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
```

### import 行

netbsd-arafura-s1/sys/dev/pci/pcivar.hファイルは6つのinclude行を持っています。

```c
$ grep \#include netbsd-arafura-s1/sys/dev/pci/pcivar.h
#include <sys/device.h>
#include <sys/pmf.h>
#include <sys/bus.h>
#include <dev/pci/pcireg.h>
#include <dev/pci/pci_verbose.h>
#include <machine/pci_machdep.h>
```

このコードは以下のようなHaskellのimport行に変換されるべきです。

```haskell
module Dev.Pci.Pcivar where
import Sys.Device
import Sys.Pmf
import Sys.Bus
import Dev.Pci.Pcireg
import Dev.Pci.PciVerbose
import Machine.PciMachdep
```

当該のモジュールが依存しているモジュールはimportで列挙する必要があります。
例えばhssrc/Auich.hsを見てみましょう。

```haskell
{-# LANGUAGE ForeignFunctionInterface #-}
module Auich where
import Foreign.C.Types
import Foreign.Ptr
import Bus
import Ac97var
-- snip --
p_AuichSoftc_codec_if :: Ptr AuichSoftc -> IO (Ptr (Ptr Ac97CodecIf))
p_AuichSoftc_codec_if p = return $ plusPtr p offsetOf_AuichSoftc_codec_if
p_AuichSoftc_sc_modem_offset :: Ptr AuichSoftc -> IO (Ptr BusSizeT)
p_AuichSoftc_sc_modem_offset p = return $ plusPtr p offsetOf_AuichSoftc_sc_modem_offset
```

hssrc/Auich.hsではAc97CodecIfとBusSizeTという2つの型を使っています。
これら2つの型はそれぞれAc97varとBusというモジュールで使われていまう。
そのためhssrc/Auich.hsには以下の二行が必要になるのです。

```haskell
import Bus
import Ac97var
```

### プリミティブ型の別名をHaskellソースでも宣言する

まずbus.hをよく観察してみましょう。

```c
#ifndef __BUS_H__
#define __BUS_H__

typedef size_t bus_size_t;

typedef unsigned long   vaddr_t;
typedef vaddr_t bus_space_handle_t;

#endif /* __BUS_H__ */
```

ここには3つのtypedefがあります。
これらは以下のような型名の付け替えをしています。

* size_t → bus_size_t
* unsigned long → vaddr_t → bus_space_handle_t

このような名前の付け替えをHaskellで行なうには以下2つの作業が必要になります。

#### プリミティブ型に対応するHaskellの型名を見つける

先のbus.hの例ではsize_tとunsigned longがプリミティブ型に相当します。
jhcのライブラリを参照するかぎりでは、それぞれ以下の型が用意されています。

```
$ grep -r \"size_t ajhc/lib/jhc
ajhc/lib/jhc/Jhc/Type/C.hs:newtype {-# CTYPE "size_t" #-}    CSize    = CSize WordPtr
$ grep -r \"unsigned\ long\" ajhc/lib/jhc
ajhc/lib/jhc/Jhc/Type/C.hs:newtype {-# CTYPE "unsigned long" #-}  CULong   = CULong WordPtr
```

そのためbus.hの例でsize_tをCSizeに、unsigned longをCULongに読み替えることにします。
この読み替えの法則はなんらかの設定ファイルにしてやる必要があると思います。

#### 型名の付け替え

プリミティブ型の読み替えが終わったら、Haskellのtypeキーワードを使って名前の別名を決めます。

"size_t → bus_size_t"でしたから、以下のようなHaskellソースコードが得られます。

```haskell
type BusSizeT = CSize
```

このBusSizeTはファイル名と同じ変換法則で決めます。

```haskell
ghci> concat $ fmap (\(x:xs) -> toUpper x:xs) $ filter (not . null) $ fmap (filter isAlphaNum) $ groupBy (\a b -> isAlphaNum a && isAlphaNum b) "bus_size_t"
"BusSizeT"
```

"unsigned long → vaddr_t"でしたから、以下のHaskellソースコードが得られます。

```haskell
type VaddrT = CULong
```

最後に"vaddr_t → bus_space_handlet_t"でしたから、以下のHaskellソースコードが得られます。

```haskell
type BusSpaceHandleT = VaddrT
```

### 構造体の型を宣言する

ac97var.hファイルを見てみると2つの構造体が宣言されています。

```c
struct ac97_codec_if;

struct ac97_codec_if_vtbl {
	void (*lock)(struct ac97_codec_if *);
	int var;
};

struct ac97_codec_if {
	struct ac97_codec_if_vtbl *vtbl;
};
```

このような構造体は単にnewtypeしてください。
つまり例の変換規則で名前を変換して、

```haskell
ghci> concat $ fmap (\(x:xs) -> toUpper x:xs) $ filter (not . null) $ fmap (filter isAlphaNum) $ groupBy (\a b -> isAlphaNum a && isAlphaNum b) "ac97_codec_if"
"Ac97CodecIf"
ghci> concat $ fmap (\(x:xs) -> toUpper x:xs) $ filter (not . null) $ fmap (filter isAlphaNum) $ groupBy (\a b -> isAlphaNum a && isAlphaNum b) "ac97_codec_if_vtbl"
"Ac97CodecIfVtbl"
```

()を実体としたnewtypeをするだけです。

```haskell
newtype {-# CTYPE "struct ac97_codec_if" #-} Ac97CodecIf = Ac97CodecIf ()
newtype {-# CTYPE "struct ac97_codec_if_vtbl" #-} Ac97CodecIfVtbl = Ac97CodecIfVtbl ()
```

これらの構造体はStorableにする必要はありません。
構造体の参照はオフセットを指定した局所参照しかしないためです。

### 関数ポインタの型を定義する

構造体のメンバーの中には関数ポインタもあります。
このような場合には以下二種類の定義をする必要があります。

#### 対応するFunPtr型

例えばac97var.hではstruct ac97_codec_if_vtbl構造体ののメンバーとしてlock関数ポインタが定義されています。

```c
struct ac97_codec_if_vtbl {
	void (*lock)(struct ac97_codec_if *);
	int var;
};
```

この関数ポインタは以下のような意味を持っています。

* struct ac97_codec_if *型を第一引数に取り
* void型を返す

このような関数はHaskellであれば以下のような型になるでしょう。
いったんtypeで型の別名をつけます。

```haskell
type Ac97CodecIfVtbl_lock = Ptr Ac97CodecIf -> IO ()
```

しかし、このlockメンバーは関数ポインタです。
このlock関数ポインタの型はHaskellのFunPtr型を使って以下のようになります。

```haskell
FunPtr Ac97CodecIfVtbl_lock
```

#### foreign import ccall "dynamic"を使ったFunPtr型から通常のHaskell関数への変換関数

しかしこのFunPtrで表わされる関数ポインタはそのままではHaskellから実行できません。
そこでFunPtrを通常のHaskell関数に変換する関数をforeign import ccall "dynamic"を使って定義します。

```haskell
foreign import ccall "dynamic" call_Ac97CodecIfVtbl_lock :: FunPtr (Ac97CodecIfVtbl_lock) -> Ac97CodecIfVtbl_lock
```

この関数束縛の名前は以下のように命名してください。

```
call_構造体の名前を先の関数で変換したもの_関数ポインタメンバー名文字列
```

### 構造体メンバーのオフセットをC言語からHaskellに取り込む

auich.hには4つの構造体メンバーがあります。
この構造体メンバーの構造体頭からのオフセットをC言語の式とforeign import primitive(jhc独自機能)
を使ってHaskellに取り込みます。

```c
struct auich_softc {
	struct ac97_codec_if *codec_if;
	int	sc_modem_offset;
	bus_space_handle_t aud_ioh;
	bus_size_t aud_size;
};
```

のようなC言語で定義された構造体メンバーを以下のように変換します。

```haskell
foreign import primitive "const.offsetof(struct auich_softc, codec_if)"
  offsetOf_AuichSoftc_codec_if :: Int
foreign import primitive "const.offsetof(struct auich_softc, sc_modem_offset)"
  offsetOf_AuichSoftc_sc_modem_offset :: Int
foreign import primitive "const.offsetof(struct auich_softc, aud_ioh)"
  offsetOf_AuichSoftc_aud_ioh :: Int
foreign import primitive "const.offsetof(struct auich_softc, aud_size)"
  offsetOf_AuichSoftc_aud_size :: Int
```

型名は常にIntにします。ポインタのオフセット計算にはInt型を使うためです。
また、束縛する名前は以下のような命名則です。

```
offsetOf_構造体の名前を先の関数で変換したもの_メンバー名文字列
```

### 構造体メンバーのオフセットを求める関数を作る

C言語の構造体の各メンバーのオフセットを求める関数を作ります。
以下のソースコードでは4つのメンバーがあるので、4つの関数を作る必要があります。

```c
struct auich_softc {
	struct ac97_codec_if *codec_if;
	int	sc_modem_offset;
	bus_space_handle_t aud_ioh;
	bus_size_t aud_size;
};
```

まず構造体の型はnewtypeで作成してあるはずです。その型名はAuichSoftcでしょう。
オフセットを求める関数に第一引数はPtr AuichSoftcになります。

次に一つ目の構造体メンバーに注目します。
この型名はこれまで変換した通りPtr Ac97CodecIfになります。
オフセットを求める関数はメンバーの型へのポインタを返すべきです。
つまりPtr (Ptr Ac97CodecIf)を返すべきです。

これで関数の型は以下のように決まりました。

```haskell
p_AuichSoftc_codec_if :: Ptr AuichSoftc -> IO (Ptr (Ptr Ac97CodecIf))
```

この関数名は以下のように命名してください。

```
p_構造体の名前を先の関数で変換したもの_メンバー名
```

最後に型の実体を決めます。
構造体メンバーのオフセットは前章で決定したので、HaskellのplusPtr関数を使ってオフセット計算できます。
ここで"plusPtr :: Ptr a -> Int -> Ptr b"のような型であれるため、
先のようにオフセット計算の関数では型シグニチャを書いて、オフセット計算した後の型を強制する必要があります。
このシグニチャによって安全なポインタアクセスと、局所参照の両立ができるようになるのです。

```haskell
p_AuichSoftc_codec_if p = return $ plusPtr p offsetOf_AuichSoftc_codec_if
```

残り3つの構造体メンバーのオフセットも同様に決定します。

```haskell
p_AuichSoftc_sc_modem_offset :: Ptr AuichSoftc -> IO (Ptr BusSizeT)
p_AuichSoftc_sc_modem_offset p = return $ plusPtr p offsetOf_AuichSoftc_sc_modem_offset
p_AuichSoftc_aud_ioh :: Ptr AuichSoftc -> IO (Ptr BusSpaceHandleT)
p_AuichSoftc_aud_ioh p = return $ plusPtr p offsetOf_AuichSoftc_aud_ioh
p_AuichSoftc_aud_size :: Ptr AuichSoftc -> IO (Ptr CSize)
p_AuichSoftc_aud_size p = return $ plusPtr p offsetOf_AuichSoftc_aud_size
```
