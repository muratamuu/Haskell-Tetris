Haskell-Tetris
==============


Haskellで簡単なテトリスゲームを作ります。GUIはGtkとGtk2Hsを使用します。

なお、[Newral技術者ブログ](http://newral.info/publics/index/79/&anchor_link=page79_341#page79_341)というサイトでプログラミングの過程を記事にしております。是非ご覧下さい。  

###記事へのリンク

[Haskellでテトリス(Part1)](http://newral.info/publics/index/79/r_id=223/c_id=341/detail=1/&anchor_link=page79_341_223#page79_341_223)  
[Haskellでテトリス(Part2)](http://newral.info/publics/index/79/r_id=224/c_id=341/detail=1/&anchor_link=page79_341_224#page79_341_224)  
[Haskellでテトリス(Part3)](http://newral.info/publics/index/79/r_id=236/c_id=341/detail=1/&anchor_link=page79_341_236#page79_341_236)  
[Haskellでテトリス(Part4)](http://newral.info/publics/index/79/r_id=237/c_id=341/detail=1/&anchor_link=page79_341_237#page79_341_237)  
[Haskellでテトリス(Part5)](http://newral.info/publics/index/79/r_id=239/c_id=341/detail=1/&anchor_link=page79_341_239#page79_341_239)  

***テトリス完成まで随時更新中***  


###ソースコードガイド

Tetris_part3.hs (Part3でプログラミングしたソースコード)  
Tetris_part4.hs (Part4でプログラミングしたソースコード)  
Tetris_part5.hs (Part5でプログラミングしたソースコード)  

###動作確認環境

MaxOSX or Linux(Ubuntu14.04)

###環境準備

0) Haskell Platform は当然インストールしておく！

1) 必要な外部ライブラリをごっそりインストールしておきます

```
$ sudo apt-get install libgtk2.0-dev libpango1.0-dev libglib2.0-dev libcairo2-dev
```

2) もし環境変数PKG_CONFIG_PATHを通していなければ*.pcのありかを追加しておく

```
$ export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
```

3) Haskellのパッケージ管理ツール(cabal)のリポジトリを最新にします

```
$ cabal update
```

ここでハマることがあります。もし、`cabal: Codec.Compression.Zlib〜`というエラーがでる場合はリポジトリデータのダウンロードに失敗しています。一度このエラーが出ると再度updateしてもうまくいきません。まずは`~/.cabal/packags/hackage.haskell.org`を全部削除して再度試してください。私だけかもしれませんが結構頻繁にcabal updateに失敗するのです。通信環境があまり良くないのかも・・


4) ちょっと手間ですが先にcabal自体をアップデートしておく

```
$ cabal install cabal-install
```

5) Gtk2Hsのビルドツールをインストール

```
$ cabal install gtk2hs-buildtools
```

6) ビルドツールのパスを通す

```
$ export PATH=~/.cabal/bin:$PATH
```

7) Gtk2Hsをインストール

```
$ cabal install gtk
```

###実行方法

以下のようにghciで読み込んでmain関数を呼び出してください。

```
$ ghci Tetris.hs
> main
```
