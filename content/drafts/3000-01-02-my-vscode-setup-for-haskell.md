---
title: My VSCode setup for Haskell
description: todo
tags: vscode, haskell, macos
tableOfContents: true
---

<style>
figure {
    text-align: center;
}

figure img {
    width: 500px;
}

figure:nth-child(41) img {
    width: 700px;
}
</style>

When I started to learn [Haskell][web:haskell], I hoped that I'll be able to get similar _IDE_ experience as I have with [Scala][web:scala] and [IntelliJ Idea][web:idea]. Unfortunately the opposite was true. There are some _Haskell_ plugins for _Idea_, but none of them worked for me (mostly due to crashes or hangs) and they seem to be pretty immature as well. But I found pretty good feedback on using _VSCode_ for _Haskell_ development, so I tried it and with help of few _plugins_, you get pretty decent _IDE-like_ experience with syntax highlighting, error reporting and code completion. In this blogpost, I'll present my _VSCode_ setup I use for (almost) everyday _Haskell_ developing.

<!-- MORE -->

# Installation steps
Please note that I use _macOS_ as my operating system so some things in this blog post may be _macOS-specific_. However, most things (except installing dependencies) should be similar for any OS.

## Stack - the build tool
If you already don't have [Stack][web:stack] installed, this is the first step you need to do. _Stack_ is a build tool for _Haskell_ we will use to build some packages from source. One of its great features is that it will download correct version of [GHC][web:ghc]. It can be easily installed using [Homebrew][web:homebrew]:

```txt
$ brew install haskell-stack
```

## Syntax Highlighting
![VSCode Haskell Syntax Highlighting extension][img:vscode-hsl]

We definitely want to have some syntax highlighting for the _Haskell_ code. This can be done by installing the [Haskell Syntax Highlighting][vscode:hsl] extension. Now the code looks more fancy.

![syntax highlighting in action][img:syntax-highlighting]

## Brittany - the source formatter
[Brittany][github:brittany] is one of the popular source code formatters for _Haskell_ that integrates well with other tools you'll install based on this tutorial. We need to install both the _Britanny_ executable itself and the _VSCode extension_.

### Installing the executable
_Brittany_ is not available on _Homebrew_, but we can install it using _Stack_:

```txt
stack install brittany
```

If this is the very first thing ever you use _Stack_ for, then it may take some time. _Stack_ needs to download correct version of _GHC_ and build _Brittany_ and all its dependencies from source code, so maybe it's time to have some coffee :-)

### Installing the VSCode extension
![VSCode extension for Brittany][img:vscode-brittany]

To integrate installed executable with _VSCode_, you need to install the [brittany extension][vscode:brittany] to _VSCode_. After that, source code can be formatted by hitting the __⇧⌘F__ combo.

## HLint - the linter
[HLint][github:hlint] is _linter_ for _Haskell_ that is really useful for simplifying code and searching for redundancies. Similar to _Brittany_, we need to install both the executable and the _VSCode_ extension.

### Installing the executable
_HLint_ is available through _Homebrew_ and can be installed using following command:

```txt
stack install hlint
```

### Installing the VSCode extension
![VSCode extension for HLint][img:vscode-hlint]

To integrate _HLint_ with _VSCode_, we need to install the [haskell-linter][vscode:hlint] extension. After that, suggestions will be visible directly from the _VSCode_:

![HLint in action][img:hlint]

## Haskell IDE Engine - the IDE experience
The last, but important step is to install the [Haskell IDE Engine][github:hie]. This is the engine that communicates with _VSCode_ via [Language Server Protocol][web:lsp] and provides such features as displaying compilation warnings/errors, code completion and showing documentation when you mouse hover the selected function. Again we need to install the executable and _VSCode_ extension.

### Installing the executable
Unfortunately installing the _Haskell IDE Engine_ is little bit more tedious. It needs to be installed from source code and needs to be installed for the _specific_ _GHC_ version you use. For all my projects I use the [LTS Haskell 14.*][web:stackage] which uses the _GHC_ version `8.6.5`, so I'll assume this version in following steps. So as the first step, we need to download the source codes:

```txt
git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules
cd haskell-ide-engine/
```

Now, we need to build it using the following commands:
```txt
stack ./install.hs hie-8.6.5
stack ./install.hs build-data
```

This may take some time so it may be good time for another coffee :-)

### Installing the VSCode extension
![VSCode Haskell Language Server extension][img:vscode-hls]

Last step is to install the [Haskell Language Server][vscode:hls] extension. This will enable all the fancy features described above, on screenshot below you can see example how documentation is shown for particular function on mouseover:

![displaying documentation on mouseover][img:hls]

# Summary
todo

[img:hlint]: /assets/images/my-vscode-setup-for-haskell/hlint.png
[img:hls]: /assets/images/my-vscode-setup-for-haskell/hls.png
[img:syntax-highlighting]: /assets/images/my-vscode-setup-for-haskell/syntax-highlighting.png
[img:vscode-brittany]: /assets/images/my-vscode-setup-for-haskell/vscode-brittany.png
[img:vscode-hlint]: /assets/images/my-vscode-setup-for-haskell/vscode-hlint.png
[img:vscode-hls]: /assets/images/my-vscode-setup-for-haskell/vscode-hls.png
[img:vscode-hsl]: /assets/images/my-vscode-setup-for-haskell/vscode-hsl.png

[github:brittany]: https://github.com/lspitzner/brittany
[github:hie]: https://github.com/haskell/haskell-ide-engine
[github:hlint]: https://github.com/ndmitchell/hlint
[vscode:brittany]: https://github.com/MaxGabriel/brittany-vscode-extension
[vscode:hlint]: https://github.com/hoovercj/vscode-haskell-linter
[vscode:hls]: https://github.com/alanz/vscode-hie-server
[vscode:hsl]: https://github.com/JustusAdam/language-haskell
[web:ghc]: https://www.haskell.org/ghc/
[web:haskell]: https://www.haskell.org/
[web:homebrew]: https://brew.sh/
[web:idea]: https://www.jetbrains.com/idea/
[web:lsp]: https://microsoft.github.io//language-server-protocol/specifications/specification-3-14/
[web:scala]: https://scala-lang.org
[web:stack]: https://docs.haskellstack.org/en/stable/README/
[web:stackage]: https://www.stackage.org