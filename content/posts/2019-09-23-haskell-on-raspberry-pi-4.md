---
title: Haskell on Raspberry PI 4
description: Raspberry Pi 4 now offers up to 4GB of RAM, finally enough to build & run Haskell programs with GHC and Stack on ARM platform.
tags: haskell, stack, raspberry pi, ghc, arm
withTOC: true
---

Last year, I purchased the [Raspberry Pi 3B+] for some hobby projects. Shortly after I started to learn [Haskell], I was wondering if I could build and run _Haskell_ software on the [ARMv7] platform. So I made some attempts with my _Raspberry_ but quickly realized that despite being able to run compiled binaries, the 1GB or RAM is just not enough to build them using the [GHC]. So I gave it up for a while.

Then the [Raspberry Pi 4B] came out, with option to have up to 4GB of RAM. So I purchased the 4GB model and tried again. This time, with greatly increased available RAM size, I was able to make things working. You can find the entire process described in steps bellow.

<!-- MORE -->

# Before we begin
First, we should talk about platforms supported by _GHC_. You should not face any trouble installing and running _GHC_ on your Windows/Linux/macOS machine, because these are [Tier 1] platforms, meaning that they are top priority for _GHC_ team and should have full support. On the other hand, _ARM_ architecture is [Tier 2], meaning that _GHC_ build might not be available for our platform in desired version, which was actually the biggest issue I faced. We'll talk about this later.

Also worth mentioning that for my experimenting, I'm running official [Raspbian] distribution, meaning that I'm using _32bit_ architecture. If anyone will try this with some _64bit_ Linux distro, I'd gladly provide link here.

# Installation steps

## Increase SWAP size
Although 4GB of RAM _ought to be enough for anyone™_, I decided to increase the default value of _100MB_ to _1GB_, just in case.

```bash
$ sudo dphys-swapfile swapoff  # disable swap
$ sudo vim /etc/dphys-swapfile # and set 'CONF_SWAPSIZE' to 1024
$ sudo dphys-swapfile setup    # refresh with new settings
$ sudo dphys-swapfile swapon   # re-enable swap
```

## Install Stack
This step will download current version of [Stack] for our architecture. 

```bash
$ curl -sSL https://get.haskellstack.org/ | sh
```

It's also good to add path to dir `~/.local/bin` to `$PATH`, since this is the directory where binaries installed by `stack install` are placed. You can do that by adding following line to `.bashrc`:

```bash
export PATH="$PATH:~/.local/bin"
```

## Setup Stack project
Here comes the tricky part. As I mentioned above, because _ARM_ is _GHC Tier 2_ platform, there might not be _GHC_ version matching the [Stackage] resolver you use in your project. For example, my project used version [lts-14.6] that matches with _GHC 8.6.5_. However, when you try to build the project using `stack setup`, you might face following error:

```txt
$ stack setup
No setup information found for ghc-8.6.5 on your platform.
This probably means a GHC bindist has not yet been added for OS key 'linux-armv7', 'linux-armv7-ncurses6', 'linux-armv7-tinfo6'.
Supported versions: ghc-7.10.2, ghc-7.10.3, ghc-8.0.1, ghc-8.0.2, ghc-8.2.1, ghc-8.2.2, ghc-8.6.3
```

I really wasn't interested to custom build proper _GHC_ version from sources, but you can force _GHC_ version by adding below shown argument to any `stack` command call, so I decided to force version `8.6.3` instead of `8.6.5`, which is unavailable. Fortunately, I haven't faced any compatibility issues.

```bash
$ stack setup --compiler ghc-8.6.3
```

Alternative would be to use older version of _Stack_ resolver, but that might not be an option, cause the last one for _GHC_ version `8.6.3` is [lts-13.11] and some of the packages might be present in too old version.

## Solving troubles with GHC
After running the `stack setup` from previous step, you might face some errors related to missing [LLVM] dependencies. In my case, _GHC_ version `8.6.3` requires following dependency to be installed:

```bash
$ sudo aptitude install llvm-6.0-dev
```

Also you'll need to add the following path to `$PATH` variable (for example by adding to `.bashrc`):

```bash
export PATH="$PATH:/usr/lib/llvm-6.0/bin/"
```

Next problem you might face is this error when trying to build project using `stack build`:

```txt
/tmp/ghc19719_0/ghc_8.s: Assembler messages:

/home/pi/Repositories/haskell-tools/mat35//tmp/ghc19719_0/ghc_8.s:41:0: error:
     Error: selected processor does not support `movw r3,:lower16:stg_bh_upd_frame_info' in ARM mode
   |
41 |         movw    r3, :lower16:stg_bh_upd_frame_info
   | ^
```

I was able to fix this by simple wrapper, that wraps the original `GHC` binary and adds the  `-opta-march=armv7-a` argument to it:

```bash
$ cd ~/.stack/programs/arm-linux/ghc-8.6.3/bin
$ mv ghc-8.6.3 ghc-8.6.3-bin
$ touch ghc-8.6.3
$ chmod +x ghc-8.6.3

```

Then add following content to the newly created file:

```bash
#!/bin/sh
~/.stack/programs/arm-linux/ghc-8.6.3/bin/ghc-8.6.3-bin -opta-march=armv7-a $@
```

# Enjoy
And that's pretty much all. Now you should be able to build and run any _Haskell_ library or program, just prepare yourself that compilation times are still pretty much longer than on your laptop/PC.

[Raspberry Pi 3B+]: https://www.raspberrypi.org/products/raspberry-pi-3-model-b-plus/
[Raspberry Pi 4B]: https://www.raspberrypi.org/products/raspberry-pi-4-model-b/
[Raspbian]: https://www.raspbian.org/
[Haskell]: https://www.haskell.org/
[ARMv7]: https://en.wikipedia.org/wiki/ARM_architecture
[GHC]: https://www.haskell.org/ghc/
[lts-13.11]: https://www.stackage.org/lts-13.11
[lts-14.6]: https://www.stackage.org/lts-14.6
[LLVM]: https://llvm.org/
[Stack]: https://docs.haskellstack.org/en/stable/README/
[Stackage]: https://www.stackage.org/
[Tier 1]: https://gitlab.haskell.org/ghc/ghc/wikis/platforms#tier-1-platforms
[Tier 2]: https://gitlab.haskell.org/ghc/ghc/wikis/platforms#tier-2-platforms