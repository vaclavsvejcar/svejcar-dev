# svejcar.dev

This is the source code for my personal website https://svejcar.dev. It's built using [Hakyll](https://jaspervdj.be/hakyll/) static site generator, [Haskell](https://www.haskell.org)-based alternative to [Jekyll](https://jekyllrb.com).

## License
Blog source code (located in `src/` directory) is licensed under the [BSD-3-Clause license][BSD-3-Clause]. The content of the blog (blog posts, images, etc., located in `content/` and `assets/` directories) is licensed under the [CC BY-ND 4.0].

## How to build

### 1/ Download Haskell Stack
```bash
$ curl -sSL https://get.haskellstack.org/ | sh
```

### 2/ Init the build environment
```bash
$ git clone https://github.com/vaclavsvejcar/svejcar-dev.git
$ cd svejcar-dev/
$ stack init
$ stack build
```

### 3/ Preview draft version
```bash
$ stack run rebuild draft
$ stack run watch draft
```
Website should now run on https://localhost:8000

### 4/ Build production site
```bash
$ stack run rebuild
```
Generated content is located in `_site/` directory.

[BSD-3-Clause]: https://opensource.org/licenses/BSD-3-Clause
[CC BY-ND 4.0]: https://creativecommons.org/licenses/by-nd/4.0/