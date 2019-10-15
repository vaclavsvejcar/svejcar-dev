---
title: Table of Contents in Hakyll
tags: hakyll, haskell, meta, pandoc
withTOC: true
---

Shortly after I implemented this blog in [Hakyll][hakyll], I realized that having some sort of _table of contents_ could help when reading some of the longer blog posts. I really wanted simple solution with minimum coding, that would allow enabling _TOC_ for individual blog posts and I also wanted to have numbered headings, as it makes navigation through the page easier.

After some time spent by searching optimal solution, I figured out that [Pandoc][pandoc], the document converter used by _Hakyll_ to convert _Markdown_ to _HTML_, already contains built-in support for this. In this blog post, I'll describe how I implemented the _Table of Content_ functionality you can see even on this page. You can also check the full source code of this blog on [GitHub][git_svejcar-dev].

<!-- MORE -->

todo

[hakyll]: https://jaspervdj.be/hakyll/
[pandoc]: https://pandoc.org/
[git_svejcar-dev]: https://github.com/vaclavsvejcar/svejcar-dev