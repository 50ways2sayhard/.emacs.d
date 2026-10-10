# AGENTS.md

## Elisp 文件头

- 写任何 `.el` 文件，第一行必须带 `lexical-binding` cookie：
  `;;; foo.el --- Summary -*- lexical-binding: t; -*-`
- "任何"包括临时文件：写到 `/tmp` 下再 eval 的一次性脚本、给 `emacsclient --eval` 用的片段文件、试验性代码，全都算。落到磁盘的 elisp 文件不区分正式与临时。
- 改已有 `.el` 文件时，若第一行缺这个 cookie，就补上。
- 例外：`elpaca/` 下的第三方包只读，不要去动它们的文件头。
