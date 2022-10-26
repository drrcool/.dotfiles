let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/qoedash
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +81 ~/qoedash/src/data/formatting/FormatTableData.js
badd +1 src/data/recipes/performTablePipeline.js
badd +0 src/components/Summary/sectionConfig.js
argglobal
%argdel
edit ~/qoedash/src/data/formatting/FormatTableData.js
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 28 + 28) / 57)
exe 'vert 2resize ' . ((&columns * 28 + 28) / 57)
argglobal
balt src/data/recipes/performTablePipeline.js
let s:l = 79 - ((6 * winheight(0) + 8) / 16)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 79
let s:c = 18 - ((12 * winwidth(0) + 14) / 28)
if s:c > 0
  exe 'normal! ' . s:c . '|zs' . 18 . '|'
else
  normal! 018|
endif
wincmd w
argglobal
if bufexists(fnamemodify("src/components/Summary/sectionConfig.js", ":p")) | buffer src/components/Summary/sectionConfig.js | else | edit src/components/Summary/sectionConfig.js | endif
if &buftype ==# 'terminal'
  silent file src/components/Summary/sectionConfig.js
endif
balt ~/qoedash/src/data/formatting/FormatTableData.js
let s:l = 1 - ((0 * winheight(0) + 8) / 16)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 28 + 28) / 57)
exe 'vert 2resize ' . ((&columns * 28 + 28) / 57)
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
