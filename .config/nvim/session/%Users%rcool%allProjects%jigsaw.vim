let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/allProjects/jigsaw
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +24 ~/allProjects/jigsaw/dre_alerts/dre_apk_jigsaw_alert.py
badd +6 ~/allProjects/jigsaw/dre_alerts/dre_apk_os_fw_alert.py
badd +6 ~/allProjects/jigsaw/dre_alerts/dre_apk_os_version_alert.py
badd +4 ~/allProjects/jigsaw/dre_alerts/dre_comcast_jigsaw.py
badd +3 ~/allProjects/jigsaw/dre_alerts/jigsaw_dre.py
badd +4 ~/allProjects/jigsaw/dre_alerts/jigsaw_metaflow_run_dre.py
badd +5 ~/allProjects/jigsaw/dre_alerts/metaflow_new_device_only.py
badd +6 ~/allProjects/jigsaw/dre_alerts/os_version_example.py
argglobal
%argdel
edit ~/allProjects/jigsaw/dre_alerts/metaflow_new_device_only.py
argglobal
balt ~/allProjects/jigsaw/dre_alerts/jigsaw_metaflow_run_dre.py
let s:l = 5 - ((4 * winheight(0) + 23) / 46)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 5
normal! 0
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
