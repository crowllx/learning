let get_data path =
  let in_file = open_in path in
  let contents = In_channel.input_lines in_file in
  close_in in_file;
  contents

let explode s =
  let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let implode l =
  let buffer = Buffer.create (List.length l) in
  List.iter (Buffer.add_char buffer) l;
  Buffer.contents buffer

let eql c expect = c == expect

(* let get_num l = *)
(*   let a_str = *)
(*     List.take_while (fun c -> match c with '0' .. '9' -> true | _ -> false) l *)
(*   in *)
(*   let next_l = List.drop (List.length a_str + 1) l in *)
(*   let b_str = *)
(*     List.take_while *)
(*       (fun c -> match c with '0' .. '9' -> true | _ -> false) *)
(*       next_l *)
(*   in *)
(*   (a_str, b_str, List.drop (List.length b_str) next_l) *)

type keyword = Do | Don't | Number of int | None

let string_of_keyword k =
  match k with
  | Do -> "do"
  | Don't -> "don't"
  | Number n -> "number " ^ string_of_int n
  | None -> "none"

let rec check_num l n =
  let x = List.hd l in
  match x with
  | '0' .. '9' -> check_num (List.drop 1 l) (n @ [ x ])
  | _ -> (n, l)

let find_do l =
  let len = if List.length l < 7 then List.length l else 7 in
  let str = implode (List.take len l) in
  (* Printf.printf "%s\n" str; *)
  (* Printf.printf "%s \n" (implode (List.take 5 (List.drop 4 l))); *)
  if String.starts_with ~prefix:"do()" str then (Do, List.drop 3 l)
  else if String.starts_with ~prefix:"don't()" str then (Don't, List.drop 6 l)
  else
    let () = Printf.printf "%s\n" str in
    (None, l)

let rec check_next c e l a b =
  if List.length l > 0 then
    match (c, e) with
    | 'm', 'm' -> check_next (List.hd l) 'u' (List.tl l) 0 0
    | 'u', 'u' -> check_next (List.hd l) 'l' (List.tl l) 0 0
    | 'l', 'l' -> check_next (List.hd l) '(' (List.tl l) 0 0
    | '(', '(' ->
        let a_str, rem = check_num l [] in
        if List.length a_str > 0 then
          check_next (List.hd rem) ',' (List.tl rem)
            (int_of_string (implode a_str))
            0
        else check_next (List.hd rem) 'm' (List.tl rem) 0 0
    | ',', ',' ->
        let b_str, rem = check_num l [] in
        if List.length b_str > 0 && List.hd rem == ')' then
          (Number (a * int_of_string (implode b_str)), 0, rem)
        else check_next (List.hd rem) 'm' (List.tl rem) 0 0
    | 'd', _ -> (
        let res, rem = find_do (c :: l) in
        match res with
        | Do -> (Do, 0, rem)
        | Don't -> (Don't, 0, rem)
        | _ -> check_next (List.hd l) 'm' (List.tl l) 0 0)
    | _ -> check_next (List.hd l) 'm' (List.tl l) 0 0
  else (None, 0, l)

let check_length l =
  let len = List.length l in
  len >= 1 && len <= 3

let rec search (seq : char list) =
  let active = ref true in
  let rec _search (seq : char list) acc =
    match seq with
    | [] -> acc
    | xs -> (
        let word, n, rem = check_next (List.hd seq) 'm' seq 0 0 in
        match word with
        | Do ->
            active := true;
            _search rem acc
        | Don't ->
            active := false;
            _search rem acc
        | Number n ->
            let v = if !active then n else 0 in
            _search rem (acc + v)
        | None -> _search rem acc)
  in
  _search seq 0

let solve_part1 =
  let data = String.concat "" (get_data "data/day3") in
  search (explode data)

let () = print_endline (string_of_int solve_part1)
--- Startup times for process: Primary (or UI client) ---

times in msec
 clock   self+sourced   self:  sourced script
 clock   elapsed:              other lines

000.000  000.000: --- NVIM STARTING ---
000.143  000.143: event init
000.218  000.075: early init
000.291  000.073: locale set
000.335  000.044: init first window
000.900  000.565: inits 1
000.909  000.008: window checked
000.911  000.002: parsing arguments
001.329  000.038  000.038: require('vim.shared')
001.405  000.032  000.032: require('vim.inspect')
001.438  000.028  000.028: require('vim._options')
001.440  000.108  000.048: require('vim._editor')
001.441  000.169  000.023: require('vim._init_packages')
001.442  000.362: init lua interpreter
002.379  000.938: nvim_ui_attach
002.605  000.226: nvim_set_client_info
002.607  000.001: --- NVIM STARTED ---

--- Startup times for process: Embedded ---

times in msec
 clock   self+sourced   self:  sourced script
 clock   elapsed:              other lines

000.000  000.000: --- NVIM STARTING ---
000.099  000.098: event init
000.147  000.048: early init
000.177  000.030: locale set
000.200  000.024: init first window
000.423  000.223: inits 1
000.434  000.011: window checked
000.436  000.001: parsing arguments
000.866  000.031  000.031: require('vim.shared')
000.935  000.031  000.031: require('vim.inspect')
000.977  000.035  000.035: require('vim._options')
000.980  000.112  000.045: require('vim._editor')
000.981  000.170  000.027: require('vim._init_packages')
000.983  000.377: init lua interpreter
001.052  000.069: expanding arguments
001.074  000.022: inits 2
001.283  000.209: init highlight
001.285  000.002: waiting for UI
001.436  000.152: done waiting for UI
001.444  000.008: clear screen
001.583  000.009  000.009: require('vim.keymap')
002.088  000.078  000.078: sourcing nvim_exec2()
002.186  000.739  000.652: require('vim._defaults')
002.188  000.005: init default mappings & autocommands
002.479  000.031  000.031: sourcing /usr/share/nvim/runtime/ftplugin.vim
002.513  000.012  000.012: sourcing /usr/share/nvim/runtime/indent.vim
002.609  000.065  000.065: sourcing /usr/share/nvim/archlinux.lua
002.611  000.083  000.018: sourcing /etc/xdg/nvim/sysinit.vim
003.039  000.360  000.360: require('options.set')
003.083  000.042  000.042: require('options.remap')
003.115  000.031  000.031: require('options.autocmds')
003.483  000.068  000.068: require('lazy')
003.538  000.045  000.045: require('ffi')
003.560  000.011  000.011: require('vim.fs')
003.625  000.064  000.064: require('vim.uri')
003.632  000.093  000.018: require('vim.loader')
003.698  000.056  000.056: require('lazy.stats')
003.764  000.053  000.053: require('lazy.core.util')
003.838  000.073  000.073: require('lazy.core.config')
003.935  000.043  000.043: require('lazy.core.handler')
004.164  000.113  000.113: require('lazy.pkg')
004.169  000.171  000.059: require('lazy.core.meta')
004.172  000.236  000.065: require('lazy.core.plugin')
004.178  000.339  000.060: require('lazy.core.loader')
004.318  000.044  000.044: require('lazy.core.fragments')
005.852  000.065  000.065: require('lazy.core.handler.keys')
005.909  000.051  000.051: require('lazy.core.handler.event')
005.958  000.046  000.046: require('lazy.core.handler.cmd')
005.983  000.024  000.024: require('lazy.core.handler.ft')
006.345  000.086  000.086: sourcing nvim_exec2() called at /usr/share/nvim/runtime/filetype.lua:0
006.348  000.127  000.041: sourcing /usr/share/nvim/runtime/filetype.lua
006.350  000.146  000.019: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
006.399  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
006.407  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
006.647  000.035  000.035: require('vague.config.internal')
006.650  000.241  000.206: require('vague')
007.171  000.103  000.103: sourcing nvim_exec2() called at /home/crowll/.local/share/nvim/lazy/vague.nvim/colors/vague.lua:0
007.255  000.080  000.080: sourcing nvim_exec2() called at /home/crowll/.local/share/nvim/lazy/vague.nvim/colors/vague.lua:0
007.374  000.048  000.048: require('vague.groups.common')
007.461  000.030  000.030: require('vague.utilities')
007.462  000.081  000.051: require('vague.groups.diff')
007.525  000.053  000.053: require('vague.groups.cmp')
007.554  000.023  000.023: require('vague.groups.blink')
007.583  000.024  000.024: require('vague.groups.netrw')
007.675  000.042  000.042: require('vague.groups.syntax')
007.721  000.045  000.045: require('vague.groups.treesitter')
007.723  000.122  000.035: require('vague.groups.lsp-native')
007.793  000.039  000.039: require('vague.groups.lsp-plugin')
007.830  000.030  000.030: require('vague.groups.neotest')
007.887  000.053  000.053: require('vague.groups.mini')
007.923  000.032  000.032: require('vague.groups.neotree')
007.995  000.064  000.064: require('vague.groups.telescope')
008.062  000.040  000.040: require('vague.groups.treesitter-context')
008.091  000.028  000.028: require('vague.groups.dashboard')
008.126  000.032  000.032: require('vague.groups.snacks-picker')
008.167  000.037  000.037: require('vague.groups.snacks-input')
008.223  000.055  000.055: require('vague.groups.snacks-indent')
008.282  000.058  000.058: require('vague.groups.rainbow-delimiters')
008.390  000.104  000.104: require('vague.groups.vim-better-whitespace')
008.461  000.069  000.069: require('vague.groups.fzf-lua')
008.481  001.194  000.203: require('vague.groups')
008.482  001.224  000.030: require('vague.highlights')
009.101  000.035  000.035: require('vague.terminal')
009.108  002.064  000.623: sourcing /home/crowll/.local/share/nvim/lazy/vague.nvim/colors/vague.lua
009.918  000.064  000.064: require('treesitter-context.config')
009.924  000.256  000.192: require('treesitter-context')
009.956  000.314  000.058: sourcing /home/crowll/.local/share/nvim/lazy/nvim-treesitter-context/plugin/treesitter-context.lua
009.959  000.337  000.023: sourcing nvim_exec2() called at /home/crowll/.local/share/nvim/lazy/playground/plugin/nvim-treesitter-playground.lua:0
009.964  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/.local/share/nvim/lazy/playground/plugin/nvim-treesitter-playground.lua:0
009.970  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/.local/share/nvim/lazy/playground/plugin/nvim-treesitter-playground.lua:0
010.222  000.034  000.034: require('nvim-treesitter.utils')
010.636  000.065  000.065: require('vim.treesitter.language')
010.677  000.025  000.025: require('vim.func')
010.712  000.031  000.031: require('vim.func._memoize')
010.730  000.251  000.129: require('vim.treesitter.query')
010.766  000.035  000.035: require('vim.treesitter._range')
010.782  000.375  000.089: require('vim.treesitter.languagetree')
010.787  000.405  000.030: require('vim.treesitter')
011.160  000.936  000.531: require('nvim-treesitter.parsers')
011.538  000.115  000.115: require('nvim-treesitter.compat')
011.615  000.038  000.038: require('nvim-treesitter.ts_utils')
011.619  000.081  000.042: require('nvim-treesitter.tsrange')
011.638  000.018  000.018: require('nvim-treesitter.caching')
011.644  000.382  000.169: require('nvim-treesitter.query')
011.653  000.450  000.068: require('nvim-treesitter.configs')
011.654  000.493  000.043: require('nvim-treesitter.info')
011.688  000.033  000.033: require('nvim-treesitter.shell_command_selectors')
011.701  001.642  000.146: require('nvim-treesitter.install')
011.722  000.021  000.021: require('nvim-treesitter.statusline')
011.746  000.023  000.023: require('nvim-treesitter.query_predicates')
011.747  001.711  000.026: require('nvim-treesitter')
011.799  000.050  000.050: require('vim.iter')
012.182  002.170  000.410: sourcing /home/crowll/.local/share/nvim/lazy/nvim-treesitter/plugin/nvim-treesitter.lua
012.187  002.194  000.024: sourcing nvim_exec2() called at /home/crowll/.local/share/nvim/lazy/playground/plugin/nvim-treesitter-playground.lua:0
012.193  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/.local/share/nvim/lazy/playground/plugin/nvim-treesitter-playground.lua:0
012.201  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/.local/share/nvim/lazy/playground/plugin/nvim-treesitter-playground.lua:0
012.655  000.040  000.040: require('nvim-treesitter.indent')
012.692  000.022  000.022: require('nvim-treesitter.highlight')
012.697  003.352  000.754: require('nvim-treesitter.parsers')
012.699  003.487  000.134: require('nvim-treesitter-playground')
012.790  003.601  000.114: sourcing /home/crowll/.local/share/nvim/lazy/playground/plugin/nvim-treesitter-playground.lua
012.794  003.624  000.023: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
012.800  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
012.807  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
013.329  000.466  000.466: sourcing /home/crowll/.local/share/nvim/lazy/vim-fugitive/plugin/fugitive.vim
013.358  000.514  000.048: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
013.374  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
013.477  000.008  000.008: sourcing /home/crowll/.local/share/nvim/lazy/vim-fugitive/ftdetect/fugitive.vim
013.480  000.040  000.032: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
013.487  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
013.870  000.053  000.053: sourcing /home/crowll/.local/share/nvim/lazy/nvim-web-devicons/plugin/nvim-web-devicons.vim
013.878  000.095  000.042: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
013.901  000.003  000.003: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
013.917  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
013.959  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
013.968  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
014.340  000.043  000.043: require('lualine_require')
014.543  000.567  000.524: require('lualine')
014.889  000.015  000.015: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.381  000.150  000.150: require('vague.groups.lualine')
015.668  000.015  000.015: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.792  000.007  000.007: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.804  000.003  000.003: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.812  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.845  000.003  000.003: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.855  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.873  000.014  000.014: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.882  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.888  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.893  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.902  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.908  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.916  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
015.930  000.012  000.012: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
016.454  000.106  000.106: require('lualine.utils.mode')
016.833  000.019  000.019: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
016.853  000.009  000.009: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.147  000.016  000.016: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.159  000.003  000.003: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.169  000.003  000.003: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.175  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.189  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.261  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.269  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.348  000.003  000.003: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.354  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.361  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.365  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.370  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.377  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.382  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.390  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.395  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.402  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.407  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.414  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.419  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.427  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.539  000.006  000.006: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.545  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.549  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.554  000.003  000.003: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.897  000.006  000.006: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.909  000.003  000.003: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.914  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.920  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.927  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.943  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.951  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.971  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.979  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.984  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.988  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.995  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
017.999  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.006  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.013  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.017  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.023  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.028  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.034  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.038  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.043  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.051  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.055  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.062  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.088  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.093  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.099  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.105  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.720  000.017  000.017: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.751  000.006  000.006: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.765  000.011  000.011: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.773  000.005  000.005: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
018.779  000.003  000.003: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
019.140  000.098  000.098: sourcing /home/crowll/.local/share/nvim/lazy/vim-commentary/plugin/commentary.vim
019.144  000.152  000.054: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
019.151  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
019.160  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
019.397  000.016  000.016: sourcing /home/crowll/.local/share/nvim/lazy/zen-mode.nvim/plugin/zen-mode.vim
019.402  000.045  000.029: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
019.407  000.002  000.002: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
019.414  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
019.742  000.049  000.049: require('zen-mode.util')
019.749  000.101  000.053: require('zen-mode.config')
019.877  000.069  000.069: require('zen-mode.plugins')
019.881  000.130  000.062: require('zen-mode.view')
019.883  000.416  000.184: require('zen-mode')
019.982  000.004  000.004: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
019.990  000.005  000.005: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
020.379  000.009  000.009: require('vim.F')
020.859  000.515  000.506: sourcing /home/crowll/.local/share/nvim/lazy/vimwiki/autoload/vimwiki/vars.vim
021.909  000.164  000.164: sourcing /home/crowll/.local/share/nvim/lazy/vimwiki/autoload/vimwiki/u.vim
024.267  000.030  000.030: sourcing /home/crowll/.local/share/nvim/lazy/vimwiki/autoload/vimwiki/markdown_base.vim
024.355  000.855  000.825: sourcing /home/crowll/.local/share/nvim/lazy/vimwiki/autoload/vimwiki/base.vim
024.468  000.072  000.072: sourcing /home/crowll/.local/share/nvim/lazy/vimwiki/autoload/vimwiki/path.vim
026.489  006.404  004.797: sourcing /home/crowll/.local/share/nvim/lazy/vimwiki/plugin/vimwiki.vim
026.495  006.434  000.030: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
026.509  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
026.524  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
026.757  000.014  000.014: sourcing /home/crowll/.local/share/nvim/lazy/plenary.nvim/plugin/plenary.vim
026.759  000.036  000.022: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
026.762  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
026.770  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
026.987  000.187  000.187: sourcing /home/crowll/.local/share/nvim/lazy/telescope.nvim/plugin/telescope.lua
026.991  000.207  000.019: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
026.995  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
027.002  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
027.408  000.403  000.403: require('telescope.builtin')
027.491  000.003  000.003: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
027.499  000.001  000.001: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
027.948  000.118  000.118: require('nvim-tree.log')
028.184  000.058  000.058: require('nvim-tree.notify')
028.189  000.148  000.090: require('nvim-tree.events')
028.328  000.062  000.062: require('nvim-tree.iterators.node-iterator')
028.356  000.166  000.104: require('nvim-tree.utils')
028.370  000.420  000.105: require('nvim-tree.view')
028.542  000.049  000.049: require('nvim-tree.core')
028.660  000.061  000.061: require('nvim-tree.git.utils')
028.692  000.031  000.031: require('nvim-tree.renderer.components.devicons')
028.767  000.038  000.038: require('nvim-tree.classic')
028.772  000.079  000.041: require('nvim-tree.node')
028.776  000.233  000.061: require('nvim-tree.node.directory')
028.777  000.321  000.040: require('nvim-tree.actions.finders.find-file')
028.819  000.041  000.041: require('nvim-tree.actions.finders.search-node')
028.820  000.397  000.035: require('nvim-tree.actions.finders')
028.932  000.041  000.041: require('nvim-tree.node.file')
028.933  000.077  000.036: require('nvim-tree.actions.fs.create-file')
029.033  000.052  000.052: require('nvim-tree.lib')
029.103  000.038  000.038: require('nvim-tree.node.link')
029.133  000.099  000.061: require('nvim-tree.node.directory-link')
029.136  000.202  000.052: require('nvim-tree.actions.fs.remove-file')
029.184  000.047  000.047: require('nvim-tree.actions.fs.rename-file')
029.232  000.047  000.047: require('nvim-tree.actions.fs.trash')
029.234  000.413  000.040: require('nvim-tree.actions.fs')
029.349  000.045  000.045: require('nvim-tree.diagnostics')
029.351  000.083  000.037: require('nvim-tree.actions.moves.item')
029.379  000.027  000.027: require('nvim-tree.actions.moves.parent')
029.443  000.063  000.063: require('nvim-tree.actions.moves.sibling')
029.444  000.209  000.036: require('nvim-tree.actions.moves')
029.616  000.096  000.096: require('nvim-tree.actions.node.file-popup')
029.977  000.249  000.249: require('nvim-tree.renderer.components.full-name')
030.023  000.405  000.156: require('nvim-tree.actions.node.open-file')
030.079  000.055  000.055: require('nvim-tree.actions.node.run-command')
030.142  000.062  000.062: require('nvim-tree.actions.node.system-open')
030.164  000.021  000.021: require('nvim-tree.actions.node.buffer')
030.165  000.721  000.082: require('nvim-tree.actions.node')
030.208  000.020  000.020: require('nvim-tree.actions.root.change-dir')
030.235  000.026  000.026: require('nvim-tree.actions.root.dir-up')
030.237  000.071  000.024: require('nvim-tree.actions.root')
030.281  000.020  000.020: require('nvim-tree.actions.tree.find-file')
030.323  000.021  000.021: require('nvim-tree.actions.tree.modifiers.collapse')
030.347  000.022  000.022: require('nvim-tree.actions.tree.modifiers.expand')
030.348  000.065  000.022: require('nvim-tree.actions.tree.modifiers')
030.368  000.020  000.020: require('nvim-tree.actions.tree.open')
030.389  000.019  000.019: require('nvim-tree.actions.tree.toggle')
030.408  000.018  000.018: require('nvim-tree.actions.tree.resize')
030.409  000.171  000.028: require('nvim-tree.actions.tree')
030.409  002.039  000.057: require('nvim-tree.actions')
030.549  000.134  000.134: require('vim.diagnostic')
030.563  003.057  000.347: require('nvim-tree')
030.605  000.034  000.034: require('nvim-tree.legacy')
030.668  000.011  000.011: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
030.676  000.005  000.005: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
030.718  000.033  000.033: require('nvim-tree.keymap')
030.756  000.037  000.037: require('nvim-tree.appearance')
031.058  000.033  000.033: require('nvim-tree.buffers')
031.119  000.027  000.027: require('nvim-tree.git.runner')
031.159  000.039  000.039: require('nvim-tree.watcher')
031.163  000.102  000.036: require('nvim-tree.git')
031.263  000.046  000.046: require('nvim-tree.node.file-link')
031.265  000.071  000.026: require('nvim-tree.node.factory')
031.332  000.066  000.066: require('nvim-tree.node.root')
031.390  000.025  000.025: require('nvim-tree.enum')
031.394  000.061  000.036: require('nvim-tree.explorer.filters')
031.437  000.043  000.043: require('nvim-tree.marks')
031.480  000.040  000.040: require('nvim-tree.explorer.live-filter')
031.543  000.062  000.062: require('nvim-tree.explorer.sorter')
031.608  000.064  000.064: require('nvim-tree.actions.fs.clipboard')
031.743  000.031  000.031: require('nvim-tree.renderer.decorator')
031.746  000.061  000.030: require('nvim-tree.renderer.decorator.bookmarks')
031.774  000.027  000.027: require('nvim-tree.renderer.decorator.copied')
031.814  000.039  000.039: require('nvim-tree.renderer.decorator.cut')
031.860  000.044  000.044: require('nvim-tree.renderer.decorator.diagnostics')
031.907  000.046  000.046: require('nvim-tree.renderer.decorator.git')
031.946  000.038  000.038: require('nvim-tree.renderer.decorator.hidden')
031.999  000.051  000.051: require('nvim-tree.renderer.decorator.modified')
032.030  000.029  000.029: require('nvim-tree.renderer.decorator.opened')
032.073  000.042  000.042: require('nvim-tree.renderer.decorator.user')
032.107  000.032  000.032: require('nvim-tree.renderer.components.padding')
032.111  000.465  000.056: require('nvim-tree.renderer.builder')
032.115  000.505  000.040: require('nvim-tree.renderer')
032.122  001.147  000.101: require('nvim-tree.explorer')
032.153  000.031  000.031: require('nvim-tree.explorer.watch')
032.188  000.030  000.030: require('nvim-tree.renderer.components')
032.511  000.278  000.278: require('nvim-web-devicons.filetypes')
032.663  000.109  000.109: require('nvim-web-devicons.default.icons_by_filename')
032.902  000.236  000.236: require('nvim-web-devicons.default.icons_by_file_extension')
032.984  000.080  000.080: require('nvim-web-devicons.default.icons_by_operating_system')
033.021  000.036  000.036: require('nvim-web-devicons.default.icons_by_desktop_environment')
033.048  000.026  000.026: require('nvim-web-devicons.default.icons_by_window_manager')
033.049  000.526  000.040: require('nvim-web-devicons.icons-default')
033.245  001.055  000.251: require('nvim-web-devicons')
034.844  000.076  000.076: require('nvim-tree.help')
034.969  000.059  000.059: require('nvim-tree.appearance.hi-test')
035.014  000.167  000.108: require('nvim-tree.api')
035.105  000.051  000.051: require('nvim-tree.commands')
035.319  000.034  000.034: sourcing /usr/share/nvim/runtime/plugin/editorconfig.lua
035.323  000.059  000.025: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
035.426  000.086  000.086: sourcing /usr/share/nvim/runtime/plugin/gzip.vim
035.428  000.101  000.014: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
035.489  000.041  000.041: sourcing /usr/share/nvim/runtime/plugin/man.lua
035.491  000.056  000.015: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
035.860  000.095  000.095: sourcing /usr/share/nvim/runtime/pack/dist/opt/matchit/plugin/matchit.vim
035.871  000.363  000.268: sourcing /usr/share/nvim/runtime/plugin/matchit.vim
035.873  000.378  000.015: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
035.952  000.061  000.061: sourcing /usr/share/nvim/runtime/plugin/matchparen.vim
035.954  000.075  000.014: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
035.977  000.005  000.005: sourcing /usr/share/nvim/runtime/plugin/netrwPlugin.vim
035.980  000.023  000.018: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
036.054  000.040  000.040: sourcing /usr/share/nvim/runtime/plugin/osc52.lua
036.056  000.056  000.016: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
036.149  000.078  000.078: sourcing /usr/share/nvim/runtime/plugin/rplugin.vim
036.151  000.093  000.015: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
036.197  000.030  000.030: sourcing /usr/share/nvim/runtime/plugin/shada.vim
036.198  000.044  000.015: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
036.230  000.008  000.008: sourcing /usr/share/nvim/runtime/plugin/spellfile.vim
036.231  000.031  000.023: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
036.294  000.042  000.042: sourcing /usr/share/nvim/runtime/plugin/tarPlugin.vim
036.296  000.061  000.019: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
036.355  000.036  000.036: sourcing /usr/share/nvim/runtime/plugin/tohtml.lua
036.358  000.059  000.023: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
036.405  000.015  000.015: sourcing /usr/share/nvim/runtime/plugin/tutor.vim
036.407  000.045  000.030: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
036.523  000.092  000.092: sourcing /usr/share/nvim/runtime/plugin/zipPlugin.vim
036.527  000.115  000.023: sourcing nvim_exec2() called at /home/crowll/tools/dotfiles/nvim/init.lua:0
037.496  000.123  000.123: sourcing nvim_exec2() called at /home/crowll/.local/share/nvim/lazy/vague.nvim/colors/vague.lua:0
037.664  000.158  000.158: sourcing nvim_exec2() called at /home/crowll/.local/share/nvim/lazy/vague.nvim/colors/vague.lua:0
038.254  000.929  000.649: sourcing /home/crowll/.local/share/nvim/lazy/vague.nvim/colors/vague.lua
038.474  000.011  000.011: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.743  000.004  000.004: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.750  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.753  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.756  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.758  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.761  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.763  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.767  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.770  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.772  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.774  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.775  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.781  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.782  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.784  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.786  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.787  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.789  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.791  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.792  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.794  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.795  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.797  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.800  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.801  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.802  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.804  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.805  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.823  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.825  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.829  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.830  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.831  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.833  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.836  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.837  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.838  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.840  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.841  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.844  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.846  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.847  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.848  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.850  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.851  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.854  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.856  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.857  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.860  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.861  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.864  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.865  000.000  000.000: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.867  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.868  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.912  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.915  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.917  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.918  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.920  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.921  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.924  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.925  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.926  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.928  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.946  000.002  000.002: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.955  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.961  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.967  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
038.972  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.005  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.014  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.018  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.023  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.028  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.031  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.036  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.040  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.071  000.007  000.007: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.126  000.003  000.003: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.130  000.002  000.002: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.162  000.002  000.002: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.168  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.174  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.179  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.184  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.190  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.194  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.202  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.206  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.209  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.215  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.218  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.235  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.240  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.247  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.252  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.257  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.261  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.287  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.313  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.319  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.325  000.003  000.003: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.328  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.331  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.334  000.002  000.002: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.409  000.004  000.004: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.416  000.002  000.002: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.423  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.426  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.430  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.436  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.441  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.450  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.455  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.459  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.481  000.002  000.002: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.489  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.501  000.002  000.002: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.506  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.522  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.531  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.535  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.538  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.543  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.547  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.551  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.574  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.578  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.583  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.594  000.008  000.008: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.598  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.604  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.608  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.721  000.011  000.011: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.737  000.004  000.004: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.747  000.008  000.008: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.752  000.004  000.004: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.757  000.002  000.002: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
039.782  000.001  000.001: sourcing nvim_exec2() called at ColorScheme Autocommands for "*":0
042.934  039.759  015.198: require('options.lazy')
043.934  000.583  000.583: require('vim.lsp.log')
044.365  000.426  000.426: require('vim.lsp.protocol')
044.561  000.188  000.188: require('vim.lsp.util')
044.661  000.041  000.041: require('vim.lsp.sync')
044.666  000.103  000.062: require('vim.lsp._changetracking')
044.797  000.069  000.069: require('vim.lsp._transport')
044.825  000.156  000.087: require('vim.lsp.rpc')
044.926  001.985  000.529: require('vim.lsp')
044.959  042.323  000.146: sourcing /home/crowll/.config/nvim/init.lua
044.992  000.354: sourcing vimrc file(s)
045.223  000.068  000.068: sourcing /usr/share/nvim/runtime/filetype.lua
045.399  000.049  000.049: sourcing /usr/share/nvim/runtime/syntax/synload.vim
045.449  000.169  000.120: sourcing /usr/share/nvim/runtime/syntax/syntax.vim
045.453  000.224: inits 3
047.441  001.987: reading ShaDa
047.529  000.089: opening buffers
047.622  000.092: BufEnter autocommands
047.623  000.002: editing files in windows
047.655  000.031: VimEnter autocommands
047.752  000.058  000.058: require('vim.termcap')
047.776  000.014  000.014: require('vim.text')
047.820  000.093: UIEnter autocommands
048.030  000.141  000.141: sourcing /usr/share/nvim/runtime/autoload/provider/clipboard.vim
048.035  000.074: before starting main loop
048.242  000.207: first screen update
048.243  000.001: --- NVIM STARTED ---

