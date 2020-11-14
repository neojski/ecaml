open! Core_kernel
open! Ecaml

(* To build this file, run
   dune build hello_world.so

   To test, run
   emacs -Q -L _build/default --batch --eval "(progn (require 'hello_world) (say-hello \"emacs\"))"
*)

let term_exit_advice = Symbol.intern "term-handle-exit"
let cleanup = Symbol.intern "cleanup"
let window_config_register = Value.intern ":fzf-windows"

let fzf_buf () =
  Buffer.create ~name:"*fzf*"

let () =
  defun
    cleanup
    [%here]
    (Returns Value.Type.unit)
    ~docstring:"_"
    (let open Defun.Let_syntax in
     (* TODO: how do I know it requires three arguments? *)
     let%map_open _ = optional "x" ignored 
     and _ = optional "y" ignored 
     and _ = optional "z" ignored in

     let f = Advice.of_function cleanup in
     Advice.remove f ~from_function:term_exit_advice;

     let data =
       Current_buffer.contents ()
       |> Text.to_utf8_bytes
       |> String.split_lines
       |> List.last_exn
       |> String.chop_prefix_exn ~prefix:"> " 
       |> String.strip
     in

     Symbol.funcall1_i
       (Symbol.intern "jump-to-register")
       window_config_register;

     Buffer.Blocking.kill (fzf_buf ());

     message data;


     (* TODO: There's Selected_window.find_file but I don't know how to do async in functions *)
     Symbol.funcall1_i (Symbol.intern "find-file") (Value.of_utf8_bytes data)

    )

let make_term ~name ~prog ~args =
  Symbol.funcallN_i
    (Symbol.intern "make-term")
    ([ (Value.of_utf8_bytes name)
     ; (Value.of_utf8_bytes prog)
     ; Value.nil
     ]
     @ List.map args ~f:Value.of_utf8_bytes)
;;

let () =
  message "Hello, world!";
  defun
    ("say-hello" |> Symbol.intern)
    [%here]
    ~docstring:{|
Takes one argument NAME and says "Hello, NAME"
|}
    (Returns Value.Type.unit)
    (let open Defun.Let_syntax in
     let%map_open name = required "name" string in

     Symbol.funcall1_i
       (Symbol.intern "window-configuration-to-register")
       window_config_register;

     let buf = fzf_buf () in
     make_term ~name:"fzf" ~prog:"sh" ~args:["-c"; "fzf --no-unicode"];

     Symbol.funcall1_i
       (Symbol.intern "split-window-vertically")
       (Value.of_int_exn (-10));
     (* Selected_window.split_vertically_exn (); *)

     Selected_window.other_window 1;
     Selected_window.Blocking.switch_to_buffer buf;

     Minor_mode.enable Minor_mode.visual_line;

     Current_buffer.set_buffer_local
       Mode_line.Format.in_buffer
       (Mode_line.Format.of_value_exn (Value.of_utf8_bytes "fzf"));

     (* TODO: Use minor_mode module. *)
     Symbol.funcall0_i (Symbol.intern "term-char-mode");

     let f = Advice.of_function cleanup in
     Advice.add f ~to_function:term_exit_advice;

     message ("Hello, " ^ name));
  provide ("hello_world" |> Symbol.intern)
;;
