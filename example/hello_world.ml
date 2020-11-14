open! Core_kernel
open Async_kernel
open! Ecaml

(* This is heavily based on bling/fzf.el *)

(* To build this file, run
   dune build example/hello_world.so --watch

   To test, run
   emacs -Q -L _build/default/example --eval "(progn (require 'hello_world) (say-hello \"emacs\"))"
*)

let term_exit_advice = Symbol.intern "term-handle-exit"
let cleanup = Symbol.intern "cleanup"
let window_config_register = Value.intern ":fzf-windows"
let fzf_buf_name = "*fzf*"

let () =
  defun cleanup [%here] (Returns_deferred Value.Type.unit) ~docstring:"_"
    (* TODO: how do I know it requires three arguments? *)
    (let%map_open.Defun _ = optional "x" ignored
     and _ = optional "y" ignored
     and _ = optional "z" ignored in
     let data =
       Current_buffer.contents () |> Text.to_utf8_bytes |> String.split_lines
       |> List.last_exn
       |> String.chop_prefix_exn ~prefix:"> "
       |> String.strip
     in
     let%bind () =
       match Buffer.find ~name:fzf_buf_name with
       | Some buffer -> Buffer.kill buffer
       | None -> return ()
     in
     let f = Advice.of_function cleanup in
     Advice.remove f ~from_function:term_exit_advice;
     Symbol.funcall1_i (Symbol.intern "jump-to-register") window_config_register;
     Selected_window.find_file data)

let make_term ~name ~prog ~args =
  Symbol.funcallN_i
    (Symbol.intern "make-term")
    ( [ Value.of_utf8_bytes name; Value.of_utf8_bytes prog; Value.nil ]
      @ List.map args ~f:Value.of_utf8_bytes )

let () =
  defun
    ~interactive:No_arg
    ("say-hello" |> Symbol.intern)
    [%here] ~docstring:{|
Takes one argument NAME and says "Hello, NAME"
|}
    (Returns_deferred Value.Type.unit)
    (let%map_open.Defun _name = required "name" string in

     Symbol.funcall1_i
       (Symbol.intern "window-configuration-to-register")
       window_config_register;

     let buf = Buffer.create ~name:fzf_buf_name in
     make_term ~name:"fzf" ~prog:"sh" ~args:[ "-c"; "fzf --no-unicode" ];

     Symbol.funcall1_i
       (Symbol.intern "split-window-vertically")
       (Value.of_int_exn (-10));

     (* Selected_window.split_vertically_exn (); *)
     Selected_window.other_window 1;
     let%bind () = Selected_window.switch_to_buffer buf in

     Minor_mode.enable Minor_mode.visual_line;

     Current_buffer.set_buffer_local Mode_line.Format.in_buffer
       (Mode_line.Format.of_value_exn (Value.of_utf8_bytes "fzf"));

     (* TODO: Use minor_mode module. *)
     Symbol.funcall0_i (Symbol.intern "term-char-mode");

     let f = Advice.of_function cleanup in
     Advice.add f ~to_function:term_exit_advice;

     Symbol.funcall0_i (Symbol.intern "turn-off-evil-mode");

     return ());
  provide ("hello_world" |> Symbol.intern)
