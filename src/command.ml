open! Core_kernel
open! Import

include Value.Make_subtype (struct
    let name = "command"
    let here = [%here]
    let is_in_subtype = Value.is_command
  end)

module Raw_prefix_argument = struct
  type t =
    | Absent
    | Int of int
    | Minus
    | Nested of int
  [@@deriving sexp_of]

  let minus = "-" |> Value.intern

  let to_value = function
    | Absent -> Value.nil
    | Int i -> i |> Value.of_int_exn
    | Minus -> minus
    | Nested i -> Value.cons (i |> Value.of_int_exn) Value.nil
  ;;

  let of_value_exn value =
    if Value.is_nil value
    then Absent
    else if Value.is_integer value
    then Int (Value.to_int_exn value)
    else if Value.is_cons value
    then Nested (Value.car_exn value |> Value.to_int_exn)
    else if Value.eq value minus
    then Minus
    else raise_s [%message
           "[Raw_prefix_argument.of_value] got unexpected value" (value : Value.t)]
  ;;

  let for_current_command () = Symbol.value_exn Q.current_prefix_arg |> of_value_exn

  let numeric_value t =
    Symbol.funcall1 Q.prefix_numeric_value (t |> to_value) |> Value.to_int_exn
  ;;
end

let call_interactively value raw_prefix_argument =
  Symbol.set_value Q.current_prefix_arg
    (raw_prefix_argument |> Raw_prefix_argument.to_value);
  Symbol.funcall1_i Q.call_interactively value;
;;
