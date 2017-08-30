(** [File] is for functions that operate on files on the filesystem. *)

open! Core_kernel
open! Import

(** [(Info-goto-node "(elisp)Information about Files")] *)
val exists        : string -> bool (** [(describe-function 'file-exists-p)] *)
val is_directory  : string -> bool (** [(describe-function 'file-directory-p)] *)
val is_executable : string -> bool (** [(describe-function 'file-executable-p)] *)
val is_readable   : string -> bool (** [(describe-function 'file-readable-p)] *)
val is_regular    : string -> bool (** [(describe-function 'file-regular-p)] *)
val is_symlink    : string -> bool (** [(describe-function 'file-symlink-p)] *)
val is_writable   : string -> bool (** [(describe-function 'file-writable-p)] *)

(** [(describe-function 'file-truename)]
    [(Info-goto-node "(elisp)Truenames")] *)
val truename : string -> string

(** [(describe-function 'delete-file)]
    [(Info-goto-node "(elisp)Changing Files")] *)
val delete : string -> unit

(** [(describe-function 'copy-file)]
    [(Info-goto-node "(elisp)Changing Files")] *)
val copy : src:string -> dst:string -> unit

(** [(describe-function 'rename-file)]
    [(Info-goto-node "(elisp)Changing Files")] *)
val rename : src:string -> dst:string -> unit
