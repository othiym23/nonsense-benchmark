(*
 * To build this program requires that you have OCaml, Findlib,
 * cryptokit, and Ocamlnet 3.x+ installed. I leave as an exercise
 * to the interested reader to figure out how to do that, but I
 * will say it took me the better part of an evening to get all the
 * dependencies to compile and install using a set of half-baked
 * Homebrew recipes I will probably publish as a tap.
 *)
let rec prover hash nonce =
    let hexnonce = Printf.sprintf "%x" nonce in

    let hashex input =
        let hencoder = Cryptokit.Hexa.encode() in
        let hex i    = Cryptokit.transform_string hencoder i in
        let sha256   = Cryptokit.Hash.sha256() in
        let hash     = Cryptokit.hash_string sha256 input in
        hex hash in

    let proved input =
        let proof    = hashex input in
        let sentinel = String.sub proof 61 3 in
        sentinel = "000" in

    if proved (hash ^ hexnonce)
      then hash ^ ":" ^ hexnonce
      else prover hash (nonce + 1)
;;

(* Boilerplate from http://projects.camlcity.org/projects/dl/ocamlnet-3.6.3/doc/html-main/Netplex_intro.html *)
class prover_processor : Netplex_types.processor =
  let empty_hooks = new Netplex_kit.empty_processor_hooks() in
object(self)
  inherit Netplex_kit.processor_base empty_hooks

  (* FIXME: this needs to be converted to work with Equeue to work properly *)
  method process ~when_done container fd proto_name =
    let rch = new Netchannels.socket_descr fd in
    let ich = Netchannels.lift_in  (`Raw (rch :> Netchannels.raw_in_channel)) in
    let och = Netchannels.lift_out (`Raw (rch :> Netchannels.raw_out_channel)) in

    (* send the handshake *)
    och # output_string "ok\n";
    och # flush();

    (* wait for response *)
    let payload = ich # input_line() in
    ich # close_in();

    (* get proof of work *)
    let proof = prover payload 0 in

    (* shut 'er down '*)
    och # output_string proof;
    och # close_out();
    when_done()

  method supported_ptypes = [ `Multi_processing; `Multi_threading ]
end

(* Boilerplate from http://projects.camlcity.org/projects/dl/ocamlnet-3.6.3/doc/html-main/Netplex_intro.html *)
class prover_processor_factory : Netplex_types.processor_factory =
object(self)
  method name = "work_prover"
  method create_processor ctrl_cfg cfg_file cfg_addr =
    new prover_processor
end

let main() =
  (* Create a parser for the standard Netplex command-line arguments: *)
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  (* Parse the command-line arguments: *)
  Arg.parse
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: server [options]";

  (* OCaml multithreading only uses one core(?!), so use multithreading *)
  let parallelizer = Netplex_mp.mp() in
  let factory = new prover_processor_factory in

  (* þe olde boot-straps puller *)
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories
    Netplex_workload.workload_manager_factories
    [ factory ]
    cmdline_cfg
;;

main()
