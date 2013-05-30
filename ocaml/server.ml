let hashex input = Sha256.to_hex (Sha256.string input) ;;

let proved input =
        let proof = hashex input in
        let sentinel = String.sub proof 61 3 in
        sentinel = "000" ;;

let rec prover hash nonce =
        let hexnonce = Printf.sprintf "%x" nonce in
        if proved (hash ^ hexnonce)
        then hash ^ ":" ^ hexnonce
        else prover hash (nonce + 1) ;;

let main = Printf.printf "%s\n" (prover "0eccfe263668d171bd19b7d491c3ef5c43559e6d3acf697ef37596181c6fdf4c" 0) ;;
