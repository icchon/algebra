module IdMap = Map.Make (Int)

module CubicBasis = struct
  type t = { roots : int IdMap.t; omega : int }

  let compare b1 b2 =
    let c = IdMap.compare Int.compare b1.roots b2.roots in
    if c <> 0 then c else Int.compare b1.omega b2.omega

  let empty = { roots = IdMap.empty; omega = 0 }
end

module CubicBasisMap = Map.Make (CubicBasis)

module CubicNumber = struct
  type state = {
    mutable w_to_id : int Q_rootp.CoeffMap.t;
    mutable id_to_w : Q_rootp.t array;
  }

  let global_l2_registry = { w_to_id = Q_rootp.CoeffMap.empty; id_to_w = [||] }

  type t = { terms : Q_rootp.t CubicBasisMap.t }

  let zero = { terms = CubicBasisMap.empty }
  let derive _ = zero
  let one = { terms = CubicBasisMap.singleton CubicBasis.empty Q_rootp.one }
  let is_zero x = CubicBasisMap.is_empty x.terms

  (* 括弧が必要な項かどうかを判定するローカル関数 *)
  let is_atomic_str s =
    let n = String.length s in
    let rec check i depth =
      if i >= n then true
      else
        match s.[i] with
        | '{' -> check (i + 1) (depth + 1)
        | '}' -> check (i + 1) (depth - 1)
        | ('+' | '-') when depth = 0 ->
            if i = 0 then check (i + 1) depth else false
        | _ -> check (i + 1) depth
    in
    check 0 0

  let extract_omega (q : Q_rootp.t) =
    let target_basis = Q_rootp.PrimeSet.of_list [ -1; 3 ] in
    let v_coeff =
      match Q_rootp.CoeffMap.find_opt target_basis q.terms with
      | Some c -> c
      | None -> Q_rootp.RationalField.zero
    in
    let v = Q_rootp.make_rational v_coeff in
    let u =
      Q_rootp.add q (Q_rootp.neg (Q_rootp.mul v (Q_rootp.make_root (-3))))
    in
    let omega_coeff = Q_rootp.mul v (Q_rootp.of_int 2) in
    let const_part = Q_rootp.add u v in
    (const_part, omega_coeff)

  let omega =
    let b = { CubicBasis.roots = IdMap.empty; CubicBasis.omega = 1 } in
    { terms = CubicBasisMap.singleton b Q_rootp.one }

  let make_rational r =
    let q = Q_rootp.make_rational r in
    if Q_rootp.is_zero q then zero
    else { terms = CubicBasisMap.singleton CubicBasis.empty q }

  let of_q_rootp q =
    let q = Q_rootp.cleanup q in
    if Q_rootp.is_zero q then zero
    else
      let const_p, omega_p = extract_omega q in
      let res = ref CubicBasisMap.empty in
      if not (Q_rootp.is_zero const_p) then
        res := CubicBasisMap.add CubicBasis.empty const_p !res;
      (if not (Q_rootp.is_zero omega_p) then
         let b_omega =
           { CubicBasis.roots = IdMap.empty; CubicBasis.omega = 1 }
         in
         res := CubicBasisMap.add b_omega omega_p !res);
      { terms = !res }

  let multiply_basis (b1 : CubicBasis.t) (b2 : CubicBasis.t) =
    let res_coeff = ref Q_rootp.one in
    let new_omega = b1.omega + b2.omega in
    let new_roots =
      IdMap.merge
        (fun id e1 e2 ->
          let sum = Option.value ~default:0 e1 + Option.value ~default:0 e2 in
          if sum >= 3 then (
            let w = global_l2_registry.id_to_w.(id) in
            for _ = 1 to sum / 3 do
              res_coeff := Q_rootp.mul !res_coeff w
            done;
            let rem = sum mod 3 in
            if rem = 0 then None else Some rem)
          else if sum = 0 then None
          else Some sum)
        b1.roots b2.roots
    in
    (!res_coeff, { CubicBasis.roots = new_roots; CubicBasis.omega = new_omega })

  let normalize x =
    let temp_map = ref CubicBasisMap.empty in
    let add_term b c =
      if Q_rootp.is_zero c then ()
      else
        let old =
          CubicBasisMap.find_opt b !temp_map
          |> Option.value ~default:Q_rootp.zero
        in
        temp_map := CubicBasisMap.add b (Q_rootp.add old c) !temp_map
    in
    (* Step 1: 立方根部分の簡約 *)
    x.terms
    |> CubicBasisMap.iter (fun b c ->
        let current_coeff = ref c in
        let new_roots = ref IdMap.empty in
        b.roots
        |> IdMap.iter (fun id p ->
            let w = global_l2_registry.id_to_w.(id) in
            match Q_rootp.cubic_root w with
            | Some k ->
                let factor = if p = 1 then k else Q_rootp.mul k k in
                current_coeff := Q_rootp.mul !current_coeff factor
            | None -> new_roots := IdMap.add id p !new_roots);
        add_term { b with roots = !new_roots } !current_coeff);
    (* Step 2: omega の正規化 (omega^2 -> -1 - omega) *)
    let final_map = ref CubicBasisMap.empty in
    let add_final b c =
      if Q_rootp.is_zero c then ()
      else
        let old =
          CubicBasisMap.find_opt b !final_map
          |> Option.value ~default:Q_rootp.zero
        in
        final_map := CubicBasisMap.add b (Q_rootp.add old c) !final_map
    in
    !temp_map
    |> CubicBasisMap.iter (fun b c ->
        let rem = ((b.omega mod 3) + 3) mod 3 in
        if rem = 2 then (
          add_final { b with omega = 0 } (Q_rootp.neg c);
          add_final { b with omega = 1 } (Q_rootp.neg c))
        else add_final { b with omega = rem } c);
    let cleaned_map =
      CubicBasisMap.filter (fun _ c -> not (Q_rootp.is_zero c)) !final_map
    in
    { terms = cleaned_map }

  let add x y =
    let f _basis c1 c2 =
      match (c1, c2) with
      | Some v1, Some v2 ->
          let sum = Q_rootp.add v1 v2 in
          if Q_rootp.is_zero sum then None else Some sum
      | Some v, None | None, Some v -> Some v
      | None, None -> None
    in
    normalize { terms = CubicBasisMap.merge f x.terms y.terms }

  let neg x = { terms = CubicBasisMap.map Q_rootp.neg x.terms }

  let mul x y =
    let res = ref CubicBasisMap.empty in
    CubicBasisMap.iter
      (fun b1 c1 ->
        CubicBasisMap.iter
          (fun b2 c2 ->
            let coeff_l1, b_prod = multiply_basis b1 b2 in
            let new_val = Q_rootp.mul (Q_rootp.mul c1 c2) coeff_l1 in
            let old_val =
              CubicBasisMap.find_opt b_prod !res
              |> Option.value ~default:Q_rootp.zero
            in
            res := CubicBasisMap.add b_prod (Q_rootp.add old_val new_val) !res)
          y.terms)
      x.terms;
    normalize { terms = !res }

  (* Helper to split x into A + B*omega where A, B have no omega *)
  let split_omega x =
    let a_terms = ref CubicBasisMap.empty in
    let b_terms = ref CubicBasisMap.empty in
    CubicBasisMap.iter
      (fun b c ->
        if b.omega = 1 then
          b_terms := CubicBasisMap.add { b with omega = 0 } c !b_terms
        else if b.omega = 0 then a_terms := CubicBasisMap.add b c !a_terms
        else () (* Should be normalized, but ignore omega=2 just in case *))
      x.terms;
    ({ terms = !a_terms }, { terms = !b_terms })

  let rec inv x =
    if is_zero x then failwith "Division by zero";
    (* 分解: x = A + B*omega *)
    let a, b = split_omega x in
    if is_zero b then (
      (* omegaがない場合、純粋な拡大体（rootsのみ）の問題として解く *)
      (* ここでは omega=0 を仮定した基底のみを生成することで特異行列を回避 *)
      let ids =
        CubicBasisMap.fold
          (fun (b : CubicBasis.t) _ acc ->
            IdMap.fold
              (fun id _ s -> if List.mem id s then s else id :: s)
              b.roots acc)
          x.terms []
      in
      let rec gen_combinations = function
        | [] ->
            [ { CubicBasis.roots = IdMap.empty; omega = 0 } ] (* omega=0 のみ生成 *)
        | id :: rest ->
            let sub = gen_combinations rest in
            let with1 =
              List.map
                (fun (b : CubicBasis.t) ->
                  { b with roots = IdMap.add id 1 b.roots })
                sub
            in
            let with2 =
              List.map
                (fun (b : CubicBasis.t) ->
                  { b with roots = IdMap.add id 2 b.roots })
                sub
            in
            sub @ with1 @ with2
      in
      let local_space = gen_combinations ids in
      let size = List.length local_space in
      let b_to_idx =
        List.mapi (fun i b -> (b, i)) local_space
        |> List.to_seq |> CubicBasisMap.of_seq
      in
      let idx_to_b = Array.of_list local_space in
      let module Rule = struct
        type coeff = Q_rootp.t
        type basis = CubicBasis.t

        let all = idx_to_b
        let size = size
        let to_int b = CubicBasisMap.find b b_to_idx
        let multiply = multiply_basis
      end in
      let module E = Extension_field.Make (Q_rootp) (Rule) in
      let x_arr = Array.make size Q_rootp.zero in
      CubicBasisMap.iter (fun b c -> x_arr.(Rule.to_int b) <- c) x.terms;
      let res_arr = E.inv x_arr in
      let res_terms = ref CubicBasisMap.empty in
      Array.iteri
        (fun i c ->
          if not (Q_rootp.is_zero c) then
            res_terms := CubicBasisMap.add idx_to_b.(i) c !res_terms)
        res_arr;
      normalize { terms = !res_terms })
    else
      (* omegaがある場合: 1/(A+B*omega) = (A-B - B*omega) / (A^2 - AB + B^2) *)
      let aa = mul a a in
      let ab = mul a b in
      let bb = mul b b in
      let norm = add (add aa (neg ab)) bb in
      (* Norm = A^2 - AB + B^2 *)
      let inv_norm = inv norm in
      (* 再帰: normはomegaを含まないはずなので上の分岐に入る *)
      let num_real = add a (neg b) in
      (* Numerator Real part: A - B *)
      let num_imag = neg b in
      (* Numerator Omega part: -B *)
      let res_real = mul num_real inv_norm in
      let res_imag = mul num_imag inv_norm in
      (* res = res_real + res_imag * omega *)

      (* omega を掛ける処理を map 操作で代用 *)
      let res_imag_shifted =
        let m = ref CubicBasisMap.empty in
        CubicBasisMap.iter
          (fun b c -> m := CubicBasisMap.add { b with omega = 1 } c !m)
          res_imag.terms;
        { terms = !m }
      in
      add res_real res_imag_shifted

  let register_as_basis w =
    let w = Q_rootp.cleanup w in
    let id =
      let existing_id = ref (-1) in
      Array.iteri
        (fun i w_reg -> if Q_rootp.equal w w_reg then existing_id := i)
        global_l2_registry.id_to_w;
      if !existing_id <> -1 then !existing_id
      else
        let new_id = Array.length global_l2_registry.id_to_w in
        global_l2_registry.id_to_w <-
          Array.append global_l2_registry.id_to_w [| w |];
        new_id
    in
    let b = { CubicBasis.roots = IdMap.singleton id 1; CubicBasis.omega = 0 } in
    { terms = CubicBasisMap.singleton b Q_rootp.one }

  let make_root3 w =
    let w = Q_rootp.cleanup w in
    if Q_rootp.is_zero w then zero
    else
      match Q_rootp.cubic_root w with
      | Some k -> of_q_rootp k
      | None -> register_as_basis w

  let basis_to_latex (b : CubicBasis.t) =
    let o_str =
      match b.omega with 0 -> "" | 1 -> "\\omega" | 2 -> "\\omega^2" | _ -> ""
    in
    let r_str =
      if IdMap.is_empty b.roots then ""
      else
        let terms =
          IdMap.fold
            (fun id p acc ->
              let w_str =
                Q_rootp.to_string_latex global_l2_registry.id_to_w.(id)
              in
              let s = "\\sqrt[3]{" ^ w_str ^ "}" in
              (if p = 1 then s else s ^ "^{" ^ string_of_int p ^ "}") :: acc)
            b.roots []
        in
        String.concat "" (List.rev terms)
    in
    match (o_str, r_str) with "", s | s, "" -> s | os, rs -> os ^ rs

  let to_string_latex x =
    let items = ref [] in
    CubicBasisMap.iter
      (fun b c ->
        let c_str = Q_rootp.to_string_latex c in
        let b_str = basis_to_latex b in
        let term =
          match b_str with
          | "" -> c_str
          | _ ->
              if c_str = "1" then b_str
              else if c_str = "-1" then "-" ^ b_str
              else if is_atomic_str c_str then c_str ^ b_str
              else "\\left(" ^ c_str ^ "\\right)" ^ b_str
        in
        items := term :: !items)
      x.terms;
    match List.rev !items with
    | [] -> "0"
    | hd :: tl ->
        List.fold_left
          (fun acc t ->
            if String.length t > 0 && t.[0] = '-' then
              acc ^ " - " ^ String.sub t 1 (String.length t - 1)
            else acc ^ " + " ^ t)
          hd tl

  let equal x y = CubicBasisMap.equal Q_rootp.equal x.terms y.terms

  include Add_monoid.Extend (struct
    type nonrec t = t

    let zero, add, equal = (zero, add, equal)
  end)

  include Add_group.Extend (struct
    type nonrec t = t

    let neg, add = (neg, add)
  end)

  include Mul_monoid.Extend (struct
    type nonrec t = t

    let one, mul, equal = (one, mul, equal)
  end)

  include Mul_group.Extend (struct
    type nonrec t = t

    let mul, inv = (mul, inv)
  end)

  include Division_ring.Extend (struct
    type nonrec t = t

    let mul, inv, pow = (mul, inv, pow)
  end)
end

include CubicNumber
