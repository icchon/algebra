module PrimeSet = Set.Make (Int)
module CoeffMap = Map.Make (PrimeSet)
module RationalField = Rational
module BasisMap = Map.Make (PrimeSet)

module DynamicNumber = struct
  type t = { terms : RationalField.t CoeffMap.t }

  let zero = { terms = CoeffMap.empty }
  let one = { terms = CoeffMap.singleton PrimeSet.empty RationalField.one }
  let is_zero x = CoeffMap.is_empty x.terms

  let cleanup x =
    { terms = CoeffMap.filter (fun _ c -> not (RationalField.is_zero c)) x.terms }

  let make_rational r =
    if RationalField.is_zero r then zero
    else { terms = CoeffMap.singleton PrimeSet.empty r }

  let of_int n = make_rational (RationalField.of_int n)

  let make_root p =
    let rec factorize i n acc_c acc_b =
      if i * i > n then
        if n = 1 then (acc_c, acc_b) else (acc_c, PrimeSet.add n acc_b)
      else if n mod (i * i) = 0 then factorize i (n / (i * i)) (acc_c * i) acc_b
      else if n mod i = 0 then
        factorize (i + 1) (n / i) acc_c (PrimeSet.add i acc_b)
      else factorize (i + 1) n acc_c acc_b
    in
    if p = 0 then zero
    else if p = 1 then one
    else if p = -1 then
      { terms = CoeffMap.singleton (PrimeSet.singleton (-1)) RationalField.one }
    else if p < 0 then
      let c, b = factorize 2 (-p) 1 (PrimeSet.singleton (-1)) in
      { terms = CoeffMap.singleton b (RationalField.of_int c) }
    else
      let c, b = factorize 2 p 1 PrimeSet.empty in
      { terms = CoeffMap.singleton b (RationalField.of_int c) }

  let add x y =
    let f _basis c1 c2 =
      match (c1, c2) with
      | Some v1, Some v2 ->
          let sum = RationalField.add v1 v2 in
          if RationalField.is_zero sum then None else Some sum
      | Some v, None | None, Some v -> Some v
      | None, None -> None
    in
    cleanup { terms = CoeffMap.merge f x.terms y.terms }

  let neg x = { terms = CoeffMap.map RationalField.neg x.terms }

  let multiply_basis s1 s2 =
    let common = PrimeSet.inter s1 s2 in
    let diff =
      PrimeSet.union (PrimeSet.diff s1 common) (PrimeSet.diff s2 common)
    in
    let coeff = PrimeSet.fold (fun p acc -> acc * p) common 1 in
    (RationalField.of_int coeff, diff)

  let mul x y =
    let res = ref CoeffMap.empty in
    CoeffMap.iter
      (fun b1 c1 ->
        CoeffMap.iter
          (fun b2 c2 ->
            let coeff_q, b_prod = multiply_basis b1 b2 in
            let new_coeff =
              RationalField.mul (RationalField.mul c1 c2) coeff_q
            in
            let old_coeff =
              CoeffMap.find_opt b_prod !res
              |> Option.value ~default:RationalField.zero
            in
            let sum = RationalField.add old_coeff new_coeff in
            if RationalField.is_zero sum then res := CoeffMap.remove b_prod !res
            else res := CoeffMap.add b_prod sum !res)
          y.terms)
      x.terms;
    cleanup { terms = !res }

  let make_root_rat (n, d) =
    if n = 0 then zero
    else if n > 0 then
      let num = make_root (n * d) in
      let den = make_rational (RationalField.v (1, d)) in
      mul num den
    else
      let abs_n = abs n in
      let r_sqrt = mul (make_root (abs_n * d)) (make_rational (RationalField.v (1, d))) in
      let i = make_root (-1) in
      mul i r_sqrt

  let equal x y = is_zero (cleanup (add x (neg y)))

  let inv x =
    if is_zero x then failwith "Division by zero";
    let get_local_basis_space x =
      let initial_primes =
        CoeffMap.fold
          (fun b _ acc -> PrimeSet.union b acc)
          x.terms PrimeSet.empty
      in
      let primes_list = PrimeSet.elements initial_primes in
      let rec generate = function
        | [] -> [ PrimeSet.empty ]
        | p :: ps ->
            let rests = generate ps in
            rests @ List.map (fun b -> PrimeSet.add p b) rests
      in
      generate primes_list
    in
    let local_space = get_local_basis_space x in
    let size = List.length local_space in
    let b_to_idx =
      List.mapi (fun i b -> (b, i)) local_space
      |> List.to_seq |> BasisMap.of_seq
    in
    let idx_to_basis = Array.of_list local_space in
    let module LocalRule = struct
      type coeff = RationalField.t
      type basis = PrimeSet.t
      let all = idx_to_basis
      let size = size
      let to_int b = BasisMap.find b b_to_idx
      let multiply = multiply_basis
    end in
    let module Field = Extension_field.Make (RationalField) (LocalRule) in
    let x_arr = Array.make size RationalField.zero in
    CoeffMap.iter (fun b c -> x_arr.(LocalRule.to_int b) <- c) x.terms;
    let res_arr = Field.inv x_arr in
    let res_terms = ref CoeffMap.empty in
    Array.iteri
      (fun i c ->
        if not (RationalField.is_zero c) then
          res_terms := CoeffMap.add idx_to_basis.(i) c !res_terms)
      res_arr;
    { terms = !res_terms }

  let get_rational_part x =
    CoeffMap.find_opt PrimeSet.empty x.terms |> Option.value ~default:RationalField.zero

  let to_rational_opt x =
    if CoeffMap.is_empty x.terms then Some RationalField.zero
    else if CoeffMap.cardinal x.terms = 1 then
      CoeffMap.find_opt PrimeSet.empty x.terms
    else None

  (* --- 3乗根探索のためのヘルパー --- *)

  (* 数値を u + v*root(p) の形に分解する *)
  let analyze_structure x =
    let bases = CoeffMap.bindings x.terms |> List.map fst in
    let is_rational_basis b = PrimeSet.is_empty b in
    
    (* Case 1: 有理数のみ *)
    if List.for_all is_rational_basis bases then
      Some (get_rational_part x, RationalField.zero, 1)
    else
      (* Case 2, 3, 4: a + b*root(p) の形かチェック *)
      let complex_bases = List.filter (fun b -> not (is_rational_basis b)) bases in
      match complex_bases with
      | [basis] ->
          (* basisに含まれる素数の積を p とする *)
          let p = PrimeSet.fold ( * ) basis 1 in
          let u = get_rational_part x in
          let v = CoeffMap.find basis x.terms in
          Some (u, v, p)
      | _ -> None

  let solve_cubic_rational a b c =
    if RationalField.is_zero c then [ RationalField.zero ]
    else
      let an, ad = a in
      let _, bd = b in
      let cn, cd = c in
      let int_a = abs (an * bd * cd) in
      let int_c = abs (cn * ad * bd) in
      let p_candidates = Integer.get_divisors int_c in
      let q_candidates = Integer.get_divisors int_a in
      let roots = ref [] in
      List.iter
        (fun p ->
          List.iter
            (fun q ->
              let candidates = [ RationalField.v (p, q); RationalField.v (-p, q) ] in
              List.iter
                (fun t ->
                  if not (List.exists (RationalField.equal t) !roots) then
                    let t2 = RationalField.mul t t in
                    let t3 = RationalField.mul t2 t in
                    let res = RationalField.(add (add (mul a t3) (mul b t)) c) in
                    if RationalField.is_zero res then roots := t :: !roots)
                candidates)
            q_candidates)
        p_candidates;
      !roots

  (* 方程式 4pt^3 + 3nt - v = 0 を解いて候補を探す *)
  let solve_auxiliary_equation p n v =
    let p_q = RationalField.of_int p in
    let a = RationalField.mul (RationalField.of_int 4) p_q in
    let b = RationalField.mul (RationalField.of_int 3) (get_rational_part n) in
    let c = RationalField.neg v in (* v は有理数係数 *)
    
    if RationalField.is_zero a then []
    else solve_cubic_rational a b c

  let rec square_root x =
    let x = cleanup x in
    match to_rational_opt x with
    | Some q -> (
        match RationalField.square_root q with
        | Some k -> Some (make_rational k)
        | None -> None)
    | None -> (
        match analyze_structure x with
        (* 変数名 u_val, v_val に変更して RationalField.v との衝突を回避 *)
        | Some (u_val, v_val, p) when p <> 1 ->
            (* u + v*root(p) の平方根 *)
            (* u^2 - p*v^2 *)
            let d2 = RationalField.(sub (mul u_val u_val) (mul (of_int p) (mul v_val v_val))) in
            let d2_dyn = make_rational d2 in
            (match square_root d2_dyn with
            | None -> None
            | Some d ->
                (* s^2 = (u + d)/2, t^2 = (u - d)/2 等の公式 *)
                let half = make_rational (RationalField.v (1, 2)) in
                let s2_cand1 = mul (add (make_rational u_val) d) half in
                let s2_cand2 = mul (add (make_rational u_val) (neg d)) half in
                
                let try_s2 s2 =
                  match square_root s2 with
                  | Some s when not (is_zero s) ->
                      (* t = v / (2s) *)
                      let t = mul (make_rational v_val) (inv (mul (of_int 2) s)) in
                      let root_p = make_root p in
                      let res = add s (mul t root_p) in
                      if equal (mul res res) x then Some res
                      else
                        let neg_res = neg res in
                        if equal (mul neg_res neg_res) x then Some neg_res
                        else None
                  | _ -> None
                in
                match try_s2 s2_cand1 with
                | Some r -> Some r
                | None -> try_s2 s2_cand2)
        | _ -> None)

  let rec cubic_root x =
    let x = cleanup x in
    match analyze_structure x with
    | Some (u, _, 1) -> 
        (* Case 1: 有理数 *)
        (match RationalField.cubic_root u with
        | Some k -> Some (make_rational k)
        | None -> None)
        
    | Some (u_val, v_val, p) ->
        (* Case 2, 3, 4: 一般の u + v*root(p) *)
        (* ノルム N = u^2 - p*v^2 を計算 *)
        let norm_val = RationalField.(sub (mul u_val u_val) (mul (of_int p) (mul v_val v_val))) in
        let norm_dyn = make_rational norm_val in
        
        (* ノルムの3乗根 n を求める *)
        (match cubic_root norm_dyn with
        | Some n ->
            (* 補助方程式 4pt^3 + 3nt - v = 0 の有理数解 t を探す *)
            let t_candidates = solve_auxiliary_equation p n v_val in
            
            let rec try_candidates = function
              | [] -> None
              | t :: rest -> (
                  let t_dyn = make_rational t in
                  (* s = sqrt(n + p*t^2) *)
                  let s2 = add n (mul (of_int p) (mul t_dyn t_dyn)) in
                  match square_root s2 with
                  | Some s ->
                      (* 候補: s + t*root(p) *)
                      let root_p = make_root p in
                      let res1 = add s (mul t_dyn root_p) in
                      if equal (mul res1 (mul res1 res1)) x then Some res1
                      else
                        let res2 = add (neg s) (mul t_dyn root_p) in
                        if equal (mul res2 (mul res2 res2)) x then Some res2
                        else try_candidates rest
                  | None -> try_candidates rest)
            in
            try_candidates t_candidates
        | None -> None)
        
    | None -> None

  let is_atomic s =
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

  let to_string_latex x =
    let terms_list = ref [] in
    CoeffMap.iter
      (fun b c ->
        let c_str = RationalField.to_string_latex c in
        let elements = PrimeSet.elements b in
        let has_i = List.mem (-1) elements in
        let primes = List.filter (fun x -> x <> -1) elements in
        let i_str = if has_i then "i" else "" in
        let root_str =
          if primes = [] then ""
          else
            let product = List.fold_left ( * ) 1 primes in
            "\\sqrt{" ^ string_of_int product ^ "}"
        in
        let b_str = i_str ^ root_str in
        let term =
          match b_str with
          | "" -> c_str
          | _ ->
              if c_str = "1" then b_str
              else if c_str = "-1" then "-" ^ b_str
              else if is_atomic c_str then c_str ^ b_str
              else "(" ^ c_str ^ ")" ^ b_str
        in
        terms_list := term :: !terms_list)
      x.terms;
    match List.rev !terms_list with
    | [] -> "0"
    | hd :: tl ->
        List.fold_left
          (fun acc t ->
            if String.length t > 0 && t.[0] = '-' then
              acc ^ " - " ^ String.sub t 1 (String.length t - 1)
            else acc ^ " + " ^ t)
          hd tl

  let conj x = x
  let to_string _ = "not implemented"

  include Add_monoid.Extend (struct
    type nonrec t = t
    let zero, equal, add = (zero, equal, add)
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
    let inv, mul = (inv, mul)
  end)
  include Division_ring.Extend (struct
    type nonrec t = t
    let pow, inv, mul = (pow, inv, mul)
  end)
end

include DynamicNumber