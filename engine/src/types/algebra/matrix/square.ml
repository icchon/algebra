module Make (S : Ring.S) (D : Dim.S) = struct
  module R = Rectangular.Make (S) (D) (D)
  include R

  let mul m1 m2 = R.matmul m1 m2
  let one = R.identity D.n

  module MM = Mul_monoid.Extend (struct
    include R

    let mul = mul
    let one = one
  end)

  include MM
end

module ExtendField (K : Field.S) (D : Dim.S) = struct
  module R = Rectangular.Make (K) (D) (D)
  include R
  include Rectangular.ExtendField (K) (D) (D)

  let rref m =
    let rows = Array.length m in
    if rows = 0 then [||]
    else
      let cols = Array.length m.(0) in
      let res = Array.map Array.copy m in
      let pivot_row = ref 0 in
      for j = 0 to cols - 1 do
        if !pivot_row < rows then (
          let sel = ref !pivot_row in
          while !sel < rows && K.equal res.(!sel).(j) K.zero do
            incr sel
          done;
          if !sel < rows then (
            let tmp = res.(!pivot_row) in
            res.(!pivot_row) <- res.(!sel);
            res.(!sel) <- tmp;
            let p_val = res.(!pivot_row).(j) in
            res.(!pivot_row) <-
              Array.map (fun x -> K.div x p_val) res.(!pivot_row);
            for i = 0 to rows - 1 do
              if i <> !pivot_row then
                let factor = res.(i).(j) in
                for k = j to cols - 1 do
                  res.(i).(k) <-
                    K.sub res.(i).(k) (K.mul factor res.(!pivot_row).(k))
                done
            done;
            incr pivot_row))
      done;
      res

  let det m =
    let n = Array.length m in
    if n = 0 then K.one
    else if n <> Array.length m.(0) then failwith "Square matrix required"
    else
      let res = Array.map Array.copy m in
      let d = ref K.one in
      let sign = ref K.one in
      for j = 0 to n - 1 do
        let pivot = ref j in
        while !pivot < n && K.equal res.(!pivot).(j) K.zero do
          incr pivot
        done;
        if !pivot = n then d := K.zero
        else (
          if !pivot <> j then (
            let tmp = res.(j) in
            res.(j) <- res.(!pivot);
            res.(!pivot) <- tmp;
            sign := K.neg !sign);
          let p_val = res.(j).(j) in
          d := K.mul !d p_val;
          for i = j + 1 to n - 1 do
            let factor = K.div res.(i).(j) p_val in
            for k = j + 1 to n - 1 do
              res.(i).(k) <- K.sub res.(i).(k) (K.mul factor res.(j).(k))
            done
          done)
      done;
      K.mul !sign !d

  let is_invertible m = not (K.equal (det m) K.zero)

  let inverse m =
    let n = Array.length m in
    if n <> Array.length m.(0) then failwith "Square matrix required";
    if not (is_invertible m) then None
    else
      let identity_n = R.identity n in
      let augmented =
        Array.init n (fun i -> Array.append m.(i) identity_n.(i))
      in
      let reduced = rref augmented in
      Some (Array.init n (fun i -> Array.sub reduced.(i) n n))

  let solve a b =
    let rows_a = Array.length a in
    let rows_b = Array.length b in
    if rows_a <> rows_b then failwith "Dimension mismatch";
    let cols_a = Array.length a.(0) in
    let cols_b = Array.length b.(0) in
    let augmented = Array.init rows_a (fun i -> Array.append a.(i) b.(i)) in
    let reduced = rref augmented in
    let sol = Array.init cols_a (fun _ -> Array.make cols_b K.zero) in
    let possible = ref true in
    for i = 0 to rows_a - 1 do
      let pivot_idx = ref (-1) in
      for j = 0 to cols_a - 1 do
        if !pivot_idx = -1 && not (K.equal reduced.(i).(j) K.zero) then
          pivot_idx := j
      done;

      if !pivot_idx = -1 then
        for j = cols_a to cols_a + cols_b - 1 do
          if not (K.equal reduced.(i).(j) K.zero) then possible := false
        done
      else
        for j = 0 to cols_b - 1 do
          sol.(!pivot_idx).(j) <- reduced.(i).(cols_a + j)
        done
    done;
    if !possible then Some sol else None
end
