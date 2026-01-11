module Id = struct
  type 'a t = 'a

  let pure x = x
  let map f x = f x
  let lift2 f x y = f x y
  let fold_left f acc x = f acc x
end

module Tup2 = struct
  type 'a t = 'a * 'a

  let pure x = (x, x)
  let map f (x, y) = (f x, f y)
  let lift2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)
  let fold_left f acc (x, y) = f (f acc x) y
end

module Arr = struct
  type 'a t = 'a array

  let pure x = [| x |]
  let map = Array.map

  let lift2 f x y =
    let l = min (Array.length x) (Array.length y) in
    Array.init l (fun i -> f x.(i) y.(i))

  let fold_left = Array.fold_left
end

module Li = struct
  type 'a t = 'a list

  let pure x = [ x ]
  let map = List.map

  let lift2 f x y =
    let rec aux acc l1 l2 =
      match (l1, l2) with
      | h1 :: t1, h2 :: t2 -> aux (f h1 h2 :: acc) t1 t2
      | _ -> List.rev acc
    in
    aux [] x y

  let fold_left = List.fold_left
end

module Tup3 = struct
  type 'a t = 'a * 'a * 'a

  let pure x = (x, x, x)
  let map f (x, y, z) = (f x, f y, f z)
  let lift2 f (x1, y1, z1) (x2, y2, z2) = (f x1 x2, f y1 y2, f z1 z2)
  let fold_left f acc (x, y, z) = f (f (f acc x) y) z
end

module Tup4 = struct
  type 'a t = 'a * 'a * 'a * 'a

  let pure x = (x, x, x, x)
  let map f (x, y, z, w) = (f x, f y, f z, f w)

  let lift2 f (x1, y1, z1, w1) (x2, y2, z2, w2) =
    (f x1 x2, f y1 y2, f z1 z2, f w1 w2)

  let fold_left f acc (x, y, z, w) = f (f (f (f acc x) y) z) w
end

module Vector (D : Dim.S) = struct
  type 'a t = 'a array

  let pure x = Array.make D.n x

  let map f x =
    if Array.length x <> D.n then failwith "dimension mismatch"
    else Array.map f x

  let lift2 f x y =
    if Array.length x <> D.n || Array.length y <> D.n then
      failwith "demension mismatch"
    else Array.init D.n (fun i -> f x.(i) y.(i))

  let fold_left = Array.fold_left
end

module Rectangular (M : Dim.S) (N : Dim.S) = struct
  type 'a t = 'a array array

  let pure x = Array.make_matrix M.n N.n x

  let map f m =
    if Array.length m <> M.n then failwith "row dimension mismatch";
    Array.map
      (fun row ->
        if Array.length row <> N.n then failwith "col dimension mismatch";
        Array.map f row)
      m

  let lift2 f m1 m2 =
    if Array.length m1 <> M.n || Array.length m2 <> M.n then
      failwith "row dimension mismatch";
    Array.init M.n (fun i ->
        let r1 = m1.(i) in
        let r2 = m2.(i) in
        if Array.length r1 <> N.n || Array.length r2 <> N.n then
          failwith "col dimension mismatch";
        Array.init N.n (fun j -> f r1.(j) r2.(j)))

  let fold_left f acc m =
    Array.fold_left (fun a row -> Array.fold_left f a row) acc m
end

module Square (N : Dim.S) = struct
  include Rectangular (N) (N)
end
