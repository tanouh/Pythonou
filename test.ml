open Eval

let _t1 = assert( eval_string "print(42)"  = ["42"] )

let _t1 = assert( eval_string
"
for i in [1, 2, 3]:
  print(i)
"  = ["1";"2";"3"] )

let _t3 =
  let prog = "
def f(n):
  if n < 2:
    return n
  return f(n-1)+f(n-2)

print(f(10))
"
  in
  assert( eval_string prog  = ["55"] )

(* let _t4 =
  let prog = "
def range(n):
  if n>0:
    return range(n-1)+[n-1]
  return []

print(range(10))
"
  in
  assert( eval_string prog  = ["[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]"] )

let _t5 =
  let prog = "


def range(n):
  if n>0:
    return range(n-1)+[n-1]
  return []

def sub(l,from,to):
  if from<to:
    return [l[from]]+sub(l,from+1,to)
  return []

def rev(l):
  if len(l):
    return rev((sub(l,1,len(l))))+[l[0]]
  return []

def rev2(l):
  if len(l)<=1:
    return l
  mid = len(l)/2
  return rev((sub(l,mid,len(l))))+rev((sub(l,0,mid)))

print(rev([1,2,3]))
print(rev2(rev([1,2,3])))
print(rev2(rev((range(10)))))
"
  in
  assert( eval_string prog  = [
            "[3, 2, 1]";
            "[1, 2, 3]";
            "[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]"
        ] )
 *)
