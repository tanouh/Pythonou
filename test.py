# def f(x):
#   return x + 10

# def range(n):
#   if n>0:
#     return range(n-1)+[n-1]
#   return []


# println((42 * 5) - (10 * 2) / (3 % 5))
# println(1 > 0)
# println(True)
# println("a" + "b")
# println("a" + "1")
# println([1,2,3,4])
# for i in [1, 2, 3]:
#   println(i)
# x = 10
# println(x)
# for i in [1,2,10]:
# 	println(i)
# 	println(x)
# 	x = 2*x
# println(f(x))
# println(f(2))
# println(f (x))
# println (f (x*2 + 10))
# println(f(f(f(x))))

# println(type(5))
# println(len([1,2,3,4]))
# println(type([1]))

# t1 = [[1],[4,5]]
# println(t1[1])
# println(type(t1))

# t1 = [[2],[6,7]]
# t1[x - x] = [1,2,3]
# t1[1 - x + x] = 0
# println(t1)

# print(range(10))

def range(n):
  if n>0:
    return range(n-1)+[n-1]
  return []

def sub(l,ffrom,to):
  if ffrom<to:
    return [l[ffrom]]+sub(l,ffrom+1,to)
  return []

def rev(l):
  if len(l):
    return rev((sub(l,1,len(l))))+[l[0]]
  return []

def rev2(l):
  if len(l)<=1:
    return l
  mid = len(l)//2
  return rev((sub(l,mid,len(l))))+rev((sub(l,0,mid)))

print(rev([1,2,3]))
print(rev2(rev([1,2,3])))
print(rev2(rev((range(10)))))
