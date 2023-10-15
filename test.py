def f(x):
  return x + 10

def range(n):
  if n>0:
    return range(n-1)+[n-1]
  return []

def g(x):
  print(x)


println((42 * 5) - (10 * 2) / (3 % 5))
println(1 > 0)
println(True)
println("a" + "b")
println("a" + "1")
println([1,2,3,4])
for i in [1, 2, 3]:
  println(i)
x = 10
println(x)
for i in [1,2,10]:
	println(i)
	println(x)
	x = 2*x
println(f(x))
println(f(2))
println(f (x))
println (f (x*2 + 10))
println(f(f(f(x))))

println(type(5))
println(len([1,2,3,4]))
println(type([1]))

t1 = [[1],[4,5]]
println(t1[1])
println(type(t1))

t1 = [[2],[6,7]]
t1[x - x] = [1,2,3]
t1[1 - x + x] = 0
println(t1)

println(range(10))
println([0,1][1])


println(42)
println("Hello", "World!")
