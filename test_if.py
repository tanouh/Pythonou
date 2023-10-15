
def comparetoten(x):
        if (x > 10):
                println("gter than 10")
        else:
                println("less than 10")
def isEven(x):
        if (x % 2 == 0):
                println("even")
        else:
                println("odd")
if (True):
        println("Hello, World 1")

x = 10

println(x)

if (x != 0): 
        for i in [1,2,10]:
                println(i)
                println(x)
                x = 2*x
comparetoten(10*2)
for i in [1,2,3,4,5]:       
        y = (i+2)* 3 % 5
        println(y)
        isEven(y)

println("####### TEST WHILE #######")
j = 10
x = 0
while (j>0):
     x = x + 1
     j = j - 1
println(x)


