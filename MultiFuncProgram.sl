func main is
     if 12 >= 32
     	then if (fac 7) >= (fac 6)
	     then (printIfGT 7 2 "kewl\n")
	     else print "nope\n"
	else print "never got out of main\n"
end

func fac n is
     if n >= 2
     	then n * (fac (n - 1))
	else n
end

func printIfGT a b str is
     if a >= b
     	then print str
	else print "Not printing\n"
end