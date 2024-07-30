let fibonacci: fn(x):    	
    if x < 2:
    	return x
    else: 
    	fibonacci(x - 1) + fibonacci(x - 2)
    ~
~
fibonacci(28)
