data Bill = T | F
not:: Bill -> Bill
not b = case b of 
		F -> T
		T -> F
