#test 1 
encoding small string  <br />
8 bytes -> 3 bytes + 20 bytes overhead = 23 bytes  <br />
1 : 2.8 compression 

#test 2
encoding itself  <br />
1951 bytes -> 1258 bytes  + 214 bytes  overhead = 1472 bytes  <br />
1.3 : 1 compression  <br />

#test 3 
encoding large file  <br />
56.9 mB -> 42.3 mB + less than 1000 bytes overhead  <br />
1.35 : 1 compression  <br />
