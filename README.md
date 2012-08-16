Edgy - An F# Graph DSL
====

Directed Graph Example:
	
	let graph = 
	    [
	        Path 2 <== 1
	        Path 2 <== 1 ==> 3
	        Path 2 ==> 4 <== 3 ==> 5
	        Path 2 ==> 4 <== 3 <== 8
	        Path 4 ==> 6
	    ] |> combine
	
Weighted Graph Example:

	let weightedGraph = 
	    [
	        Path "A" <=|1.0|= "B" =|0.5|=> "C" <=|0.2|= "D"
	        Path "A" =|0.4|=> "B" =|0.2|=> "C"
	    ] |> combine

The edge and vertex types are fully generic.