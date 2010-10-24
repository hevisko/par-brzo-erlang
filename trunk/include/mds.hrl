% Define a record to present a DFA.
-record(dfa, {states=[],
			  symbols=[],
			  start=[],
			  transition=dict:new(),
			  finals=[]
			 }).
