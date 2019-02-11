
example :

      -->A--->B--
     /    \      \
    C      -->D----->E
     \           /
      ---->F-----

- create a list of successors : [(A,[B,D]),(B,[E]),(C,[A,F]),(D,[E]),(F,[E])]
- create a list of predecessors : [(A,[C]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[C])]- find the starting steps (steps wich are not in any successor list)
- to visit list is to be ordered always

- to visit = [C] -- visit C -- visited = [C]
- successors of C for which all predecessors have been visited: [A,F]
-- to visit = [A,F] -- visit A -- visited = [C,A]
-- successors of A for which all predecessors have been visited: [B,D]
--- to visit = [B,D,F] -- visit B -- visited = [C,A,B]
--- successors of B for which all predecessors have been visited: [] (E still has D and F has unvisisted pred)
--- to visit = [D,F] -- visit D -- visited = [C,A,B,D]
--- successors of D for which all predecessors have been visited: [] (E still has F as unvisisted pred)
-- to visit [F] -- visit F -- visited = [C,A,B,D,F]
-- successors of F for which all predecessors have been visited: [E] (B,D,F have all been visited)
-- no visit [E] -- visit E -- visited = [C,A,B,D,F,E]
-- nothing to visit, we're done


example :

      -->I--->S--
     /    \      \
    Q------------>Y
     \           /
      ---->C-----

- create a list of successors : [(C,[Y]),(I,[S,Y]),(Q,[C,I,Y]),(S,[Y])]
- create a list of predecessors : [(C,[Q]),(I,[Q]),(S,[I]),(Y,[C,I,Q,S])]
- to visit list is to be inserted in front (depth first traversal)

- to visit = [Q] -- visit Q -- visited = [Q]
- successors of Q for which all predecessors have been visited: [C,I] (Y has S as unvisited pred)
-- to visit = [C,I] -- visit C -- visited = [Q,C]
-- successors of C for which all predecessors have been visited: [] (Y has S as unvisited pred)
-- to visit = [I] -- visit I -- visited = [Q,C,I]
-- successors of I for which all predecessors have been visited: [S] (Y has S as unvisited pred)
-- to visit = [S] -- visit S -- visited = [Q,C,I,S]
-- successors of S for which all predecessors have been visited [Y] 
-- to visit = [Y] -- visit Y -- visited = [Q,C,I,S,Y]
- nothing to visit we're done





