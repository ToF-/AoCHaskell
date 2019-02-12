## Finding the correct sequence of steps

### Rules

- a step can start only when all its predecessors are done
- when several steps are possible, do the steps in alphabetical order

### Algorithm

* to each step associate its successors and its predecessors
* start with the steps having no predecessors : N and a list of steps done D. N should always be in alphabetical order
* for each S in N
  * add the S in D list
  * find the successors of S for which all predecessors have been done, and add them in the N (ordered) list
  * when no such successors can be found, take the next step in N
  * when no next step in N D contains all the steps

example :

      -->A--->B--
     /    \      \
    C      -->D----->E
     \           /
      ---->F-----

* successors : [(A,[B,D]),(B,[E]),(C,[A,F]),(D,[E]),(F,[E])]
   predecessors : [(A,[C]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[C])]- find the starting steps (steps wich are not in any successor list)
* N = [C] --> D = [C]
*  while N /= []
  * successors of C for which all predecessors have been done: [A,F]
  * N = [A,F] --> D = [C,A]
  * successors of A for which all predecessors have been done: [B,D]
  * N = [B,D,F] --> D = [C,A,B]
  * successors of B for which all predecessors have been done: [] (E still has D and F as not done yet predecessors)
  * N = [D,F] --> D = [C,A,B,D]
  * successors of D for which all predecessors have been done: [] (E still has F as not done yet predecessor)
  * N [F] --> D = [C,A,B,D,F]
  * successors of F for which all predecessors have been done: [E] (B,D,F have all been done)
  * no visit [E] --> D = [C,A,B,D,F,E]
  * nothing in N, we're done

## Finding the shortest time to execute steps

### Rules

- a step can start only when all its predecessors are done
- when several steps are possible, do the steps in order to have the shortest time, otherwise in alphabetical

### Algorithm

* create a list of successors and predecessors for each steps
* set the critical time for each step, taking into account
  * the minimal duration for a step (e.g. 0 or 60)
  * the step duration of each step (time of A = minimum+1, time of B = minimum+2, etc.)
* start with the steps having no predecessors N, the list should always be sorted by critical time, (then albhabetical) descending
* 


