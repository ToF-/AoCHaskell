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
* starters : members of the successors keys that have no successors : [C]
  predecessors : [(A,[C]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[C])] + [(C,[])]

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

* create a list of successors for each step
* find the end step, for which there is no successors
* create a list of starters : members of the successors keys that have no successors
* create a list of predecessors of all steps including the starters (empty list of predecessors) 
* create a list of steps done
* create a list W of n free workers
* start with T = 0
* while end step is not elem of steps done
  * N = ordered list of steps that are doable (steps with no predecessors)
  * while there are free workers in W 
    * assign each free worker Job with Step S with finish time T + duration of S
  * M = assigned worker with the minimum finish time
  * J = Job of that worker, S = step of that job
  * update predessors of all successors of S, removing S as a predessors
  * remove S from predecessors
  * add S in list of steps done
  * free Worker M
  * continue with T = finish time of J 

example :

      -->A--->B--
     /    \      \
    C      -->D----->E
     \           /
      ---->F-----

* successors : [(A,[B,D]),(B,[E]),(C,[A,F]),(D,[E]),(F,[E])]
* end step : E
* starters : members of the successors keys that have no successors : [C]
  predecessors : [(A,[C]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[C])] + [(C,[])]
* create list of steps done Done = []
* create a list of 2 free workers W= [w1,w2]
* start with T = 0
* E ∉ Done
  * assign available steps to available workers 
    * Next = [] ++ [C]
    * F = W | isFree = [w1 free,w2 free]
    * W = [w1 job C 3, w2 free]
    * Next = []
    * predecessors = [(A,[C]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[])]
  * end the next job with minimal finish time
    * M = minimum W | ¬ isFree = [w1 Job C 3]
    * predecessors = [(A,[]),(B,[A]),(D,[A]),(E,[B,D,F]),(F,[])]
    * Done = [C]
    * W = [w1 free, w2 free]
    * T = 3
* E ∉ Done
  * assign available steps to available workers 
    * Next = [] ++ [A,F]
    * F = W | isFree = [w1 free,w2 free]
    * W = [w1 job A 4, w2 job F 9]
    * Next = []
    * predecessors = [(B,[A]),(D,[A]),(E,[B,D,F])]
    * Done = [C]
  * end the next job with minimal finish time
    * M = minimum W | ¬ isFree = [w1 Job A 4]
    * predecessors = [(B,[]),(D,[]),(E,[B,D,F])]
    * Done = [C,A]
    * W = [w1 free, w2 Job F 9]
    * T = 4
* E ∉ Done
  * assign available steps to available workers 
    * Next = [] ++ [B,D]
    * F = W | isFree = [w1 free]
    * W = [w1 job B 6, w2 job F 9]
    * Next = [D]
    * predecessors = [(E,[B,D,F])]
  * end the next job with minimal finish time
    * M = minimum W | ¬ isFree = [w1 Job B 6]
    * predecessors = [(E,[D,F])]
    * Done = [C,A,B]
    * W = [w1 free, w2 Job F 9]
    * T = 6
* E ∉ Done
  * assign available steps to available workers 
    * Next = [D] ++ []
    * F = W | isFree = [w1 free]
    * W = [w1 job D 10, w2 job F 9]
    * Next = []
  * end the next job with minimal finish time
    * M = minimum W | ¬ isFree = [w2 Job F 9]
    * predecessors = [(E,[D])]
    * Done = [C,A,B,F]
    * W = [w1 Job D 10,w2 Free]
    * T = 9
* E ∉ Done
  * assign available steps to available workers 
    * Next = [D] ++ [] 
    * F = W | isFree = [w1 free]
    * W = [w1 job D 10]
    * Next = []
  * end the next job with minimal finish time
    * M = minimum W | ¬ isFree = [w1 Job D 10]
    * predecessors = [(E,[])]
    * Done = [C,A,B,F,D]
    * W = [w1 free ,w2 free]
    * T = 10
* E ∉ Done
  * assign available steps to available workers 
    * Next = [] ++ [E] 
    * F = W | isFree = [w1 free,w2 free]
    * W = [w1 job E 15]
    * Next = []
  * end the next job with minimal finish time
    * M = minimum W | ¬ isFree = [w1 Job E 15]
    * predecessors = []
    * Done = [C,A,B,F,D,E]
    * W = [w1 free ,w2 free]
    * T = 15
 * E ∈ Done
T = 15
