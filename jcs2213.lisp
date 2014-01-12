; Artificial Intelligence: Fall 2012 Assignment 2
; Professor: Alexander Pasik
; Author: John Sizemore

; OVERVIEW: This assignment solves the 8-puzzle using the A* search algorithm. The following code defines a procedure for enqueuing nodes as well as 
; expanding and searching them. Included are three heuristics: the Hamming distance calculating the number of misplaced tiles; the Manhattan distance
; calculating distances on a tile-by-tile basis; and linear conflict, which adds to the Manhattan distance an extra number of moves if two tiles are in their
; correct row but are reversed relative to each others' goal positions.

; This function moves the space one place to the left. If the space is on the left boundary then no move takes place.
(defun move-left (puzzle)
	   (if (or (equal 0 (position 0 puzzle)) (equal 3 (position 0 puzzle)) (equal 6 (position 0 puzzle)))
	       nil
	       (progn
		 (let ((index (position 0 puzzle)))
		  (concatenate 'list (subseq puzzle 0 (- index 1)) '(0) (subseq puzzle (- index 1) index) (subseq puzzle (+ index 1)))
		  )
		  )
		 )
	   )

; This function moves the space one place to the right. If the space is on the right boundary then no move takes place.
(defun move-right (puzzle)
	   (if (or (equal 2 (position 0 puzzle)) (equal 5 (position 0 puzzle)) (equal 8 (position 0 puzzle)))
	       nil
	       (progn
		 (let ((index (position 0 puzzle)))
		  (concatenate 'list (subseq puzzle 0 index) (subseq puzzle (+ index 1) (+ index 2)) '(0) (subseq puzzle (+ index 2)))
		  )
		  )
		 )
	   )
; This function moves the space one place upward. If the space is on the upper boundary then no move takes place. 
(defun move-up (puzzle)
	   (if (or (equal 0 (position 0 puzzle)) (equal 1 (position 0 puzzle)) (equal 2 (position 0 puzzle)))
	       nil
	       (progn
		 (let ((index (position 0 puzzle)))
		  (concatenate 'list (subseq puzzle 0 (- index 3)) '(0) (subseq puzzle (- index 2) index) (list (elt puzzle (- index 3))) (subseq puzzle (+ index 1)))
		  )
		  )
		 )
	   )

; This function moves the space on place downward. If the space is on the lower boundary then no move takes place.
(defun move-down (puzzle)
	   (if (or (equal 6 (position 0 puzzle)) (equal 7 (position 0 puzzle)) (equal 8 (position 0 puzzle)))
	       nil
	       (progn
		 (let ((index (position 0 puzzle)))
		  (concatenate 'list (subseq puzzle 0 index) (list (elt puzzle (+ index 3))) (subseq puzzle (+ index 1) (+ index 3)) '(0) (subseq puzzle (+ index 4)))
		  )
		  )
		 )
	   )

; This function creates a puzzle with as many moves as specified by the function's argument. The puzzle originally starts with the goal state specified by 
; (0 1 2 3 4 5 6 7 8). A random number is picked from 0 to 4 representing the four possible actions (up, down, left, right). 
; An move function that results in no move (e.g. moving the space right when the space is as far right as it can go) will return NIL.
; If that is the case, the function simply tries again with another random number. If the move is successful, a counter is incremented and the working puzzle is set to
; the output of the move function. This process repeats until the counter matches the number of requested moves. The final puzzle with all of the requested moves is
; then returned as a 9-element list.
(defun make-puzzle (moves)
	   (let
	       ((puzzle '(0 1 2 3 4 5 6 7 8))
		(i 0)
		(temp nil)
		)
	     (loop while (< i moves) do
		  (let
		      ((x (random 4)))
		    (cond
		      ((= x 0)
		       (setf temp (move-up puzzle))
		       )
		      ((= x 1)
		       (setf temp (move-down puzzle))
		       )
		      ((= x 2)
		       (setf temp (move-left puzzle))
		       )
		      ((= x 3)
		       (setf temp (move-right puzzle))
		       )
		      )
		    )
		  (unless (null temp)
		    (setf puzzle temp)
		    (setf i (+ i 1))
		    )
		  )
	     puzzle
	     )
	   )

; The random state generator. This function generates 5 random states. The parameter is optional;
; it denotes the number of moves the puzzle is supposed to make. The default value is 250 moves.
(defun random-case (&optional (moves 250))
  (let
      ((return-list nil))
    (loop for i from 0 to 4 do
	 (setf return-list (append (list (make-puzzle moves)) return-list))
	 )
    return-list
    )
  )

; The function for counting misplaced tiles. Since each number in the goal state of '(0 1 2 3 4 5 6 7 8) is the same as that number's index in the goal list, the
; function checks whether or not the value at a specific index is equal to itself. If not, the tile is misplaced and a counter is incremented. When the function
; has finished, the number of misplaced tiles is returned.
(defun misplaced (puzzle)
	   (let
	       (
		(misplaced 0)
		)
	     (loop for i in puzzle do
		  (unless (or (equal i (position i puzzle)) (equal i 0))
		    (setf misplaced (+ misplaced 1))
		    )
		  )
	     misplaced
	     )
	   )

(defun x-distance (value puzzle) ; distance value is from correct row
	   (abs (- (floor (mod value 3)) (mod (position value puzzle) 3)))
	   )

(defun y-distance (value puzzle) ; distance value is from correct column
	   (abs (- (floor (/ value 3)) (floor (/ (position value puzzle) 3))))
	   )

; This function calculates the manhattan distance of a puzzle. Each tile is passed in to both of the functions above which calculate x distance and y distance.
; The x distance is how many rows the tile is away from the correct row. The y distance is how many columns the tile is away from the correct column. The outputs of
; these two functions are added together to give the manhattan distance for any given tile. The sum of the computation for all tiles is added to the final distance which
; the function then returns.
(defun manhattan (puzzle)
  (let
      (
       (distance 0)
       )
    (loop for i in puzzle do
	 (unless (equal i 0)
	   (setf distance (+ distance (+ (x-distance i puzzle) (y-distance i puzzle))))
	   )
	 )
    distance
    )
  )

; The extra credit linear conflict heuristic. This heuristic adds 2 to the distance under the following condition:

; Linear Conflict Condition: For two tiles X and Y in the same row, 2 is added to the distance if the goal rows of X and Y are the same,
; Y's goal position is to the right of X's goal position, and X is to the right of Y in the state.

; If the row consists of only two numbers that are supposed to be in that row then the heuristic performs exactly like the condition above.
; The linear conflict addition is meaningless if a row only contains one of its correct numbers and adds 0 extra moves in that case per row in which that applies. 
; If there is a third tile that isn't supposed to exist on that row, then the tiles can still move "through" them. Therefore, the relaxed problem
; allowing movement for manhattan distance still applies in the case of linear conflict EXCEPT for tiles in a row that are supposed to be in that row.
; Basically, the implication is that if X, Y, and Z exist in a row that contains X & Y's goal positions but not Z's, then X and Y can still freely move through
; Z just as in manhattan distance but X and Y cannot move "through" each other unless they exist together on a row that is not their shared goal row.

; The more interesting behavior occurs if all three numbers in the same row are in their goal row but not all of them are in correct places.
; Using the middle row as an example, the cases are the following.

; 3 4 5: LC should give 0 additional moves as expected. Neither manhattan distance nor linear conflict will generate any moves for this row.
; 3 5 4: LC should give 2 additional moves as the 5 and the 4 are reversed.
; 4 3 5: LC should give 2 additional moves as the 4 and the 3 are reversed.
; 4 5 3: According to the above definition, LC would result in 4 extra moves because the 3 comes after the 4 and the 5. 
;        This is an overestimation. Only 2 additional moves are needed.
;        The 3 must move to another row and then the 4 and the 5 each shift right once apiece to be in their correct spots. The 3 then goes left twice and up once,
;        resulting in 6 total moves.
; 5 3 4: The above definition will also result in 4 extra moves for this configuration since the 5 comes before the 3 and the 4. 
;        Only two extra moves are needed.
;        The 5 must move one down, then the 3 and 4 move left once apiece. The 5 goes two to the right and once up. This is a total of 6 moves.
; 5 4 3: The above definition will overestimate at 6 extra moves since the 5 and 4 come before the 3 and the 5 comes before the 4. Only 4 extra moves are needed.
;        Both the 3 and the 4 move down once apiece. The 5 then moves 2 to the right to get to the correct position. The 4 then goes back up once to be correct.
;        The 3 must move 2 to the left and up once. This is a total of 8 moves.

; Linear conflict adds upon the manhattan distance heuristic. If no linear conflicts exist, the output of the linear conflict 
; heuristic will always equal the output of the manhattan distance heuristic. A dominant heuristic is one such than h2(n) >= h1(n) for all nodes n.
; Therefore, linear conflict dominates manhattan distance as it is impossible to ever have an output with linear conflict 
; that will not at least be equal to the output of manhattan distance. 

; Furthermore, linear conflict is admissible. Manhattan distance will simply count the moves
; the two tiles take to get to their correct spaces. The relaxed nature of the problem lets the tiles "slide" over one another for this heuristic.
; However, it is obvious that this sort of behavior cannot happen in reality. The linear conflict heuristic is therefore a closer approximation to the actual
; puzzle by acknowledging this phenomenon. One tile must move out of the way to allow the other to pass if both tiles are in their correct row.
; This is where the two extra moves come in; one move is for tile X to move to an adjacent row so that tile Y may pass. The second move accounts for
; tile X moving back into the correct row/column. All other moves match what would otherwise be seen in manhattan distance.

; It should be noted that running the program using the manhattan distance + linear conflict heuristic might sometimes yield results in which using strictly
; manhattan distance will expand fewer nodes. This is the result of an irreconcilable problem with queuing order. When a parent node is expanded, the ideal case
; is that one of the child nodes has a minimum f(n) value compared to the other children. In this case it is obvious which node is to be expanded next.
; However, this is not always the case. Compared with everything else in the fringe, two or more child nodes of the parent may share a minimum f(n) value. 
; Which one of these children is to be expanded next at this point is no longer an issue of f(n) but rather an issue with how the moves are queued. 
; For example, a parent could expand with two children nodes that have f(n) values of 10 and 10 is the global minimum of f(n) when compared to all other nodes
; presently in the fringe. Therefore, one of these two children is going to be expanded next. For example, child node C1 could be when the parent node
; moves down and is on the cheapest path to the solution. This is the "good" node. Child node C2 could be when the parent node moves left but is NOT on the cheapest
; path to the solution. This is the "bad" node. However, the queuing order defined by the program could place "left" moving nodes ahead of the "down" moving nodes
; when both nodes have equal f(n) values. With that sort of configuration, the "bad" node would be expanded first and it's children would continue expanding 
; themselves until a situation is reached where the "good" node will have to be revisited. If the code is reordered and the "down" moving nodes are expanded before 
; the "left" moving nodes, then the "good" node will be expanded first and all will be correct. However, this situation changes between puzzle configruations 
; and there is no global solution to this problem. A queuing order based on direction when children nodes are equal with minimal cost cannot satisfy every possible
; configuration. In a situation where manhattan distance + linear conflict performs worse than simply using manhattan distance, changing the directional queuing
; order eventually results in a reversed outcome that behaves as expected. The queuing order can be changed in the successor function.

(defun extracredit (puzzle)
  (let
      (
       (distance (manhattan puzzle))
       (top-row (subseq puzzle 0 3))
       (mid-row (subseq puzzle 3 6))
       (bot-row (subseq puzzle 6))
       )
    (when (and (member 1 top-row) (member 2 top-row) (> (position 1 top-row) (position 2 top-row)))
      (setf distance (+ distance 2)))
    (when (or (and (member 3 mid-row) (member 4 mid-row) (> (position 3 mid-row) (position 4 mid-row))) (and (member 3 mid-row) (member 5 mid-row) (> (position 3 mid-row) (position 5 mid-row))))
      (setf distance (+ distance 2)))
    (when (equal mid-row '(5 3 4))
      (setf distance (- distance 2)))
    (when (and (member 4 mid-row) (member 5 mid-row) (> (position 4 mid-row) (position 5 mid-row)))
      (setf distance (+ distance 2)))
    (when (or (and (member 6 bot-row) (member 7 bot-row) (> (position 6 bot-row) (position 7 bot-row))) (and (member 6 bot-row) (member 8 bot-row) (> (position 6 bot-row) (position 8 bot-row))))
      (setf distance (+ distance 2)))
    (when (and (member 7 bot-row) (member 8 bot-row) (> (position 7 bot-row) (position 8 bot-row)))
      (setf distance (+ distance 2)))
    (when (equal bot-row '(8 6 7))
      (setf distance (- distance 2)))
    distance
    )
)   

;;; QUEUE FUNCTIONS

; The "key" variable is for the heuristic function.
; f(n) = g(n) + h(n) where g(n) is the path cost from initial to n and h(n) is the heuristic output from state to goal
; This structure defines the queue. The queue has four things: an enqueuing function based on the output of f(n) above;
; a key function for the heuristic, a "last" variable for the last node in the queue, and an "elements" variable for the queue members.
(defstruct q
	(enqueue #'identity)
	(key #'identity)
	(last nil)
	(elements nil))

; Node structure definition. Contains state, parent, action, path cost, and depth. State refers to the puzzle configuration. The parent specifies the node whose action
; preceded the current node. The action is what was actually done to get to the current state from the parent state. Path cost defines the
; cost to get to the current node from the initial node. The depth refers to the hierarchy of expanded nodes; in other words, how many nodes one must travel from
; the current node to get to the initial node.
(defstruct node
  (state nil)
  (parent nil)
  (action nil)
  (path-cost 0)
  (depth 0))

; Function checking if the queue is empty.
(defun q-emptyp (q)
	(= (length (q-elements q)) 0))

; Function returning what is at the front of the queue.
(defun q-front (q)
	(elt (q-elements q) 0))

; Function removing the head element of the queue.
(defun q-remove (q)
	(when (list (q-elements q))
		(pop (q-elements q))))

; Function inserting an element into the queue. Calls the queueing function to do so; in this case, the A* enqueuing function defined above.
(defun q-insert (q items)
	(funcall (q-enqueue q) q items)
	q)

; The A* insert function. This function takes the queue, the nodes to be inserted, and the heuristic function as arguments.
; Recall that f(n) = g(n) + h(n), where g(n) is the cost (or in the 8-puzzle's case, depth) to a node from the initial node. h(n) is the
; estimated cost from a node to the goal. h(n) is also known as the heuristic function.
; f(n) is calculated for each node to be inserted. This f(n) value is then compared to the f(n) values in the queue. The nodes in the
; queue are inserted in ascending order of f(n). The lower f(n) is for a node, the closer it is determined to be to the goal. Since the nodes at the front
; of the queue are the first to be expanded, the function will place the node to be inserted before the first node that has an f(n) value greater than or equal
; to the inserted nodes f(n) value. The function recursively calls itself until the list of nodes to be inserted is empty.
(defun a*-insert (q nodes key)
  (let
      ((node (list (car nodes)))
       (fnode (+ (node-path-cost (car nodes)) (funcall key (node-state (car nodes)))))
       (inserted nil))
    (loop for i in (q-elements q) do ; loop through nodes
	 (when (and (null inserted) (<= fnode (+ (node-path-cost i) (funcall key (node-state i)))))  ; find the place in the queue with a higher f(n) & insert
	   (setf inserted t)
	   (setf (q-elements q) (append (subseq (q-elements q) 0 (position i (q-elements q))) node (subseq (q-elements q) (position i (q-elements q)))))   
	   )
	 )
    (when (null inserted) ; if nothing was inserted, the node had a higher f(n) than all nodes currently inside the queue. Put the node last.
      (setf (q-elements q) (append (q-elements q) node))
      )
    )
  (unless (null (cdr nodes))
    (a*-insert q (cdr nodes) key)
    )
)

; The A* enqueuing function. If the queue is empty, simply add the first node. If it isn't empty, call the A* insert function (defined above).
(defun enqueue-a* (q nodes)
  (if (q-emptyp q)
      (setf (q-elements q) nodes)
      (a*-insert q nodes (q-key q))
      )
)

;;; SEARCHING FUNCTIONS

; Once initial state is in queue, pop first thing out of queue, open it's children and then enqueue them based on f(n)

; The number of nodes expanded, or the number of nodes in the "closed" list
(defvar *nodes-expanded* 0)

; Successor function. Creates a list of actions, states, and costs that are possible from a single parent state. The action is either left, down, right, or up. The
; state will be the state that results after performing the action. The cost will always be one, since the new state is derived from one move from the parent.
(defun successor (state)
  (let
      ((asc-list nil))
    (when (move-right state)
      (setf asc-list (append asc-list (list (list "right" (move-right state) 1)))))
    (when (move-down state)
      (setf asc-list (append asc-list (list (list "down" (move-down state) 1)))))
    (when (move-up state)
      (setf asc-list (append asc-list (list (list "up" (move-up state) 1)))))
    (when (move-left state)
      (setf asc-list (append asc-list (list (list "left" (move-left state) 1)))))
    asc-list
  )
)

; Function checking if a state is equal to the goal. The 0 designates the empty space.
(defun goalp (state) 
  (equal state '(0 1 2 3 4 5 6 7 8))
  )

; Function checking if two states are equal. This is used to avoid re-examining states that have already been expanded.
(defun samep (state1 state2)
  (equal state1 state2)
  )

; The node expansion function. Behaves as follows.
; 1. Set a local variable called "triples". The successor function returns a list containing the following information about 
; the new nodes found via expansion: action taken on the previous state, the next state, and the cost in performing that action.
; 2. Create a list of new nodes for all of the action-state-cost values in "triples".
; 3. Return the new nodes.
(defun expand (successor node)
  (let ((triples (funcall successor (node-state node))))
    (mapcar (lambda (action-state-cost)
	      (let ((action (car action-state-cost))
		    (state (cadr action-state-cost))
		    (cost (caddr action-state-cost)))
		(make-node :state state
			   :parent node
			   :action action
			   :path-cost (+ (node-path-cost node)
					 cost)
			   :depth (1+ (node-depth node)))
		))
	    triples)
))

; The action sequence function. Only called if a node is found to be the goal.
; If a node has a parent, recursively call the function and add the action to the list of actions.
; If the top node has been reached, return the action list.
(defun action-sequence (node &optional (actions nil))
  (if (node-parent node)
      (action-sequence (node-parent node)
		       (cons (node-action node) actions))
      actions
      ))

; The graph search function. Behaves as follows:
; 1. Don't evaluate the function if the fringe is empty.
; 2. If the fringe is not empty, pop a node out of the fringe.
; 3. If the popped node is the goal, return the action sequence.
; 4. If the popped node is a member of the "closed" list, recursively call graph-search and return to 1. 
;    Because the closed node has been popped off the queue, the next node will be used.
; 5. If the popped node is neither the goal nor a member of the "closed" list, expand the node with the successor function.
; 6. Add the new nodes to the queue and add the current node to the "closed" list. Recursively call graph-search.
(defun graph-search (fringe closed successor goalp samep)
  (unless (q-emptyp fringe)
    (let ((node (q-remove fringe)))
      (cond ((funcall goalp (node-state node))
	     (setf *nodes-expanded* (list-length closed))
	     (action-sequence node))
	    ((member (node-state node) closed
		     :test #'samep :key #'node-state)
	     (graph-search fringe closed
			   successor goalp samep))
	    (t (let ((successors (expand successor node)))
		 (graph-search (q-insert fringe successors)
			       (cons node closed)
			       successor goalp samep))
	       ))
      )))

; The general search function. This function takes an initial state, a successor function, a goal test function, a same state function, an enqueuing function, and
; a heuristic function. The general search function begins by creating a queue, or "fringe" for unexplored nodes. The node is then searched using a graph search (defined
; below). When the search completes, a list is created containing the following: A list of actions necessary to complete the search, the number of expanded nodes, and the
; depth of the solution tree.
(defun general-search (initial-state successor goalp
		       &key (samep #'eql)
		       (enqueue #'identity)
		       (key #'identity))
  (setf *nodes-expanded* 0)
  (let ((return-list nil)
	(fringe (make-q :enqueue enqueue :key key)))
    (q-insert fringe (list (make-node :state initial-state)))
    (setf return-list (graph-search fringe nil successor goalp samep))
    (setf return-list (append (list return-list) (list (list-length return-list))))
    (setf return-list (append return-list (list *nodes-expanded*)))
    return-list
    ))

; The wrapper function for solving the 8-puzzle. Calls the general search algorithm with an initial state, a successor function, a goal
; test function, and a specified heuristic.
(defun 8-puzzle (initial-state heuristic)
  (general-search initial-state #'successor #'goalp :samep #'samep :enqueue #'enqueue-a* :key heuristic)
  )


;;; TESTING FUNCTION
(defun test-program (&optional (moves 100))
  (let
      ((test-cases (random-case moves)))
    (loop for i in test-cases do
	 (format t "*************** Test Case ~D: ~A ***************~C" (1+ (position i test-cases)) i #\Newline)
	 (format t "Misplaced Tiles: ~A ~C ~C" (8-puzzle i #'misplaced) #\Newline #\Newline)
	 (format t "Manhattan Distance: ~A ~C ~C" (8-puzzle i #'manhattan) #\Newline #\Newline)
	 (format t "Manhattan Distance + Linear Conflict: ~A ~C ~C" (8-puzzle i #'extracredit) #\Newline #\Newline)
	 (format t "***************************************************************~C~C" #\Newline #\Newline)
	 )
    )
  t
  )