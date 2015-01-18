;;; by Andrew Hancock

;;; ***** Frequency List layer *****

;;; FUNCTION NAME: make-fl
;;; DESCRIPTION:   Analyzes the frequency of symbols in the input message
;;;                and returns the result as a list of frequency entries.
;;; INPUT PARAMS:  A message in the form of a list of symbols.
;;; OUTPUT:        A Frequency list containing entries for each symbol.
;;; CALLED BY:     make-huffman-tree
;;; CALLS:         make-fl-iter

(defun make-fl (message)
  "Returns a frequency list for the input message"
  (make-fl-iter message ()))

;;; FUNCTION NAME:  make-fl-iter
;;; DESCRIPTION:    Recursive function which builds a list of frequency 
;;;                 entries for each symbol.
;;;                 Basis: When no symbols are left, return the list.
;;;                 Recursive step: Create a new list with the current symbol 
;;;                     incremented by one.
;;; INPUT PARAMS:   The list of remaining symbols and the current frequency list
;;; OUTPUT:         A Frequency list containing entries for each symbol
;;; CALLED BY:      make-fl
;;; CALLS:          fl-inc-symbol

(defun make-fl-iter (symbols fl)
  "Returns a frequency list for the list of symbols"
  (cond ((endp symbols) ;Basis
	 fl)
	(t ;Recursive
	 (make-fl-iter (rest symbols) (fl-inc-symbol fl (first symbols))))))

;;; Function NAME: fl-inc-symbol
;;; DESCRIPTION:   Recursive function which increments the current symbol in 
;;;                the frequency list, or adds it if not present.
;;;                Basis 1: Symbol not found, return a new fl entry with count 1
;;;                Basis 2: Symbol found, return new fl entry incremented by 1
;;;                    cons onto the rest of the list 
;;;                Recursive: Symbol not found, look further in the list and
;;;                    cons this entry onto the results.   
;;; INPUT PARAMS:  A frequency list and a symbol to increment
;;; OUTPUT:        A new frequency list with the specified symbol incremented.
;;; CALLED BY:     make-fl-iter
;;; CALLS:         fl-make-entry, fl-symbol, fl-count

(defun fl-inc-symbol (fl symbol)
  "Return a new frequency list with the specified entry incremented by 1."
  (cond
    ((endp fl) ;Basis 1
     (list (fl-make-entry symbol 1))) 
    ((equal (fl-symbol (first fl)) symbol) ;Basis 2
     (cons (fl-make-entry symbol (1+ (fl-count (first fl)))) (rest fl)))
    (t ;Recursive
     (cons (first fl) (fl-inc-symbol (rest fl) symbol)))))

;;; FUNCTION NAME: fl-make-entry
;;; DESCRIPTION:   Return a new frequency list entry composed of
;;;                the input symbol and count
;;; INPUT PARAMS:  A symbol and a number representing the frequency
;;; OUTPUT:        A frequency list entry
;;; CALLED BY:     fl-inc-symbol
;;; CALLS:          

(defun fl-make-entry (symbol freq)
  "Returns a new frequency list entry from symbol and freq."
  (list (list symbol) freq))

;;; FUNCTION NAME: fl-count
;;; DESCRIPTION:   Return the count from a frequency entry
;;; INPUT PARAMS:  A frequency list entry
;;; OUTPUT:        The count from the first frequency list entry
;;; CALLED BY:     fl-inc-symbol
;;; CALLS:         

(defun fl-count (fl-entry)
  "Returns the count of the fl-entry"
  (second  fl-entry))

;;; FUNCTION NAME: fl-symbol
;;; DESCRIPTION:   Return the symbol from a frequency entry
;;; INPUT PARAMS:  A frequency list entry
;;; OUTPUT:        The symbol from the frequency entry
;;; CALLED BY:     fl-inc-symbol
;;; CALLS:         

(defun fl-symbol (fl-entry)
  "Returns the symbol of the fl-entry"
  (first ( first fl-entry)))

;;; ***** Tree construction layer *****

;;; FUNCTION NAME: htree-less
;;; DESCRIPTION:   Return t if weight htree1 is less than weight of htree2
;;; INPUT PARAMS:  htree1, htree2 - Huffman trees
;;; OUTPUT:        t if weight of htree1 is less than htree2 else nil
;;; CALLED BY:     htree-sort
;;; CALLS:         

(defun htree-less (htree1 htree2)
  "Return true if the weight of htree1 is less than the weight of htree2"  
   (<  (htree-weight htree1)  (htree-weight htree2)))

;;; FUNCTION NAME: htree-symbols
;;; DESCRIPTION:   Return a list of symbols contained in the Huffman tree
;;; INPUT PARAMS:  htree - a Huffman trees
;;; OUTPUT:        A list of symbols contained in the Huffman tree
;;; CALLED BY:     htree-merge, encode-symbol, decode-iter
;;; CALLS:         htree-root

(defun htree-symbols (htree)
  "Return the list of symbols contained in the htree"
  (first (htree-root htree)))

;;; FUNCTION NAME: htree-weight
;;; DESCRIPTION:   Return the weight of the Huffman tree
;;; INPUT PARAMS:  htree - a Huffman tree
;;; OUTPUT:        A number representing the htree's weight
;;; CALLED BY:     htree-merge
;;; CALLS:         htree-root

(defun htree-weight (htree)
  "Return the weight of htree"
  (second (htree-root htree)))

;;; FUNCTION NAME: htree-root
;;; DESCRIPTION:   Return the root node of a Huffman tree
;;; INPUT PARAMS:  htree - a Huffman tree
;;; OUTPUT:        The root Node of htree
;;; CALLED BY:     htree-symbols, htree-weight
;;; CALLS:         

(defun htree-root (htree)
  "Return the root node of htree"
  (first htree))

;;; FUNCTION NAME: sort-htree
;;; DESCRIPTION:   Sorts a list of htrees by weight in ascending order
;;; INPUT PARAMS:  A list of htrees
;;; OUTPUT:        A list of htrees sorted by weight
;;; CALLED BY:     make-huffman-tree, make-huffman-tree-iter 
;;; CALLS:         htree-less

(defun htree-sort (htrees)
  "Sort a list of Huffman trees by weight."
  (sort htrees #'htree-less))

;;; FUNCTION NAME: make-huffman-tree
;;; DESCRIPTION:   Returns a Huffman tree constructed from a message.
;;; INPUT PARAMS:  A list of symbols representing a message
;;; OUTPUT:        A Huffman tree
;;; CALLED BY:
;;; CALLS:         htree-sort, make-huffman-tree-iter

(defun make-huffman-tree (message)
  "Build a Huffman tree from a list of symbols representing a message"
  (make-huffman-tree-iter (htree-sort (map 'list #'list (make-fl message)))))

;;; FUNCTION NAME: make-huffman-tree-iter
;;; DESCRIPTION:   A recursive function to construct a single Huffman tree from
;;;                a list of Huffman trees.
;;;                Basis: The list contains only one htree, return that htree.
;;;                Recursive: Two or more htrees remain. Sort the list of 
;;;                     htrees and merge the first two elements.
;;; INPUT PARAMS:  A list of Huffman Trees
;;; OUTPUT:        A htree composed of the fully merged input htrees
;;; CALLED BY:     make-huffman-tree
;;; CALLS:         htree-sort, htree-merge

(defun make-huffman-tree-iter (htrees)
  "Return a single Huffman tree from a list of Huffman trees"
  (if (equal (length htrees) 1) ;Basis
      (first htrees) 
      (make-huffman-tree-iter ;Recursive
       (htree-sort (cons 
		    (htree-merge (first htrees) (second htrees))
		    (rest (rest htrees)))))))


;;; FUNCTION NAME: htree-merge
;;; DESCRIPTION:   Merge two htrees into a single htree 
;;;                keeping the htree1 and htree2 as sub-htrees
;;; INPUT PARAMS:  htree1, htree2 - the htrees to merge
;;; OUTPUT:        A new htree composed of the merged input trees
;;; CALLED BY:     make-huffman-tree-iter
;;; CALLS:         htree-symbols, htree-weight

(defun htree-merge (htree1 htree2)
  "Return a new Huffman tree composed of the merged htree1 and htree2."
  (list 
   (list (append (htree-symbols htree1) (htree-symbols htree2))
	 (+ (htree-weight htree1) (htree-weight htree2)))
   htree1 htree2 ))
  
;;; FUNCTION NAME: leaf-p 
;;; DESCRIPTION:   Returns T if htree is a leaf or nil otherwise
;;; INPUT PARAMS:  A Huffman tree
;;; OUTPUT:        T if htree is a leaf or nil
;;; CALLED BY:     encode-symbol, decode
;;; CALLS:         

(defun leaf-p (htree)
  "Return T if htree is a leaf."
  (endp (rest htree)))


;;; FUNCTION NAME: left-subhtree
;;; DESCRIPTION:   Returns the left sub-tree of a Huffman tree
;;; INPUT PARAMS:  A Huffman tree
;;; OUTPUT:        A Huffman tree which is the left sub-tree of htree
;;; CALLED BY:     encode-symbol, decode
;;; CALLS:         

(defun left-subhtree (htree)
  "Return the left subtree of htree."
  (second htree))

;;; FUNCTION NAME: right-subhtree
;;; DESCRIPTION:   Returns the right sub-tree of a Huffman tree
;;; INPUT PARAMS:  A Huffman tree
;;; OUTPUT:        A Huffman tree which is the right subtree of htree
;;; CALLED BY:     encode-symbol, decode
;;; CALLS:         

(defun right-subhtree (htree)
  "Return the right sub-tree of htree."
  (third htree))

;;; ***** Encoding and decoding layer *****

;;; FUNCTION NAME: encode
;;; DESCRIPTION:   A recursive function to encode a message to binary given a 
;;;                message and a Huffman tree.
;;;                Basis: No more symbols left to encode, return nil.
;;;                Recursive: Encode the current symbol and return a new list
;;;                    with that symbol and the results of the next recursive 
;;;                    call appended to the end.
;;; INPUT PARAMS:  A list of symbols representing a message and a Huffman tree
;;; OUTPUT:        A list of bits representing the encoded message
;;; CALLED BY:
;;; CALLS:         encode-symbol 

(defun encode (message huffman-tree)
  "Returns a list of bits representing message encoded with huffman-tree."
  (if (endp message) ;Basis
      nil
      ;Recursive
      (append (encode-symbol (first message) huffman-tree)
	      (encode (rest message) huffman-tree))))

;;; FUNCTION NAME: encode-symbol
;;; DESCRIPTION:   A recursive function Encode a single symbol into a list of 
;;;                bits using a Huffman tree.
;;;                Basis: huffman-tree is a leaf node, return its symbol.
;;;                Recursive: Traverse the sub-tree which contains the symbol
;;;                    and cons the appropriate bit onto the result list.
;;; INPUT PARAMS:  A symbol and a Huffman-tree
;;; OUTPUT:        A list of bits representing the symbol encoded with 
;;;                huffman-tree.
;;; CALLED BY:     encode
;;; CALLS:         leaf-p, left-subhtree, right-subhtree
	 
(defun encode-symbol (symbol huffman-tree)
  "Return a list of bits representing the symbol encoded with huffman-tree"
  (cond ((leaf-p huffman-tree) ;Basis
	 nil)
	;Recursive
	((null (member symbol (htree-symbols (left-subhtree huffman-tree))))
	 (cons '1 (encode-symbol symbol (right-subhtree huffman-tree))))
	(t 
	 (cons '0 (encode-symbol symbol (left-subhtree huffman-tree))))))

;;; FUNCTION NAME: decode
;;; DESCRIPTION:   Decode a message from bits using huffman-tree
;;; INPUT PARAMS:  bits - a list of bits containing an encoded message
;;;                huffman-tree - The huffman-tree used to encode the message 
;;; OUTPUT:        A list of bits representing the encoded message
;;; CALLED BY:
;;; CALLS:         decode-iter

(defun decode (bits huffman-tree)
  "Return a message decoded from bits using huffman-tree"
  (decode-iter bits huffman-tree huffman-tree))

;;; FUNCTION NAME: decode-iter
;;; DESCRIPTION:   A recursive function which decodes a list of bits into a 
;;;                message.
;;;                Basis: Remaining bits are empty, return nil.
;;;                Recursive 1: A leaf node is reached in tree-iter, cons the
;;;                    symbol of the node onto the result of the recursive call.
;;;                Recursive 2: Not a leaf node. Recursively call with the 
;;;                    subhtree indicated by the first bit of bits.
;;; INPUT PARAMS:  bits - A list of remaining bits to decode
;;;                full-htree - the htree used to encode the message
;;;                tree-iter - a reference to the htree to decode the first bit
;;;                            of bits
;;; OUTPUT:        A symbol represented by the first bits using full-htree
;;; CALLED BY:     decode
;;; CALLS:         htree-symbols, right-subhtree, left-subhtree

(defun decode-iter (bits full-htree tree-iter)
  "Returns a list of symbols represented by a list of bits using the provided
Huffman tree"
  (cond ((leaf-p tree-iter) ;Recursive 1
	 (cons (first (htree-symbols tree-iter)) 
	       (decode-iter bits full-htree full-htree)))
	((endp bits)  ;Basis
	 nil)
	;Recursive 2
	((equal '1 (first bits)) 
	 (decode-iter (rest bits) full-htree (right-subhtree tree-iter)))
	(T 
	 (decode-iter (rest bits) full-htree (left-subhtree tree-iter)))))


