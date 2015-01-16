Huffman Coding in Common Lisp
=============================
	This program demonstrates Huffman coding in Common Lisp. Messages
are represented as lists of symbols are and encoded as a list of
literal ones and zeros. 
It was developed and tested using Carnegie Mellon University Common Lisp.

Usage
=====
Load the file into your Lisp environment to define all of the
functions. 
Here is an example demonstrating tree construction, encoding, and decoding:

First, define a message to be encoded:
`(defvar message '(A B A C A B A C A D A B A C A D A E A B A C A E A F))`

Construct a Huffman Tree:
`(defvar tree (make-huffman-tree message))`
`tree`
> (((E F D B C A) 26)
>  (((E F D B C) 13) (((E F D) 5) (((E) 2)) (((F D) 3) (((F) 1)) (((D) 2))))
>   (((B C) 8) (((B) 4)) (((C) 4))))
>  (((A) 13)))

Encode the message:
`(defvar encoded-message (encode message tree))`
`encoded-message`
> (1 0 1 0 1 0 1 1 1 0 1 0 1 0 1 1 1 0 0 1 1 1 0 1 0 1 0 1 1 1 0 0 1 1
> 1 0 0 0 1 0 1 0 1 0 1 1 1 0 0 0 1 0 0 1 0)

Finally, decode the message:
> (decode message tree)
> (A B A C A B A C A D A B A C A D A E A B A C A E A F)

Design Considerations
=====================
The program defines three layers of abstraction. 
  1. The "Frequency List" layer, which computes a list of
     symbol-number pairs representing symbols and their count 
     within the input list. The
  2. The "Tree Construction" layer returns a Huffman Tree for a
     particular frequency list computed in the first layer.
  3. The "Encoding/Decoding" layer supports encoding and decoding the
     list of symbols representing the message using a tree constructed
     in the second layer. A user defined tree can be provided as well.


In order to demonstrate a functional style of programming, a number of
restrictions were observed:
  1. No looping constructs. Only recursion is permitted.
  2. No use of defvar or defconstant, except for demonstration
     purposes as above.
  3. No sideffects or destructive functions allowed, with the sole
     exception of a call to sort in the function htree-sort

