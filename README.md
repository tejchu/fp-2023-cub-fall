# Final Exam 

Your task is to implement sparse matrices using [quadtrees](https://en.wikipedia.org/wiki/Quadtree).

This project contains some definitions and tests, but there are also a number of holes to fill in. There are 5 tasks in this exam listed below. You can get the specified number of points for each task. 

Tasks 1-3 and 4-5 are more or less equal in complexity, and each group costs 12 points. Only 12 points are counted towards the final grade, so feel free to pick one group of tasks and do it.  

Make sure that your project compiles, and that the tests pass before submitting. 


## Quadtree 

A quadtree is a tree with 4 subtrees in each internal node.
It can be used to efficiently store two-dimentional data structures, such as images or matrices.

A quadtree `Quad (Leaf 0) (Leaf 1) (Leaf 2) (Leaf 3)` represents the following matrix: 

```
0 1 
2 3
```

If there are quadrants with repeated elements, we can join them together into a leaf in order to save up memory. 

Consider this matrix: 

```
0 0 1 2
0 0 3 4 
5 6 9 9 
7 8 9 9 
```

Let's split it into 4 quadrants: 

```
0 0 | 1 2
0 0 | 3 4 
----|----
5 6 | 9 9 
7 8 | 9 9 
```

You can see that the north-western and the south-eastern quadrants are made out of the same numbers. Not to be wasteful, we replace them with leaves: 

```
Quad (Leaf 0)
     (Quad (Leaf 1)
           (Leaf 2)
           (Leaf 3)
           (Leaf 4)
     )
     (Quad (Leaf 5)
           (Leaf 6)
           (Leaf 7)
           (Leaf 8)
     )
     (Leaf 9)
```

Quadtrees work great for sparse matrices with many `0`s. These elements do not contribute to either addition or multiplication of matrices, thus don't need to take up space.

## Conventions 

To make our life a little easier, we will only be working with square matrices of the size `2^b`. We will store the dimention (boundary) `b` of each node in it: 

```
Quad 2 (Leaf 1 0)
       (Quad 1 (Leaf 0 1)
             1 (Leaf 0 2)
             1 (Leaf 0 3)
             1 (Leaf 0 4)
       )
       (Quad 1 (Leaf 0 5)
             1 (Leaf 0 6)
             1 (Leaf 0 7)
             1 (Leaf 0 8)
       )
       (Leaf 1 9)
```

The coordinates of the elements start from the north-western corner of the matrix: 

```
 | 0 1 | 2 3 
-|-----|------
0| NW  | NE
1|     | 
-|-----|------
2| SW  | SE  
3|     | 
```

The quadrants are listed in the order `NW`, `NE`, `SW`, `SE`. 

There cannot be a `Leaf` with negative boundary. 
Neither can there be a `Quad` with a boundary `0` or less. 

All matrices should be normalized, i.e. they shouldn't contain an internal `Quad` node with all `4` children equal to each other. 

## Tasks

1. Implement matrix operations [src/Matrix.hs](src/Matrix.hs): 
   1) (1 point) `add :: matrix a -> matrix a -> matrix a` -- addition of two matrices 
   2) (1 point) `sub :: matrix a -> matrix a -> matrix a`
   -- substraction 
   3) (1 point) `neg :: matrix a -> matrix a` -- elementwise negation
   4) (3 point) `mult :: matrix a -> matrix a -> matrix a` -- matrix multiplication
   5) (1 point) `transpose :: matrix a -> matrix a` -- matrix transposition
   6) (1 point) `scalarMult :: a -> matrix a -> matrix a` -- multiplication of a scalar value and a matrix

   Notes: 

   * The functions are not defined when the dimentions of the matrices are incompatible: throw an error in this case. 
   * The normal algebraic properties of the matrix operations should hold. 
   * Make sure all unit tests pass. Ignore property based tests in this task.

2. (1 point) Fix a bug in the matrix generator [test/Test/Matrix/Gen.hs](test/Test/Matrix/Gen.hs) so that it generates square matrices of size `2^b` for some `b`. 

3. (3 points) Find and implement 3 missing algebraic properties of matrix operations in [test/Test/Matrix/Prop.hs](test/Test/Matrix/Prop.hs)

4. (8 points) Implement insertion of one element into a matrix at the specified coordinate in [src/QuadTree.hs](src/QuadTree.hs). One way to do it is to first compute the trail of "breadcrumbs" through the quadrants to the coordinate, and then do the insertion using them. For example, for a matrix with bound `2`, element with coordinate `(2, 3)` is in the `SW` quadrant of the `SE` quadrant of the matrix, thus the trail is `[SE, SW]`. 
You can also pick another way to achieve the same goal. 

5. (4 points) Impelement unit and property-based tests for insertion. 
