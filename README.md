### Conversion from dependency structures to constituency structures

#### Words

First off, the algorithm uses trees of words and part-of-speech
tags. Part-of-speech tags are simple `String`s. Words are records with
three fields: a text[^1], a part of speech tag and a serial (a 1-index
into the sentence).

> data Word =
>      Word { text   :: String
>           , pos    :: String
>           , serial :: Integer
>           }

Words are parsed as follows:

    Word := QuotedString '/' QuotedString '/' Integer

So, for instance, they are represented as:

    "dog"/"NN"/2


#### Dependency Trees

Dependency trees---our input format---are represented as nodes with a
governor and a list of dependants. A leaf is represented by a `Node`
with no dependants.

> data Tree =
>      Node { governor   ::  Word
>           , dependants :: [Tree]
>           }

Dependency trees are parsed as follows:

    Node := Word | '(' Word Node* ')'

So the above word is still a valid tree, but so is the following:

    ("ROOT"/"ROOT"/0
      ("likes"/"VBZ"/4
        ("dog"/"NN"/2 "my"/"PRP"/1)
        ("also"/"RB"/3)
        ("eating"/"VBG"/5 "sausage"/"NN"/6)
      )
    )

Which represents the following tree:

```tree
["ROOT" ["likes" ["dog" "my"] "also" ["eating" "sausage"]]]
```


#### Constituency Trees

The main difference between constituency trees---our output
format---and dependency trees is that dependency trees store words at
every node, whereas in constituency trees only store words in the
leaves, and the nodes are marked with part-of-speech tags.

> data Tree =
>      Leaf Word
>    | Node POS [Tree]

The printing algorithm for constituency trees is very similar to the
one for parsing dependency trees, so the following is a valid
constituency tree (produced by our conversion algorithm).

    ("ROOT"
      ("VP"
        ("NP"
          ("PRP" "my"/"PRP"/1)
          ("NN" "dog"/"NN"/2)
        )
        ("RB" "also"/"RB"/3)
        ("VBZ" "likes"/"VBZ"/4)
        ("VP"
          ("VBG" "eating"/"VBG"/5)
          ("NN" "sausage"/"NN"/6)
        )
      )
    )

This string represents the following tree:

```tree
["ROOT" ["VP" ["NP" ["PRP" "my"] ["NN" "dog"] ] ["RB" "also"] ["VBZ" "likes"] ["VP" ["VBG" "eating"] ["NN" "sausage"] ] ] ]
```


#### Conversion à la Collins

For the conversion algorithm we use the simple algorithm as proposed
by Collins et al., which tries to produce the simplest possible
constituency trees from dependency trees. The algorithm is as follows:

  - if we encounter a node *without* dependencies, we simply convert it
    into a node bearing the part-of-speech tag and a leaf bearing the
    governor;

  - if we encounter a node *with* dependencies, we do several things:

      * we compute x, the part-of-speech tag of the governor;
      * we compute xp, the phrasal projection of x (using `toXP`);
      * we recursively apply the algorithm to the dependencies;
      * we create a new node for x, using the current governor, and
        insert it into the dependencies;
      * lastly, we combine all of the above in a new node for xp.

Here is the algorithm written out in Haskell:

> collins :: Dep.Tree -> Con.Tree
> collins (Dep.Node gov [])   = Con.Node (pos gov) [Con.Leaf gov]
> collins (Dep.Node gov deps) = Con.Node xp (insert gov' deps')
>   where
>     x     = pos gov :: POS
>     xp    = toXP x  :: POS
>     gov'  = Con.Node x [Con.Leaf gov] ::  Con.Tree
>     deps' = map collins deps          :: [Con.Tree]
>        -- ^ apply `collins` to each dependency



### References

Michael Collins, Jan Hajic, Lance Ramshaw, and Christoph
Tillmann. [A Statistical Parser for Czech][^Collins1999]. Proceedings
of ACL-1999, pages 505-512, 1999.

Dan Klein and Christopher D. Manning. 2003. [Accurate Unlexicalized
Parsing][^Klein2003]. Proceedings of the 41st Meeting of the
Association for Computational Linguistics, pp. 423-430.

Richard Socher, John Bauer, Christopher D. Manning and Andrew
Y. Ng. 2013. [Parsing With Compositional Vector Grammars][^Socher2013].
Proceedings of ACL 2013

Fei Xia and Martha Palmer, 2001. [Converting Dependency Structures to
Phrase Structures][^Xia2001], Proceedings of the 1st Human Language
Technology Conference (HLT-2001), San Diego, Mar 18-21, 2001.

[^Collins1999]: http://www.aclweb.org/anthology/P99-1065
[^Klein2003]: http://nlp.stanford.edu/~manning/papers/unlexicalized-parsing.pdf
[^Socher2013]: http://nlp.stanford.edu/pubs/SocherBauerManningNg_ACL2013.pdf
[^Xia2001]: http://www.aclweb.org/anthology-new/H/H01/H01-1014.pdf

[^1]: While we refer to this field as the "text", which is how it
      would be used whilst using `dep2con` standalone, when we
      integrate it with `SemAnTE` we will use it to store ids.
