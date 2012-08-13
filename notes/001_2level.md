# Examples of 2-level supercompilation

The task is (`pfp/2l/fin2.pfp`):

```
fin2 <1>;

fin2 = \x -> case (fin1 x) of {
   False() -> False();
   True()  -> case x of {
     S(x1) -> fin2 x1;
     Z()   -> True()
   }
};

fin1 = \x -> case x of {
  S(x1) -> fin1(x1);
  Z()   -> True()
};
```

The complexity of this task is quadratic in terms of the size of an input.

```
scala> mrsc.pfp.samples.snippets.s001
## Supercompiling by SC2
## Graph
|__(fin2 <1>)
  |->*
  |__((\case (fin1 0) of {False() -> False(); True() -> case 0 of {S(_) -> (fin2 0); Z() -> True()}}) <1>)
    |->
    |__case (fin1 <1>) of {False() -> False(); True() -> case <1> of {S(_) -> (fin2 0); Z() -> True()}}
      |->*
      |__case ((\case 0 of {S(_) -> (fin1 0); Z() -> True()}) <1>) of {False() -> False(); True() -> case <1> of {S(_) -> (fin2 0); Z() -> True()}}
        |->
        |__case case <1> of {S(_) -> (fin1 0); Z() -> True()} of {False() -> False(); True() -> case <1> of {S(_) -> (fin2 0); Z() -> True()}}
          |<1> = S(<101>)
          |__case (fin1 <101>) of {False() -> False(); True() -> case S(<101>) of {S(_) -> (fin2 0); Z() -> True()}}
            |->*
            |__case ((\case 0 of {S(_) -> (fin1 0); Z() -> True()}) <101>) of {False() -> False(); True() -> case S(<101>) of {S(_) -> (fin2 0); Z() -> True()}}
              |->
              |__case case <101> of {S(_) -> (fin1 0); Z() -> True()} of {False() -> False(); True() -> case S(<101>) of {S(_) -> (fin2 0); Z() -> True()}}*
          |<1> = Z()
          |__case True() of {False() -> False(); True() -> case Z() of {S(_) -> (fin2 0); Z() -> True()}}
            |->
            |__case Z() of {S(_) -> (fin2 0); Z() -> True()}
              |->
              |__True()

## Embeddings
### graph 1 ###
case case <1> of {S(_) -> (fin1 0); Z() -> True()} of {False() -> False(); True() -> case <1> of {S(_) -> (fin2 0); Z() -> True()}}
<*>
case case <101> of {S(_) -> (fin1 0); Z() -> True()} of {False() -> False(); True() -> case S(<101>) of {S(_) -> (fin2 0); Z() -> True()}}


## Supercompiling the pair of expressions
### the top expression
case case <1> of {S(_) -> (fin1 0); Z() -> True()} of {False() -> False(); True() -> case <1> of {S(_) -> (fin2 0); Z() -> True()}}
case <1> of {S(_) -> (let (#(\(\case 1 of {S(_) -> ((3 0) 1); Z() -> (let (#(\(let (#(\(\case 1 of {S(_) -> ((3 0) 1); Z() -> case 0 of {S(_) -> (5 0); Z() -> True()}}))) in ((0 1) 1)))) in (0 1))}))) in ((0 1) 1)); Z() -> True()}
### renaming of the bottom expression
case case <1> of {S(_) -> (fin1 0); Z() -> True()} of {False() -> False(); True() -> case S(<1>) of {S(_) -> (fin2 0); Z() -> True()}}
case <1> of {S(_) -> (let (#(\(\case 1 of {S(_) -> ((3 0) 1); Z() -> (let (#(\(let (#(\(\case 1 of {S(_) -> ((3 0) 1); Z() -> case 0 of {S(_) -> (5 0); Z() -> True()}}))) in ((0 1) 1)))) in (0 1))}))) in ((0 1) 1)); Z() -> True()}

```