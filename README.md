# Push and Pull for Linear Haskell

## Box

The `Linear.Box` module exports a data type `Box` defined like this:

``` haskell
data Box r a where { Box :: a %r -> Box r a }
```

Here, `r` is the *multiplicity*, which is either `1` or `Many`.

## Pushable

If we wanted to "push" a multiplicity from the outside of a list
onto all the elements of a list, we could define a function to do so.

``` haskell
pushList :: Box r (List a) %1-> List (Box r a)
pushList (Box (Cons car cdr)) = Cons (Box car) (pushList (Box cdr))
pushList (Box Nil) = Nil
```

More generally, we can define a `Pushable` typeclass:

``` haskell
class Pushable f where
  push :: Box r (f a) %1-> f (Box r a)
```

## Deriving Pushable

``` haskell
import TH.Pushable

data List a where
  Cons :: a %1-> List a %1-> List a
  Nil  :: List a

$(derivePushable ''List)
```
