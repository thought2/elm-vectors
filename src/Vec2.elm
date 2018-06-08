module Vec2 exposing (..)

{-|


# Type Aliases

@docs Vec2


# Type Constructors

@docs vec2


# Functor

@docs map


# Applicative

@docs pure, apply, liftA1, liftA2, liftA3


# Monad

@docs bind, andThen, return


# Foldable

@docs foldl, foldl1, foldr, foldr1


# Set / Get / Update

@docs getNth0, getNth1, setNth0, setNth1, mapNth0, mapNth1


# Conversions

@docs toTuple, fromTuple, toList, fromList


# Other Operations

@docs reverse, asArgs

-}

import Types exposing (..)


-- TYPE ALIASES


{-| Polymorphic 2-dimensional vector type

    myVec : Vec2 Bool

-}
type alias Vec2 a =
    Types.Vec2 a



-- TYPE CONSTRUCTORS


{-| Construct (Vec2 a) types

    myVec : Vec2 Bool
    myVec =
        vec2 True False

-}
vec2 : a -> a -> Vec2 a
vec2 =
    Types.Vec2



-- FUNCTOR


{-| Map over all fields

    map not (Vec2 True False) == Vec2 False True

-}
map : (a -> b) -> Vec2 a -> Vec2 b
map f (Vec2 x y) =
    Vec2 (f x) (f y)



-- APPLICATIVE


{-| Initialize all fields with the same value

    Vec2.pure True == (Vec2 True True)

-}
pure : a -> Vec2 a
pure x =
    Vec2 x x


{-| Apply functions from a vector one by one to values of another vector.

    apply (Vec2 not not) (Vec2 True False)
        == Vec2 False True

-}
apply : Vec2 (a -> b) -> Vec2 a -> Vec2 b
apply (Vec2 x1 y1) (Vec2 x2 y2) =
    Vec2 (x1 x2) (y1 y2)


{-| Lift unary function to the context and apply

    liftaA1 not (Vec2 True False)
        == Vec2 False True

-}
liftA1 : (a -> b) -> Vec2 a -> Vec2 b
liftA1 f v =
    apply (pure f) v


{-| Lift binary function to the context and apply

    liftaA2 (+) (Vec2 1 2) (Vec2 10 20)
        == Vec2 11 22

-}
liftA2 : (a -> b -> c) -> Vec2 a -> Vec2 b -> Vec2 c
liftA2 f v1 v2 =
    apply (liftA1 f v1) v2


{-| Lift ternary function to the context and apply

    liftaA3
        clamp
        (Vec2 0 0)
        (Vec2 100 200)
        (Vec2 200 300)
        == Vec2 100 200

-}
liftA3 :
    (a -> b -> c -> d)
    -> Vec2 a
    -> Vec2 b
    -> Vec2 c
    -> Vec2 d
liftA3 f v1 v2 v3 =
    apply (liftA2 f v1 v2) v3



-- MONAD


{-| Lift ternary function to the context and apply

Say you have a library providing you the following 'complex' vector constructor.

    createVec : Int -> Vec2 (List String)
    createVec n =
        vec2 (List.repeat n "x") (List.repeat n "y")

And this vector:

    myVec =
        vec2 1 2

With `bind` you can archive the following:

    bind createVec myVec
        == Vec2 "x" "yy"

-}
bind : (a -> Vec2 b) -> Vec2 a -> Vec2 b
bind f (Vec2 x y) =
    let
        (Vec2 x_ _) =
            f x

        (Vec2 _ y_) =
            f y
    in
    Vec2 x_ y_


{-| Same as `bind`
-}
andThen : (a -> Vec2 b) -> Vec2 a -> Vec2 b
andThen =
    bind


{-| Same as `pure`
-}
return : a -> Vec2 a
return =
    pure



-- -- FOLDABLE


{-| Fold left over the vector

    foldl (::) [] (Vec2 2 1)
        == [1, 2]

-}
foldl : (a -> b -> b) -> b -> Vec2 a -> b
foldl f acc (Vec2 x y) =
    f y (f x acc)


{-| Like `foldl` but without initial value

    foldl1 (-) (Vec2 1 3)
        == (1 - 3)

-}
foldl1 : (a -> a -> a) -> Vec2 a -> a
foldl1 f (Vec2 x y) =
    f x y


{-| Fold right over the vector

    foldr (::) [] (Vec2 2 1)
        == [2, 1]

-}
foldr : (a -> b -> b) -> b -> Vec2 a -> b
foldr f acc (Vec2 x y) =
    f x (f y acc)


{-| Like `foldr` but without initial value

    foldr1 (-) (Vec2 1 3)
        == (3 - 1)

-}
foldr1 : (a -> a -> a) -> Vec2 a -> a
foldr1 f (Vec2 x y) =
    f y x



-- GET / SET/ UPDATE


{-| Takes element at position 0

    getNth0 (Vec2 0 1) == 0

-}
getNth0 : Vec2 a -> a
getNth0 (Vec2 x _) =
    x


{-| Takes element at position 1

    getNth1 (Vec2 0 1) == 1

-}
getNth1 : Vec2 a -> a
getNth1 (Vec2 _ y) =
    y


{-| Sets element at position 0

    setNth0 1 (Vec2 0 0) ==  (Vec2 1 0)

-}
setNth0 : a -> Vec2 a -> Vec2 a
setNth0 x (Vec2 _ y) =
    Vec2 x y


{-| Sets element at position 0

    setNth0 1 (Vec2 0 0) ==  (Vec2 0 1)

-}
setNth1 : a -> Vec2 a -> Vec2 a
setNth1 y (Vec2 x _) =
    Vec2 x y


{-| maps over element at position 0

    mapNth0 not (Vec2 True True) == Vec2 False True

-}
mapNth0 : (a -> a) -> Vec2 a -> Vec2 a
mapNth0 f (Vec2 x y) =
    Vec2 (f x) y


{-| maps over element at position 1

    mapNth0 not (Vec2 True True) == Vec2 True False

-}
mapNth1 : (a -> a) -> Vec2 a -> Vec2 a
mapNth1 f (Vec2 x y) =
    Vec2 x (f y)



-- CONVERSIONS


{-| creates a 2-tuple from vector

    toTuple (Vec2 0 1) == (0, 1)

-}
toTuple : Vec2 a -> ( a, a )
toTuple (Vec2 x y) =
    ( x, y )


{-| creates a vector from a 2-tuple

    fromTuple (0, 1) == (Vec2 0 1)

-}
fromTuple : ( a, a ) -> Vec2 a
fromTuple ( x, y ) =
    Vec2 x y


{-| creates a list from vector elements

    toList (Vec2 0 1) == [0, 1]

-}
toList : Vec2 a -> List a
toList =
    foldr (::) []


{-| creates a vector just from length 2 lists

    fromList [] == Nothing
    fromList [0, 1] == Just (Vec2 0 1)

-}
fromList : List a -> Maybe (Vec2 a)
fromList list =
    case list of
        x :: y :: [] ->
            Just (Vec2 x y)

        _ ->
            Nothing



-- OTHER OPERATIONS


{-| Reverse the elements of a vector

    reverse (Vec2 1 2) == Vec2 2 1

-}
reverse : Vec2 a -> Vec2 a
reverse (Vec2 x y) =
    Vec2 y x


{-| use vector elements as function arguments

    asArgs (+) (Vec2 1 2) == 3

-}
asArgs : (a -> a -> b) -> Vec2 a -> b
asArgs f (Vec2 x y) =
    f x y
