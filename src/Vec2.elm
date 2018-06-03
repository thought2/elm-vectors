module Vec2 exposing (..)

import Types exposing (..)


-- FUNCTOR


map : (a -> b) -> Vec2 a -> Vec2 b
map f (Vec2 x y) =
    Vec2 (f x) (f y)



-- APPLICATIVE


pure : a -> Vec2 a
pure x =
    Vec2 x x


apply : Vec2 (a -> b) -> Vec2 a -> Vec2 b
apply (Vec2 x1 y1) (Vec2 x2 y2) =
    Vec2 (x1 x2) (y1 y2)


liftA1 : (a -> b) -> Vec2 a -> Vec2 b
liftA1 f v =
    apply (pure f) v


liftA2 : (a -> b -> c) -> Vec2 a -> Vec2 b -> Vec2 c
liftA2 f v1 v2 =
    apply (liftA1 f v1) v2


liftA3 :
    (a -> b -> c -> d)
    -> Vec2 a
    -> Vec2 b
    -> Vec2 c
    -> Vec2 d
liftA3 f v1 v2 v3 =
    apply (liftA2 f v1 v2) v3



-- MONAD


bind : (a -> Vec2 b) -> Vec2 a -> Vec2 b
bind f (Vec2 x y) =
    let
        (Vec2 x_ _) =
            f x

        (Vec2 _ y_) =
            f y
    in
    Vec2 x_ y_


andThen : (a -> Vec2 b) -> Vec2 a -> Vec2 b
andThen =
    bind


return : a -> Vec2 a
return =
    pure



-- FOLDABLE


foldl : (a -> b -> b) -> b -> Vec2 a -> b
foldl f acc (Vec2 x y) =
    f y (f x acc)


foldl1 : (a -> a -> a) -> Vec2 a -> a
foldl1 f (Vec2 x y) =
    f y x


foldr : (a -> b -> b) -> b -> Vec2 a -> b
foldr f acc (Vec2 x y) =
    f x (f y acc)


foldr1 : (a -> a -> a) -> Vec2 a -> a
foldr1 f (Vec2 x y) =
    f x y



-- GET / UPDATE


mapNth0 : (a -> a) -> Vec2 a -> Vec2 a
mapNth0 f (Vec2 x y) =
    Vec2 (f x) y


mapNth1 : (a -> a) -> Vec2 a -> Vec2 a
mapNth1 f (Vec2 x y) =
    Vec2 x (f y)


getNth0 : Vec2 a -> a
getNth0 (Vec2 x _) =
    x


getNth1 : Vec2 a -> a
getNth1 (Vec2 _ y) =
    y



-- CONVERSIONS


toTuple : Vec2 a -> ( a, a )
toTuple (Vec2 x y) =
    ( x, y )


fromTuple : ( a, a ) -> Vec2 a
fromTuple ( x, y ) =
    Vec2 x y


toList : Vec2 a -> List a
toList =
    foldl (::) []


fromList : List a -> Maybe (Vec2 a)
fromList list =
    case list of
        x :: y :: [] ->
            Just (Vec2 x y)

        _ ->
            Nothing



-- MISC


reverse : Vec2 a -> Vec2 a
reverse (Vec2 x y) =
    Vec2 y x


asArgs : (a -> a -> b) -> Vec2 a -> b
asArgs f (Vec2 x y) =
    f x y



-- LIST OPERATIONS


combine : List (Vec2 a) -> Vec2 (List a)
combine xs =
    let
        add =
            liftA2 (::)
    in
    List.foldr add (Vec2 [] []) xs


unCombine : Vec2 (List a) -> List (Vec2 a)
unCombine (Vec2 xs ys) =
    List.map2 Vec2 xs ys
