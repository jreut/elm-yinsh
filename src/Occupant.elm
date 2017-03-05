module Occupant exposing (Occupant, init, lift, fold)


type Occupant marker
    = Occupant (Maybe marker)


init : Occupant marker
init =
    Occupant (Nothing)


lift : marker -> Occupant marker
lift marker =
    Occupant (Just marker)


fold : Occupant marker -> Maybe marker
fold (Occupant occupant) =
    occupant
