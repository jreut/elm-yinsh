module Occupant exposing (Occupant, init, from)


type Occupant marker
    = Occupant (Maybe marker)


init : Occupant marker
init =
    Occupant (Nothing)


from : marker -> Occupant marker
from marker =
    Occupant (Just marker)
