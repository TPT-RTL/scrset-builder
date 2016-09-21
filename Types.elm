module Types exposing (..)

-- App


type Msg
    = SetEnv Environment
    | UpdateWidthList WidthMsg
    | UpdateSizeList SizeMsg


type Unit
    = Pixel
    | ViewWidth
    | Percent


type Condition
    = MinWidth
    | MaxWidth


type WidthMsg
    = AddWidth
    | DeleteWidth Int
    | UpdateWidthMeasure Int Int


type SizeMsg
    = AddSize
    | DeleteSize Int
    | UpdateSizeCondition Int Condition
    | UpdateSizeMeasure Int Int
    | UpdateSizeUnit Int Unit
    | UpdateSizeWidth Int WidthMsg


type alias Model =
    { env : Environment
    , image : Image
    }


type alias Environment =
    { width : Width
    , density : Float
    }


type alias Image =
    { widths : List Width
    , sizes : List Size
    }


type alias Width =
    { id : Int
    , measure : Int
    , unit : Unit
    }


type alias Size =
    { id : Int
    , condition : Condition
    , measure : Int
    , unit : Unit
    , width : Width
    }
