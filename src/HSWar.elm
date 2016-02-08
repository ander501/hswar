module HSWar where 

import Char
import Color
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Keyboard
import Math.Matrix2 exposing ((-+-), (-*-))
import Math.Matrix2
import Math.SL2R
import List
import Signal
import Signal exposing (Signal)
import Text
import Time
import Window

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

---------------------------------------------------------------------------- --}
type alias Direction = { x:Int, y:Int }

type UserInput = PlayerOne Direction 
               | PlayerTwo Direction 
               | Reset Bool 


userInput : Signal UserInput
userInput =
    Signal.mergeMany [
        Signal.map PlayerOne Keyboard.wasd,
        Signal.map PlayerTwo Keyboard.arrows,
        Signal.map Reset Keyboard.space]


type alias Input =
    { timeDelta : Float
    , userInput : UserInput
    }



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}
type alias Phase = {
  position : Math.SL2R.Point,
  velocity : Math.SL2R.Tangent
}

type alias GameState = { 
      firstPlayer : Phase,
      secondPlayer: Phase,
      shots: List Phase,
      keys: List Char
    }

defaultGame : GameState
defaultGame =
    {
      firstPlayer = { position = (1, -0.5, 0, 1), velocity = (0, 0, 0, 0) } ,
      secondPlayer = { position = (1, 0.5, 0, 1), velocity = (0, 0, 0, 0) } ,
      shots = [],
      keys = []
    }



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame input =
    direct input
    >> step input
    >> fire input
    >> friction 0.97
    >> reset input.userInput

stepObject: Float -> Phase -> Phase
stepObject t { position, velocity }
    = { position = position -*- Math.SL2R.exp (Math.Matrix2.scale t velocity) |> normalize
      , velocity = velocity}
      

turn : Math.SL2R.Tangent
turn = (0, -0.0001, 0.0001, 0)

boost : Math.SL2R.Tangent
boost = (0.0001, 0, 0, -0.0001)

direct : Input -> GameState -> GameState
direct { userInput } state = 
    let thrust {x, y} phase 
            = { phase | velocity <- (Math.Matrix2.scale (toFloat x) turn) 
                                    -+- (Math.Matrix2.scale (abs <| toFloat y) boost) 
                                          -+- phase.velocity }
    in case userInput of
         PlayerOne direction 
             -> { state | firstPlayer <- thrust direction state.firstPlayer }
         PlayerTwo direction
             -> { state | secondPlayer <- thrust direction state.secondPlayer }
         _ -> state

mapObjects : (Phase -> Phase) -> GameState -> GameState
mapObjects f state =
    { state | 
              firstPlayer <- f state.firstPlayer,
              secondPlayer <- f state.secondPlayer,
              shots <- List.map f state.shots }

step : Input -> GameState -> GameState
step { timeDelta } = mapObjects <| stepObject timeDelta

friction : Float -> GameState -> GameState
friction f = mapObjects (\ phase -> { phase | velocity <- Math.Matrix2.scale f phase.velocity})

muzzleVelocity : Phase
muzzleVelocity = {
  position = (1.05, 0, 0, (20.0/21.0)), 
  velocity = (0.0006, 0, 0, -0.0006) }

fire : Input -> GameState -> GameState
fire { userInput } = fireChoice userInput

fireChoice : UserInput -> GameState -> GameState
fireChoice userInput state = 
    let 
      createShot {y} player muzzle shots 
          = if (y == -1) 
            then
                { position = player.position -*- muzzle.position, 
                             velocity = player.velocity -+- muzzle.velocity }
                :: shots
            else
                shots
    in case userInput of
         PlayerOne direction -> { state | shots <- createShot direction state.firstPlayer muzzleVelocity state.shots }
         PlayerTwo direction -> { state | shots <- createShot direction state.secondPlayer muzzleVelocity state.shots }
         _             -> state

reset : UserInput -> GameState -> GameState
reset userInput state =
    case userInput of
      Reset true -> defaultGame
      _ -> state

normalize : Math.SL2R.Point -> Math.SL2R.Point
normalize p =
    if (abs(logBase e(Math.Matrix2.det p)) < 2) 
    then
        p
    else
        Math.Matrix2.scale (1 / sqrt(Math.Matrix2.det p)) p

{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

{--
display : (Int,Int) -> GameState -> Element
display (w,h) gameState =
    let s = toFloat w / 6.0
    in [ world (gameState.firstPlayer.position) gameState,
         world (gameState.secondPlayer.position) gameState,
         world earth gameState] 
        |> List.map (Graphics.Collage.scale s
             >> \c -> collage (w) (h // 3) [c])
        |> flow left
--}

{--}
display : (Int, Int) -> GameState -> Element
display (w, h) gameState =
     [ world earth gameState
      |> scale (toFloat <| h // 2)
      >> (\c -> collage w h [c])]
     |> layers
    
--}

shipColor = Color.black

earth : Math.SL2R.Point
earth = (1, 0, 0, 1)

ship1 : List Math.SL2R.Point 
ship1 = [ (1.02, 0, 0, 50.0/51.0), 
           (50/51.0, -0.02, 0, 1.02), 
          (50/51.0, 0.02, 0, 1.02)]

ship2 : List Math.SL2R.Point 
ship2 = [ (1.02, 0, 0, 50.0/51.0), 
          (50/51.0, -0.02, 0, 1.02), 
          (0.99, 0, 0, 100.0/99.0), 
          (50/51.0, 0.02, 0, 1.02)]

plot : Math.SL2R.Point -> Math.SL2R.Point -> ( Float, Float )
plot origin p = p -*- (Math.SL2R.inv origin)
              |> Math.SL2R.toPoincareDisc

vectorLineStyle = { defaultLine | width <- 0.002, color <- shipColor }

vectorPolygon : Math.SL2R.Point -> Math.SL2R.Point -> List Math.SL2R.Point -> Form
vectorPolygon origin center points
    = List.map ((-*-) center) points
        |> List.map (plot origin)
        |> polygon
        |> outlined vectorLineStyle

dot : Math.SL2R.Point -> Math.SL2R.Point -> Form
dot origin point
    = circle 0.01        
       |> filled Color.black 
       |> move (plot origin point)


world : Math.SL2R.Point -> GameState -> Form 
world origin { firstPlayer, secondPlayer, shots} =
     ( vectorPolygon origin firstPlayer.position ship1 )
     :: ( vectorPolygon origin secondPlayer.position ship2 )
     :: List.map (\s -> dot origin s.position) shots
        |> group


{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta : Signal Float
delta =
    Time.fps 30


input : Signal Input
input =
    Signal.sampleOn delta (Signal.map2 Input delta userInput)


gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame input


main : Signal Element
main =
    Signal.map2 display Window.dimensions gameState
