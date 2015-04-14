
import Char
import Color
import Graphics.Element (..)
import Graphics.Collage
import Keyboard
import Math.Matrix2 (..)
import Math.SL2R
import List
import Signal
import Signal (Signal)
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
               | PlayerOneFire Bool 
               | PlayerTwoFire Bool 


userInput : Signal UserInput
userInput =
    Signal.mergeMany [
        Signal.map PlayerOne Keyboard.wasd,
        Signal.map PlayerTwo Keyboard.arrows,
        Signal.map Reset Keyboard.space,
        Char.toCode 'e'  
            |> Keyboard.isDown
            |> Signal.map PlayerOneFire,
        Char.toCode '/'
            |> Keyboard.isDown 
            |> Signal.map PlayerTwoFire ]


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
      shots: List Phase
    }

defaultGame : GameState
defaultGame =
    {
      firstPlayer = { position = (Matrix2 1 -1 0 1), velocity = (Matrix2 0.1 0 0 -0.1) } ,
      secondPlayer = { position = (Matrix2 1 1 0 1), velocity = (Matrix2 0 0 0 0) } ,
      shots = []
    }



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame input =
    move input
    >> fire input
    >> reset input.userInput

stepObject: Float -> Phase -> Phase
stepObject t { position, velocity } = { position = position -*- Math.SL2R.exp t velocity, velocity = velocity}

move : Input -> GameState -> GameState
move { timeDelta } state  
    = { state |
        firstPlayer <- stepObject timeDelta state.firstPlayer,
        secondPlayer <- stepObject timeDelta state.secondPlayer,
        shots <- List.map (stepObject timeDelta) state.shots }

muzzleVelocity : Phase
muzzleVelocity = { position = Matrix2 1.125 0 0 (8.0/9.0), velocity = Matrix2 0.1 0 0 -0.1}

fire : Input -> GameState -> GameState
fire { userInput } = fireChoice userInput

fireChoice : UserInput -> GameState -> GameState
fireChoice userInput state = 
    let 
      createShot player muzzle 
          = { position = muzzle.position -*- player.position, 
              velocity = muzzle.velocity -+- player.velocity }
    in case userInput of
         PlayerOneFire True -> { state | shots <- (createShot state.firstPlayer muzzleVelocity) :: state.shots}
         PlayerTwoFire True -> { state | shots <- (createShot state.secondPlayer muzzleVelocity) :: state.shots }
         _             -> state

reset : UserInput -> GameState -> GameState
reset userInput state =
    case userInput of
      Reset true -> defaultGame
      _ -> state

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
             >> \c -> Graphics.Collage.collage (w) (h // 3) [c])
        |> flow left
--}

{--}
display : (Int, Int) -> GameState -> Element
display (w, h) gameState =
    world earth gameState
       |> Graphics.Collage.scale 10
             >> (\c -> Graphics.Collage.collage w h [c])
--}

earth : Math.SL2R.Point
earth = (Matrix2 1 0 0 1)

ship1 : List Math.SL2R.Point 
ship1 = [ Matrix2 1.05 0 0 (20.0/21.0), Matrix2 (20/21.0) -0.05 0 1.05, Matrix2 (20/21.0) 0.05 0 1.05]

ship2 : List Math.SL2R.Point 
ship2 = [ Matrix2 1.05 0 0 (20.0/21.0), Matrix2 (20/21.0) -0.05 0 1.05, Matrix2 1 0 0 1, Matrix2 (20/21.0) 0.05 0 1.05]

plot : Math.SL2R.Point -> Math.SL2R.Point -> ( Float, Float )
plot origin p = p -*- (Math.SL2R.inv origin)
              |> Math.SL2R.toPoincareDisc

polygon : Math.SL2R.Point -> Math.SL2R.Point -> List Math.SL2R.Point -> Graphics.Collage.Form
polygon origin center points
    = List.map (\ p -> p -*- center) points
        |> List.map (plot origin)
        |> Graphics.Collage.polygon
        |> Graphics.Collage.outlined (Graphics.Collage.solid Color.black)

dot : Math.SL2R.Point -> Math.SL2R.Point -> Graphics.Collage.Form
dot origin point
    = Graphics.Collage.circle 0.01        
       |> Graphics.Collage.filled Color.black 
       |> Graphics.Collage.move (plot origin point)


world : Math.SL2R.Point -> GameState -> Graphics.Collage.Form 
world origin { firstPlayer, secondPlayer, shots} =
     ( polygon origin firstPlayer.position ship1 )
     :: ( polygon origin secondPlayer.position ship2 )
     :: List.map (\s -> dot origin s.position) shots
        |> Graphics.Collage.group


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





