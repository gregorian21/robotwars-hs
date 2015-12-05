type Position    = (Int, Int)
type Arena       = (Int, Int)
data Direction   = N | E | S | W            deriving (Show, Eq, Read, Enum)
data Instruction = L | R | M                deriving (Show, Eq, Read)
data Robot       = Robot Position Direction deriving (Show)

instructRobot (Robot pos dir) M           arena = Robot (advanceRobot pos dir arena) dir
instructRobot (Robot pos dir) instruction _     = Robot pos (turnRobot dir instruction)

advanceRobot (x,y) N (_,n) = if y < n then (x,y+1) else error "out of bounds"
advanceRobot (x,y) E (m,_) = if x < m then (x+1,y) else error "out of bounds"
advanceRobot (x,y) S _     = if y > 0 then (x,y-1) else error "out of bounds"
advanceRobot (x,y) W _     = if x > 0 then (x-1,y) else error "out of bounds"

turnRobot N   L = W
turnRobot W   R = N
turnRobot dir L = pred dir
turnRobot dir R = succ dir

input = ["5 5", "1 2 N", "LMLMLMLMM", "3 3 E", "MMRMMRMRRM"]
arena  = (read (head dimensions) :: Int, read (last dimensions) :: Int)
         where dimensions = words (head input)

game = parseInput (tail input)
       where parseInput []            = []
             parseInput (x:y:zs)      = (parseRobot (words x), parseInstructions y) : parseInput zs
             parseRobot (a:b:c)       = Robot (read a, read b) (read (head c))
             parseInstructions []     = []
             parseInstructions (x:xs) = ((read [x])::Instruction) : parseInstructions xs

execute []                         = []
execute ((robot, instructions):rs) = ((instructRobot robot (head instructions) arena), tail instructions) : execute rs

playGame ((r,[]):rs) = (r,[]):rs
playGame rs          = playGame (execute rs)
