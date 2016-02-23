type Coefficient = Integral
type Grounding   = Integral
type Constant    = Integral

-- Get a solution to linear diophantene equation
solve a b c :: (Coefficient a, Constant c, Grounding g) => a -> a -> c -> (g,g)

-- Get the form of all solutions to diophantene equatio
solve a b c (x,y) = (Coefficient a, Constant c, Grounding g) => a -> a -> c -> (g,g) -> ((c,a),(c,a))
