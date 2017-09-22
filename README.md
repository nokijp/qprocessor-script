# QProcessor

QProcecssor is a library written in Haskell to simulate quantum computing on a classical computer.

```haskell
import Quantum.QProcessor.Gate
import Quantum.QProcessor.Manipulator

{-
q0: |0>--H--*--*--@==
            |  |
q1: |1>-----*--⊕--@==
            |
q2: |0>-----⊕-----@==
-}
program :: Manipulator [Bool]
program = do
  q0 <- newQVar False
  transition $ hadamard q0
  q1 <- newQVar True
  q2 <- newQVar False
  transition $ toffoli q0 q1 q2
  transition $ cnot q0 q1
  mapM measure [q0, q1, q2]

main :: IO ()
main = do
  bs <- runManipulator program
  print bs
```