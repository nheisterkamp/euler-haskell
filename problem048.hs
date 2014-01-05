import Tools
import Data.Digits

ns = [1..1000]

fun n = n ^ n

main = putStrLn $ show
	$ unDigits 10
		$ reverse . (take 10) . reverse
			$ digits 10
				$ sum
					$ map fun ns
