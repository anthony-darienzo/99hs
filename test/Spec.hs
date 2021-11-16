import qualified Q1t10
import qualified Q11t20
import qualified Q21t28
import qualified Q31t41

main :: IO ()
main = do
    let failQ1t10 = head [n | n <- [1..], not (Q1t10.tests n)]
    if failQ1t10 <= 10 then 
        error $ "Test failed at Q" ++ show failQ1t10
            else putStrLn "Q1-10 Passed!"
    let failQ11t20 = head [n | n <- [11..], not (Q11t20.tests n)]
    if failQ11t20 <= 20 then
        error $ "Test failed at Q" ++ show failQ11t20
            else putStrLn "Q11-20 Passed!"
    let failQ21t28 = head [n | n <- [21..], not (Q21t28.tests n)]
    if failQ21t28 <= 28 then
        error $ "Test failed at Q" ++ show failQ21t28
            else putStrLn "Q21-28 Passed!"
    let failQ31t41 = head [n | n <- [31..], not (Q31t41.tests n)]
    if failQ31t41 <= 41 then
        error $ "Test failed at Q" ++ show failQ31t41
            else putStrLn "Q31-41 Passed!"
    putStrLn "Tests complete!"
