
robot (name, hp, attack) = \get -> get (name, hp, attack)

myRobot = robot ("myRobot", 200, 15)


name (n, _, _) = n
hp (_, h, _) = h
attack (_, _, attack) = attack

getName aRobot = aRobot  name
getHp aRobot = aRobot hp
getAttack aRobot = aRobot attack

printRobot aRobot = aRobot $ \(name, hp, attack) -> "[" ++ name ++ "] " ++ "hp: " ++ (show hp) ++ ", attack: " ++ (show attack) 

damage aRobot attackDamage = aRobot (\(n, h, a) -> robot (n, if h > attackDamage then h - attackDamage else 0, a))


fastRobot = robot ("fastRobot", 115, 27)
slowRobot = robot ("slowRobot", 250, 14)

fight attacker target = damage target attackDamage
  where attackDamage = if (getHp attacker > 10)
                        then getAttack attacker
                        else 0

slowRobotRound1 = fight fastRobot slowRobot 
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1 
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2 
fastRobotRound1 = fight slowRobot fastRobot 
fastRobotRound2 = fight slowRobotRound1 fastRobotRound1 
fastRobotRound3 = fight slowRobotRound2 fastRobotRound2 

battle firstRobot secondRobot = if getHp attackedRobot > 0 
  then [(firstRobot, attackedRobot)] ++ battle attackedRobot firstRobot
  else [(firstRobot, attackedRobot)] 
    where attackedRobot = fight firstRobot secondRobot
    

f n1 n2 = if ark > 0 then [(n1, ark)] ++ f ark n1 else [(n1, ark)]
  where ark = (fst n2 - snd n1, snd n2) 
 

threeRoundFight = take 6 $ battle fastRobot slowRobot

main = do putStrLn $ myRobot name
