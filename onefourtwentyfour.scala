import scala.collection.mutable.{Map => MutMap}

//Max score may either be None (nobody yet qualified) or (score, isTied)
type MaxScoreState = Option[(Int, Boolean)]
//Round States are of the form (numPlayers, playersBehind, pot, Option(highScore, isTied))
type RoundState = (Int, Int, Int, MaxScoreState)
//State of a player during his turn - (partialScore, one, four, diceLeft)
type PlayerState = (Int,Boolean, Boolean, Int)
//Turn states are of the form (roundState,partialScore,one,four,diceLeft)
type TurnState = (RoundState,PlayerState)
type PlayerStrategy = Map[(RoundState,PlayerState,Seq[Int]),PlayerState]

val numPlayers = 1
//max pot at start of game.  Can go up to max + num players by end of game (when nobody qualifies)
val maxPot = 10
val numDice = 3

//Player starts will all dice and no score/qualification
val startPlayerState:PlayerState = (0,false,false,numDice)
//Round starts with all players free, ante from each player, and nobody qualified
//Note that playersBehind is inclusive of current player, so initialized at numPlayers
val startRoundState:RoundState = (numPlayers,numPlayers,numPlayers,None)

//Collection of all possible scores
val possibleScores: Seq[MaxScoreState] = None +: (numDice to (numDice*6)).flatMap(
    score => Seq(Some(score,false),Some(score,true)) )

//Collection of all turn states, for each number of diceLeft
val possiblePlayerStates = //: Map[Int,Seq[TurnState]]
    (0 to numDice).map { diceLeft => 
        val curSeq = for (one <- Seq(false,true);
            four <- Seq(false,true);
            numPointsDice <- Some(2);//Some(numDice - diceLeft - (if (one) 1 else 0) - (if (four) 1 else 0));
            partialScore <- (numPointsDice to 6*numPointsDice);
            if (numPointsDice >= 0)) yield (partialScore,one,four,diceLeft)
        (diceLeft,curSeq)
    }.toMap

//Collection of all ways a player can end its turn
val possibleEndPlayerStates: Seq[PlayerState] = 
    for (endScore <- (numDice - 2) to 6*(numDice - 2);
        one <- Seq(true,false);
        four <- Seq(true,false)) yield (endScore,one,four,0)

//Collection of all states a round can be in, for each number of playersBehind.
val possibleRoundStates: Map[Int,Seq[RoundState]] = 
    (0 to numPlayers).map { playersBehind =>
        val curSeq = for (score <- possibleScores;
            pot <- 0 to maxPot + numPlayers - playersBehind) yield (numPlayers,playersBehind,pot,score)
        (playersBehind, curSeq)
    }.toMap

//Pre-compute a map of dice roll probabilities.  this is (numdice -> (combination -> probability))
val diceProbabilityMaps = {
    //Use this to enumerate over dice outcomes
    def getDiceOutcomes(n:Int): Seq[List[Int]] = if (n == 0) 
        Seq(List()) else getDiceOutcomes(n-1) flatMap {
            outcome => (1 to 6).map(_ +: outcome)
        }
    (1 to 6) map { i =>
        val outcomes = getDiceOutcomes(i)
        val denom = outcomes.length.toDouble
        //Note we use _.sorted.reverse to identify equivalent combinations
        (i,outcomes groupBy (_.sorted.reverse) map {case (k,l) => (k->l.length/denom)})
    } toMap
}

def rsDistFromRSPS(rs:RoundState,psDist: Map[PlayerState,Double]):Map[RoundState,Double] = {
    //Helper function for dealing with a single playstate
    //Given a (presumably final) playstate, return the new round state
    def updateRS(rs:RoundState,ps:PlayerState):RoundState = {
        val (playerScore, one, four, _) = ps
        rs match {
            //Common to all - decrement playersBehind, pass through numPlayers

            //Did not qualify - increase pot
            case (numPlayers, playersBehind, pot, maxScore) if (!one | !four) =>
                (numPlayers, playersBehind-1, pot+1, maxScore)
            //First to qualify - update highscore
            case (numPlayers, playersBehind, pot, None) =>
                (numPlayers, playersBehind-1, pot, Some((playerScore,false)))
            //Qualified but did not meet high score - high score stays unchanged
            case (numPlayers, playersBehind, pot, Some((highScore,isTied))) if (playerScore < highScore) =>
                (numPlayers, playersBehind-1, pot, Some((highScore,isTied)))
            //Qualified and beat high score - new untied high score
            case (numPlayers, playersBehind, pot, Some((highScore,_))) if (playerScore > highScore) =>
                (numPlayers, playersBehind-1, pot, Some((playerScore,false)))
            //Qualified and matched high score - high score is tied (maybe still)
            case (numPlayers, playersBehind, pot, Some((highScore,_))) if (playerScore == highScore) =>
                (numPlayers, playersBehind-1, pot, Some((highScore,true)))
        }
    }

    val roundDist = MutMap[RoundState,Double]().withDefaultValue(0.0)
    for ((playState,playProb) <- psDist) {
        val newRS = updateRS(rs,playState)
        roundDist(newRS) += playProb
    }
    roundDist.toMap
}

//Given a RoundState and a contingent strategy, calculate the distribution over the resulting Round States
def applyStrategy(rs:RoundState,strat:PlayerStrategy): Map[RoundState,Double] = {
    //This will be a map from dice remaining to (mutable map from player state to probability)
    // val playerDist: scala.collection.immutable.Map[Int,Map[PlayerState,Double]] =
    //     (0 to 6) map (i => (i,Map[PlayerState,Double]().withDefaultValue(0.0))) toMap
    val playerDist = ((0 to 6) map (i => (i,MutMap[PlayerState,Double]().withDefaultValue(0.0))) toMap)
    //Initially we have 6 dice remaining w/probability 1
    playerDist(6)(startPlayerState) = 1.0
    //From high dice remaining to low, apply strategy and track probability of new states 
    for (diceLeft <- (numDice to 1 by -1); 
        (roll,rollProb) <- diceProbabilityMaps(diceLeft); 
        (playState,playProb) <- playerDist(diceLeft)) {
        val newPlayState = strat(rs,playState,roll)
        val newDiceLeft = newPlayState._4
        //Add this conditional probability to the new state (which has fewer dice)
        //Note we could refactor and not keep calculating this 1/outcomes.
        playerDist(newDiceLeft)(newPlayState) += playProb * rollProb
    }
    //Now that we've pushed everything to 0 dice we have the player state distribution at the end of the turn.
    //Need to convert this to to a distribution over RoundState.
    rsDistFromRSPS(rs,playerDist(0).toMap)
}

//Given a distribution over roundstates and a contingent strategy, calculate the distribution after applying the strategy
def rsDistFromDistAndStrategy(rsDist:Map[RoundState,Double],strat:PlayerStrategy):Map[RoundState,Double] = {
    val roundDist = MutMap[RoundState,Double]().withDefaultValue(0.0)
    for ((rs,rsProb) <- rsDist) {
        val newRSDist = applyStrategy(rs,strat)
        for ((newRS, newProb) <- newRSDist) {
            roundDist(newRS) += newProb * rsProb
        }
    }
    roundDist.toMap
}

//Given a player state and round state (both presumably final), what's the player's score?
//Assumption for simplification:  Pot is split on a push
def PSRS2score(rs:RoundState,ps:PlayerState):Double = {
    val (playerScore, one, four, _) = ps
    val qualPenalty = if (one & four) 0.0 else 1.0
    (rs match {
             //Nobody qualified - it's a push
             case (numPlayers, _, pot, None) => pot / numPlayers.toDouble
             //Tie - it's a push
             case (numPlayers, _, pot, Some((_,true))) => pot / numPlayers.toDouble
             //Did not qualify, not a push - got nothing
             case _ if (!one | !four) => 0.0
             //Qualified and not a tie and you had highest - you win pot
             case (_, _, pot, Some((highScore,false))) if (highScore == playerScore) => pot
             //Otherwise get nothing
             case _ => 0.0
         }) - qualPenalty
}

val optStrategy = {
    strategy = MutMap[(RoundState,PlayerState,Seq[Int]),PlayerState]()
    stateVal = MutMap[(RoundState,PlayerState),Double]()

    //Use this to memoize getting from a roundstate to a distribution of endstates 
    val stateToEnd = (0 to numPlayers).map {i => (i,MutMap[RoundState,Map[RoundState,Double]]())}.toMap
    possibleScores.foreach {rs => stateToEnd(0)(rs) = Map(rs -> 1.0)}

    for (playersBehind <- 1 to numPlayers) {
        if (playersBehind == 1) {
            // Can take score at face value
        } else {
            // Do something about converting roundstate to endstate
        }
        for (pot <- 1 to maxPot + (numPlayers - playersBehind);
            rs <- possibleScores) {
            for (one <- Seq(false,true);
                four <- Seq(false,true)) {
                maxDiceLeft = numDice - (if (one) 1 else 0) - (if (four) 1 else 0)
                //Initialize for 0 dice left
                for (turnScore <- numDice to 6*numDice) {
                    val ps = (turnScore,one,four,0)
                    stateVal((rs,ps)) = PSRS2score(rs,ps)  //Except this isn't really the right way to score it
                }
                //Now build up strategies and values from 1 die left to maxDiceLeft
            }
            //At this point have strategy and state for all playerstates in current round
        }
        //At this point have strategy and state for everything in this playersBehind
    }   
    stateVal
 }

//Need a map from a state during the round to the distribution over end score states, 
//assuming downstream players play optimally.  (Idea is you simulate to end of turn,
//look at where that leaves next player, and get the distribution going forward.)
val roundToEnd = Map[RoundState,Seq[(Double,MaxScoreState)]]()
//Initialize this - if no more playersBehind, it's simply the current score.
for (pot <- 0 to maxPot;
    score <- possibleScores) {
    roundToEnd((numPlayers,0,pot,score)) = Seq((1.0,score))
}

//Need a map from a state during the round to optimal contingent strategies for that player's turn
//A contingent strategy is a map from a player state and rolled dice to a new player state
//We'll convert this to an immutable map once done constructing it
val optStratMap = MutMap[(RoundState,PlayerState,Seq[Int]),PlayerState]()




val blah = MutMap[Int,Int]()
blah += (1->2)
blah += (3->4)
blah.keys map {blah(_)}