class SimulationState(val mined:Double, val wallet:Double, val deposit:Double, val tick:Int)

class Simulation(val model:JohnsDilemma, var policy:Int, var maxTicks:Int){

  def simulationStep(oldState:SimulationState, tick:Int):SimulationState = {
    val minedNow=oldState.mined + model.mines
    val walletNow=oldState.wallet + model.mines
    val depositNow=oldState.deposit * (1 + model.interest)
    if(tick>0 && walletNow > model.fee && tick%policy == 0){
      new SimulationState(minedNow, 0, depositNow + walletNow - model.fee, tick)
    } else {
      new SimulationState(minedNow, walletNow, depositNow, tick)
    }
  }

  def run: SimulationState = {
    var currentState = new SimulationState(0.0, 0.0, 0.0, 0)
    for(tick <- (0 to maxTicks)){
      currentState = simulationStep(currentState, tick)
    }
    currentState
  }

}

class JohnsDilemma(val interest:Double, val fee:Double, val mines:Double){

  // Good values for the range of simulations
  val policyMin = 1
  val maxTicks = 52

  def run: Unit ={
    for(policy <- (policyMin to maxTicks)) {
      val simulation=new Simulation(this, policy, maxTicks)
      val result:SimulationState = simulation.run
      //println(s"Policy = $policy, Mined = ${result.mined}, Total = ${result.deposit + result.wallet}")
      val gain = result.deposit + result.wallet
      val profit = gain - result.mined
      val yieldRate = gain / result.mined
      println(s"Policy = $policy, Yield = ${yieldRate}, Profit = ${profit}, Gain = ${gain}")
    }
  }


}

object JohnsDilemma extends App{

  // Constant values as explained in the brief,
  // but we will allow them to be changed in later iterations.
  // The interest rate is 5% but compounded weekly, so we will be having
  // tick values of 1 week in the model.
  val DEFAULT_INTEREST = 0.05 / 52 // 5% interest, but compounded weekly.
  val DEFAULT_FEE = 500 // It doesn't matter what the currency is, as long as we are consistent
  val DEFAULT_MINING_VALUE = 150 * 7 // Mines 150 'values' per day.

  println("Hello From John's Dilemma")

  val model = new JohnsDilemma(DEFAULT_INTEREST, DEFAULT_FEE, DEFAULT_MINING_VALUE)
  model.run

}
