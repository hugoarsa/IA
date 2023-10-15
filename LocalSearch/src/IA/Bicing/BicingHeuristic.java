package IA.Bicing;

import aima.search.framework.HeuristicFunction;

public class BicingHeuristic implements HeuristicFunction {
	public double getHeuristicValue(Object state){
        int h = -((BicingBoard) state).calculate_heur1_slow();
        return h;
    }
}
