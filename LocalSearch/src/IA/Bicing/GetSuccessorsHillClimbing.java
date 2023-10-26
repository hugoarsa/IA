package IA.Bicing;

import aima.search.framework.SuccessorFunction;
import aima.search.framework.Successor;
import java.util.ArrayList;
import java.util.List;

public class GetSuccessorsHillClimbing implements SuccessorFunction {
	public List<Successor> getSuccessors(Object aState) {
		ArrayList<Successor> retVal = new ArrayList<Successor>();
		BicingBoard currentState = (BicingBoard) aState;
		int ntrucks = currentState.getNumberTrucks();
		int nstations = currentState.getNumberStations();
		for(int i = 0; i < ntrucks; i++) {
			for(int j = 0; j < nstations; j++) {
				for(int k = -30; k <= 30; k++) {
					if(currentState.canAddStop(i, j, k)) {
						BicingBoard successorState = new BicingBoard(currentState);
						successorState.addStop(i, j, k);
						//System.out.println("ADD STOP HEURISTIC " + successorState.getGainHeuristic());
						
						String action = "Truck " + i + " added stop " + j + 
										" with number of bikes taken/left " + k + " to their route";
						retVal.add(new Successor(action, successorState));
					}
				}
			}
		}
		
		for(int i = 0; i < ntrucks; i++) {
			for(int j = 0; j < nstations; j++) {
				for(int k = -30; k <= 30; k++) {
					for(int j2 = 0; j2 < nstations; j2++) {
						for(int k2 = -30; k2 <= 30; k2++) {
							if(currentState.canAddTwoStop(i, j, k, j2, k2)) {
								BicingBoard successorState = new BicingBoard(currentState);
								successorState.addTwoStop(i, j, k, j2, k2);
								//System.out.println("ADD STOP HEURISTIC " + successorState.getGainHeuristic());
								
								String action = "Truck " + i + " added stop " + j + 
												" with number of bikes taken/left " + k + " to their route";
								retVal.add(new Successor(action, successorState));
							}
						}
					}
				}
			}
		}
		
		//Apply operator removeStop
		for(int i = 0; i < ntrucks; i++) {
			BicingBoard successorState = new BicingBoard(currentState);
			if(currentState.canRemoveStop(i)) {
				successorState.removeStop(i);
				//System.out.println("REMOVE STOP HEURISTIC " + successorState.getGainHeuristic());
				String action = "Truck " + i + " removed the last stop from their route";
				retVal.add(new Successor(action, successorState));
			}
		}
		
		//Apply operator switchStop
		for(int i = 0; i < ntrucks; i++) {
			for(int j1 = 0; j1 < nstations; j1++) {
				for(int j2 = 0; j2 < nstations; j2++) {
					for(int k = -30; k <= 30; k++) {
						if(currentState.canSwitchStop(i, j1, j2, k)) {
							BicingBoard successorState = new BicingBoard(currentState);
							successorState.switchStop(i, j1, j2, k);
							//System.out.println("SWITCH STOP HEURISTIC " + successorState.getGainHeuristic());

							String action = "Truck " + i + " switched stop " + j1 + 
											" for stop " + j2 + " with number of bikes taken/left "
											+ k + " on their route";
							retVal.add(new Successor(action, successorState));
						}
					}
				}
			}
		}
		
		return retVal;
	}
}
