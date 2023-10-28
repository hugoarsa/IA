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
		Route[] routeAssignations = currentState.getRouteAssignations();
		/*
		//Apply operator addTwoStop
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
		*/
		//Apply operator jumpStartRoute
		for(int i = 0; i < ntrucks; i++) {
			for(int j = 0; j < nstations; j++) {
				for(int j2 = 0; j2 < nstations; j2++) {
					if(currentState.canJumpStartRoute(i, j, j2)) {
						BicingBoard successorState = new BicingBoard(currentState);
						successorState.jumpStartRoute(i, j, j2);
						
						String action = "Truck " + i + " jumpstarted their route with stop " + j + " as their" + 
										"origin stop and stop " + j2 + " as their first destination";
						retVal.add(new Successor(action, successorState));
					}
				}
			}
		}
		
		return retVal;
	}
}
