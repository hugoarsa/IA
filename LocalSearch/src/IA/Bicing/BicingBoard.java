package IA.Bicing;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Optional;

public class BicingBoard {

    /// Numero de bicis
    private int nbikes;
    
    /// Numero de estaciones
    private int nstations;
    
    /// Numero de camiones
    private int ntrucks;
    
    /// Lista con las estaciones que nos da el enunciado
    static private Estaciones stations;
    
    static private int[][] distances;
    
    /// Vector con las rutas posibles
    private Route[] routes;
    
    /// Vector que nos dice si las estaciones son de inicio
    private Boolean[] start_stations;
    
    /// Vector que contiene de la estacion i el impacto que ejercemos sobre ella
    private int[] impact_stations;
    
    private int benefit1;
    
    private int benefit2;


    //Solución vacía
    public BicingBoard(Estaciones e, int nb, int nt) {
        nbikes = nb;
        ntrucks = nt;
        nstations = e.size();
        stations = e;
        
        // Inicializar los vectores de utilización y recorridos con el tamaño adecuado
        routes = new Route[nt];
        
        start_stations = new Boolean[nstations];
        
        // inicializar las stations
        
        impact_stations = new int[nstations];
        
        // inicializar distances
        
        distances = calculateDistanceMatrix(e);

        for (int i = 0; i < ntrucks; ++i){
            routes[i].setFirstStop(null);
            routes[i].setSecondStop(null);
            routes[i].setThirdStop(null);
        }
        
        benefit1 = 0;
        benefit2 = 0;



    }

    //Solución optima
    public BicingBoard(Estaciones e, int nb, int nt) {
        nbikes = nb;
        ntrucks = nt;
        nstations = e.size();
        stations = e;
        
        // Inicializar los vectores de utilización y recorridos con el tamaño adecuado
        routes = new Route[nt];
        
        start_stations = new Boolean[nstations];
        
        // inicializar las stations
        
        impact_stations = new int[nstations];
        
        // inicializar distances
        
        distances = calculateDistanceMatrix(e);

        List max_bikes = findTopK(e, ntrucks);

        for (int i = 0; i < ntrucks; ++i){
            int firstStop_id = rand.nextInt(nstations);
            Estacion firstStop = e.get(firstStop_id);
            start_stations[firstStop_id] = 1;
            max_bikes.remove(0);


            
            Estacion secondStop = e.get(closestStops[0]);

            Estacion thirdStop = e.get(closestStops[1]);

            routes[i].setFirstStop(firstStop);
            routes[i].setSecondStop(secondStop);
            routes[i].setThirdStop(thirdStop);
        }




    }

    //Solución random
    public BicingBoard(Estaciones e, int nb, int nt) {
        nbikes = nb;
        ntrucks = nt;
        nstations = e.size();
        stations = e;
        
        // Inicializar los vectores de utilización y recorridos con el tamaño adecuado
        routes = new Route[nt];
        
        start_stations = new Boolean[nstations];
        
        // inicializar las stations
        
        state_stations = new int[nstations];
        
        // inicializar distances
        
        distances = calculateDistanceMatrix(e);

        for (int i = 0; i < ntrucks; ++i){
            int firstStop_id = rand.nextInt(nstations);
            Estacion firstStop = e.get(firstStop_id);
            start_stations[firstStop_id] = 1;

            int secondStop_id = rand.nextInt(nstations);
            if (start_stations[secondStop_id]){
                while (start_stations[secondStop_id]){
                   secondStop_id = rand.nextInt(nstations); 
                }
            }
            Estacion secondStop = e.get(secondStop_id);

            int thirdStop_id = rand.nextInt(nstations);
            if (start_stations[thirdStop_id] || thirdStop_id == secondStop_id){
                while (start_stations[thirdStop_id]){
                   thirdStop_id = rand.nextInt(nstations); 
                }
            }
            Estacion thirdStop = e.get(thirdStop_id);

            routes[i].setFirstStop(firstStop);
            routes[i].setSecondStop(secondStop);
            routes[i].setThirdStop(thirdStop);
        }
    }    

    /*!\brief Calcula la distancia entre dos estaciones
    *
    * @param [i] Estacion i
    * @param [j] Estacion j
    */
    public static int calculateDistance(Estacion i, Estacion j) {
        int ix = i.getCoordX();
        int iy = i.getCoordY();
        int jx = j.getCoordX();
        int jy = j.getCoordY();

        // Calculate the distance using the formula d(i, j) = |ix − jx| + |iy − jy|
        int distance = Math.abs(ix - jx) + Math.abs(iy - jy);
        return distance;
    }
    
    public static int[][] calculateDistanceMatrix(Estaciones e) {
        int numStations = e.size();
        int[][] distanceMatrix = new int[numStations][numStations];

        for (int i = 0; i < numStations; i++) {
            for (int j = 0; j < numStations; j++) {
                distanceMatrix[i][j] = calculateDistance(e.get(i), e.get(j));
            }
        }

        return distanceMatrix;
    }
    
    public int calculate_heur1_slow() {
    	int gain = 0;
	    for (int i = 0; i < stations.size(); i++) {
	        Estacion s = stations.get(i);
	        int gain_i = 0;
	        if(impact_stations[i]>0) {
	            gain_i = Math.min(impact_stations[i],s.getDemanda() - s.getNumBicicletasNext());
	        } else if ((s.getDemanda() - s.getNumBicicletasNext())>0){ 
	        	//si legamos aqui asumimos que el impacto es negativo o 0
	            //si además entra a este if (es decir esta en deficit)
	            //hemos de descontar el impacto que tuvimos
	            gain_i = impact_stations[i];
	        }
	        gain = gain + gain_i;
	    }
    	return gain;
    }
    
    public int calculate_heur2_slow() {
    	int gain = 0;
    		
	    	for (int i = 0; i < stations.size(); i++) {
	            Estacion s = stations.get(i);
	            int gain_i = 0;
	            if(impact_stations[i]>0) {
	            	gain_i = Math.min(impact_stations[i],s.getDemanda() - s.getNumBicicletasNext());
	            } else if ((s.getDemanda() - s.getNumBicicletasNext())>0){ 
	            	//si legamos aqui asumimos que el impacto es negativo o 0
	            	//si además entra a este if (es decir esta en deficit)
	            	//hemos de descontar el impacto que tuvimos
	            	gain_i = impact_stations[i];
	            }
	            gain = gain + gain_i;
	        }
	    	
	    	for (int i = 0; i < ntrucks; i++) {
	    		Route a = routes[i];
	    		//gain = gain + a.getCostGas();
	    	}
    	return gain;
    }

    public List findTopK(List input, int k) {
    List array = new ArrayList<>(input);
    List topKList = new ArrayList<>();

    for (int i = 0; i < k; i++) {
        int maxIndex = 0;

        for (int j = 1; j < array.size(); j++) {
            if (array.get(j) > array.get(maxIndex)) {
                maxIndex = j;
            }
        }

        topKList.add(array.remove(maxIndex));
    }

    return topKList;
    }
    
    /////////////////////////////////////////
    ///////////////OPERATORS/////////////////
    /////////////////////////////////////////
    
    private boolean checkSum(Optional<Stop> i_optFirstStop, Optional<Stop> i_optSecondStop, Optional<Stop> i_optThirdStop) {
    	int firstStopImpact = 0;
    	int secondStopImpact = 0;
		int thirdStopImpact = 0;
		if(i_optFirstStop.isPresent()) {
			firstStopImpact = i_optFirstStop.get().getImpact();
		}
		if(i_optSecondStop.isPresent()) {
			secondStopImpact = i_optSecondStop.get().getImpact();
		}
		if(i_optThirdStop.isPresent()) {
			thirdStopImpact = i_optThirdStop.get().getImpact();
		}
		return (firstStopImpact + secondStopImpact + thirdStopImpact) >= 0;
    }
    
    public boolean canAddStop(int i_truckID, int i_stopID, int i_bikesImpact) {
    	Route route = routes[i_truckID];
		Stop stopToAdd = new Stop(i_stopID, i_bikesImpact);
    	if(!route.getFirstStop().isPresent()) {
    		return !start_stations[i_stopID] && i_bikesImpact >= 0 && 
    				i_bikesImpact <= 30 && i_bikesImpact <= stations.get(i_stopID).getNumBicicletasNoUsadas();
    	}
    	else if (!route.getSecondStop().isPresent()) {
    		boolean sumBool = checkSum(route.getFirstStop(), Optional.of(stopToAdd), Optional.empty());
    		return i_bikesImpact <= 0 && sumBool;
    	}
    	else if (!route.getThirdStop().isPresent()) {
    		boolean sumBool = checkSum(route.getFirstStop(), route.getSecondStop(), Optional.of(stopToAdd));
    		return i_bikesImpact <= 0 && sumBool;
    	}
    	return false;
    }
    
    public void addStop(int i_truckID, int i_stopID, int i_bikesImpact) {
    	Stop stopToAdd = new Stop(i_stopID, i_bikesImpact);
    	Route route = routes[i_truckID];
    	if(!route.getFirstStop().isPresent()) {
    		route.setFirstStop(stopToAdd);
    		start_stations[i_stopID] = true;
    		state_stations[i_stopID] -= i_bikesImpact;
    	}
    	else if (!route.getSecondStop().isPresent()) {
    		route.setSecondStop(stopToAdd);
    		state_stations[i_stopID] -= i_bikesImpact;
    	}
    	else {
    		route.setThirdStop(stopToAdd);
    		state_stations[i_stopID] -= i_bikesImpact;
    	}
    }
    
    
    public boolean canRemoveStop(int i_truckID, int i_stopID) {
    	Route route = routes[i_truckID];
    	if(route.getFirstStop().isPresent() || route.getSecondStop().isPresent() || route.getThirdStop().isPresent()) {
    		return true;
    	}
    	return false;
    }
    
    public void removeStop(int i_truckID, int i_stopID) {
    	Stop stopToRemove;
    	Route route = routes[i_truckID];
    	if(route.getThirdStop().isPresent()) {
    		stopToRemove = route.getThirdStop().get();
    		route.setThirdStop(null);
    	}
    	else if (route.getSecondStop().isPresent()) {
    		stopToRemove = route.getSecondStop().get();
    		route.setSecondStop(null);
    	}
    	else {
    		stopToRemove = route.getFirstStop().get();
    		route.setFirstStop(null);
    		start_stations[i_stopID] = false;
    	}
		int removedImpact = stopToRemove.getImpact();
		//This could be error prone depending on how java handles nulls. Don't think so but check
		state_stations[i_stopID] += removedImpact;
    }
    
    public boolean canSwitchStop(int i_truckID, int i_oldStopID, int i_newStopID, int i_newBikesImpact) {
    	Route route = routes[i_truckID];
    	Stop oldStop;
    	Stop newStop = new Stop(i_newStopID, i_newBikesImpact);
    	if(route.getFirstStop().isPresent()) {
    		oldStop = route.getFirstStop().get();
    		if(oldStop.getStationId() == i_oldStopID) {
    			boolean sumBool = checkSum(Optional.of(newStop), route.getSecondStop(), route.getThirdStop());
    			return !start_stations[i_newStopID] && i_newBikesImpact >= 0 && sumBool &&
        				i_newBikesImpact <= 30 && i_newBikesImpact <= stations.get(i_newStopID).getNumBicicletasNoUsadas();
    		}
    		else if (route.getSecondStop().isPresent()) {
    			oldStop = route.getSecondStop().get();
    			if(oldStop.getStationId() == i_oldStopID) {
    				boolean sumBool = checkSum(route.getFirstStop(), Optional.of(newStop), route.getThirdStop());
    	    		return i_newBikesImpact <= 0 && sumBool;
    			}
    			else if (route.getThirdStop().isPresent()) {
    				oldStop = route.getThirdStop().get();
    				if(oldStop.getStationId() == i_oldStopID) {
        				boolean sumBool = checkSum(route.getFirstStop(), route.getSecondStop(), Optional.of(newStop));
        	    		return i_newBikesImpact <= 0 && sumBool;
    				}
    			}
    		}
    	}
    	return false;
    }
    
    public void switchStop (int i_truckID, int i_oldStopID, int i_newStopID, int i_newBikesImpact) {
    	Route route = routes[i_truckID];
    	Stop oldStop = route.getFirstStop().get();
    	Stop newStop = new Stop(i_newStopID, i_newBikesImpact);
    	if (oldStop.getStationId() == i_oldStopID) {
    		start_stations[i_oldStopID] = false;
    		route.setFirstStop(newStop);
    		start_stations[i_newStopID] = true;
    	}
    	else {
    		oldStop = route.getSecondStop().get();
    		if(oldStop.getStationId() == i_oldStopID) {
    			route.setSecondStop(newStop);
    		}
    		else {
    			oldStop = route.getThirdStop().get();
    			route.setThirdStop(newStop);
    		}
    	}
		state_stations[i_oldStopID] += oldStop.getImpact();
		state_stations[i_newStopID] -= i_newBikesImpact;
    }
    
    public boolean canSetRoute (int i_truckID, Optional<Stop> i_optFirstStop, Optional<Stop> i_optSecondStop, Optional<Stop> i_optThirdStop) {
    	Route route = routes[i_truckID];
    	boolean sumBool = checkSum(i_optFirstStop, i_optSecondStop, i_optThirdStop);
    	boolean firstStopCheck, secondStopCheck, thirdStopCheck;
    	firstStopCheck = secondStopCheck = thirdStopCheck = true;
    	int originalFirstStopID = -1;
    	if(route.getFirstStop().isPresent()) {
    		originalFirstStopID = route.getFirstStop().get().getStationId();
    	}
    	if(i_optFirstStop.isPresent()) {
    		Stop firstStop = i_optFirstStop.get();
    		int firstStopImpact = firstStop.getImpact();
    		int firstStopID = firstStop.getStationId();
    		firstStopCheck = (!start_stations[firstStopID] || (originalFirstStopID == firstStopID)) &&
    						 firstStopImpact >= 0 && firstStopImpact <= 30 && 
    						 firstStopImpact <= stations.get(firstStopID).getNumBicicletasNoUsadas();
    	}
    	else {
    		firstStopCheck = !i_optSecondStop.isPresent() && !i_optThirdStop.isPresent();

    	}
    	if(i_optSecondStop.isPresent()) {
    		secondStopCheck = i_optSecondStop.get().getImpact() <= 0;
    	}
    	else {
    		secondStopCheck = !i_optThirdStop.isPresent();
    	}
    	if(i_optThirdStop.isPresent()) {
    		thirdStopCheck = i_optThirdStop.get().getImpact() <= 0;
    	}
    	return sumBool && firstStopCheck && secondStopCheck && thirdStopCheck;
    }
    
    public void removeOldRoute(int i_truckID) {
    	Route route = routes[i_truckID];
    	if(route.getThirdStop().isPresent()) {
    		Stop thirdStop = route.getThirdStop().get();
    		state_stations[thirdStop.getStationId()] += thirdStop.getImpact();
    		route.setThirdStop(null);
    	}
    	if(route.getThirdStop().isPresent()) {
    		Stop secondStop = route.getSecondStop().get();
    		state_stations[secondStop.getStationId()] += secondStop.getImpact();
    		route.setSecondStop(null);
    	}
    	if(route.getFirstStop().isPresent()) {
    		Stop firstStop = route.getFirstStop().get();
    		state_stations[firstStop.getStationId()] += firstStop.getImpact();
    		start_stations[firstStop.getStationId()] = false;
    		route.setFirstStop(null);
    	}
    }
    
    public void addNewRoute (int i_truckID, Optional<Stop> i_optFirstStop, Optional<Stop> i_optSecondStop, Optional<Stop> i_optThirdStop) {
    	Route route = routes[i_truckID];
    	route.setFirstStop(null);
    	route.setSecondStop(null);
    	route.setThirdStop(null);
    	if(i_optFirstStop.isPresent()) {
    		Stop firstStop = i_optFirstStop.get();
    		route.setFirstStop(firstStop);
    		state_stations[firstStop.getStationId()] -= firstStop.getImpact();
    		start_stations[firstStop.getStationId()] = true;
    		if(i_optSecondStop.isPresent()) {
        		Stop secondStop = i_optSecondStop.get();
        		route.setSecondStop(secondStop);
        		state_stations[secondStop.getStationId()] -= secondStop.getImpact();
        		if(i_optThirdStop.isPresent()) {
            		Stop thirdStop = i_optThirdStop.get();
            		route.setThirdStop(thirdStop);
            		state_stations[thirdStop.getStationId()] -= thirdStop.getImpact();
            	}
        	}
    	}
    }
    
    public void setRoute (int i_truckID, Optional<Stop> i_optFirstStop, Optional<Stop> i_optSecondStop, Optional<Stop> i_optThirdStop) {
    	removeOldRoute(i_truckID);
    	addNewRoute(i_truckID, i_optFirstStop, i_optSecondStop, i_optThirdStop);
    }
    
    public boolean canRemoveImpact (int i_truckID, int i_stopID, int i_impactRemoved) {
    	Route route = routes[i_truckID];
    	Stop modifiedStop;
    	if(route.getFirstStop().isPresent()) {
    		Stop firstStop = route.getFirstStop().get();
    		if(firstStop.getStationId() == i_stopID) {
    			modifiedStop = new Stop(i_stopID, firstStop.getImpact() - i_impactRemoved);
    			int newImpact = modifiedStop.getImpact();
    			boolean sumBool = checkSum(Optional.of(modifiedStop), route.getSecondStop(), route.getThirdStop());
    			return  newImpact >= 0 && sumBool && newImpact <= 30 && 
        				newImpact <= stations.get(i_stopID).getNumBicicletasNoUsadas();
    		}
    		if(route.getSecondStop().isPresent()) {
    			Stop secondStop = route.getSecondStop().get();
        		if(secondStop.getStationId() == i_stopID) {
        			modifiedStop = new Stop(i_stopID, secondStop.getImpact() - i_impactRemoved);
        			int newImpact = modifiedStop.getImpact();
        			boolean sumBool = checkSum(route.getFirstStop(), Optional.of(modifiedStop), route.getThirdStop());
        			return  newImpact <= 0 && sumBool;
        		}
        		if(route.getThirdStop().isPresent()) {
        			Stop thirdStop = route.getThirdStop().get();
            		if(thirdStop.getStationId() == i_stopID) {
            			modifiedStop = new Stop(i_stopID, thirdStop.getImpact() - i_impactRemoved);
            			int newImpact = modifiedStop.getImpact();
            			boolean sumBool = checkSum(route.getFirstStop(), route.getSecondStop(), Optional.of(modifiedStop));
            			return  newImpact <= 0 && sumBool;
            		}
        		}
    		}
    	}
    	return false;
    }
    
    public void removeImpact (int i_truckID, int i_stopID, int i_impactRemoved) {
    	Route route = routes[i_truckID];
    	Stop stopModified = route.getFirstStop().get();
    	if(stopModified.getStationId() == i_stopID) {
    		stopModified = new Stop(i_stopID, stopModified.getImpact() - i_impactRemoved);
    		route.setFirstStop(stopModified);
    		state_stations[i_stopID] -= i_impactRemoved;
    	}
    	else {
    		stopModified = route.getSecondStop().get();
    		if(stopModified.getStationId() == i_stopID) {
        		stopModified = new Stop(i_stopID, stopModified.getImpact() - i_impactRemoved);
        		route.setSecondStop(stopModified);
        		state_stations[i_stopID] -= i_impactRemoved;
        	}
    		else {
        		stopModified = route.getThirdStop().get();
        		if(stopModified.getStationId() == i_stopID) {
            		stopModified = new Stop(i_stopID, stopModified.getImpact() - i_impactRemoved);
            		route.setThirdStop(stopModified);
            		state_stations[i_stopID] -= i_impactRemoved;
            	}
        	}
    	}
    }
    
    public boolean canAddImpact (int i_truckID, int i_stopID, int i_impactAdded) {
    	Route route = routes[i_truckID];
    	Stop modifiedStop;
    	if(route.getFirstStop().isPresent()) {
    		Stop firstStop = route.getFirstStop().get();
    		if(firstStop.getStationId() == i_stopID) {
    			modifiedStop = new Stop(i_stopID, firstStop.getImpact() + i_impactAdded);
    			int newImpact = modifiedStop.getImpact();
    			boolean sumBool = checkSum(Optional.of(modifiedStop), route.getSecondStop(), route.getThirdStop());
    			return  newImpact >= 0 && sumBool && newImpact <= 30 && 
        				newImpact <= stations.get(i_stopID).getNumBicicletasNoUsadas();
    		}
    		if(route.getSecondStop().isPresent()) {
    			Stop secondStop = route.getSecondStop().get();
        		if(secondStop.getStationId() == i_stopID) {
        			modifiedStop = new Stop(i_stopID, secondStop.getImpact() + i_impactAdded);
        			int newImpact = modifiedStop.getImpact();
        			boolean sumBool = checkSum(route.getFirstStop(), Optional.of(modifiedStop), route.getThirdStop());
        			return  newImpact <= 0 && sumBool;
        		}
        		if(route.getThirdStop().isPresent()) {
        			Stop thirdStop = route.getThirdStop().get();
            		if(thirdStop.getStationId() == i_stopID) {
            			modifiedStop = new Stop(i_stopID, thirdStop.getImpact() + i_impactAdded);
            			int newImpact = modifiedStop.getImpact();
            			boolean sumBool = checkSum(route.getFirstStop(), route.getSecondStop(), Optional.of(modifiedStop));
            			return  newImpact <= 0 && sumBool;
            		}
        		}
    		}
    	}
    	return false;
    }
    
    public void addImpact (int i_truckID, int i_stopID, int i_impactAdded) {
    	Route route = routes[i_truckID];
    	Stop stopModified = route.getFirstStop().get();
    	if(stopModified.getStationId() == i_stopID) {
    		stopModified = new Stop(i_stopID, stopModified.getImpact() + i_impactAdded);
    		route.setFirstStop(stopModified);
    		state_stations[i_stopID] += i_impactAdded;
    	}
    	else {
    		stopModified = route.getSecondStop().get();
    		if(stopModified.getStationId() == i_stopID) {
        		stopModified = new Stop(i_stopID, stopModified.getImpact() + i_impactAdded);
        		route.setSecondStop(stopModified);
        		state_stations[i_stopID] += i_impactAdded;
        	}
    		else {
        		stopModified = route.getThirdStop().get();
        		if(stopModified.getStationId() == i_stopID) {
            		stopModified = new Stop(i_stopID, stopModified.getImpact() + i_impactAdded);
            		route.setThirdStop(stopModified);
            		state_stations[i_stopID] += i_impactAdded;
            	}
        	}
    	}
    }
    
}

