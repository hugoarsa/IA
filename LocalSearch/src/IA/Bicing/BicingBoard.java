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
    
    private int ntrucks;
    
    /// Lista con las estaciones que nos da el enunciado
    static private Estaciones stations;
    
    static private int[][] distances;
    
    /// Vector con las rutas posibles
    private Route[] routes;
    
    /// Vector que nos dice si las estaciones son de inicio
    private Boolean[] start_stations;
    
    /// Vector que nos dice como van de bicis las estaciones
    private int[] state_stations;


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
        
        state_stations = new int[nstations];
        
        // inicializar distances
        
        distances = calculateDistanceMatrix(e);

        for (int i = 0; i < ntrucks; ++i){
            routes[i].setFirstStop(null);
            routes[i].setSecondStop(null);
            routes[i].setThirdStop(null);
        }



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
        
        state_stations = new int[nstations];
        
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
    				i_bikesImpact <= 30 && i_bikesImpact <= state_stations[i_stopID];
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
        				i_newBikesImpact <= 30 && i_newBikesImpact <= state_stations[i_newStopID];
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
    /*
    public void switchStop (int i_truckID, int i_oldStopID, int i_newStopID, int i_newBikesImpact) {
    	Route route = routes[i_truckID];
    	
    }*/
}


