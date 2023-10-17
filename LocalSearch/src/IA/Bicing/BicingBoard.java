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
    
    /// Valor heurístico 1
    private int gain;
    
    /// Valor heurístico 2
    private int cost;
    
    // Infinito
    static private int inf = 1000000000;

    /////////////////////////////////////////
    /////////////INITIAL STATE///////////////
    /////////////////////////////////////////


    public BicingBoard(Estaciones e, int nb, int nt, String strat) {
        nbikes = nb;
        ntrucks = nt;
        nstations = e.size();
        stations = e;
        
        // Inicializar los vectores de utilización y recorridos con el tamaño adecuado
        routes = new Route[nt];
        
        start_stations = new Boolean[nstations];
        start_stations = {false};
        
        // inicializar las stations
        
        impact_stations = new int[nstations];
        impact_stations = {0};
        
        // inicializar distances
        
        distances = calculateDistanceMatrix(e);

        if (strat == "null"){
            //solucion null
            for (int i = 0; i < ntrucks; ++i){
                routes[i].setFirstStop(null);
                routes[i].setSecondStop(null);
                routes[i].setThirdStop(null);
            }
            
            gain = 0;
            cost = 0;
        }

        else if (strat == "optim"){
            //Solución optima
            List max_bikes = findTopK(ntrucks);
            for (int i = 0; i < ntrucks; ++i){
                int firstStop_id = max_bikes[i];
                int numBikes1 = Estaciones.get(firstStop_id).getNumBicicletasNoUsadas();
                Stop firstStop = new Stop(firstStop_id, numBikes1);
                start_stations[firstStop_id] = true;
                impact_stations[firstStop_id] += numBikes1;

                int secondStop_id = closest(firstStop_id);
                int numBikes2 = Estaciones.get(secondStop_id).getNumBicicletasNoUsadas();
                Stop secondStop = new Stop(secondStop_id, numBikes2);
                impact_stations[secondStop_id] += numBikes2;

                int thirdStop_id = closest(firstStop_id);
                int numBikes3 = Estaciones.get(thirdStop_id).getNumBicicletasNoUsadas();
                Stop thirdStop = new Stop(thirdStop_id, numBikes3);
                impact_stations[thirdStop_id] += numBikes3;

                Optional<Stop> optFirstStop = Optional.of(firstStop);
                Optional<Stop> optSecondStop = Optional.of(secondStop);
                Optional<Stop> optThirdStop = Optional.of(thirdStop);

                if (canSetRoute(i, optFirstStop, optSecondStop, optThirdStop)) setRoute(i, optFirstStop, optSecondStop, optThirdStop);
                else {
                    setRoute(i, Optional.empty(), Optional.empty(), Optional.empty());
                    impact_stations[firstStop_id] -= numBikes1;
                    impact_stations[secondStop_id] -= numBikes2;
                    impact_stations[thirdStop_id] -= numBikes3;
                }
            }

            calculate_heur1_slow();
            calculate_heur2_slow();
            
        }

        else if (strat == "random"){
            //Solución random
            for (int i = 0; i < ntrucks; ++i){
                int firstStop_id = rand.nextInt(nstations);
                if(start_stations[firstStop_id]){
                    setRoute(i, Optional.empty(), Optional.empty(), Optional.empty());
                }
                else{
                    int numBikes1 = Estaciones.get(firstStop_id).getNumBicicletasNoUsadas();
                    Stop firstStop = new Stop(firstStop_id, numBikes1);
                    start_stations[firstStop_id] = true;
                    impact_stations[firstStop_id] += numBikes1;

                    int secondStop_id = rand.nextInt(nstations);
                    if (start_stations[secondStop_id]){
                        while (start_stations[secondStop_id]){
                           secondStop_id = rand.nextInt(nstations); 
                        }
                    }
                    int numBikes2 = Estaciones.get(secondStop_id).getNumBicicletasNoUsadas();
                    Stop secondStop = new Stop(secondStop_id, numBikes2);
                    impact_stations[secondStop_id] += numBikes2;

                    int thirdStop_id = rand.nextInt(nstations);
                    if (start_stations[thirdStop_id] || thirdStop_id == secondStop_id){
                        while (start_stations[thirdStop_id]){
                           thirdStop_id = rand.nextInt(nstations); 
                        }
                    }
                    int numBikes3 = Estaciones.get(thirdStop_id).getNumBicicletasNoUsadas();
                    Stop thirdStop = new Stop(thirdStop_id, numBikes3);
                    impact_stations[thirdStop_id] += numBikes3;
                }
                Optional<Stop> optFirstStop = Optional.of(firstStop);
                Optional<Stop> optSecondStop = Optional.of(secondStop);
                Optional<Stop> optThirdStop = Optional.of(thirdStop);

                if(canSetRoute(i, optFirstStop, optSecondStop, optThirdStop)) setRoute(i, optFirstStop, optSecondStop, optThirdStop);
                else {
                    setRoute(i, Optional.empty(), Optional.empty(), Optional.empty());
                    impact_stations[firstStop_id] -= numBikes1;
                    impact_stations[secondStop_id] -= numBikes2;
                    impact_stations[thirdStop_id] -= numBikes3;
                }
            }
            calculate_heur1_slow();
            calculate_heur2_slow();
        }
    }    
    

    private int[] findTopK(int k) {
    int array[] = new int[stations.size()];
    int topKList[];

    for (int x = 0; x < stations.size(); x++){
        array[x] = stations.get(x.getNumBicicletasNoUsadas());
    }

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

    private int closest(int o){
        int dmin = inf;
        int id = 0;
        for (int i = 0; i < d.size(); ++i){
            if (distances[o][i] < dmin && v[i] == 0 && distances[o][i] != 0) {
                dmax = d[o][i];
                id = i;
            }
        }
        return id;
    }
    
    /////////////////////////////////////////
    ///////////////DISTANCES/////////////////
    /////////////////////////////////////////

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
    
    /////////////////////////////////////////
    ///////////////HEURISTIC/////////////////
    /////////////////////////////////////////
    
    /*!\brief Calcula el heurístico simple de forma lenta para la fase inicial O(S)
    *
    */
    public int get_heur1() {
    	return gain;
    }
    
    /*!\brief Calcula el heurístico simple de forma lenta para la fase inicial O(S)
    *
    */
    public int get_heur2() {
    	return gain - cost;
    }
    
    /*!\brief Calcula el heurístico simple de forma lenta para la fase inicial O(S)
    *
    */
    public int calculate_heur1_slow() {
    	int acc = 0;
	    for (int i = 0; i < stations.size(); i++) {
	        acc = acc + station_gain(i);
	    }
    	return acc;
    }
    
    /*!\brief Calcula el heuristíco complejo (teniendo en cuenta carburante, de forma lenta para la solucion inicial O(S + R)
    *
    */
    public int calculate_heur2_slow() {
    	int acc = 0;    	
    	for (int i = 0; i < ntrucks; i++) {
	    	acc = acc + getCostGas(i);
	    }
    	return acc;
    }
    
    /*!\brief Calcula la ganancia o pérdida asociada a una estacion
    *
    * @param [r] La ruta de la que queremos calcular su coste en gasolina
    */
    private int station_gain(int s_index) {
    	Estacion s = stations.get(s_index);
    	int gain_i = 0;
        if(impact_stations[s_index]>0) {
            gain_i = Math.min(impact_stations[s_index],s.getDemanda() - s.getNumBicicletasNext());
        } else if ((s.getDemanda() - s.getNumBicicletasNext())>0){ 
        	//si legamos aqui asumimos que el impacto es negativo o 0
            //si además entra a este if (es decir esta en deficit)
            //hemos de descontar el impacto que tuvimos
            gain_i = impact_stations[s_index];
        }
        return gain_i;
    }
    
    
    
    /*!\brief Calcula el coste de gasolina de completar la ruta r
    *
    * @param [r] La ruta de la que queremos calcular su coste en gasolina
    */
    private int getCostGas(int r_index) {
    	Route r = routes[r_index];
    	Optional<Stop> ns1 = r.getFirstStop();
    	Optional<Stop> ns2 = r.getSecondStop();
    	Optional<Stop> ns3 = r.getThirdStop();
    	
    	int i_cost = 0;
    	
    	if(ns1.isPresent() && ns2.isPresent()) {
    		Stop s1 = ns1.get();
    		Stop s2 = ns2.get();
    		int taken = - s1.getImpact();
    		//coste = coste + km * euro/km
    		i_cost = i_cost + distances[s1.getStationId()][s2.getStationId()] * ((taken + 9)/10);
    		int remain = taken - s2.getImpact();
    		if(ns3.isPresent()) {
    			Stop s3 = ns3.get();
    			i_cost = i_cost + distances[s2.getStationId()][s3.getStationId()] * ((remain + 9)/10);
    		}
    	} 
    	
    	return i_cost;
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
    	gain = gain - station_gain(i_stopID);
    	cost = cost + getCostGas(i_truckID);
    	
    	Stop stopToAdd = new Stop(i_stopID, i_bikesImpact);
    	Route route = routes[i_truckID];
    	
    	if(!route.getFirstStop().isPresent()) {
    		route.setFirstStop(stopToAdd);
    		start_stations[i_stopID] = true;
    		impact_stations[i_stopID] -= i_bikesImpact;
    	}
    	else if (!route.getSecondStop().isPresent()) {
    		route.setSecondStop(stopToAdd);
    		impact_stations[i_stopID] -= i_bikesImpact;
    	}
    	else {
    		route.setThirdStop(stopToAdd);
    		impact_stations[i_stopID] -= i_bikesImpact;
    	}
    	
    	gain = gain + station_gain(i_stopID);
    	cost = cost - getCostGas(i_truckID);
    }
    
    
    public boolean canRemoveStop(int i_truckID, int i_stopID) {
    	Route route = routes[i_truckID];
    	if(route.getFirstStop().isPresent() || route.getSecondStop().isPresent() || route.getThirdStop().isPresent()) {
    		return true;
    	}
    	return false;
    }
    
    public void removeStop(int i_truckID, int i_stopID) {
    	gain = gain - station_gain(i_stopID);
    	cost = cost + getCostGas(i_truckID);
    	
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
		impact_stations[i_stopID] += removedImpact;
		
		gain = gain + station_gain(i_stopID);
    	cost = cost - getCostGas(i_truckID);
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
    	gain = gain - station_gain(i_oldStopID) - station_gain(i_newStopID);
    	cost = cost + getCostGas(i_truckID);
    	
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
		impact_stations[i_oldStopID] += oldStop.getImpact();
		impact_stations[i_newStopID] -= i_newBikesImpact;
		
		gain = gain + station_gain(i_oldStopID) + station_gain(i_newStopID);
    	cost = cost - getCostGas(i_truckID);
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
    		impact_stations[thirdStop.getStationId()] += thirdStop.getImpact();
    		route.setThirdStop(null);
    	}
    	if(route.getThirdStop().isPresent()) {
    		Stop secondStop = route.getSecondStop().get();
    		impact_stations[secondStop.getStationId()] += secondStop.getImpact();
    		route.setSecondStop(null);
    	}
    	if(route.getFirstStop().isPresent()) {
    		Stop firstStop = route.getFirstStop().get();
    		impact_stations[firstStop.getStationId()] += firstStop.getImpact();
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
    		impact_stations[firstStop.getStationId()] -= firstStop.getImpact();
    		start_stations[firstStop.getStationId()] = true;
    		if(i_optSecondStop.isPresent()) {
        		Stop secondStop = i_optSecondStop.get();
        		route.setSecondStop(secondStop);
        		impact_stations[secondStop.getStationId()] -= secondStop.getImpact();
        		if(i_optThirdStop.isPresent()) {
            		Stop thirdStop = i_optThirdStop.get();
            		route.setThirdStop(thirdStop);
            		impact_stations[thirdStop.getStationId()] -= thirdStop.getImpact();
            	}
        	}
    	}
    }
    
    public void setRoute (int i_truckID, Optional<Stop> i_optFirstStop, Optional<Stop> i_optSecondStop, Optional<Stop> i_optThirdStop) {
    	
    	int gain1 = 0, gain2 = 0, gain3 = 0;
    	if(i_optFirstStop.isPresent()) gain1 = station_gain(i_optFirstStop.get().getStationId());
    	if(i_optSecondStop.isPresent()) gain2 = station_gain(i_optSecondStop.get().getStationId());
    	if(i_optThirdStop.isPresent()) gain3 = station_gain(i_optThirdStop.get().getStationId());
    	gain = gain - gain1 - gain2 - gain3;
    	
    	cost = cost + getCostGas(i_truckID);
    	
    	removeOldRoute(i_truckID);
    	addNewRoute(i_truckID, i_optFirstStop, i_optSecondStop, i_optThirdStop);

    	gain1 = 0; gain2 = 0; gain3 = 0;
    	if(i_optFirstStop.isPresent()) gain1 = station_gain(i_optFirstStop.get().getStationId());
    	if(i_optSecondStop.isPresent()) gain2 = station_gain(i_optSecondStop.get().getStationId());
    	if(i_optThirdStop.isPresent()) gain3 = station_gain(i_optThirdStop.get().getStationId());
    	gain = gain + gain1 + gain2 + gain3;
    	
    	cost = cost - getCostGas(i_truckID);
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
    	gain = gain - station_gain(i_stopID);
    	cost = cost + getCostGas(i_truckID);
    	
    	Route route = routes[i_truckID];
    	Stop stopModified = route.getFirstStop().get();
    	if(stopModified.getStationId() == i_stopID) {
    		stopModified = new Stop(i_stopID, stopModified.getImpact() - i_impactRemoved);
    		route.setFirstStop(stopModified);
    		impact_stations[i_stopID] -= i_impactRemoved;
    	}
    	else {
    		stopModified = route.getSecondStop().get();
    		if(stopModified.getStationId() == i_stopID) {
        		stopModified = new Stop(i_stopID, stopModified.getImpact() - i_impactRemoved);
        		route.setSecondStop(stopModified);
        		impact_stations[i_stopID] -= i_impactRemoved;
        	}
    		else {
        		stopModified = route.getThirdStop().get();
        		if(stopModified.getStationId() == i_stopID) {
            		stopModified = new Stop(i_stopID, stopModified.getImpact() - i_impactRemoved);
            		route.setThirdStop(stopModified);
            		impact_stations[i_stopID] -= i_impactRemoved;
            	}
        	}
    	}
    	
    	gain = gain + station_gain(i_stopID);
    	cost = cost - getCostGas(i_truckID);
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
    	gain = gain - station_gain(i_stopID);
    	cost = cost + getCostGas(i_truckID);
    	
    	Route route = routes[i_truckID];
    	Stop stopModified = route.getFirstStop().get();
    	if(stopModified.getStationId() == i_stopID) {
    		stopModified = new Stop(i_stopID, stopModified.getImpact() + i_impactAdded);
    		route.setFirstStop(stopModified);
    		impact_stations[i_stopID] += i_impactAdded;
    	}
    	else {
    		stopModified = route.getSecondStop().get();
    		if(stopModified.getStationId() == i_stopID) {
        		stopModified = new Stop(i_stopID, stopModified.getImpact() + i_impactAdded);
        		route.setSecondStop(stopModified);
        		impact_stations[i_stopID] += i_impactAdded;
        	}
    		else {
        		stopModified = route.getThirdStop().get();
        		if(stopModified.getStationId() == i_stopID) {
            		stopModified = new Stop(i_stopID, stopModified.getImpact() + i_impactAdded);
            		route.setThirdStop(stopModified);
            		impact_stations[i_stopID] += i_impactAdded;
            	}
        	}
    	}
    	
    	gain = gain + station_gain(i_stopID);
    	cost = cost - getCostGas(i_truckID);
    }
    
}

