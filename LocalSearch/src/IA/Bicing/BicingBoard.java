package IA.Bicing;
import java.util.Random;
import java.util.Optional;
import java.util.PriorityQueue;
import java.util.Arrays;

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
    private boolean[] start_stations;
    
    /// Vector que contiene de la estacion i el impacto que ejercemos sobre ella
    private int[] impact_stations;
    
    /// Valor heurístico 1
    private int gain;
    
    /// Valor heurístico 2
    private int cost;
    
    // Infinito
    static private int inf = 1000000000;

    /////////////////////////////////////////
    /////////////BASIC METHODS///////////////
    /////////////////////////////////////////
    
    public int getNumberBikes() {
    	return nbikes;
    }
    
    public int getNumberStations() {
    	return nstations;
    }
    
    public int getNumberTrucks() {
    	return ntrucks;
    }
    
    public Estaciones getStations() {
    	return stations;
    }
    
    public int[][] getDistances(){
    	return distances;
    }
    
    public Route[] getRouteAssignations() {
    	return routes;
    }
    
    public boolean[] getOriginStations() {
    	return start_stations;
    }
    
    public int[] getImpactStations() {
    	return impact_stations;
    }
    
    public int getGainHeuristic() {
    	return gain;
    }
    
    public int getCostHeuristic() {
    	return cost;
    }
    
    public int getLongitudTotal() {
    	int longitud = 0;
    	for(int i = 0; i < ntrucks; i++) {
    		Route r = routes[i];
        	Optional<Stop> ns1 = r.getFirstStop();
        	Optional<Stop> ns2 = r.getSecondStop();
        	Optional<Stop> ns3 = r.getThirdStop();
        	if(ns1.isPresent() && ns2.isPresent()) {
        		Stop s1 = ns1.get();
        		Stop s2 = ns2.get();
        		longitud = longitud + distances[s1.getStationId()][s2.getStationId()];
        		if(ns3.isPresent()) {
        			Stop s3 = ns3.get();
        			longitud = longitud + distances[s2.getStationId()][s3.getStationId()];
        		}
        	} 
    	}
    	return longitud;
    }
    
    public void printRoutes() {
    	for(int i = 0; i < routes.length; i++) {
    		Route route = routes[i];
    		int a, b, c;
    		int x, y ,z;
    		a = b = c = -1;
    		x = y = z = 0;
    		Optional<Stop> optFirstStop = route.getFirstStop();
    		if(optFirstStop.isPresent()) {
    			a = optFirstStop.get().getStationId();
    			x = optFirstStop.get().getImpact();
    		}
    		Optional<Stop> optSecondStop = route.getSecondStop();
    		if(optSecondStop.isPresent()) {
    			b = optSecondStop.get().getStationId();
    			y = optSecondStop.get().getImpact();
    		}
    		Optional<Stop> optThirdStop = route.getThirdStop();
    		if(optThirdStop.isPresent()) {
    			c = optThirdStop.get().getStationId();
    			z = optThirdStop.get().getImpact();
    		}
    		System.out.println("[" + a + "//" + x + "," + b + "//" + y + "," + c + "//" + z + "]");
    	}
    }
    
    public void printStations() {
    	int i = 0;
    	for (Estacion e : stations) {
    		System.out.println("STATION " + i);
    		System.out.println("Demanda " + e.getDemanda());
    		System.out.println("Next " + e.getNumBicicletasNext());
    		System.out.println("NoUsadas " + e.getNumBicicletasNoUsadas());
    		i++;
    	}
    }
    /////////////////////////////////////////
    /////////////COPY CONSTRUCTOR////////////
    /////////////////////////////////////////
    
    public BicingBoard(BicingBoard other) {
    	this.nbikes = other.getNumberBikes();
    	this.nstations = other.getNumberStations();
    	this.ntrucks = other.getNumberTrucks();
    	Route[] routesOther = other.getRouteAssignations();
    	this.routes = new Route[routesOther.length];
    	for(int i = 0; i < routesOther.length; i++) {
    		this.routes[i] = routesOther[i].shallowCopy();
    	}
    	int[] impactsOther = other.getImpactStations();
    	this.impact_stations = new int[impactsOther.length];
    	for(int i = 0; i < impactsOther.length; i++) {
    		this.impact_stations[i] = impactsOther[i];
    	}
    	boolean[] originsOther = other.getOriginStations();
    	this.start_stations = new boolean[originsOther.length];
    	for(int i = 0; i < originsOther.length; i++) {
    		this.start_stations[i] = originsOther[i];
    	}
    	this.gain = other.getGainHeuristic();
    	this.cost = other.getCostHeuristic();
    }
    
    
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
        
        for (int i = 0; i < ntrucks; ++i)
        	routes[i] = new Route(null,null,null);
        
        start_stations = new boolean[nstations];
        
        for (int i = 0; i < nstations; ++i)
        	start_stations[i] = false;

        impact_stations = new int[nstations];
        
        for (int i = 0; i < nstations; ++i)
        	impact_stations[i] = 0;
        
        distances = calculateDistanceMatrix(e);

        if (strat == "null"){
            //solucion null
            //System.out.println(routes);
    		//System.out.println(start_stations);
    		//System.out.println(impact_stations);
            gain = 0;
            cost = 0;
        }

        else if (strat == "optim"){
            //Solución optima
            int[] max_bikes = findTopK(ntrucks);
            for (int i = 0; i < ntrucks; ++i){
                int firstStop_id = max_bikes[i];
                int numBikes1 = e.get(firstStop_id).getNumBicicletasNoUsadas();
                Stop firstStop = new Stop(firstStop_id, -numBikes1);
                start_stations[firstStop_id] = true;
                impact_stations[firstStop_id] -= numBikes1;

                int left_bikes1 = 0;
                int left_bikes2 = 0;
                
                int secondStop_id = closest(firstStop_id);
                
                if (e.get(secondStop_id).getDemanda() <= numBikes1 && e.get(secondStop_id).getDemanda() >= 0) {
                	left_bikes1 = e.get(secondStop_id).getDemanda();
                	numBikes1 -= left_bikes1;
                }
                else if (e.get(secondStop_id).getDemanda() > numBikes1){
                	left_bikes1 = e.get(secondStop_id).getDemanda();
                	numBikes1 -= left_bikes1;
                	left_bikes2 = numBikes1 - e.get(secondStop_id).getDemanda();
                	numBikes1 -= left_bikes2;
                }

                Stop secondStop = new Stop(secondStop_id, left_bikes1);
                impact_stations[secondStop_id] += left_bikes1;

                int thirdStop_id = closest(secondStop_id);
                Stop thirdStop = new Stop(thirdStop_id, left_bikes2);
                impact_stations[thirdStop_id] += left_bikes2;

                Optional<Stop> optFirstStop = Optional.of(firstStop);
                Optional<Stop> optSecondStop = Optional.of(secondStop);
                Optional<Stop> optThirdStop = Optional.of(thirdStop);

                if (canSetRoute(i, optFirstStop, optSecondStop, optThirdStop)) setRoute(i, optFirstStop, optSecondStop, optThirdStop);
                else {
                    setRoute(i, Optional.empty(), Optional.empty(), Optional.empty());
                    impact_stations[firstStop_id] += numBikes1;
                    impact_stations[secondStop_id] -= left_bikes1;
                    impact_stations[thirdStop_id] -= left_bikes2;
                }
            }

            gain = calculate_heur1_slow();
            cost = calculate_heur2_slow();
            
        }

        else if (strat == "random"){
            //Solución random
        	Random rand = new Random();
            for (int i = 0; i < ntrucks; ++i){
                int firstStop_id = rand.nextInt(nstations);
                if(start_stations[firstStop_id]){
                    setRoute(i, Optional.empty(), Optional.empty(), Optional.empty());
                }
                else{
                    int numBikes1 = e.get(firstStop_id).getNumBicicletasNoUsadas();
                    Stop firstStop = new Stop(firstStop_id, -numBikes1);
                    start_stations[firstStop_id] = true;
                    impact_stations[firstStop_id] -= numBikes1;
                    
                    int left_bikes1 = 0;
                    int left_bikes2 = 0;
                    
                    int secondStop_id = closest(firstStop_id);
                    
                    if (e.get(secondStop_id).getDemanda() <= numBikes1 && e.get(secondStop_id).getDemanda() >= 0) {
                    	left_bikes1 = e.get(secondStop_id).getDemanda();
                    	numBikes1 -= left_bikes1;
                    }
                    else if (e.get(secondStop_id).getDemanda() > numBikes1){
                    	left_bikes1 = e.get(secondStop_id).getDemanda();
                    	numBikes1 -= left_bikes1;
                    	left_bikes2 = numBikes1 - e.get(secondStop_id).getDemanda();
                    	numBikes1 -= left_bikes2;
                    }
                    
                    Stop secondStop = new Stop(secondStop_id, left_bikes1);
                    impact_stations[secondStop_id] += left_bikes1;

                    int thirdStop_id = rand.nextInt(nstations);
                    if (start_stations[thirdStop_id] || thirdStop_id == secondStop_id){
                        while (start_stations[thirdStop_id]){
                           thirdStop_id = rand.nextInt(nstations); 
                        }
                    }

                    Stop thirdStop = new Stop(thirdStop_id, left_bikes2);
                    impact_stations[thirdStop_id] += left_bikes2;
                    
	                Optional<Stop> optFirstStop = Optional.of(firstStop);
	                Optional<Stop> optSecondStop = Optional.of(secondStop);
	                Optional<Stop> optThirdStop = Optional.of(thirdStop);
	
	                if(canSetRoute(i, optFirstStop, optSecondStop, optThirdStop)) setRoute(i, optFirstStop, optSecondStop, optThirdStop);
	                else {
	                    setRoute(i, Optional.empty(), Optional.empty(), Optional.empty());
	                    impact_stations[firstStop_id] += numBikes1;
	                    impact_stations[secondStop_id] -= left_bikes1;
	                    impact_stations[thirdStop_id] -= left_bikes2;
	                }
            	}
            }
            gain = calculate_heur1_slow();
            cost = calculate_heur2_slow();
        }
    }    

    private int[] findTopK(int k) {
    	if (k <= 0 || k > stations.size()) {
            //System.out.println("Invalid value of k.");
            return new int[0];
        }

        int[] topStations = new int[k];

        int[] stationIndices = new int[stations.size()];
        for (int i = 0; i < stations.size(); i++) {
            stationIndices[i] = i;
        }

        for (int i = 0; i < k; i++) {
            int maxIndex = i;
            for (int j = i + 1; j < stations.size(); j++) {
                if (stations.get(stationIndices[j]).getNumBicicletasNoUsadas() > stations.get(stationIndices[maxIndex]).getNumBicicletasNoUsadas()) {
                    maxIndex = j;
                }
            }
            int temp = stationIndices[i];
            stationIndices[i] = stationIndices[maxIndex];
            stationIndices[maxIndex] = temp;
        }

        for (int i = 0; i < k; i++) {
            topStations[i] = stationIndices[i];
        }

        return topStations;
    }
    
    private int closest(int o){
        int dmin = inf;
        int id = 0;
        for (int i = 0; i < distances[o].length; ++i){
            if (distances[o][i] < dmin && impact_stations[i] == 0 && distances[o][i] != 0) {
                dmin = distances[o][i];
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
        } else if (s.getNumBicicletasNext()-s.getDemanda()>0){ 
        	//si legamos aqui asumimos que el impacto es negativo o 0
            //si además entra a este if (es decir esta en deficit)
            //hemos de descontar el impacto que tuvimos
            gain_i = -(Math.abs(impact_stations[s_index]) - (s.getNumBicicletasNext()-s.getDemanda()));
        } else {
        	//si no podemos quitar de entrada penalizamos todo
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
			//System.out.println("DEBUG1");
			firstStopImpact = i_optFirstStop.get().getImpact();
		}
		if(i_optSecondStop.isPresent()) {
			//System.out.println("DEBUG2");
			secondStopImpact = i_optSecondStop.get().getImpact();
		}
		if(i_optThirdStop.isPresent()) {
			//System.out.println("DEBUG3");
			thirdStopImpact = i_optThirdStop.get().getImpact();
		}
		int sum = firstStopImpact + secondStopImpact + thirdStopImpact;
		if(sum >= 0) {
			//System.out.println("IMPACTS: " + firstStopImpact + "//" + secondStopImpact + "//" + thirdStopImpact);
			//System.out.println("SUM: " + sum);
		}
		return (firstStopImpact + secondStopImpact + thirdStopImpact) >= 0;
    }
    
    public boolean canAddStop(int i_truckID, int i_stopID, int i_bikesImpact) {
    	Route route = routes[i_truckID];
		Stop stopToAdd = new Stop(i_stopID, i_bikesImpact);
		//System.out.println("DEBUG2");
    	if(!route.getFirstStop().isPresent()) {
    		//System.out.println("DEBUG3");
    		return !start_stations[i_stopID] && i_bikesImpact >= 0 && 
    				i_bikesImpact <= 30 && i_bikesImpact <= stations.get(i_stopID).getNumBicicletasNoUsadas();
    	}
    	else if (!route.getSecondStop().isPresent()) {
    		//System.out.println("DEBUG4");
    		boolean sumBool = checkSum(route.getFirstStop(), Optional.of(stopToAdd), Optional.empty());
    		return i_bikesImpact <= 0 && sumBool;
    	}
    	else if (!route.getThirdStop().isPresent()) {
    		//System.out.println("DEBUG5");
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
    
    public boolean canAddTwoStop(int i_truckID, int i_stopID, int i_bikesImpact, int i_stop2ID, int i_bikesImpact2) {
    	Route route = routes[i_truckID];
		Stop stopToAdd = new Stop(i_stopID, i_bikesImpact);
		Stop secondStopToAdd = new Stop(i_stop2ID, i_bikesImpact2);
		if(i_stopID == i_stop2ID) return false;
    	if(!route.getFirstStop().isPresent()) {
    		boolean firstStopCheck = !start_stations[i_stopID] && i_bikesImpact >= 0 && 
    				i_bikesImpact <= 30 && i_bikesImpact <= stations.get(i_stopID).getNumBicicletasNoUsadas();
    		boolean sumBool = checkSum(Optional.of(stopToAdd), Optional.of(secondStopToAdd), Optional.empty());
    		boolean secondStopCheck = i_bikesImpact2 <= 0;
    		return firstStopCheck && secondStopCheck && sumBool;
    	}
    	/*else if (!route.getSecondStop().isPresent()) {
    		boolean sumBool = checkSum(route.getFirstStop(), Optional.of(stopToAdd), Optional.of(secondStopToAdd)); 		
    		return i_bikesImpact <= 0 && i_bikesImpact2 <= 0 && sumBool;
    	}*/
    	return false;
    }
    
    public void addTwoStop(int i_truckID, int i_stopID, int i_bikesImpact, int i_stop2ID, int i_bikesImpact2) {
    	gain = gain - station_gain(i_stopID);
    	gain = gain - station_gain(i_stop2ID);
    	cost = cost + getCostGas(i_truckID);
    	
    	Stop stopToAdd = new Stop(i_stopID, i_bikesImpact);
    	Stop secondStopToAdd = new Stop(i_stop2ID, i_bikesImpact2);
    	Route route = routes[i_truckID];
    	
    	if(!route.getFirstStop().isPresent()) {
    		route.setFirstStop(stopToAdd);
    		route.setSecondStop(secondStopToAdd);
    		start_stations[i_stopID] = true;
    		impact_stations[i_stopID] -= i_bikesImpact;
    		impact_stations[i_stop2ID] -= i_bikesImpact2;
    	}
    	else {
    		route.setSecondStop(stopToAdd);
    		route.setThirdStop(secondStopToAdd);
    		impact_stations[i_stopID] -= i_bikesImpact;
    		impact_stations[i_stop2ID] -= i_bikesImpact2;
    	}
    	
    	gain = gain + station_gain(i_stopID);
    	gain = gain + station_gain(i_stop2ID);
    	cost = cost - getCostGas(i_truckID);

    }
    
    public boolean canRemoveStop(int i_truckID) {
    	Route route = routes[i_truckID];
    	if(route.getFirstStop().isPresent() || route.getSecondStop().isPresent() || route.getThirdStop().isPresent()) {
    		return true;
    	}
    	return false;
    }
    
    public void removeStop(int i_truckID) {
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
    		start_stations[stopToRemove.getStationId()] = false;
    	}
    	gain = gain - station_gain(stopToRemove.getStationId());
		int removedImpact = stopToRemove.getImpact();
		//This could be error prone depending on how java handles nulls. Don't think so but check
		impact_stations[stopToRemove.getStationId()] += removedImpact;
		
		gain = gain + station_gain(stopToRemove.getStationId());
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