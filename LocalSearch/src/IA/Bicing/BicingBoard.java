package IA.Bicing;
import java.util.Random;


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

}


