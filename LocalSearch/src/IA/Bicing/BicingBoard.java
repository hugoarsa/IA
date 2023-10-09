package IA.Bicing;



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
}




