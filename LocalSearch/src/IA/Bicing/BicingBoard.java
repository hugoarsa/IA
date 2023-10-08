package IA.Bicing;

import javafx.util.Pair;  // Importa la clase Pair


public class BicingBoard {

	/// Numero de bicis
	static private int nbikes;
	
	/// Numero de estaciones
	static private int nstations;
	
	static private int ntrucks;
	
	/// Lista con las estaciones que nos da el enunciado
	static private Estaciones stations;
	
	/// Vector con las rutas posibles
	private Route[] routes;
	
	private Pair<Integer, Boolean>[] meta_stations;


    public BicingBoard(int nbikes, int nstations, Estaciones stations) {
        this.nbikes = nbikes;
        this.nstations = nstations;
        this.stations = stations;
        
        // Inicializar los vectores de utilización y recorridos con el tamaño adecuado
        routes = new Route[ntrucks];
        meta_stations = new Pair<Integer, Boolean>[nstations];
    }
}




