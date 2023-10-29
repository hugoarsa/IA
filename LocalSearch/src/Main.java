import IA.Bicing.Stop;
import IA.Bicing.Route;
import IA.Bicing.Estaciones;
import IA.Bicing.Estacion;
import IA.Bicing.BicingBoard;
import IA.Bicing.GetSuccessorsHillClimbing;
import IA.Bicing.BicingHeuristic;
import IA.Bicing.BicingHeuristicComplex;
import IA.Bicing.BicingGoalTest;

import aima.search.framework.Problem;
import aima.search.framework.Search;
import aima.search.framework.SearchAgent;
import aima.search.informed.HillClimbingSearch;
import aima.search.informed.SimulatedAnnealingSearch;

import java.sql.Time;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.Scanner;  // Import the Scanner class

public class Main {
	
    public static void main(String[] args) throws Exception{

        /*
        TODO un print con las opciones del programa: -help, -end, -begin
        */
    	int init = 0;
        int nbikes = 1250;
        int nstations = 25;
        int ntrucks = 5;
        int demand = 0;
        int seed = 1234;
        int heuristic = 0;
        
        for (int i = 0; i < args.length; i++) {
          switch (args[i]) {
          case "-b":
          try {
        	  nbikes = Integer.parseInt(args[++i]);
              }
          catch (NumberFormatException e)
              {
               System.out.println(args[i] + " is not an integer.");
               System.exit(0);
              }

          break;

          case "-e":
          try {
        	  nstations = Integer.parseInt(args[++i]);
              }
          catch (NumberFormatException e)
              {
                System.out.println(args[i] + " is not a integer.");
                System.exit(0);
              }

          break;

          case "-s":
          try {
               seed = Integer.parseInt(args[++i]);
              }
          catch (NumberFormatException e)
              {
                System.out.println(args[i] + " is not an integer.");
                System.exit(0);
              }
          break;
          
          case "-i":
              try {
                   init = Integer.parseInt(args[++i]);
                  }
              catch (NumberFormatException e)
                  {
                    System.out.println(args[i] + " is not an integer.");
                    System.exit(0);
                  }
              break;

          case "-t":
          try {
        	  ntrucks =  Integer.parseInt(args[++i]);
              }
          catch (NumberFormatException e)
              {
                System.out.println(args[i] + " is not an integer.");
                System.exit(0);
              }
          break;
          
          case "-d":
              try {
            	  demand = Integer.parseInt(args[++i]);
                  }
              catch (NumberFormatException e)
                  {
                    System.out.println(args[i] + " is not a integer.");
                    System.exit(0);
                  }

          break;

          case "-h":
              try {
                  heuristic = Integer.parseInt(args[++i]);
              } catch (NumberFormatException e) {
                  System.out.println(args[i] + "is not an integer.");
                  System.exit(0);
              }
          break;

          default:
        	  // arg
              System.out.println("The option " + args[i] + " not valid.");
              System.out.println("-b [Number of packages] ");
              System.out.println("-e [Number of stations]");
              System.out.println("-t [Number of trucks]");
              System.out.println("-s [Seed]");
              System.out.println("-d [Option of demand normal/rush (0/1)]");
              System.out.println("-h [Heuristic Function just gain/complex(0/1)]");
              System.exit(0);
              break;
          }
        }
        Estaciones est = new Estaciones(nstations, nbikes, demand, seed);
        BicingBoard board = new BicingBoard(est,nbikes,ntrucks,init);
        // board.printStations();
        board.printRoutes();
        System.out.println("The initial gain is " + board.get_heur1());
        System.out.println("The initial cost is " + board.get_heur2());
        System.out.println("The initial total distance traversed is " + board.getLongitudTotal() + "m");
        if(heuristic == 0) {
            BicingHillClimbingSearch(board);
        } else if(heuristic == 1) {
        	BicingHillClimbingSearchComplex(board);
        }

     }
    
    private static void BicingHillClimbingSearch(BicingBoard board) {
        System.out.println("\nBicing HillClimbing Simple Heuristic  -->");
        try {
            long time = System.currentTimeMillis();
            Problem problem =  new Problem(board,
            				new GetSuccessorsHillClimbing(),
            				new BicingGoalTest(),
            				new BicingHeuristic());
            Search search =  new HillClimbingSearch();
            SearchAgent agent = new SearchAgent(problem,search);

            BicingBoard newBoard = (BicingBoard)search.getGoalState();
            time = System.currentTimeMillis() - time;
            newBoard.printStations();
            newBoard.printRoutes();
            System.out.println("DEBUG ACTIONS: ");
            printActions(agent.getActions());
            System.out.println("The gain is " + newBoard.get_heur1());
            System.out.println("The gain computing cost is " + newBoard.get_heur2());
            System.out.println("The total distance traversed is " + newBoard.getLongitudTotal());
            printInstrumentation(agent.getInstrumentation());
            System.out.println(time + " ms");
            System.out.println();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private static void BicingHillClimbingSearchComplex(BicingBoard board) {
        System.out.println("\nBicing HillClimbing Complex Heuristic  -->");
        try {
            long time = System.currentTimeMillis();
            Problem problem =  new Problem(board,
            				new GetSuccessorsHillClimbing(),
            				new BicingGoalTest(),
            				new BicingHeuristicComplex());
            Search search =  new HillClimbingSearch();
            SearchAgent agent = new SearchAgent(problem,search);

            BicingBoard newBoard = (BicingBoard)search.getGoalState();
            time = System.currentTimeMillis() - time;
            System.out.println("The gain is " + newBoard.get_heur1());
            System.out.println("The gain computing cost is " + newBoard.get_heur2());
            System.out.println("The total distance traversed is " + newBoard.getLongitudTotal());
            printInstrumentation(agent.getInstrumentation());
            System.out.println(time + " ms");
            System.out.println();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void printInstrumentation(Properties properties) {
        Iterator keys = properties.keySet().iterator();
        while (keys.hasNext()) {
            String key = (String) keys.next();
            String property = properties.getProperty(key);
            System.out.println(key + " : " + property);
        }

    }
    
    private static void printActions(List actions) {
        for (int i = 0; i < actions.size(); i++) {
            String action = (String) actions.get(i);
            System.out.println(action);
        }
    }
}
