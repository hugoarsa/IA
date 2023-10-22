package IA.Bicing;

import java.util.Optional;

public class Route {
    private Optional<Stop> optFirstStop;
    private Optional<Stop> optSecondStop;
    private Optional<Stop> optThirdStop;
    
    public Route() {
    	this.optFirstStop = Optional.empty();
    	this.optSecondStop = Optional.empty();
    	this.optThirdStop = Optional.empty();
    }
    
    public Route(Stop i_firstStop, Stop i_secondStop, Stop i_thirdStop) {
        this.optFirstStop = Optional.ofNullable(i_firstStop);
        this.optSecondStop = Optional.ofNullable(i_firstStop);
        this.optThirdStop = Optional.ofNullable(i_thirdStop);
    }
    
    public Route shallowCopy() {
    	Route newRoute = new Route();
    	newRoute.optFirstStop = Optional.empty();
    	newRoute.optSecondStop = Optional.empty();
    	newRoute.optThirdStop = Optional.empty();
    	if(optFirstStop.isPresent()) {
    		this.optFirstStop = Optional.of(optFirstStop.get().shallowCopy());
    	}
    	if(optSecondStop.isPresent()) {
    		this.optSecondStop = Optional.of(optSecondStop.get().shallowCopy());
    	}
    	if(optThirdStop.isPresent()) {
    		this.optThirdStop = Optional.of(optThirdStop.get().shallowCopy());
    	}
    	return newRoute;
    }

    public Optional<Stop> getFirstStop() {
        return optFirstStop;
    }

    public Optional<Stop> getSecondStop() {
        return optSecondStop;
    }

    public Optional<Stop> getThirdStop() {
        return optThirdStop;
    }

    public void setFirstStop(Stop i_firstStop) {
        this.optFirstStop = Optional.ofNullable(i_firstStop);
    }

    public void setSecondStop(Stop i_secondStop) {
        this.optSecondStop = Optional.ofNullable(i_secondStop);
    }

    public void setThirdStop(Stop i_thirdStop) {
        this.optThirdStop = Optional.ofNullable(i_thirdStop);
    }
    
}