package IA.Bicing;

import java.util.Optional;

public class Route {
    private Optional<Stop> optFirstStop;
    private Optional<Stop> optSecondStop;
    private Optional<Stop> optThirdStop;
    
    public Route(Stop i_firstStop, Stop i_secondStop, Stop i_thirdStop) {
        this.optFirstStop = Optional.ofNullable(i_firstStop);
        this.optSecondStop = Optional.ofNullable(i_firstStop);
        this.optThirdStop = Optional.ofNullable(i_thirdStop);
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