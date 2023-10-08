package IA.Bicing;

public class Route {
    private Stop firstStop;
    private Stop secondStop;
    private Stop thirdStop;

    public Route(Stop firstStop, Stop secondStop, Stop thirdStop) {
        this.firstStop = firstStop;
        this.secondStop = secondStop;
        this.thirdStop = thirdStop;
    }

    public Stop getFirstStop() {
        return firstStop;
    }

    public Stop getSecondStop() {
        return secondStop;
    }

    public Stop getThirdStop() {
        return thirdStop;
    }

    public void setFirstStop(Stop firstStop) {
        this.firstStop = firstStop;
    }

    public void setSecondStop(Stop secondStop) {
        this.secondStop = secondStop;
    }

    public void setThirdStop(Stop thirdStop) {
        this.thirdStop = thirdStop;
    }
    
}