import java.lang.management.ThreadMXBean;
import java.util.concurrent.ThreadLocalRandom;

class SwapTest implements Runnable {
    private long nTransitions;
    private State state;
	private ThreadMXBean mybean;
    private long cputime;
    SwapTest(long n, State s, ThreadMXBean b) {
	nTransitions = n;
	state = s;
	mybean = b;
    }

    public void run() {
	var n = state.size();
	if (n <= 1)
	    return;
	var rng = ThreadLocalRandom.current();
	var id = Thread.currentThread().threadId();

	var starttime = mybean.getThreadCpuTime(id);
	for (var i = nTransitions; 0 < i; i--)
	    state.swap(rng.nextInt(0, n), rng.nextInt(0, n));
	var endtime = mybean.getThreadCpuTime(id);
	cputime = endtime - starttime;
    }
	public long cpuTime() {
		return cputime;
		}
}
