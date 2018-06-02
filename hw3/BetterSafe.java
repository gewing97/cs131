import java.util.concurrent.locks.ReentrantLock;

class BetterSafe implements State {
    private byte[] value;
    private byte maxval;
    private int num_locks;
    private ReentrantLock[] bs_lock;

    BetterSafe(byte[] v){
        value = v;
        maxval = 127;
        num_locks = v.length;
        bs_lock = new ReentrantLock[num_locks];
        for(int i = 0; i < num_locks; i++){
           bs_lock[i] = new ReentrantLock();
        }
    }
    BetterSafe(byte[] v, byte m){
        value = v;
        maxval = m;
        num_locks = v.length;
        bs_lock = new ReentrantLock[num_locks];
        for(int i = 0; i < num_locks; i++){
            bs_lock[i] = new ReentrantLock();
        }
    }
    
    public int size(){ return value.length; }

    public byte[] current() { return value;}

    public boolean swap(int i, int j) {
        int first;
        int second;
        if (i < j){
            first = i;
            second = j;
        }
        else{
            first = j;
            second = i;
        }
        bs_lock[first].lock();
        bs_lock[second].lock();
        if (value[i] <= 0 || value[j] >= maxval){
            bs_lock[second].unlock();
            bs_lock[first].unlock();
            return false;
        }
        value[i]--;
        value[j]++;
        bs_lock[second].unlock();
        bs_lock[first].unlock();
        return true;
    }
}